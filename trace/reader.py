#!/usr/bin/python

# Note: Requires the distorm disassembly library.

TRACE_MAGIC = 0x43525442
TRACE_VERSION = 3
TRACE_ICACHE_SIZE = 1024
TRACE_ICACHE_MASK = 0x3ff


MAX_SYSCALL_ARGS = 5

FRM_NONE = 0
FRM_KEY = 1
FRM_STD = 2
FRM_LOADMOD = 3
FRM_SYSCALL = 4

import sys
import struct

import distorm

# Helper function.
def _unpack(f, fmt):
    return struct.unpack(fmt, f.read(struct.calcsize(fmt)))
#enddef

class FrameExn(Exception):
    pass

class Frame(object):
    def __init__(self, ty, sz):
        self.type = ty
        self.size = sz
    #enddef

    @staticmethod
    def unserialize(f, skip=False, skipNone=True, skipUnknown=False):

        (ty, sz) = _unpack(f, "=BH")

        # Just skip over this frame in the input file.
        if skip:
            f.read(sz)
            return None
        #endif

        if ty == FRM_STD:
            return StdFrame.unserializePart(ty, sz, f)
        elif ty == FRM_KEY:
            return KeyFrame.unserializePart(ty, sz, f)
        elif ty == FRM_LOADMOD:
            return LoadModuleFrame.unserializePart(ty, sz, f)
        elif ty == FRM_SYSCALL:
            return SyscallFrame.unserializePart(ty, sz, f)
        elif ty == FRM_NONE:
            if skipNone:
                f.read(sz)
                return None
            else:
                raise FrameExn("Invalid frame type.")
            #endif
        elif skipUnknown:
            f.read(sz)
            return None
        else:
            raise FrameExn("Unknown frame type.")
        #endif

    #enddef

#endclass

class StdFrame(Frame):
    def __init__(self, ty, sz, addr, tid, insn_length,
                 values_count, rawbytes, cachemask, values):
        Frame.__init__(self, ty, sz)
        self.addr = int(addr)
        self.tid = int(tid)
        self.insn_length = int(insn_length)
        self.values_count = int(values_count)
        self.rawbytes = str(rawbytes)
        self.cachemask = str(cachemask)
        self.values = list(values)
    #enddef

    @staticmethod
    def unserializePart(ty, sz, f):

        (addr, tid, plen) = _unpack(f, "=IIB")

        insn_length = plen >> 4
        values_count = plen & 0xf

        rawbytes = ""
        if insn_length > 0:
            rawbytes = f.read(insn_length)
        #endif

        cachemask = f.read(((values_count - 1) >> 3) + 1)

        values = [_unpack(f, "I")[0] for i in xrange(0, values_count)]

        return StdFrame(ty, sz,
                        addr, tid, insn_length, values_count,
                        rawbytes, cachemask, values)

    #enddef

#endclass

class KeyFrame(Frame):
    def __init__(self, sz, ty,
                 pos,
                 eax, ebx, ecx, edx, esi, edi, esp, ebp, eflags,
                 cs, ds, ss, es, fs, gs):
        Frame.__init__(self, sz, ty)

        self.pos = pos

        self.eax = eax
        self.eax = eax
        self.ebx = ebx
        self.ecx = ecx
        self.edx = edx
        self.esi = esi
        self.edi = edi
        self.esp = esp
        self.ebp = ebp
        self.eflags = eflags
        self.cs = cs
        self.ds = ds
        self.ss = ss
        self.es = es
        self.fs = fs
        self.gs = gs
    #enddef

    @staticmethod
    def unserializePart(ty, sz, f):
        return KeyFrame(ty, sz,
                        *struct.unpack("=Q9I6H", f.read(56)))
    #enddef
        
#endclass

class LoadModuleFrame(Frame):
    def __init__(self, sz, ty,
                 low_addr, high_addr, start_addr, load_offset, name):
        Frame.__init__(self, sz, ty)
        self.low_addr = int(low_addr)
        self.high_addr = int(high_addr)
        self.start_addr = int(start_addr)
        self.load_offset = int(load_offset)
        self.name = str(name)
    #enddef

    @staticmethod
    def unserializePart(ty, sz, f):
        (loaddr, hiaddr, staddr, off, name) = _unpack(f, "=4I64s")
        name = name[:name.find("\0")]
        return LoadModuleFrame(ty, sz,
                               loaddr, hiaddr, staddr, off, name)
    #enddef

#endclass

class SyscallFrame(Frame):
    def __init__(self, sz, ty,
                 addr, tid, callno, args):
        Frame.__init__(self, sz, ty)
        self.addr = int(addr)
        self.tid = int(tid)
        self.callno = int(callno)
        self.args = list(args)
    #enddef

    @staticmethod
    def unserializePart(ty, sz, f):
        (addr, tid, callno) = _unpack(f, "=III")
        args = [_unpack(f, "I") for x in xrange(0, MAX_SYSCALL_ARGS)]
        return SyscallFrame(ty, sz,
                            addr, tid, callno, args)
    #enddef
#endclass

class TraceExn(Exception):
    pass
#endclass

class TraceHeader:
    def __init__(self, magic, version, frame_count, toc_offset):
        self.magic = magic
        self.version = version
        self.frame_count = frame_count
        self.toc_offset = toc_offset
    #enddef
#endclass

class TraceReader:

    def __init__(self, f):

        self.file = f
        self.hdr = TraceHeader(*_unpack(self.file, "=IIQQ"))
        self.toc = []
        self.count = 0
        self.icache = {}

        if not self.checkHeader():
            raise TraceExn("Invalid header.")

        if self.hdr.toc_offset > 0:
            # Read in the toc.

            print "toc_offset = %d" % (self.hdr.toc_offset,)
            self.file.seek(self.hdr.toc_offset)

            toc_size = _unpack(self.file, "I")[0]

            print "toc_size = %d" % (toc_size,)

            self.toc = [_unpack(self.file, "I")[0] \
                        for i in xrange(0, toc_size)]

            # Move file pointer back to start of frames.
            self.file.seek(24)
            
        #endif

    #enddef

    def __iter__(self):
        return self
    #enddef

    def checkHeader(self):
        return (self.hdr.magic == TRACE_MAGIC) and \
               (self.hdr.version == TRACE_VERSION)
    #enddef

    def next(self):

        if self.count >= self.hdr.frame_count:
            raise StopIteration
        #endif
        self.count += 1
        frm =  Frame.unserialize(self.file)

        if type(frm) == StdFrame:

            # Handle instruction caching.
            if frm.insn_length == 0:
                frm.rawbytes = self.icache[frm.addr & TRACE_ICACHE_MASK]
                frm.insn_length = len(frm.rawbytes)
            else:
                self.icache[frm.addr & TRACE_ICACHE_MASK] = frm.rawbytes
            #endif

        #endif

        return frm
    #enddef        

#endclass

def main():

    tracefile = open(sys.argv[1])
    tr = TraceReader(tracefile)

    for frm in tr:
        pass
    #endfor

    return

    for frm in tr:

        if not frm:
            print "(unknown or empty frame)"
            continue
        #endif

        print "%s" % (frm.__class__.__name__,)
        print "  Size: %d" % (frm.size,)

        if type(frm) == StdFrame:
            disasm = ""
            try:
                disasm = distorm.Decode(frm.addr,
                                        frm.rawbytes,
                                        distorm.Decode32Bits)[0][2]
            except:
                disasm = "(invalid)"
            #endtry

            print "  Addr: 0x%x" % (frm.addr,)
            print "  Thread ID: 0x%x" % (frm.tid,)
            print "  Insn length: %d" % (frm.insn_length,)
            print "  Insn: %s" % (disasm,)
            print "  Values count: %d" % (frm.values_count,)

            valstr = ", ".join((hex(x) for x in frm.values))
            print "  Values: %s" % (valstr,)

        elif type(frm) == KeyFrame:
            # Keyframe.

            print "  Position: %d" % (frm.pos,)
            print "  Registers:"
            print "    eax: 0x%x" % (frm.eax,)
            print "    ebx: 0x%x" % (frm.ebx,)
            print "    ecx: 0x%x" % (frm.ecx,)
            print "    edx: 0x%x" % (frm.edx,)
            print "    esi: 0x%x" % (frm.esi,)
            print "    edi: 0x%x" % (frm.edi,)
            print "    esp: 0x%x" % (frm.esp,)
            print "    ebp: 0x%x" % (frm.ebp,)
            print "    eflags: 0x%x" % (frm.eflags,)
            print "     cs: 0x%x" % (frm.cs,)
            print "     ds: 0x%x" % (frm.ds,)
            print "     ss: 0x%x" % (frm.ss,)
            print "     es: 0x%x" % (frm.es,)
            print "     fs: 0x%x" % (frm.fs,)
            print "     gs: 0x%x" % (frm.gs,)

        elif type(frm) == LoadModuleFrame:
            # LoadModule frame.

            print "  Name: %s" % (frm.name,)
            print "  Low address: 0x%x" % (frm.low_addr,)
            print "  High address: 0x%x " % (frm.high_addr,)
            print "  Starting address: 0x%x" % (frm.start_addr,)
            print "  Load offset: 0x%x" % (frm.load_offset,)

        elif type(frm) == SyscallFrame:
            # Syscall frame.

            print "  Addr: 0x%x" % (frm.addr,)
            print "  Thread ID: 0x%x" % (frm.tid,)
            print "  Syscall number: %d" % (frm.callno,)
            
            argstr = ", ".join(("0x%x" % (x,) for x in frm.args))
            print "  Arguments: %s" % (argstr,)

        else:
            # Unknown frame.

            print "Unknown frame."

        #endif

        print "-" * 20

    #endfor

#enddef

main()
