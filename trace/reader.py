#!/usr/bin/python

# Note: Requires the distorm disassembly library.

import sys
import struct

import distorm

def main():

    trace = open(sys.argv[1])

    # First we read in the header.

    buf = trace.read(12)
    (magic, frame_count, toc_offset) = struct.unpack("III", buf)

    print "Magic = %x" % (magic,)
    print "Frame count = %d" % (frame_count,)
    print "=" * 20
    
    # TRACE_ICACHE_SIZE 1024
    # TRACE_ICACHE_MASK 0x3ff
    icache = {}

    for fc in xrange(0, frame_count):

        # TODO: Implement instruction cache, and data cache.

        buf = trace.read(1)
        (ty,) = struct.unpack("B", buf)

        buf = trace.read(2)
        (sz,) = struct.unpack("H", buf)

        print "Frame %d:" % (fc,)
        print "  Type: %d" % (ty,)
        print "  Size: %d" % (sz,)

        if ty == 2:
            # Standard frame.

            (addr, tid) = struct.unpack("II", trace.read(8))
            (plen,) = struct.unpack("B", trace.read(1))

            insn_length = plen >> 4
            values_count = plen & 0xf

            # Handle instruction caching.
            rawbytes = ""
            if insn_length == 0:
                rawbytes = icache[addr & 0x3ff]
            else:
                rawbytes = trace.read(insn_length)
                icache[addr & 0x3ff] = rawbytes
            #endif

            cachemask = trace.read(((values_count - 1) >> 3) + 1)

            values = []
            for i in xrange(0, values_count):
                values.append(struct.unpack("I", trace.read(4))[0])
            #endfor

            disasm = ""
            try:
                disasm = distorm.Decode(addr, rawbytes, distorm.Decode32Bits)[0][2]
            except:
                disasm = "(invalid)"
            #endtry

            print "  Addr: 0x%x" % (addr,)
            print "  Insn length: %d" % (insn_length,)
            print "  Insn: %s" % (disasm,)
            print "  Values count: %d" % (values_count,)

            valstr = ", ".join((hex(x) for x in values))
            print "  Values: %s" % (valstr,)

        #endif

        print "-" * 20




    #endfor

#enddef

main()
