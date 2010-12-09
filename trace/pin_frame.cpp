#include <iostream>
#include <cstdio>
#include <cstring>

#include "pin_frame.h"

using namespace std;
using namespace pintrace;

#define READ(in, val) in.read((char *) &val, (streamsize) sizeof(val))
#define WRITE(out, val) out.write((const char *) &val, (streamsize) sizeof(val))

Frame *Frame::unserialize(istream &in, bool noskip)
{

   unsigned char packed_type;
   FrameType type;
   uint16_t size;
   Frame *f = NULL;

   READ(in, packed_type);
   READ(in, size);

   type = (FrameType) packed_type;

   if (!noskip) {
      printf("Skipping frame, %d bytes.\n", size - 3);
      in.ignore(size - 3);
      return NULL;
   }

   switch(type) {
   case FRM_STD:
      f = new StdFrame;
      break;
   case FRM_KEY:
      f = new KeyFrame;
      break;
   case FRM_NONE:
   default:
      // TODO: Error handling here.
      printf("ERROR: Unknown frame type %d!\n", type);
      break;
   }

   f->unserializePart(in);
   return f;

}


// Writes a serialized version of the frame into buffer 'buf'. The
// buffer must be of at least this.size bytes long.
// Returns a pointer to the character just after the last character
// written to the buffer.
ostream &Frame::serialize(ostream &out, uint16_t sz)
{

   sz += 3;                     // 3 == sizeof((char) type) + sizeof(sz)

   // Compress type value into a single byte.
   unsigned char packed_type = (char) type;

   WRITE(out, packed_type);
   WRITE(out, sz);
   return out;

}

void StdFrame::clearCache()
{
   memset((void *) &cachemask, 0, MAX_CACHEMASK_BTYES);
}

ostream &StdFrame::serialize(ostream &out, uint16_t sz)
{


   // Note: ((x-1) >> 3) + 1 === ceil(x / 8.0)
   uint32_t masklen = ((values_count - 1) >> 3) + 1;

   // 9 = sizeof(addr) + sizeof(tid) + sizeof(lengths)
   sz += 9
      + (insn_length * sizeof(char))
      + masklen
      + (values_count * sizeof(uint32_t));

   ostream &out2 = Frame::serialize(out, sz);

   WRITE(out2, addr);
   WRITE(out2, tid);

   // The two length values (insn_length and values_count) are packed into a
   // single byte like so:
   // |7654|3210|
   //    |    \-- values_count
   //    \------- insn_length

   uint8_t lengths = (values_count & 0xf) | (insn_length << 4);

   WRITE(out2, lengths);

   out2.write((const char *) &rawbytes, insn_length * sizeof(char));
   out2.write((const char *) &cachemask, masklen);
   out2.write((const char *) &values, values_count * sizeof(uint32_t));

   return out2;

}

istream &StdFrame::unserializePart(istream &in)
{

   READ(in, addr);
   READ(in, tid);

   uint8_t lengths;
   READ(in, lengths);

   insn_length = lengths >> 4;
   values_count = lengths & 0xf;

   // Be a bit paranoid.
   if (insn_length > MAX_INSN_BYTES) insn_length = MAX_INSN_BYTES;

   in.read((char *) &rawbytes, insn_length * sizeof(char));

   // Note: ((x-1) >> 3) + 1 === ceil(x / 8.0)
   in.read((char *) &cachemask, ((values_count - 1) >> 3) + 1);

   in.read((char *) &values, values_count * sizeof(uint32_t));

   return in;

}

ostream &KeyFrame::serialize(ostream &out, uint16_t sz)
{

   ostream &out2 = Frame::serialize(out, sz + 56);
   
   WRITE(out2, pos);

   WRITE(out2, eax);
   WRITE(out2, ebx);
   WRITE(out2, ecx);
   WRITE(out2, edx);
   WRITE(out2, esi);
   WRITE(out2, edi);
   WRITE(out2, esp);
   WRITE(out2, ebp);
   WRITE(out2, eflags);

   WRITE(out2, cs);
   WRITE(out2, ds);
   WRITE(out2, ss);
   WRITE(out2, es);
   WRITE(out2, fs);
   WRITE(out2, gs);

   return out2;

}

istream &KeyFrame::unserializePart(istream &in)
{

   READ(in, pos);

   READ(in, eax);
   READ(in, ebx);
   READ(in, ecx);
   READ(in, edx);
   READ(in, esi);
   READ(in, edi);
   READ(in, esp);
   READ(in, ebp);
   READ(in, eflags);

   READ(in, cs);
   READ(in, ds);
   READ(in, ss);
   READ(in, es);
   READ(in, fs);
   READ(in, gs);

   return in;

}

void KeyFrame::setAll(uint32_t eax, uint32_t ebx, uint32_t ecx, uint32_t edx,
                      uint32_t esi, uint32_t edi, uint32_t esp, uint32_t ebp,
                      uint32_t eflags,
                      uint16_t cs, uint16_t ds, uint16_t ss,
                      uint16_t es, uint16_t fs, uint16_t gs)
{

   this->eax = eax;
   this->ebx = ebx;
   this->ecx = ecx;
   this->edx = edx;
   this->esi = esi;
   this->edi = edi;
   this->esp = esp;
   this->ebp = ebp;
   this->eflags = eflags;

   this->cs = cs;
   this->ds = ds;
   this->ss = ss;
   this->es = es;
   this->fs = fs;
   this->gs = gs;

}


ostream &LoadModuleFrame::serialize(ostream &out, uint16_t sz)
{

   ostream &out2 = Frame::serialize(out, sz + 80);

   WRITE(out2, low_addr);
   WRITE(out2, high_addr);
   WRITE(out2, start_addr);
   WRITE(out2, load_offset);
   WRITE(out2, name);

   return out2;

}

istream &LoadModuleFrame::unserializePart(istream &in)
{

   READ(in, low_addr);
   READ(in, high_addr);
   READ(in, start_addr);
   READ(in, load_offset);
   READ(in, name);

   return in;

}


ostream &SyscallFrame::serialize(ostream &out, uint16_t sz)
{
   
   ostream &out2 = Frame::serialize(out, sz + 12 + 4*MAX_SYSCALL_ARGS);

   WRITE(out2, addr);
   WRITE(out2, tid);
   WRITE(out2, callno);

   for (int i = 0; i < MAX_SYSCALL_ARGS; i++)
      WRITE(out2, args[i]);

   return out2;

}

istream &SyscallFrame::unserializePart(istream &in)
{

   READ(in, addr);
   READ(in, tid);
   READ(in, callno);

   for (int i = 0; i < MAX_SYSCALL_ARGS; i++)
      READ(in, args[i]);

   return in;

}


#if 0
int main(int argc, char **argv)
{

   StdFrame f;
   f.clearCache();

   f.setCached(2);
   f.setCached(3);
   f.unsetCached(7);
   f.setCached(7);

   for(unsigned int i = 0; i < MAX_VALUES_COUNT; i++) {
      cout << i << ": ";
      if (f.isCached(i))
         cout << "cached.";
      else
         cout << "not cached.";
      cout << endl;
   }

}
#endif
