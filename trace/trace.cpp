#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "frame.h"
#include "trace.h"

using namespace std;

TraceReader::TraceReader(const char *filename)
   : frm_pos(0)
{

   infile.open(filename, ios::in);

   infile.read((char *) &header, sizeof(TraceHeader));

   // Setup the cache.
   memset(icache, 0, TRACE_ICACHE_SIZE * MAX_INSN_BYTES);

   // TODO: Check magic value.
   // TODO: Read in and build the TOC structure.

}

uint32_t TraceReader::count() const
{
   return header.frame_count;
}

uint32_t TraceReader::pos() const
{
   return frm_pos;
}

// Move frame pointer to the specified offset. Returns true iff
// successful.
bool TraceReader::seek(uint32_t offset)
{

   // TODO: Make use of the TOC.

   if (offset >= header.frame_count)
      return false;

   while (frm_pos < offset)
      next(false);

   return true;

}

// Returns the current frame being pointed to and advances the frame
// pointer to the next frame.
Frame *TraceReader::next(bool noskip)
{

   Frame *f = Frame::unserialize(infile, noskip);
   if(++frm_pos > header.frame_count) // we are at the eof.
      return NULL;

   if (f->type == FRM_STD) {

      StdFrame *sf = (StdFrame *) f;

      uint32_t ipos = sf->addr & TRACE_ICACHE_MASK;

      if (sf->insn_length == 0) {
         // The instruction bytes are to be found in the cache.
         // The instruction length must be set again
         memcpy(sf->rawbytes, icache[ipos], MAX_INSN_BYTES);
         memcpy(&sf->insn_length, icache[ipos] + MAX_INSN_BYTES, 1);

      } else {
         // Update cache with current instruction bytes.
         memcpy(icache[ipos], sf->rawbytes, sf->insn_length);
         memcpy(icache[ipos] + MAX_INSN_BYTES, &sf->insn_length, 1);

      }

   }

   return f;

}

// Returns true iff the frame pointer is beyond the last frame.
bool TraceReader::eof() const
{ return frm_pos >= header.frame_count; }


// Creates a new TraceWriter that outputs the trace to the file named
// "filename". Will truncate the file.
TraceWriter::TraceWriter(const char *filename)
   : frm_count(0)
{

   outfile.open(filename,
                ios_base::out |
                ios_base::trunc);

   // Write in a temporary trace header.

   TraceHeader hdr;
   hdr.magic = 0;
   hdr.frame_count = 0;
   hdr.toc_offset = 0;

   outfile.write((const char *) &hdr, sizeof(TraceHeader));

   // Setup the cache.
   memset(icache, 0, TRACE_ICACHE_SIZE * MAX_INSN_BYTES);

}

// Returns the current number of frames in the trace.
uint32_t TraceWriter::count() const
{
   return frm_count;
}

// Adds a new frame to the trace.
void TraceWriter::add(Frame &frm)
{

   if (frm.type == FRM_STD) {
      // Use instruction and data caches to reduce frame size.

      StdFrame &sf = (StdFrame &) frm;

      uint32_t ipos = sf.addr & TRACE_ICACHE_MASK;

      if (memcmp(icache[ipos], sf.rawbytes, sf.insn_length) == 0) {
         // Instruction was previously cached, don't save it in trace.
         sf.insn_length = 0;
      } else {
         // Add to the cache. (raw value)
         memcpy(icache[ipos], sf.rawbytes, sf.insn_length);
      }

      // TODO: Any else we need to do here?

   }
   // TODO: Handle other kinds of frames.


   frm.serialize(outfile);
   frm_count++;
}

// Finalizes the trace file by updating header values if necessary,
// building the TOC if requested, and then closing the file.
void TraceWriter::finalize(bool buildTOC)
{

   // TODO: Build the TOC.

   // Write the real header to the file.

   TraceHeader hdr;
   hdr.magic = TRACE_MAGIC;
   hdr.frame_count = frm_count;
   hdr.toc_offset = 0;

   outfile.seekp(0);
   outfile.write((const char *) &hdr, sizeof(TraceHeader));

   outfile.close();

}

#if 0
int main(int argc, char **argv) {


   TraceWriter tw("/tmp/blah");

   StdFrame *frm = new StdFrame;
   frm->addr = 0x12345678;
   frm->tid = 0xabababab;
   frm->insn_size = 0;
   frm->opnd_count = 0;
   tw.add(*frm);

   KeyFrame *kfrm = new KeyFrame;
   kfrm->eax = 0x11111111;
   kfrm->ebx = 0x22222222;
   kfrm->ecx = 0x33333333;
   kfrm->edx = 0x44444444;
   tw.add(*kfrm);

   tw.finalize();

   TraceReader tr("/tmp/blah");

   tr.seek(1);

   while(tr.pos() < tr.count()) {

      Frame *f = tr.next();

      switch(f->type) {
      case FRM_STD:
         {
            StdFrame *sf = (StdFrame *) f;
            printf("Standard frame.\n");
            printf("Address: 0x%x\n", sf->addr);
            break;
         }
      case FRM_KEY:
         {
            KeyFrame *kf = (KeyFrame *) f;
            printf("Keyframe.\n");
            printf("eax = 0x%x\n", kf->eax);
            printf("ebx = 0x%x\n", kf->ebx);
            printf("ecx = 0x%x\n", kf->ecx);
            printf("edx = 0x%x\n", kf->edx);
            break;
         }
      default:
         break;
      }


   }
   return 0;
}
#endif
