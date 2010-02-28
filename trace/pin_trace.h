// -*- c++ -*-

#pragma once

#include <iostream>
#include <fstream>

#include "pin_frame.h"

#define TRACE_MAGIC 0x43525442

#define TRACE_ICACHE_SIZE 1024
#define TRACE_ICACHE_MASK 0x3ff

/*
 * TRACE FORMAT:
 *
 * A trace consists of a trace header (TraceHeader), followed by a
 * sequence of frames. The format of each frame is defined in
 * frame.h. Every frame begins with the same header, which contains size
 * and type information. A decoder can make use of this to determine how
 * to further parse the frame. Note that frames are variable sized.
 *
 * The trace might also contain a table of contents (TOC), which can be
 * used to quickly seek to a particular frame. The start of the table can
 * be found using the TraceHeader.toc_offset value. Currently the format
 * of the TOC has not been defined.
 *
 */

/**
 * Standard header prefixing a trace file.
 */

namespace pintrace { // We will use namespace to avoid collision

struct TraceHeader {
   uint32_t magic;
   uint32_t frame_count;
   uint32_t toc_offset;
};

/**
 * TraceReader: Allows playback of a trace file.
 */
class TraceReader {

private:

   uint32_t frm_pos;

protected:
   std::ifstream infile;
   TraceHeader header;
   // MAX instruction byte + instruction length (1 byte)
   char icache[TRACE_ICACHE_SIZE][MAX_INSN_BYTES + 1];

public:
   TraceReader(const char *filename);

   // Returns total number of frames in the trace.
   uint32_t count() const;

   // Returns current frame position in the trace.
   uint32_t pos() const;

   // Move frame pointer to the specified offset. Returns true iff
   // successful.
   bool seek(uint32_t offset);

   // Returns the current frame being pointed to and advances the frame
   // pointer to the next frame.
   // If noskip is false, the current frame is skipped and NULL is returned
   // instead.
   Frame *next(bool noskip = true);

   // Returns true iff the frame pointer is beyond the last frame.
   bool eof() const;

};

/**
 * TraceWriter: Creates a new trace file.
 */
class TraceWriter {

private:

   uint32_t frm_count;

protected:

   std::ofstream outfile;

   char icache[TRACE_ICACHE_SIZE][MAX_INSN_BYTES];

public:

   // Creates a new TraceWriter that outputs the trace to the file named
   // "filename". Will truncate the file.
   TraceWriter(const char *filename);

   // Returns the current number of frames in the trace.
   uint32_t count() const;

   // Adds a new frame to the trace.
   void add(Frame &frm);

   // Finalizes the trace file by updating header values if necessary,
   // building the TOC if requested, and then closing the file.
   void finalize(bool buildTOC = true);

};

}; // End of namespace
