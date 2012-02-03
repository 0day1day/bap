/**
 * $Id$
 *
 * Implementation of trace container.
 */

#include "trace.container.hpp"

namespace SerializedTrace {
  TraceContainerWriter::TraceContainerWriter(const char *filename,
                                             uint64_t frames_per_toc_entry_in)
    : ofs ( new ofstream(filename, ios_base::binary | ios_base::out | ios_base::trunc) ),
      num_frames (0),
      frames_per_toc_entry (frames_per_toc_entry_in)
  { }
};
