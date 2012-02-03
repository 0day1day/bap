/**
 * $Id$
 *
 * Implementation of trace container.
 */

#include "trace.container.hpp"

#define WRITE(x) { ofs.write((const char *)(&(x)), sizeof(x)); }

namespace SerializedTrace {

  TraceContainerWriter::TraceContainerWriter(const char *filename,
                                             uint64_t frames_per_toc_entry_in)
    : ofs ( filename, std::ios_base::binary | std::ios_base::out | std::ios_base::trunc ),
      num_frames (0),
      frames_per_toc_entry (frames_per_toc_entry_in)
  {
    ofs.exceptions( std::ios_base::failbit | std::ios_base::badbit | std::ios_base::eofbit );
  }

  void TraceContainerWriter::add(frame &f) {
    num_frames++;

    /* Is is time for a toc entry? */
    if ((num_frames % frames_per_toc_entry) == 0) {
      /* Yes.  Add the file offset where we will insert this frame to
         toc. */
      toc.push_back(ofs.tellp());
    }

    /* Serialize to the file */
    f.SerializeToOstream(&ofs);
  }

  void TraceContainerWriter::finish(void) {
    /* Save the offset where we will write the toc. */
    uint64_t toc_offset = ofs.tellp();

    /* Make sure the toc is the right size. */
    assert (((num_frames + (frames_per_toc_entry - 1))
             / frames_per_toc_entry) == toc.size());

    /* Write each offset to the file. */
    for (std::vector<uint64_t>::size_type i = 0; i < toc.size(); i++) {
      WRITE(toc[i]);
    }

    /* Now we need to write the magic number, number of trace frames
       and the offset of field m at the start of the trace. */

    /* Magic number. */
    ofs.seekp(magic_numer_offset);
    WRITE(magic_number);

    /* Numer of trace frames */
    ofs.seekp(num_trace_frames_offset);
    WRITE(num_frames);

    /* Offset of toc. */
    ofs.seekp(toc_offset_offset);
    WRITE(toc_offset);

    /* Finally, close the file. */
    ofs.close();
  }
};
