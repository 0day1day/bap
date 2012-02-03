/**
 * $Id$
 *
 * A container for trace frames.  We do not use protobuffers because
 * protobuffers can not stream output (the whole trace would have to
 * be in memory before being written) or input (the whole trace would
 * need to be unserialized to get one frame).
 *
 * The trace format is extremely simple:
 *
 * [<uint64_t magic number>
 *  <uint64_t n = number of trace frames>
 *  <uint64_t offset of field m (below) >
 *  [ <trace frame 0>
 *    ..............
 *    <trace frame n> ]
 *  <uint64_t m, where a table of contents entry is given
 *  for m, 2m, 3m, ..., ceil(n/m)>
 *  [ <uint64_t offset of trace frame m>
 *    <uint64_t offset of trace frame 2m>
 *    ...
 *    <uint64_t offset of trace frame ceil(n/m)> ]]
 */

#include <vector>

namespace SerializedTrace {

  const uint64_t magic_number = 7456879624156307493LL;
  const uint64_t default_frames_per_toc_entry = 10000;

  class TraceContainerWriter {

    public:

    /** Creates a trace container writer that will output to
        [filename]. An entry will be added to the table of contents
        every [frames_per_toc_entry] entries.*/
    TraceContainerWriter(const char *filename, uint64_t frames_per_toc_entry = default_frames_per_toc_entry);

    /** Add [frame] to the trace.
     * XXX: Add general frame type to Piqi */
    add(void);

    /** Finish the trace.  Builds and writes the table of contents to
     * the file. Closes the file. */
    finish(void);

    protected:

    /** Output fstream for trace container file. */
    ofstream ofs;

    /** The toc entries for frames added so far. */
    std::vector<uint64_t> toc;

    /** Number of frames added to the trace. */
    uint64_t num_frames;

    /** Frames per toc entry. */
    uint64_t frames_per_toc_entry;
  };
};
