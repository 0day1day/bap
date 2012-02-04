#ifndef TRACE_CONTAINER_HPP
#define TRACE_CONTAINER_HPP

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

#include <exception>
#include <fstream>
#include <stdint.h>
#include <string>
#include <vector>
#include "frame.piqi.pb.h"

namespace SerializedTrace {

  const uint64_t magic_number = 7456879624156307493LL;
  const uint64_t default_frames_per_toc_entry = 10000;

  const uint64_t default_auto_finish = false;

  const uint64_t magic_numer_offset = 0LL;
  const uint64_t num_trace_frames_offset = 8LL;
  const uint64_t toc_offset_offset = 16LL;

  class TraceContainerWriter {

    public:

    /** Creates a trace container writer that will output to
        [filename]. An entry will be added to the table of contents
        every [frames_per_toc_entry] entries.*/
    TraceContainerWriter(const char *filename,
                         uint64_t frames_per_toc_entry = default_frames_per_toc_entry,
                         bool auto_finish = default_auto_finish);

    /** Destructor that calls finish if auto_finish is true. */
    ~TraceContainerWriter(void);

    /** Add [frame] to the trace.
     * XXX: Add general frame type to Piqi */
    void add(frame &f);

    /** Finish the trace.  Builds and writes the table of contents to
     * the file. Closes the file. */
    void finish(void);

    protected:

    /** Output fstream for trace container file. */
    std::ofstream ofs;

    /** The toc entries for frames added so far. */
    std::vector<uint64_t> toc;

    /** Number of frames added to the trace. */
    uint64_t num_frames;

    /** Frames per toc entry. */
    const uint64_t frames_per_toc_entry;

    /** Call [finish()] in destructor if not already done. */
    bool auto_finish;

    /** True if [finish()] been called on this writer. */
    bool is_finished;
  };

  class TraceContainerReader {

  public:

    /** Read exception */
    class TraceException: public std::exception
    {

    public:
      TraceException(std::string s)
        : msg (s)
        { }

      ~TraceException(void) throw ();

      virtual const char* what() const throw()
        {
          return msg.c_str();
        }

    private:

      std::string msg;
    };

    /** Creates a trace container reader that reads from [filename]. */
    TraceContainerReader(const char *filename) throw (std::ifstream::failure);

    /** Destructor. */
    ~TraceContainerReader(void) throw ();

    /** Returns the number of frames in the trace. */
    uint64_t num_frames(void) throw ();

    /** Seek to frame number [frame_number]. */
    void seek(uint64_t frame_number) throw (TraceException);;

    /** Seek to first frame. */
    void seek_first(void) throw ();

    /** Return the frame pointed to by the frame pointer. Advances the
        frame pointer by one after. */
    frame get_frame(void) throw (TraceException);

    /** Return [num_frames] starting at the frame pointed to by the
        frame pointer. If there are not that many frames until the end
        of the trace, returns all until the end of the trace.  The
        frame pointer is set one frame after the last frame returned.
        If the last frame returned is the last frame in the trace, the
        frame pointer will point to an invalid frame. */
    std::vector<frame> get_frames(uint64_t num_frames) throw (TraceException);
  };

};

#endif
