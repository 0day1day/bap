/**
 * $Id$
 *
 * Implementation of trace container.
 */

#include "trace.container.hpp"
#include <iostream>

#define WRITE(x) { ofs.write((const char *)(&(x)), sizeof(x)); }
#define READ(x) { ifs.read((char *)(&(x)), sizeof(x)); }

namespace SerializedTrace {

  TraceContainerWriter::TraceContainerWriter(const char *filename,
                                             uint64_t frames_per_toc_entry_in,
                                             bool auto_finish_in) throw (std::ofstream::failure, TraceException)
    : ofs ( filename, std::ios_base::binary | std::ios_base::out | std::ios_base::trunc ),
      num_frames (0),
      frames_per_toc_entry (frames_per_toc_entry_in),
      auto_finish (auto_finish_in),
      is_finished (false)
  {
    ofs.exceptions( std::ios_base::failbit | std::ios_base::badbit | std::ios_base::eofbit );
    ofs.seekp(first_frame_offset);
  }

  TraceContainerWriter::~TraceContainerWriter(void) throw () {
    /** Call finish if it has not been called already ANd if
        auto_finish is set. */
    if (is_finished && auto_finish) {
      try {
        finish();
      }
      catch (std::ofstream::failure e) {
        std::cerr << "Exception " << e.what() << " occured during TraceContainerWriter's auto-finish" << std::endl;
      }
    }
  }

  void TraceContainerWriter::add(frame &f) throw (std::ofstream::failure, TraceException) {
    num_frames++;

    /* Is is time for a toc entry? */
    if ((num_frames % frames_per_toc_entry) == 0) {
      /* Yes.  Add the file offset where we will insert this frame to
         toc. */
      toc.push_back(ofs.tellp());
    }

    /* Serialize to the file */
    if (!(f.SerializeToOstream(&ofs))) {
      throw (TraceException("Unable to serialize frame to ostream"));
    }

  }

  void TraceContainerWriter::finish(void) throw (std::ofstream::failure) {
    /* Save the offset where we will write the toc. */
    uint64_t toc_offset = ofs.tellp();

    /* Make sure the toc is the right size. */
    assert ((num_frames / frames_per_toc_entry) == toc.size());

    /* Write frames per toc entry. */
    WRITE(frames_per_toc_entry);

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

    /* Finally, close the file and mark us as finished. */
    ofs.close();
    is_finished = true;
  }

  TraceContainerReader::TraceContainerReader(const char *filename) throw (std::ifstream::failure, TraceException)
    : ifs ( filename, std::ios_base::binary | std::ios_base::in )
  {
    ifs.exceptions( std::ios_base::failbit | std::ios_base::badbit | std::ios_base::eofbit );

    /* Verify the magic number. */
    uint64_t magic_number_read;
    READ(magic_number_read);
    if (magic_number_read != magic_number) {
      throw (TraceException("Magic number not found in trace"));
    }

    /* Read number of trace frames. */
    READ(num_frames);

    /* Find offset of toc. */
    uint64_t toc_offset;
    READ(toc_offset);

    /* Find the toc. */
    ifs.seekg(toc_offset);

    /* Read number of frames per toc entry. */
    READ(frames_per_toc_entry);

    /* Read each toc entry. */
    for (int i = 0; i < (num_frames / frames_per_toc_entry); i++) {
      uint64_t offset;
      READ(offset);
      toc.push_back(offset);
    }

    /* We should be at the end of the file now. */
    std::ifstream::off_type us = ifs.tellg();
    ifs.seekg(0, std::ios_base::end);
    if (us != ifs.tellg()) {
      throw(TraceException("The table of contents is malformed."));
    }

    /* Seek to the first frame. */
    seek(0);
  }

  TraceContainerReader::~TraceContainerReader(void) throw () {
    /* Nothing yet. */
  }

  uint64_t TraceContainerReader::get_num_frames(void) throw () {
    return num_frames;
  }

  void TraceContainerReader::seek(uint64_t frame_number) throw (TraceException) {
    /* First, make sure the frame is in range. */
    check_end_of_trace_num(frame_number, "seek() to non-existant frame");

    /* Find the closest toc entry, if any. */
    uint64_t toc_number = frame_number / frames_per_toc_entry;

    if (toc_number == 0) {
      current_frame = 0;
      ifs.seekg(first_frame_offset);
    } else {
      current_frame = toc_number * frames_per_toc_entry;
      /* Use toc_number - 1 because there is no toc for frames [0,m). */
      ifs.seekg(toc[toc_number - 1]);
    }

    while (current_frame != frame_number) {
      frame f;
      f.ParseFromIstream(&ifs);
      current_frame++;
    }
  }

  std::auto_ptr<frame> TraceContainerReader::get_frame(void) throw (std::ifstream::failure, TraceException) {
    /* Make sure we are in bounds. */
    check_end_of_trace("get_frame() on non-existant frame");

    std::cerr << "Here at " << ifs.tellg() << std::endl;

    std::auto_ptr<frame> f(new frame);
    if (!(f->ParseFromIstream(&ifs))) {
      throw (TraceException("Unable to parse from istream"));
    }
    current_frame++;

    return f;
  }

  std::auto_ptr<std::vector<frame> > TraceContainerReader::get_frames(uint64_t num_frames) throw (std::ifstream::failure, TraceException) {
    check_end_of_trace("get_frames() on non-existant frame");

    std::auto_ptr<std::vector<frame> > frames(new std::vector<frame>);
    for (uint64_t i = 0; i < num_frames && current_frame < num_frames; i++) {
      frames->push_back(*(get_frame()));
    }

    return frames;
  }

  bool TraceContainerReader::end_of_trace(void) throw () {
    return end_of_trace_num(current_frame);
  }

  bool TraceContainerReader::end_of_trace_num(uint64_t frame_num) throw () {
    if (frame_num + 1 > num_frames) {
      return true;
    } else {
      return false;
    }
  }

  void TraceContainerReader::check_end_of_trace_num(uint64_t frame_num, std::string msg) throw (TraceException) {
    if (end_of_trace_num(frame_num)) {
      throw (TraceException(msg));
    }
  }

    void TraceContainerReader::check_end_of_trace(std::string msg) throw (TraceException) {
      return check_end_of_trace_num(current_frame, msg);
    }
};
