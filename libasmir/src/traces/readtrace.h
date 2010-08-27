#ifndef _READTRACE_H_
#define _READTRACE_H_

#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <assert.h>
#include "trace.h"
#include "trace_v40.h"
#include "trace_v41.h"
#include "trace_v50.h"
#include "irtoir.h"
#include "pin_trace.h"

extern "C" bap_blocks_t * read_trace_from_file(const string &filename,
					       int offset,
					       bool print,
					       bool atts,
					       bool pintrace);

#endif
