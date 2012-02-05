/** $Id$
 *
 * New c++ trace reader.  Tests c++ TraceContainerReader API.
 */

#include <exception>
#include <iostream>
#include "trace.container.hpp"

using namespace SerializedTrace;

void print(frame &f) {
  std::cout << f.DebugString() << std::endl;
}

void go(const char *f) {
  TraceContainerReader t(f);

  // t.seek(0);

  // t.seek(10000);

  // t.seek(0);

  print(*(t.get_frame()));
}

int main(int argc, char **argv) {
  if (argc != 2) {
    if (argv[0]) {
      std::cout << "Usage: " << argv[0] << " <trace>" << std::endl;
    }
    exit(1);
  }

  go(argv[1]);
}
