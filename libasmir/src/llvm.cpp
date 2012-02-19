/** $Id$
 *
 *  Helper functions for llvm code generation
 *
 *  XXX: Add support for multibyte reads and writes
*/

#include <cassert>
#include <map>
#include <stdint.h>

typedef uint64_t addr_t;
typedef uint8_t value_t;
typedef std::map<addr_t,value_t> memory;

static memory m;

extern "C" {

  void fake_assert(uint32_t b) {
    assert(b);
  }

  memory create_memory(void) {
    m.clear();
  }

  void set_memory(addr_t a, value_t v) {
    m[a] = v;
  }

  value_t get_memory(addr_t a) {
    /* Sets memory if undefined. This may not be what we want.q */
    return m[a];
  }

}
