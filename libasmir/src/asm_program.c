#include <errno.h>
#include <stdlib.h>
#include <assert.h>

#include "asm_program.h"
#include "libiberty.h"

// Argh, these functions are documented at
// http://sourceware.org/binutils/docs/bfd/Opening-and-Closing.html
// but don't seem to be in the header files...
extern void *bfd_alloc (bfd *abfd, bfd_size_type wanted);
extern void *bfd_alloc2 (bfd *abfd, bfd_size_type nmemb, bfd_size_type size);



static void initialize_sections(asm_program_t *p);
static bfd* initialize_bfd(const char *filename);

/* find the segment for memory address addr */
static section_t* get_section_of(asm_program_t *prog, bfd_vma addr)
{
  section_t *segment;

  for (segment = prog->segs; segment != NULL; segment = segment->next)
    if ((addr >= segment->start_addr) && (addr < segment->end_addr))
      return segment;

  return NULL;
}



bfd_byte *asmir_get_ptr_to_instr(asm_program_t *prog, bfd_vma addr)
{
  section_t *seg = get_section_of(prog, addr);
  if(seg == NULL){
    fprintf(stderr, "get_ptr_to_instr: Segment for %llx not found\n", (long long int)addr);
    return NULL;
  }
  return seg->data + (addr - seg->start_addr);
}


asm_program_t *
asmir_open_file(const char *filename)
{
  asm_program_t *prog = malloc(sizeof(asm_program_t));
  if (!prog)
    return NULL;
  bfd *abfd = initialize_bfd(filename);
  if (!abfd) {
    free(prog);
    return NULL;
  }
  prog->abfd = abfd;
  initialize_sections(prog);

  return prog;
}


void asmir_close(asm_program_t *p)
{
  // FIXME: free sections
  bfd_close_all_done(p->abfd);
  free(p);
}

static int ignore() {
  return 1;
}

int asmir_get_instr_length(asm_program_t *prog, bfd_vma addr)
{
  disassembler_ftype disas = disassembler(prog->abfd);
  fprintf_ftype old_fprintf_func = prog->disasm_info.fprintf_func;
  prog->disasm_info.fprintf_func = (fprintf_ftype)ignore;
  assert(disas);
  int len = disas(addr, &prog->disasm_info);
  prog->disasm_info.fprintf_func = old_fprintf_func;
  return len;
}


static int
my_read_memory (bfd_vma memaddr,
		bfd_byte *myaddr,
		unsigned int length,
		struct disassemble_info *info)
{
  int ret = buffer_read_memory(memaddr,myaddr,length,info);

  if (EIO == ret) {
    section_t *seg = get_section_of(info->application_data, memaddr);
    if (NULL == seg)
      return EIO;

    info->buffer = seg->data;
    info->buffer_vma = seg->start_addr;
    info->buffer_length = seg->datasize;
    info->section = seg->section;

    ret = buffer_read_memory(memaddr,myaddr,length,info);
  }
  return ret;
}


static void init_disasm_info2(bfd *abfd, struct disassemble_info *disasm_info)
{
  init_disassemble_info (disasm_info, stdout, (fprintf_ftype) fprintf);
  disasm_info->flavour = bfd_get_flavour (abfd);
  disasm_info->arch = bfd_get_arch (abfd);
  disasm_info->mach = bfd_get_mach (abfd);
  disasm_info->octets_per_byte = bfd_octets_per_byte (abfd);
  disasm_info->disassembler_needs_relocs = FALSE;

  if (bfd_big_endian (abfd))
    disasm_info->display_endian = disasm_info->endian = BFD_ENDIAN_BIG;
  else if (bfd_little_endian (abfd))
    disasm_info->display_endian = disasm_info->endian = BFD_ENDIAN_LITTLE;

  disassemble_init_for_target(disasm_info);

  disasm_info->read_memory_func = my_read_memory;
}

static void init_disasm_info(asm_program_t *prog)
{
  init_disasm_info2(prog->abfd, &prog->disasm_info);
  prog->disasm_info.application_data = prog;
}


static void
initialize_sections(asm_program_t *prog)
{
  struct disassemble_info *disasm_info = &prog->disasm_info;
  bfd *abfd = prog->abfd;
  unsigned int opb = bfd_octets_per_byte(abfd);
  disasm_info->octets_per_byte = opb;
  init_disasm_info(prog);
  section_t **nextseg = &prog->segs;
  asection *section;

  for(section = abfd->sections; section; section = section->next) {
    section_t *seg;
    bfd_byte *data;
    bfd_vma datasize = bfd_get_section_size_before_reloc(section);

    if(datasize == 0) continue;

    data = bfd_alloc2(abfd, datasize, sizeof(bfd_byte));
    bfd_get_section_contents(abfd, section, data, 0, datasize);
    seg = bfd_alloc(abfd, sizeof(section_t));
    seg->data = data;
    seg->datasize = datasize;
    seg->start_addr = section->vma;
    seg->end_addr = section->vma + datasize/opb;
    seg->section = section;
    seg->is_code = !!(section->flags & SEC_CODE);


    seg->next = NULL;
    *nextseg = seg;
    nextseg = &seg->next;
  }

}


static bfd *
initialize_bfd(const char *filename)
{
  bfd * abfd;
  char **matching;
  char *target = "i686-pc-linux-gnu";

  bfd_init();

  if(!bfd_set_default_target(target)) {
    fprintf(stderr, "initialize_bfd: couldn't set default bfd target\n");
    return NULL;
  }

  abfd = bfd_openr(filename, target);
  if(abfd == NULL) {
    fprintf(stderr, "initialize_bfd: cannot open %s\n", filename);
    return NULL;
  }

  if (bfd_check_format (abfd, bfd_archive)) {
    fprintf(stderr, "initalize_bfd: archive files  not supported\n");
    bfd_close_all_done(abfd);
    return NULL;
  }

  if(!bfd_check_format_matches(abfd, bfd_object, &matching)) {
    fprintf(stderr, "initialize_bfd: bfd_check_format_matches failed\n");
    bfd_close_all_done(abfd);
    return NULL;
  }
  return abfd;
}




struct bprintf_buffer {
  char *str; // the start of the string
  char *end; // the null terminator at the end of the written string.
  size_t size; // the size of str
};

int bprintf(struct bprintf_buffer *dest, const char *fmt, ...) {
  va_list ap;
  int ret;
  size_t size_left = dest->size - (dest->end - dest->str);
  va_start(ap, fmt);
  ret = vsnprintf(dest->end, size_left, fmt, ap);
  va_end(ap);
  if (ret >= size_left) {
    // we seem to need to call va_start again... is this legal?
    dest->size = dest->size+ret+1-size_left;
    char *str = (char*)realloc(dest->str, dest->size);

    assert(str); // this code is full of xalloc anyways...

    dest->end = str + (dest->end - dest->str);
    dest->str = str;
    size_left = dest->size - (dest->end - dest->str);
    va_start(ap, fmt);
    ret = vsnprintf(dest->end, size_left, fmt, ap);
    va_end(ap);
    assert(ret == size_left-1 && ret > 0);
  }
  dest->end += ret;
  return ret;
}

// returns a string which is valid until the next call to this function
char* asmir_string_of_insn(asm_program_t *prog, bfd_vma inst)
{
  static struct bprintf_buffer bits = {NULL, NULL, 0};

  disassembler_ftype disas = disassembler(prog->abfd);
  fprintf_ftype old_fprintf_func = prog->disasm_info.fprintf_func;
  void *oldstream = prog->disasm_info.stream;
  prog->disasm_info.fprintf_func = (fprintf_ftype)bprintf;
  prog->disasm_info.stream = &bits;

  bits.end = bits.str;
  
  disas(inst, &prog->disasm_info);

  prog->disasm_info.fprintf_func = old_fprintf_func;
  prog->disasm_info.stream = oldstream;
  
  return bits.str;
}


enum bfd_architecture asmir_get_asmp_arch(asm_program_t *prog) {
  return bfd_get_arch(prog->abfd);
}


// from old translate.cpp fake_prog_for_arch()
// Returns a fake asm_program_t for use when disassembling bits out of memory
asm_program_t* asmir_new_asmp_for_arch(enum bfd_architecture arch)
{
  int machine = 0; // TODO: pick based on arch
  asm_program_t *prog = malloc(sizeof(asm_program_t));
  
  prog->abfd = bfd_openw("/dev/null", NULL);
  assert(prog->abfd);
  bfd_set_arch_info(prog->abfd, bfd_lookup_arch(arch, machine));

  //not in .h bfd_default_set_arch_mach(prog->abfd, arch, machine);
  bfd_set_arch_info(prog->abfd, bfd_lookup_arch(arch, machine));
  init_disasm_info(prog);
  return prog;
}
