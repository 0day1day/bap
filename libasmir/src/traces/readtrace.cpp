#include <stdlib.h>
#include "readtrace.h"

using namespace std;

bap_blocks_t * read_trace_from_file(const string &filename, int offset, bool print, bool atts)
{
    ifstream trace ;
	trace.open(filename.c_str());

	if (!trace.is_open()) {
		cerr << "Couldn't open " << filename.c_str() << endl;
		exit(1);
	}

    // a block to accumulate the lifted traces
    bap_blocks_t * result = new bap_blocks_t;
    Trace * trc ;
    EntryHeader eh; 
    TraceHeader hdr;

    try {
            trace.read(BLOCK(hdr), TRACE_HEADER_FIXED_SIZE); 
            
            assert(hdr.magicnumber == MAGIC_NUMBER) ;
            switch (hdr.version)
            {
                    case 40: trc = new Trace_v40(&trace); break;
                    case 41: trc = new Trace_v41(&trace); break;
                    case 50: trc = new Trace_v50(&trace); break;
                    default: throw "unsupported trace version";
            }
            trc->consume_header(&hdr) ;

            /* Since the traces do not contain any architecture information *
             * we only support x86 for now - ethan                          */
            VexArch arch = VexArchX86;
            asm_program_t * prog = asmir_new_asmp_for_arch(bfd_arch_i386);

            // Initializations
            translate_init();
            int counter = 0 ;
            while(!trace.eof()) {
                    // Reading each entry header
                    trc->read_entry_header(&eh);

                    counter ++ ;
                    if (counter > offset) {

                            bap_block_t *bblock = new bap_block_t;
                            bblock->bap_ir = new vector<Stmt *>();
                            bblock->inst = eh.address;
                            // lift the instruction to VEX IL
                            bblock->vex_ir = translate_insn(arch, eh.rawbytes, eh.address);
                            // and then to BAP IL
                            generate_bap_ir_block(prog, bblock);
                            if (atts) 
                                bblock->bap_ir->front()->attributes = trc->operand_status(&eh) ;
                            // append to result
                            result->push_back(bblock);
                            int i;
                            if (print)
                                    for ( i = 0 ; i < bblock->bap_ir->size() ; i ++)
                                            cout << bblock->bap_ir->at(i)->tostring() << endl ; 
                    }
            }
    }
    catch (const char * s) 
    {
            cout << s << endl ;
    }
    return result;
}


