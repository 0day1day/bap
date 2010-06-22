
#include "readtrace.h"
#include <cstdlib>

using namespace std;

bap_blocks_t * read_trace_from_file(const string &filename, int offset, bool print, bool atts, bool pintrace)
{
    // a block to accumulate the lifted traces
    bap_blocks_t * result = new bap_blocks_t;
    uint32_t counter = 0 ;

    if(pintrace) {
        
        pintrace::TraceReader tr(filename.c_str());

        // FIXME: Currently only x86
        VexArch arch = VexArchX86;
        asm_program_t * prog = asmir_new_asmp_for_arch(bfd_arch_i386);

        // Initializations
        translate_init();
        bool syscall = false;
        while(tr.pos() < tr.count()) { // Reading each instruction
            pintrace::Frame *f = tr.next();
            counter += 1;

            switch(f->type) {
                case FRM_STD: // TODO: We should consider key frame
                    {
                        pintrace::StdFrame *sf = (pintrace::StdFrame *) f;
                        bap_block_t *bblock = new bap_block_t;
                        bblock->bap_ir = new vector<Stmt *>();
                        bblock->inst = sf->addr;
                        bblock->vex_ir = translate_insn(
                                            arch,
                                            (unsigned char *)sf->rawbytes,
                                            sf->addr);

                        generate_bap_ir_block(prog, bblock);
                        if (atts)
                            bblock->bap_ir->front()->attributes = sf->getOperands() ;

                        result->push_back(bblock);
                        //for (int i = 0 ; i < bblock->bap_ir->size() ; i ++)
                        //    cout << bblock->bap_ir->at(i)->tostring() << endl ;
                        break;
                    }
                case FRM_SYSCALL: 
                    {
                        pintrace::SyscallFrame *sf = (pintrace::SyscallFrame *) f;
                        bap_block_t *bblock = new bap_block_t;
                        bblock->bap_ir = new vector<Stmt *>();
                        bblock->inst = sf->addr;
                        if (syscall) {
                          
                           //generate_bap_ir_block(prog, bblock);
                           Label * label = new Label("Read Syscall");
                           if (atts)
                               label->attributes = sf->getOperands() ;
                           bblock->bap_ir->push_back(label);
                           bblock->vex_ir = NULL;
                           result->push_back(bblock);
                        }
                        //cerr << sf->callno << " " << sf->args[0] << " " << sf->args[4] << endl;
                        if ((sf->callno == 3) && (sf->args[0] == 4) && (sf->args[4] == 0))
                            syscall = true;
                        else syscall = false;
                        //for (int i = 0 ; i < bblock->bap_ir->size() ; i ++)
                        //    cout << bblock->bap_ir->at(i)->tostring() << endl ;
                        break;
                    }

                default:
                    break;

            }

        }

    }
    else {
        ifstream trace ;
        trace.open(filename.c_str());

        if (!trace.is_open()) {
            cerr << "Couldn't open " << filename.c_str() << endl;
            exit(1);
        }

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
    }
    return result;
}


