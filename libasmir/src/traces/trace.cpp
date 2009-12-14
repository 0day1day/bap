/* A common trace interface for the supported traces */
#include "trace_vXX.h"

Trace::Trace(ifstream * tracefile)
{
	trace = tracefile ;
}

void Trace::read_taint_byte_record(TaintByteRecord * byte_rec)
{
	trace->read(BLOCK(*byte_rec), TAINT_BYTE_RECORD_FIXED_SIZE);
}

void Trace::read_taint_record(taint_record_t * rec)
{
	trace->read(BLOCK(*rec), TAINT_RECORD_FIXED_SIZE);
	int i;
	if (rec->numRecords > MAX_NUM_TAINTBYTE_RECORDS) {
		cerr << "num: " << rec->numRecords << endl ; 
		throw "taintbyte records number exceeds maximum";
	}
	for ( i = 0 ; i < rec->numRecords ; i ++ )
		read_taint_byte_record(&rec->taintBytes[i]);

}

void Trace::read_records(taint_record_t records[MAX_OPERAND_LEN], uint32_t length, uint64_t tainted)
{
	int i;
	if (length > MAX_OPERAND_LEN) 
      throw "operand length exceeds maximum";
	for ( i = 0 ; i < length ; i ++ )
		if (tainted & (1L<<i))
			read_taint_record(&records[i]);

}

void Trace::read_operand(OperandVal * operand)
{
	trace->read(BLOCK(*operand), OPERAND_VAL_FIXED_SIZE);
	read_records(operand->records, operand->length, operand->tainted);

}

void Trace::read_operands(OperandVal operand[MAX_NUM_OPERANDS], uint8_t num)
{
	int i;
	if (num > MAX_NUM_OPERANDS)
	{
		cerr << "num: " << (int)num << endl ; 
		throw "number of operands exceeds maximum";
	}
	for ( i = 0 ; i < num ; i ++ )
		read_operand(operand+i);
}

void Trace::read_entry_header(EntryHeader * eh)
{
	// reading in the static part of entry header
	trace->read(BLOCK(*eh), ENTRY_HEADER_FIXED_SIZE);
	read_operand(&(eh->oper));
	read_operands(eh->operand, eh->num_operands);
	// read the raw instruction bytes;
	trace->read(BLOCK(eh->rawbytes), eh->inst_size);
}

void Trace::read_module()
{
	ModuleRecord proc;
	trace->read(BLOCK(proc), MODULE_RECORD_FIXED_SIZE); 
}

void Trace::read_process()
{
        ProcRecord proc;
        trace->read(BLOCK(proc), PROC_RECORD_FIXED_SIZE); 
        int i;
        for (i = 0 ; i < proc.n_mods ; i ++)
                read_module();
}

void Trace::read_procs(TraceHeader * hdr)
{
        int i;
        for (i = 0 ; i < hdr->n_procs ; i ++ )
                read_process();
}

void Trace::consume_header(TraceHeader * hdr)
{
        read_procs(hdr) ;
}

/* Creates a string representation of the contents (during *
 * the execution of the instruction for the following      *
 * contexts:                                               *
 * Delta: mapping an instruction operand to a value        *
 * Tau: mapping an instruction operand to its taint status */
string Trace::operand_status(EntryHeader * eh)
{
        ostringstream os;
        int i;
        for ( i = 0 ; i < eh->num_operands ; i ++ )
        {
                switch (eh->operand[i].type)
                { 
                        case TRegister: 
                                os << " " << register_name(eh->operand[i].addr) << " = " ;
                                os << eh->operand[i].value << ", " ;
                                os << eh->operand[i].tainted << " ; " ;
                                break;
                        case TMemLoc: 
                                os << " mem[" << eh->operand[i].addr << "] = " ; 
                                os << eh->operand[i].value << ", " ;
                                os << eh->operand[i].tainted << " ; " ;
                                break ;
                        case TImmediate : 
                        case TJump : 
                        case TFloatRegister : 
                                // we do not extract information from such operands
                                break ;
                        case TMemAddress:
                                os << " addr[" << eh->operand[i].addr << "] = " ; 
                                os << eh->operand[i].value << ", " ;
                                os << eh->operand[i].tainted << " ; " ;
                                break ;

                        default : 
                                cerr << "type: " << eh->operand[i].type << endl ; 
                                assert(0) ;
                }
        }
        os << endl;
        return os.str();
}

