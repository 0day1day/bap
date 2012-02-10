#include <string>
#include <vector>
#include <iostream>
#include <assert.h>

//#include "typecheck_ir.h"
#include "irtoir-internal.h"
#include "config.h"

// flag to disable warnings (for unit test's sake)
bool print_warnings = 1;

// enable/disable lazy eflags computation.
// this is for transitional purposes, and should be removed soon.
bool use_eflags_thunks = 0;

// Use R_XS_BASE registers instead of gdt/ldt
bool use_simple_segments = 1;

bool translate_calls_and_returns = 0;

using namespace std;

//
// For labeling untranslated VEX IR instructions
//
static string uTag = "Unknown: ";
static string sTag = "Skipped: ";

//
// Special Exp to record the AST for shl, shr
//

Exp * count_opnd = NULL;

//======================================================================
// Forward declarations
//======================================================================
Exp *emit_mux0x( vector<Stmt *> *irout, reg_t type, Exp *cond, Exp *exp0, Exp *expX );
//string inst_to_str(asm_program_t *prog, Instruction *inst );
//static void add_special_returns(bap_block_t *block);
void do_cleanups_before_processing();


//======================================================================
// 
// Helper functions for output (generally should only be used by automated
// testing)
//
//======================================================================
void asmir_set_print_warning(bool value) {
  print_warnings = value;
}

bool asmir_get_print_warning() {
  return print_warnings;
}
//======================================================================
// 
// Helper functions for the translation
//
//======================================================================

/// Set whether to use the thunk code with function calls, or not.
void set_use_eflags_thunks(bool value){
  use_eflags_thunks = value;
}

/// Set whether to use code with simple segments or not.
void asmir_set_use_simple_segments(bool value){
  use_simple_segments = value;
}

// Terrible name, but to be consistent, named similar to above. 
// Return what the current eflags thunks values is 
bool get_use_eflags_thunks()
{
  return use_eflags_thunks;
}

void set_call_return_translation(int value)
{
  cerr <<"Warning: set_call_return_translation() is deprecated. Use replace_calls_and_returns instead.\n";
  translate_calls_and_returns = (bool) value;
}

//----------------------------------------------------------------------
// A panic function that prints a msg and terminates the program
//----------------------------------------------------------------------
void panic( string msg )
{
  ostringstream os;
  os << "Panic: " << msg;
  throw os.str().c_str();
  /*
    cerr << "Panic: " << msg << endl;
    exit(1);
  */
}


//---------------------------------------------------------------------
// Helper wrappers around arch specific functions
//---------------------------------------------------------------------


Temp *mk_temp( reg_t type, vector<Stmt *> *stmts )
{
    static int temp_counter = 0;
    Temp *ret =  new Temp( type, "T_" + int_to_str(temp_counter++) );
    stmts->push_back(new VarDecl(ret));
    return ret;
}

//----------------------------------------------------------------------
// Takes a destination address and makes a Label out of it. 
// Note that this function and mk_dest_name must produce the same
// string for the same given address!
//----------------------------------------------------------------------
Label *mk_dest_label( Addr64 dest )
{
    return new Label("pc_0x" + int_to_hex(dest));
}

//----------------------------------------------------------------------
// Takes a destination address and makes a Name out of it. 
// Note that this function and mk_dest_label must produce the same
// string for the same given address!
//----------------------------------------------------------------------
Name *mk_dest_name( Addr64 dest )
{
    return new Name("pc_0x" + int_to_hex(dest));
}


//======================================================================
// 
// Actual translation functions
//
//======================================================================

//----------------------------------------------------------------------
// arg1 and arg2 are both suppose to be 32 bit expressions.
// This function returns a 64 bit expression with arg1 occupying the
// high 32 bits and arg2 occupying the low 32 bits.
//----------------------------------------------------------------------
Exp *translate_32HLto64( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *high = new Cast( arg1, REG_64, CAST_UNSIGNED );
    Exp *low = new Cast( arg2, REG_64, CAST_UNSIGNED );

    high = new BinOp( LSHIFT, high, ex_const(REG_64,32) );

    return new BinOp( BITOR, high, low );
}
Exp *translate_64HLto64( Exp *high, Exp *low )
{
    assert(high);
    assert(low);

    high = new BinOp( LSHIFT, high, ex_const(REG_64, 32) );
    high = new BinOp( BITAND, high, ex_const(REG_64, 0xffffffff00000000LL) );
    low = new BinOp( BITAND, low, ex_const(REG_64, 0xffffffffL) );

    return new BinOp( BITOR, high, low );
}


Exp *translate_DivModU64to32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);
    arg2 = new Cast( arg2, REG_64, CAST_UNSIGNED );
    Exp *div = new BinOp( DIVIDE, arg1, arg2 );
    Exp *mod = new BinOp( MOD, ecl(arg1), ecl(arg2) );

    return translate_64HLto64( mod, div );
}

Exp *translate_DivModS64to32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);
    arg2 = new Cast( arg2, REG_64, CAST_SIGNED );
    Exp *div = new BinOp( SDIVIDE, arg1, arg2 );
    Exp *mod = new BinOp( SMOD, ecl(arg1), ecl(arg2) );

    return translate_64HLto64( mod, div );
}

Exp *translate_MullS8( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_16, CAST_SIGNED );
    Exp *wide2 = new Cast( arg2, REG_16, CAST_SIGNED );

    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_MullU32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_64, CAST_UNSIGNED );
    Exp *wide2 = new Cast( arg2, REG_64, CAST_UNSIGNED );
    
    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_MullS32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_64, CAST_SIGNED );
    Exp *wide2 = new Cast( arg2, REG_64, CAST_SIGNED );

    return new BinOp( TIMES, wide1, wide2 );
}

Exp *emit_mux0x( vector<Stmt *> *irout, reg_t type,
		 Exp *cond, Exp *exp0, Exp *expX )
{
    assert(cond);
    assert(exp0);
    assert(expX);

    // Every instance of temps and labels should have their own object,
    // i.e. always new Label(label) instead of just label. So the labels
    // and temps created above are used only once after which they need
    // to be cloned for each subsequent use. This keeps the expression tree
    // a tree instead of a graph.

    size_t initialSize = irout->size();
    
    Temp *temp = mk_temp(type,irout);

#ifndef MUX_AS_CJMP
    // FIXME: modify match_mux0x to recognize this
    Exp *widened_cond;
    reg_t result_type;

    /*
     *
     *
     *
     *
     * README
     *
     *
     *
     *
     * IF YOU CHANGE THIS, MAKE SURE YOU CHANGE MATCH_MUX0X TOO.
     *
     *
     *
     *
     *
     *
     * IF YOU DONT, THINGS WILL BREAK.
     *
     *
     *
     *
     * THANK YOU.
     */

    widened_cond = mk_temp(type,irout);
    irout->push_back(new Move(ecl(widened_cond),
			      new Cast(cond, type,
				       CAST_SIGNED)));

    // tmp = x&c | y&~c
    irout->push_back(new Move(ecl(temp), 
			      new BinOp(BITOR,
					new BinOp(BITAND,
						  ecl(exp0),
                                                  ecl(widened_cond)),
					new BinOp(BITAND,
						  ecl(expX),
						  new UnOp(NOT, 
							   widened_cond)))));

#else // def MUX_AS_CJMP

    Label *labelX = mk_label();
    Label *done = mk_label();

    // match_mux0x depends on the order/types of these statements
    // if changing them here, make sure to make the corresponding changes there
    irout->push_back( new Move( new Temp(*temp), exp0 ) );
    irout->push_back( new CJmp( cond, new Name(done->label), new Name(labelX->label) ) );
    irout->push_back( labelX );
    irout->push_back( new Move( new Temp(*temp), expX ) );
    irout->push_back( done );
#endif

    /* Make sure that MUX_LENGTH is correct */
    assert( (initialSize + MUX_LENGTH) == irout->size());
    
    return temp;
}


// FIXME: call arch specific functions
bool is_special( bfd_vma inst )
{
  return false;
}

vector<Stmt *> *translate_special( bfd_vma inst )
{
  panic("Why did this get called? We are now saying that no instruction is a special.");
}

// Translate a single block to Vine. guest_arch must be set already
void generate_bap_ir_block( asm_program_t *prog, bap_block_t *block )
{
  static unsigned int ir_addr = 100; // Argh, this is dumb

  assert(block);
  
  // Translate the block
  if (is_special(block->inst)) 
    block->bap_ir = translate_special(block->inst);
  //  else
      //block->bap_ir = translate_irbb( block->vex_ir );

  assert(block->bap_ir);
  vector<Stmt *> *vir = block->bap_ir;
  
  // Add the asm and ir addresses
  for ( int j = 0; j < vir->size(); j++ )
    {
      if(block->inst)
	vir->at(j)->asm_address = block->inst;
      vir->at(j)->ir_address = ir_addr++;
    }
}

vector<bap_block_t *>
generate_bap_ir( asm_program_t *prog, vector<bap_block_t *> vblocks )
{
    unsigned int vblocksize = vblocks.size();

    for ( int i = 0; i < vblocksize; i++ )
    {
        bap_block_t *block = vblocks.at(i);
	generate_bap_ir_block(prog, block);
    }
    return vblocks;
}


//----------------------------------------------------------------------
// 
// Helpers
//
//----------------------------------------------------------------------


string inst_to_str(asm_program_t *prog, address_t inst )
{
    return string(asmir_string_of_insn(prog, inst));
}

string get_op_str(asm_program_t *prog, address_t inst )
{
    string str = inst_to_str(prog, inst);
    istringstream stream(str);
    string token;
    getline(stream, token, '\t');
    getline(stream, token, '\t');

    return token;
}


// Needed to be able to delete the Mux0X statements in shift instructions
int match_mux0x(vector<Stmt*> *ir, unsigned int i,
		Exp **cond, Exp **exp0,	Exp **expx, Exp **res)
{

// this code depends on the order of statements from emit_mux0x()

#ifndef MUX_AS_CJMP

  if (i < 0
      || i >= ir->size()
      || ir->at(i+0)->stmt_type != VARDECL /* temp */
      || ir->at(i+1)->stmt_type != VARDECL  /* widened_cond */
      || ir->at(i+2)->stmt_type != MOVE
      || ir->at(i+3)->stmt_type != MOVE)
    return -1;

  Move *s0 = (Move*)ir->at(i+2);
  Move *s1 = (Move*)ir->at(i+3);

  if (s0->lhs->exp_type != TEMP
      || s1->lhs->exp_type != TEMP
      || s0->rhs->exp_type != CAST
      || s1->rhs->exp_type != BINOP)
    return -1;
  
  Cast *e1 = static_cast<Cast*> (s0->rhs);
  BinOp *e2 = static_cast<BinOp*> (s1->rhs);
  if (e1->cast_type != CAST_SIGNED
      || e2->binop_type != BITOR
      || e2->lhs->exp_type != BINOP
      || e2->rhs->exp_type != BINOP)
    return -1;
  
  Exp *e3 = e1->exp; /* e3 is the condition */
  BinOp *e4 = static_cast<BinOp*> (e2->lhs); /* e4 is the true branch */
  BinOp *e5 = static_cast<BinOp*> (e2->rhs); /* e5 is the false branch */

  if (e4->binop_type != BITAND
      || e5->binop_type != BITAND)
    return -1;

  Exp *e6 = e4->lhs; /* this is exp0 */
  Exp *e7 = e5->lhs; /* this is expX */
  
  if (cond)
    *cond = e3;
  if (exp0)
    *exp0 = e6;
  if (expx)
    *expx = e7;
  if (res)
    *res = NULL; /* XXX: Not sure what to do here */

  // cout << "MATCH!!!!" << endl;
  
#else
  
  if (i < 0 || i >= ir->size()
      || ir->at(i)->stmt_type != MOVE
      || ir->at(i+3)->stmt_type != MOVE
      || ir->at(i+2)->stmt_type != LABEL
      || ir->at(i+4)->stmt_type != LABEL
      || ir->at(i+1)->stmt_type != CJMP)
    return -1;
  Move *s0 = (Move*)ir->at(i);
  CJmp *s1 = (CJmp*)ir->at(i+1);
  Label *s2 = (Label*)ir->at(i+2);
  Move *s3 = (Move*)ir->at(i+3);
  Label *s4 = (Label*)ir->at(i+4);

  if (s0->lhs->exp_type != TEMP
      || s3->lhs->exp_type != TEMP
      || ((Temp*)s0->lhs)->name != ((Temp*)s3->lhs)->name)
    return -1;

  if (s1->t_target->exp_type != NAME
      || s1->f_target->exp_type != NAME
      || ((Name*)s1->f_target)->name != s2->label
      || ((Name*)s1->t_target)->name != s4->label
      )
    return -1;

  //cout <<"match_mux0x matched!\n";
  if (cond)
    *cond = s1->cond;
  if (exp0)
    *exp0 = s0->rhs;
  if (expx)
    *expx = s3->rhs;
  if (res)
    *res = s0->lhs;

#endif

  return 0;
}

reg_t get_exp_type_from_cast( Cast *cast )
{
    assert(cast);

    return cast->typ;

}

reg_t get_exp_type( Exp *exp )
{
    assert(exp);

    reg_t type;

    if ( exp->exp_type == TEMP )
    {
        type = ((Temp *)exp)->typ;
    }
    else if ( exp->exp_type == CONSTANT )
    {
        type = ((Constant *)exp)->typ;
    }
    else
    {
        panic("Expression has no type info: " + exp->tostring());
    }

    return type;
}


void 
do_cleanups_before_processing()
{
    if (count_opnd) {
	count_opnd = NULL;
    }
}

