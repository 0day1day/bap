#include "pin.H"
#include "pin_taint.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#include <cassert>

#ifdef _WIN32
/**
 * Getting WDK header files to include is a nightmare.  If you need to
 * change this, talk to me.
 *
 * -ejs
 */
namespace WINDOWS {
  // Define a target architecture for WDK
#define _X86_ 
#include "Wdm.h"
#undef _X86_
}

// Needed for STATUS_SUCCESS to work
typedef WINDOWS::NTSTATUS NTSTATUS;

#endif

using namespace std;
using namespace pintrace;

/**************** Initializers ****************/

//
TaintTracker::TaintTracker(ValSpecRec * env) 
  : syscall(0),
    source(1),
    values(env),
    taint_net(false),
    taint_args(false)
{}

//
void TaintTracker::setCount(uint32_t cnt)
{
  count = cnt;
}

//
void TaintTracker::setTaintArgs(bool taint)
{
  taint_args = taint;
}

//
void TaintTracker::setTaintEnv(string env_var)
{
  taint_env.insert(env_var);
}

//
void TaintTracker::trackFile(string file)
{
  taint_files.insert(file);
}

//
void TaintTracker::setTaintStdin()
{
#ifndef _WIN32
  fds.insert(STDIN_FILENO);
#else
  assert(FALSE);
#endif
}

//
void TaintTracker::setTaintNetwork()
{
  taint_net = true;
}

/**************** Helper Functions ****************/

//
bool TaintTracker::isValid(uint32_t type)
{
  return (type != VT_NONE);
}

// 
bool TaintTracker::isReg(uint32_t type)
{
  return isValid(type) && (type <= REGTYPE_LAST);
}

// 
uint32_t TaintTracker::exists(context ctx, uint32_t elem)
{
  return (ctx.find(elem) != ctx.end());
}

//
uint32_t TaintTracker::getSize(uint32_t type)
{
  uint32_t size;
  switch (type) {
      case VT_MEM128: case VT_REG128: size = 16; break;
      case VT_MEM64: case VT_REG64: size = 8; break;
      case VT_MEM32: case VT_REG32: size = 4; break;
      case VT_MEM16: case VT_REG16: size = 2; break;
      case VT_MEM8:  case VT_REG8:  size = 1; break;
      default:                      assert(false);
  }
  return size;
}

// Combining two taint tags
uint32_t TaintTracker::combineTaint(uint32_t oldtag, uint32_t newtag)
{
  if (newtag) {// its tainted
    if (oldtag == NOTAINT)
      return newtag; // FIXME
    else 
      return MIXED_TAINT;
  }
  return oldtag;
}

// 
void TaintTracker::printRegs()
{
  cerr << hex << endl << " ----------- Tainted Regs ------------ " << endl;
  for (context::iterator it = delta.begin(), ie = delta.end() ; it != ie ; ++it)
       cerr << REG_StringShort((REG)it->first) << " = " << it->second << endl;
}

//
void TaintTracker::printMem()
{
  cerr << hex << endl << " ----------- Tainted Mem ------------ " << endl;
  for (context::iterator it = memory.begin(), ie = memory.end() ; it != ie ; ++it)
    cerr << "Addr: " << it->first << " -> " << it->second << endl;
}

/***************** Taint Handlers *******************/

//
void TaintTracker::setTaint(context &ctx, uint32_t key, uint32_t tag)
{
  if (tag == NOTAINT)
    ctx.erase(key);
  else ctx[key] = tag;
}


// 
uint32_t TaintTracker::getTaint(context ctx, uint32_t elem)
{
  if (exists(ctx, elem))
    return ctx[elem];
  return NOTAINT;
}

// 
uint32_t TaintTracker::getMemTaint(uint32_t addr, uint32_t type)
{
  uint32_t tag = NOTAINT;
  //cerr << "Getting memory " << addr << endl;
  uint32_t size = getSize(type);
  for (uint32_t i = 0 ; i < size ; i++) {
    uint32_t status = getTaint(memory, addr+i);
    tag = combineTaint(tag, status);
  }
  return tag;
}

// 
uint32_t TaintTracker::getRegTaint(uint32_t reg)
{
  // cout << "Partial register: " << REG_StringShort((REG)reg) << endl;
  REG temp = REG_FullRegName((REG)reg);
  // cerr << "Full register: " << REG_StringShort(temp) << endl;
  return getTaint(delta,temp);
}

// 
uint32_t TaintTracker::getReadTaint()
{
  uint32_t tag = NOTAINT, tmp_tag = NOTAINT;
  for (uint32_t i = 0 ; i < count ; i++) {
    if ((values[i].usage & RD) == RD) {
      // this is a read
      if (isReg(values[i].type) 
	  && (values[i].loc != REG_EFLAGS)) // FIXME: no control-flow taint
	tmp_tag = getRegTaint(values[i].loc);
      else if (isValid(values[i].type))
	tmp_tag = getMemTaint(values[i].loc, values[i].type);
      tag = combineTaint(tag, tmp_tag);
    }
  }
  return tag;
}

/******************* Taint Analysis Rules ***************/

/******** Taint Introduction **********/

//
std::vector<TaintFrame> TaintTracker::taintArgs(int argc, char **argv)
{
  std::vector<TaintFrame> frms;
  if (taint_args) {
    cerr << "Tainting command-line arguments" << endl;
    for ( int i = 1 ; i < argc ; i++ ) {
      uint32_t len = strlen(argv[i]);
      for (uint32_t j = 0 ; j < len ; j++)
	setTaint(memory, (uint32_t)(argv[i]+j), source++);
      TaintFrame frm;
      frm.id = ARG_ID;
      frm.addr = (uint32_t)argv[i];
      frm.length = len;
      frms.push_back(frm);
    }
  }
  return frms;
}

//
std::vector<TaintFrame> TaintTracker::taintEnv(char **env)
{
  std::vector<TaintFrame> frms;
  for ( int i = 1 ; env[i] ; i++ ) {
    string var(env[i]);
    int equal = var.find('=');
    var = var.substr(0,equal);
    if (taint_env.find(var) != taint_env.end()) {
      cerr << "Tainting environment variable: " << var << endl;
      uint32_t len = strlen(env[i]) - var.size();
      uint32_t addr = (uint32_t)env[i]+equal+1;
      for (uint32_t j = 0 ; j < len ; j++)
	setTaint(memory, (addr+j), source++);
      TaintFrame frm;
      frm.id = ENV_ID;
      frm.addr = addr;
      frm.length = len;
      frms.push_back(frm);
    }
  }
  return frms;
}

/** This function is called right before a system call. */
bool TaintTracker::taintStart(uint32_t callno, uint32_t * args)
{
  //cout << "Syscall no: " << callno << endl << "Args:" ;
  //for ( int i = 0 ; i < MAX_SYSCALL_ARGS ; i ++ )
  //  cout << hex << " " << args[i] ;
  //cout << endl ;
  syscall = 0;

  bool reading_tainted = false;
  char filename[128];
  switch (callno) {
#ifndef _WIN32
      case __NR_open:
        // FIXME: use PIN_SafeCopy
        strncpy(filename, (char *)args[0],128); 
        if (taint_files.find(string(filename)) != taint_files.end()) {
          cerr << "Opening tainted file: " << string(filename) << endl;
          syscall = __NR_open;
        }
        break;
      case __NR_close:
        syscall = __NR_close;
        break;
        // TODO: do we care about the offset?
      case __NR_mmap:
      case __NR_mmap2:
        if (fds.find(args[4]) != fds.end())
          syscall = __NR_mmap2;
        break;
      case __NR_read: 
        if (fds.find(args[0]) != fds.end()) {
          syscall = __NR_read;
          reading_tainted = true;
        }
        break;
      case __NR_socketcall:
        // TODO: do we need to distinguish between sockets?
        if (taint_net) {
          syscall = __NR_socketcall;
          if (args[0] == _A1_recv)
            reading_tainted = true;
        }
        break;
      case __NR_execve:
        break;
#else /* windows */
      case __NR_createfilewin:
      {
        char tempstr[BUFSIZE];
        WINDOWS::POBJECT_ATTRIBUTES pattr = reinterpret_cast<WINDOWS::POBJECT_ATTRIBUTES> (args[2]);		  
        assert(pattr);
        assert(pattr->ObjectName);
        errno_t e;
        WINDOWS::PWSTR fname = pattr->ObjectName->Buffer;
        size_t origsize = wcslen(fname) + 1;
        size_t convertedChars = 0;
        wcstombs_s(&convertedChars, tempstr, origsize, fname, BUFSIZE-1);
        if (convertedChars < origsize) {
          cerr << "Warning: Could not convert all characters" << endl;
        }
        
        if (taint_files.find(string(tempstr)) != taint_files.end()) {
          cerr << "Opening tainted file: " << string(tempstr) << endl;
          syscall = __NR_createfilewin;
        }
        
        break;
      }
      case __NR_readfilewin:
      {    
        if (fds.find(args[0]) != fds.end()) {
          syscall = __NR_read;
          reading_tainted = true;
          cerr << "found a t-read " << endl;
          syscall = __NR_readfilewin;
        }
        break;
      }
#endif
      default:
        //cerr << "Unknown system call " << callno << endl;
        break;
  }
  return reading_tainted;
}

/** This function is called immediately following a function call. */
bool TaintTracker::taintIntroduction(uint32_t bytes, 
                                     uint32_t * args,
                                     uint32_t &addr,
                                     uint32_t &length)
{
  //cout << "ret Syscall no: " << bytes << endl << "Args:" ;
  //for ( int i = 0 ; i < MAX_SYSCALL_ARGS ; i ++ )
  //cout << hex << " " << args[i] ;
  //cout << endl ;
  switch (syscall) {
#ifndef _WIN32 /* unix */
      case __NR_socketcall:
        switch (args[0]) {
            case _A1_recv:
              addr = ((uint32_t *)args[1])[1];
              length = bytes;
              cerr << "Tainting " 
                   << bytes 
                   << "bytes from socket" << endl;
              for (uint32_t i = 0 ; i < length ; i ++)
                setTaint(memory, addr + i, source++);
              return true;
            case _A1_accept:
              cerr << "Accepting an incoming connection" << endl;
              fds.insert(bytes);
              break;
            case _A1_socket:
              cerr << "Opening a tainted socket " << bytes << endl;
              fds.insert(bytes);
              break;
            default:
              break;
        }
        break;
      case __NR_open:
        // "bytes" contains the file descriptor
        fds.insert(bytes);
        break;
      case __NR_close:
        fds.erase(args[0]);
        break;
      case __NR_mmap:
      case __NR_mmap2:
        addr = bytes;
        length = args[1];
        cerr << "Tainting " 
             << length 
             << "bytes from mmap" << endl;
        for (uint32_t i = 0 ; i < length ; i ++)
          setTaint(memory, addr + i, source++);
        return true;
        break;
      case __NR_read:
        addr = args[1];
        length = bytes;
        cerr << "Tainting " 
             << length 
             << " bytes from read at " << addr << endl;
        for (uint32_t i = 0 ; i < length ; i ++)
          setTaint(memory, addr + i, source++);
        return true;
#else /* windows */
      case __NR_createfilewin:
        
        // If opened
        if (bytes == STATUS_SUCCESS) {
          WINDOWS::PHANDLE p = reinterpret_cast<WINDOWS::PHANDLE> (args[0]);
          uint32_t fd = reinterpret_cast<uint32_t> (*p);
          cerr << "Tainting file descriptor " << fd << endl;
          fds.insert(fd);
        }
        break;
      case __NR_readfilewin:
        if (bytes == STATUS_SUCCESS) {
          WINDOWS::PIO_STATUS_BLOCK psb = reinterpret_cast<WINDOWS::PIO_STATUS_BLOCK> (args[4]);
          uint32_t length;
          assert(psb);
          assert(psb->Information);
          length = psb->Information;
          addr = args[5];
          assert(addr);
          cerr << "Tainting " 
               << length 
               << " bytes from read @" << addr << endl;
          for (uint32_t i = 0 ; i < length ; i ++)
            setTaint(memory, addr + i, source++);
          return true;
        }
        break;
#endif
      default:
        break;
  }
  return false;
}

/******** Taint Propagation **********/

//     
void TaintTracker::setTaintContext()
{
  uint32_t tag;
  for (uint32_t i = 0 ; i < count ; i++) {
    if (isReg(values[i].type)) {
      if ((tag = getRegTaint(values[i].loc)) != NOTAINT) {
	// cerr << "register: " << REG_StringShort((REG)values[i].loc) << " is tainted" << endl;
	values[i].taint = tag;
      }
    } else if (isValid(values[i].type)) {
      if ((tag = getTaint(memory,values[i].loc)) != NOTAINT) {
	//cerr << "memory: " << values[i].loc << " is tainted" << endl;
	values[i].taint = tag;
      }
    }
  }
  
}

// Reset the taint status of registers and memory
void TaintTracker::resetTaint() {
  delta.clear();
  memory.clear();
}

// 
void TaintTracker::addTaintToWritten(uint32_t tag)
{
  uint32_t loc;
  cerr <<hex ;
  for (uint32_t i = 0 ; i < count ; i++) {
    if ((values[i].usage & WR) == WR)  {
      if (isReg(values[i].type)) {
	loc = REG_FullRegName((REG)values[i].loc);
	setTaint(delta,loc,tag);
	values[i].taint = getRegTaint(loc);
	//cerr << "new " << REG_StringShort((REG)values[i].loc) 
	//     << " taint: " << values[i].taint << endl;
      } else if (isValid(values[i].type)) {
	//cerr << hex << "writing " << values[i].loc << " = " << tag << endl;
	loc = values[i].loc;
	uint32_t size = getSize(values[i].type);
	for(uint32_t j = 0 ; j < size ; j++) {
	  //cerr << " Tainting memory " << loc + j << endl;
	  setTaint(memory,loc+j,tag);
	}
	values[i].taint = getTaint(memory,loc);
	//cerr << "mem taint: " << values[i].taint << endl;
      } 
    }
  }
}

// 
void TaintTracker::taintPropagation()
{
  //printMem();
  //printRegs();
  uint32_t taint_tag = getReadTaint();
  addTaintToWritten(taint_tag);
}

/******** Taint Checking **********/

// 
bool TaintTracker::hasTaint()
{
  cerr << hex ;
  for (uint32_t i = 0 ; i < count ; i++) {
    if (isReg(values[i].type)) {
      if (getRegTaint(values[i].loc) != NOTAINT) {
	//cerr << "Tainted: " << REG_StringShort((REG)values[i].loc) << endl;
	return true;
      }
    } else if (isValid(values[i].type)) {
      if (getTaint(memory,values[i].loc) != NOTAINT) {
	//cerr << "Tainted Memory: " << values[i].loc << endl;
	return true;
      }
    }
  }
  return false;
}

// 
bool TaintTracker::propagatedTaint(bool branch)
{
  if (branch)
    return true;
  for (uint32_t i = 0 ; i < count ; i++)
    if ((values[i].usage == RD) 
        && (values[i].loc != REG_EFLAGS) 
        && values[i].taint)
      return true;
  return false;
} 

// 
bool TaintTracker::taintChecking()
{
  for (uint32_t i = 0 ; i < count ; i++)
    if ((values[i].loc == REG_INST_PTR) && values[i].taint)
      return false;
  return true;
}



