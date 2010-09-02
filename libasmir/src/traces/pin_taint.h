// -*- c++ -*-

/**** TODO: Add verbosity ****/

#pragma once

#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <vector>
#include <string.h>
#include "pin.H"
#include "pin_frame.h"

// Size of temporary buffers
#define BUFSIZE 128

// TODO: we need a type for the mapping to variables/registers
typedef uint32_t var;
// We only consider 32-bit memory addresses
// #define address uint32_t
// And enable 2^32 different taint tags
// special values: 
//  * 0        -> untainted
//  * ffffffff -> mixed taint
//  * other n  -> nth input byte
#define MIXED_TAINT 0xFFFFFFFF
#define NOTAINT 0
typedef uint32_t t;

#define MASK1 0x000000FF
#define MASK2 0x0000FF00
#define MASK3 0x00FF0000
#define MASK4 0xFF000000

typedef std::map<var,t> context;

// Some bit masks
#define LOW8   0xff
#define HIGH8  0xff000000
#define LOW16  0xffff
#define HIGH16 0xffff0000


/***************** Syscalls ***************/
// FIXME: use the ones from /usr/include/asm/unistd.h
     
#define __NR_read		  3
#define __NR_open		  5
#define __NR_close		  6
#define __NR_execve		 11
#define __NR_mmap		 90
#define __NR_socketcall	102
#define __NR_mmap2		192

// Windows system calls @ http://code.google.com/p/miscellaneouz/source/browse/trunk/winsyscalls?spec=svn26&r=26
// FIXME: We should really handle different versions of Windows better
// These are for win7
#define __NR_createfilewin	0x0042
#define __NR_readfilewin 0x0111

/********************************************/

// socket specific calls
#define _A1_socket     0x1
#define _A1_bind       0x2
#define _A1_listen     0x4
#define _A1_accept     0x5
#define _A1_send       0x9
#define _A1_recv       0xa
#define _A1_setsockopt 0xe

/********************************************/

/*********** IDs for taint sources **********/

#define ARG_ID 2
#define ENV_ID 2

/*************  Operand Usage  **************/

#define RD 0x01
#define WR 0x10
#define RW 0x11

/********************************************/


struct ValSpecRec {
  uint32_t type;               // Type of value specifier.
  uint32_t loc;                // Location of this value.
  pintrace::PIN_REGISTER value;// Actual value.
  uint32_t usage;              // Operand usage (R, RW, W, etc)
  uint32_t taint;              // Taint status of the value
};

namespace pintrace { // We will use namespace to avoid collision

  
   // Tracking the taint during program flow
   class TaintTracker {

   public:

     TaintTracker(ValSpecRec *env);

     // A function to introduce taint in the contexts
     bool taintStart(uint32_t callno, uint32_t * args);

     bool taintIntroduction(uint32_t bytes, 
                            uint32_t * args, 
                            uint32_t &addr,
                            uint32_t &length);

     std::vector<TaintFrame> taintArgs(int args, char **argv);

     std::vector<TaintFrame> taintEnv(char **env);

     // A function to propagate taint
     void taintPropagation();

     // A function to apply taint policies
     bool taintChecking();

     void setTaintContext();

     void resetTaint();
     
     void setCount(uint32_t cnt);

     void setTaintArgs(bool taint);

     void setTaintEnv(string env_var);
      
     void trackFile(string file);

     void setTaintStdin();
      
     void setTaintNetwork();

     // Helpers
     // A function to check whether the instruction arguments are tainted
     uint32_t getReadTaint();

     bool hasTaint();

     bool propagatedTaint(bool branch);
      
     void printMem();

     void printRegs();

   private:

     // A flag to denote a syscall in progress
     uint32_t syscall;

     // The taint source (producing taint tags)
     uint32_t source;

     // a context defining a map from registers to taint
     context delta;

     // We can use a byte-centric approach, each byte maps to taint
     // a context defining a map from memory locations to taint
     context memory;

     // The table containing the values of the current instruction
     ValSpecRec *values;

     // How many values are being used
     uint32_t count;


     /********** Syscall-specific vars ***********/
     std::set<string> taint_files;
     std::set<uint32_t> fds;
     bool taint_net;
     bool taint_args;
     std::set<string> taint_env;

     /********************************************/



     void addTaintToWritten(uint32_t tag);
      
     bool isReg(uint32_t type);

     uint32_t getRegTaint(uint32_t reg_int);

     uint32_t getMemTaint(uint32_t addr, uint32_t type);

     uint32_t combineTaint(uint32_t oldtag, uint32_t newtag);

     uint32_t exists(context ctx, uint32_t elem);

     uint32_t getTaint(context ctx, uint32_t elem);

     uint32_t getSize(uint32_t type);

     bool isValid(uint32_t type);

     void setTaint(context &ctx, uint32_t key, uint32_t tag);

   };

}; // End of namespace
