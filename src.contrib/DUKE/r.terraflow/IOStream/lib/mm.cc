/*C
 * Original project: Lars Arge, Jeff Chase, Pat Halpin, Laura Toma, Dean
 *                   Urban, Jeff Vitter, Rajiv Wickremesinghe 1999
 * 
 * GRASS Implementation: Lars Arge, Helena Mitasova, Laura Toma 2002
 *
 * Copyright (c) 2002 Duke University -- Laura Toma 
 *
 * Copyright (c) 1999-2001 Duke University --
 * Laura Toma and Rajiv Wickremesinghe
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Duke University
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE TRUSTEES AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE TRUSTEES OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *C*/



// A simple registration based memory manager.

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream>
using namespace std;
#include <mm.h>

#define MM_DEBUG if(0)



/* ************************************************************ */
MM_register::MM_register() {
 
   instances++;
    if (instances > 1) {
      cerr << "MM_register(): Only 1 instance of MM_register should exist.\n";
      assert(0); //core dump if debugging
      exit(1);
    }
    assert(instances == 1);
 
    // by default, we ignore if memory limit is exceeded   
    register_new = MM_IGNORE_MEMORY_EXCEEDED;
}



/* ************************************************************ */
MM_register::~MM_register(void) {
 
  if (instances > 1) {
    cerr << "MM_register(): Only 1 instance of MM_register should exist.\n";
    assert(0); //core dump if debugging    
    exit(1);
  }
  assert(instances == 1);
  instances--;
}


/* ************************************************************ */
void MM_register::print() {
  
  size_t availMB = (remaining >> 20);
  if (remaining) {
    cout << "available memory: " << availMB << "MB "
	 << "(" << remaining << "B)"
	 << endl; 
  } else {
    cout << "available memory: " << remaining << "B, exceeding: " 
	 << used - user_limit << "B"
	 << endl; 
  }
}


/* ************************************************************ */
// User-callable method to set allowable memory size
MM_err MM_register::set_memory_limit (size_t new_limit) {

  assert( new_limit > 0); 
  if (used > new_limit) {
    //    return MM_ERROR_EXCESSIVE_ALLOCATION;    
    switch (register_new) {
    case MM_ABORT_ON_MEMORY_EXCEEDED:
      cerr << " MM_register::set_memory_limit to " << new_limit 
      	   << ", used " << used << ". allocation exceeds new limit.\n";
      cerr.flush();
      assert(0); //core dump if debugging
      exit(1);
      break;
      
    case MM_WARN_ON_MEMORY_EXCEEDED:
      cerr << " MM_register::set_memory_limit to " << new_limit 
	   << ", used " << used << ". allocation exceeds new limit.\n";
      break;
      
    case MM_IGNORE_MEMORY_EXCEEDED:
      break;
    }   
    user_limit = new_limit;
    remaining = 0;
    return MM_ERROR_NO_ERROR;
  }
  
  assert(used <= new_limit);
  // These are unsigned, so be careful.
  if (new_limit < user_limit) {
    remaining -= user_limit - new_limit;
  } else {
    remaining += new_limit - user_limit;
  }
  user_limit = new_limit;
  return MM_ERROR_NO_ERROR;
}  



/* ************************************************************ */
//only warn if memory limit exceeded
void MM_register::warn_memory_limit() {
  register_new = MM_WARN_ON_MEMORY_EXCEEDED;
}


/* ************************************************************ */
//abort if memory limit exceeded
void MM_register::enforce_memory_limit() {
  register_new = MM_ABORT_ON_MEMORY_EXCEEDED;

  if (used > user_limit) {
    cerr << " MM_register::enforce_memory_limit: limit=" << user_limit 
	 << ", used=" << used << ". allocation exceeds limit.\n";
    assert(0); //core dump if debugging
    exit(1);
  }
}


/* ************************************************************ */
//ignore memory limit accounting
void MM_register::ignore_memory_limit() {
  register_new = MM_IGNORE_MEMORY_EXCEEDED;
}


/* ************************************************************ */
// provide accounting state
MM_mode MM_register::get_limit_mode() {
  return register_new;
}

/* ************************************************************ */
// provide print ccounting state
void MM_register::print_limit_mode() {
  cout << "Memory manager registering memory in ";  
  switch (register_new)  {
  case MM_ABORT_ON_MEMORY_EXCEEDED:
    cout << "MM_ABORT_ON_MEMORY_EXCEEDED";
    break;
  case MM_WARN_ON_MEMORY_EXCEEDED:
    cout << "MM_WARN_ON_MEMORY_EXCEEDED";
    break;
  case  MM_IGNORE_MEMORY_EXCEEDED:
    cout << "MM_IGNORE_MEMORY_EXCEEDED";
    break;
  }
  cout << " mode." << endl;
}



/* ************************************************************ */
//return the amount of memory available before user-specified memory
//limit will be exceeded
size_t MM_register::memory_available() {
  return remaining;    
}

/* ************************************************************ */
size_t MM_register::memory_used() {
  return used;    
}


/* ************************************************************ */
size_t MM_register::memory_limit() {
  return user_limit;    
}


/* ---------------------------------------------------------------------- */
// return the overhead on each memory allocation request 


// SIZE_SPACE is to ensure alignment on quad word boundaries.  It may be
// possible to check whether a machine needs this at configuration
// time or if dword alignment is ok.  On the HP 9000, bus errors occur
// when loading doubles that are not qword aligned.
static const size_t SIZE_SPACE=(sizeof(size_t) > 8 ? sizeof(size_t) : 8);



int   MM_register::space_overhead ()  {
  return SIZE_SPACE;
}
  



/* ************************************************************ */
// check that new allocation request is below user-defined limit.
// This should be a private method, only called by operator new.
MM_err MM_register::register_allocation(size_t request) {

  if (request > remaining) {
    remaining = 0;
    used += request;
    return MM_ERROR_INSUFFICIENT_SPACE;
    
  } else {
    used      += request;     
    remaining -= request;
    return MM_ERROR_NO_ERROR;
  }
}



/* ************************************************************ */
// do the accounting for a memory deallocation request.
// This should be a private method, only called by operators 
// delete and delete [].
MM_err MM_register::register_deallocation(size_t sz) {
  
  if (sz > used) {
    used = 0;
    remaining = user_limit;
    return MM_ERROR_UNDERFLOW;
  } else {

    used      -= sz;        
    if (used < user_limit) {
      remaining = user_limit - used;
    } else {
      assert(remaining == 0);
    }
    return MM_ERROR_NO_ERROR;
  }
}


 
/* ************************************************************ */
void* operator new (size_t sz) {
  void *p;
  
  MM_DEBUG cout << "new: sz=" << sz << ", register " 
		<< sz+SIZE_SPACE << "B ,"; 

  if (MM_manager.register_allocation (sz + SIZE_SPACE) != MM_ERROR_NO_ERROR){
    //must be MM_ERROR_INSUF_SPACE
    switch(MM_manager.register_new) {
      
    case MM_ABORT_ON_MEMORY_EXCEEDED:
      cerr << "MM error: limit ="<< MM_manager.memory_limit() <<"B. " 
	   << "allocating " << sz << "B. " 
	   << "limit exceeded by " 
	   <<  MM_manager.memory_used() -  MM_manager.memory_limit()<<"B."
	   << endl;
      assert (0);		// core dump if debugging
      exit (1);
      break;
      
    case MM_WARN_ON_MEMORY_EXCEEDED:
      cerr << "MM warning: limit="<<MM_manager.memory_limit() <<"B. " 
	   << "allocating " << sz << "B. " 
	   << " limit exceeded by " 
	   <<  MM_manager.memory_used() -  MM_manager.memory_limit()<<"B."
	   << endl;
      break;
      
    case MM_IGNORE_MEMORY_EXCEEDED:
      break;
    }
  }
  
  p = malloc(sz + SIZE_SPACE);
  
  if (!p) {
    cerr << "new: out of memory while allocating " << sz << "B" << endl;
    assert(0);
    exit (1);
  }
  
  *((size_t *) p) = sz;
  
  MM_DEBUG cout << "ptr=" << (void*) (((char *) p) + SIZE_SPACE) << endl;
  
  return ((char *) p) + SIZE_SPACE;
}




/* ---------------------------------------------------------------------- */
void operator delete (void *ptr)  {
  size_t sz;
  void *p;
  
  MM_DEBUG cout << "delete: ptr=" << ptr << ","; 

  if (!ptr) {
    cerr << "MM warning: operator delete was given a NULL pointer\n";
    cerr.flush();
    //this may actually happen: for instance when calling a default
    //destructor for something that was not allocated with new
    //e.g. ofstream str(name) ---- ~ofstream() called ==> ptr=NULL
    
    //assert(0); 
    //exit(1);
    return;
  }
  
  assert(ptr);
  p = ((char *)ptr) - SIZE_SPACE; // the base of memory
  sz = *((size_t *)p);
  
  MM_DEBUG cout << "size=" << sz <<", free " << p << "B and deallocate " 
		<< sz + SIZE_SPACE << endl;
  
  if(MM_manager.register_deallocation (sz + SIZE_SPACE) != MM_ERROR_NO_ERROR){
    //must be MM_ERROR_UNDERFLOW
    cerr << "delete: MM_manager.register_deallocation failed\n";
    assert(0);
    exit(1);
  }

  free(p);
}




/* ---------------------------------------------------------------------- */
void operator delete[] (void *ptr) {
  size_t sz;
  void *p;
  
  MM_DEBUG cout << "delete[]: ptr=" << ptr << ","; 

  if (!ptr) {
    //can this hapen? -- it does: see delete above
    cerr << "MM warning: operator delete [] was given a NULL pointer\n";
    cerr.flush();
    //assert(0);
    //exit(1);
    return;
  }
   assert(ptr);
   p = ((char *)ptr) - SIZE_SPACE; // the base of memory
   sz = *((size_t *)p);

   MM_DEBUG cout << "size=" << sz <<", free " << p << "B and deallocate " 
		 << sz + SIZE_SPACE << endl;
   
   if(MM_manager.register_deallocation (sz + SIZE_SPACE)!= MM_ERROR_NO_ERROR){
     //must be MM_ERROR_UNDERFLOW
     cerr << "delete[]: MM_manager.register_deallocation failed\n";
     assert(0);
     exit(1);
   }
   
   free(p);
}





/* ************************************************************ */
// Instantiate the actual memory manager, and allocate the 
// its static data members
MM_register MM_manager;
int MM_register::instances = 0; // Number of instances. (init)
// TPIE's "register memory requests" flag
MM_mode MM_register::register_new = MM_ABORT_ON_MEMORY_EXCEEDED; 






/* ************************************************************ */
// The counter of mm_register_init instances.  It is implicity set to 0.
unsigned int mm_register_init::count;

// The constructor and destructor that ensure that the memory manager is
// created exactly once, and destroyed when appropriate.
mm_register_init::mm_register_init(void) {
  if (count++ == 0) {
    MM_manager.set_memory_limit(MM_DEFAULT_MM_SIZE);
  }
}

mm_register_init::~mm_register_init(void) {
  --count;
}
