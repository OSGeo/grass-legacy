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



#ifndef _MM_H
#define _MM_H

#include <sys/types.h>


#define MM_REGISTER_VERSION 2

// The default amount of memory we will allow to be allocated (40MB).
#define MM_DEFAULT_MM_SIZE (40<<20)


// MM accounting modes
typedef enum {
  MM_IGNORE_MEMORY_EXCEEDED=0,
  MM_ABORT_ON_MEMORY_EXCEEDED,
  MM_WARN_ON_MEMORY_EXCEEDED
} MM_mode;


// MM Error codes
enum MM_err {
  MM_ERROR_NO_ERROR = 0,
  MM_ERROR_INSUFFICIENT_SPACE,
  MM_ERROR_UNDERFLOW,
  MM_ERROR_EXCESSIVE_ALLOCATION
};


// types of memory usage queries we can make on streams
enum MM_stream_usage {
  // Overhead of the object without the buffer
  MM_STREAM_USAGE_OVERHEAD = 1,

  // amount used by a buffer
  MM_STREAM_USAGE_BUFFER,

  // Amount currently in use.
  MM_STREAM_USAGE_CURRENT,

  // Maximum amount possibly in use.
  MM_STREAM_USAGE_MAXIMUM
};




// Declarations of a very simple memory manager desgined to work with
// BTEs that rely on the underlying OS to manage physical memory.
class MM_register {
private:
  // The number of instances of this class and descendents that exist.
  static int instances;
  
  // The amount of space remaining to be allocated.
  size_t   remaining;
  
  // The user-specified limit on memory. 
  size_t   user_limit;
  
  // the amount that has been allocated.
  size_t   used;
  
  // flag indicates how we are keeping track of memory 
  static MM_mode register_new;

protected: 
  // private methods, only called by operators new and delete.
  MM_err register_allocation  (size_t sz);
  MM_err register_deallocation(size_t sz);

  
public:
  MM_register();
  ~MM_register(void);

  MM_err set_memory_limit(size_t sz);  
  void   enforce_memory_limit ();    
  void   ignore_memory_limit ();     
  void   warn_memory_limit ();       
  MM_mode get_limit_mode();
  void print_limit_mode();

  size_t memory_available ();        
  size_t memory_used ();             
  size_t memory_limit ();            

  int    space_overhead ();          
 
  void print();

  friend class mm_register_init;
  friend void * operator new(size_t);
  friend void operator delete(void *);
  friend void operator delete[](void *);
};




// A class to make sure that MM_manager gets set up properly (only one
// instance) .
class mm_register_init {
private:
  // The number of mm_register_init objects that exist.
  static unsigned int count;
  
public:
  mm_register_init(void);
  ~mm_register_init(void);
};

static mm_register_init source_file_mm_register_init;





// Here is the single memory management object (defined in mm.C).
extern MM_register MM_manager;



#endif // _MM_H 
