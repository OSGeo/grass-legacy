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



#ifndef __EMPQ_ADAPTIVE_H
#define __EMPQ_ADAPTIVE_H


#include "minmaxheap.h"
#include "empq.h"
#include "empq_impl.h"



#define EMPQAD_DEBUG if(0)


enum regim_type {
  INMEM = 0,
  EXTMEM,
  EXTMEM_DEBUG
};


template<class T, class Key> 
class EMPQueueAdaptive {
private: 
  //dictates if the structure works in the internal/external memory regim;
  regim_type regim;  
  MinMaxHeap<T> *im;
  em_pqueue<T,Key> *em;
  UnboundedMinMaxHeap<T> *dim;	// debug, internal memory pq
public:
  /* start in INMEM regim by allocating im of size precisely twice the
     size of the (pqueue within) the em_pqueue; */
  EMPQueueAdaptive(long N) : EMPQueueAdaptive() {};
  EMPQueueAdaptive();
  ~EMPQueueAdaptive();

  void makeExternal();
  void makeExternalDebug();

  long maxlen() const;			//return the maximum nb of elts that can fit
  bool is_empty() const;		//return true if empty
  bool is_full() const;			//return true if full
  bool min(T& elt);				//return the element with min priority XXX
  //delete the element with minimum priority in the structure;
  //return false if pq is empty
  bool extract_min(T& elt);

  //extract all elts with min key, add them and return their sum XXX
  bool extract_all_min(T& elt);

  /* insert an element; if regim == INMEM, try insert it in im, and if
     it is full, extract_max pqsize/2 elements of im into a stream,
     switch to EXTMEM and insert the stream into em; if regim is
     EXTMEM, insert in em; */
  bool insert(const T& elt);  

  long size() const; //return the nb of elements in the structure

  void verify();
};



#endif
