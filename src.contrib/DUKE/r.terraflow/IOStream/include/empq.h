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

#ifndef __EMPQ_H
#define __EMPQ_H

#include <stdio.h>
#include <assert.h>



#include "ami_config.h" //for SAVE_MEMORY
#include "ami_stream.h"
#include "mm.h"
#include "mm_utils.h" //for MEMORY_LOG, getAvailableMemory
#include "imbuffer.h"
#include "embuffer.h"
#include "pqheap.h"
#include "minmaxheap.h"



template<class T,class Key> class ExtendedEltMergeType;
#define ExtendedMergeStream AMI_STREAM<ExtendedEltMergeType<T,Key> >


/**********************************************************
                  DEBUGGING FLAGS 
***********************************************************/

//enables printing messages when buffers are emptied
//#define EMPQ_EMPTY_BUF_PRINT  

//enables printing when pq gets filled from buffers
//#define EMPQ_PQ_FILL_PRINT

//enables priting inserts 
//#define EMPQ_PRINT_INSERT

//enables printing deletes
//#define EMPQ_PRINT_EXTRACTALL

//enables printing the empq on insert/extract_all_min
//#define EMPQ_PRINT_EMPQ

//enable priting the size of the EMPQ and nb of active streams
//on fillpq() amd on empty_buff_0
//#define EMPQ_PRINT_SIZE

//enable printing 'fill pq from B0' in extract_min()
//#define EMPQ_PRINT_FILLPQ_FROM_BUFF0

//enable expensive size asserts
//#define EMPQ_ASSERT_EXPENSIVE


/**********************************************************/






/* external memory priority queue

   Functionality:

   Keep a pqueue PQ of size \theta(M) in memory.  Keep a buffer B0 of
   size \theta(M) in memory.  keep an array of external-memory
   buffers, one for each level 1..log_m{n/m} (where N is the maximum
   number of items in pqueue at any time).

   invariants: 
   1. PQ contains the smallest items in the structure.
   
   2. each stream of any external memory buffers is sorted in
   increasing order.

   insert(x): if (x < maximum_item(PQ) exchange x with
   maximum_item(PQ); if buffer B0 is full, empty it; insert x in B0;
   
   extract_min():

   analysis: 
   
   1. inserts: once the buffer B0 is empty, the next sizeof(B0)
   inserts are free; one insert can cause many I/Os if cascading
   emptying of external buffers Bi occurs. Emptying level-i buffer
   costs <arity>^i*sizeof(B0)/B I/Os and occurs every
   N/<arity>^i*sizeof(B0) inserts (or less, if deletes too). It can be
   proved that the amortized time of 1 insert is 1/B*maxnb_buffers.
*/

/* 
T is assumed to be a class for which getPriority() and getValue()
are implemented; for simplicity it is assumed that the comparison
operators have been overloaded on T such that 
x < y <==> x.getPriority() < y.getPriority() 
*/

template<class T, class Key> 
class em_pqueue {

private:  
  
  //in memory priority queue
  MinMaxHeap<T> *pq;
  
  //pqueue size
  unsigned long pqsize;

  //in-memory buffer
  im_buffer<T> *buff_0;

  //in-memory buffer size
  unsigned long bufsize;

  //external memory buffers
  em_buffer<T,Key>** buff;
  
  /* number of external memory buffers statically allocated in the
     beginning; since the number of buffers needed is \log_m{n/m}, we
     cannot know it in advance; estimate it roughly and then reallocate
     it dynamically on request;
     
     TO DO: dynamic reallocation with a bigger nb of external buffer
     if structure becomes full */
  unsigned short max_nbuf;
  
  //index of next external buffer entry available for use (i.e. is NULL)
  unsigned short crt_buf;

  //external buffer arity
  unsigned int buf_arity;
  

public:
  
  //create an em_pqueue of specified size
  em_pqueue(long pq_sz, long buf_sz, unsigned short nb_buf, 
		   unsigned int buf_ar);  
  
  //create an em_pqueue capable to store <= N elements
  em_pqueue();
  em_pqueue(long N) { em_pqueue(); }; // N not used

#ifdef SAVE_MEMORY
  // create an empq, initialize its pq with im and insert amis in
  // buff[0]; im should not be used/deleted after that outside empq
  em_pqueue(MinMaxHeap<T> *im, AMI_STREAM<T> *amis);
#endif

  //copy constructor
  em_pqueue(const em_pqueue &ep);

  //clean up
  ~em_pqueue();

  //return the nb of elements in the structure 
  unsigned long size();

  //return true if empty
  bool is_empty();
  
  //return true if full
  bool is_full() { 
    cout << "em_pqueue::is_full(): sorry not implemented\n";
    exit(1);
  }

  //return the element with minimum priority in the structure
  bool min(T& elt);
  
  //delete the element with minimum priority in the structure;
  //return false if pq is empty
  bool extract_min(T& elt);
  
  //extract all elts with min key, add them and return their sum
  bool extract_all_min(T& elt);
  
  //insert an element; return false if insertion fails
  bool insert(const T& elt);
  
  //return maximum capacity of i-th external buffer
  long maxlen(unsigned short i);

  //return maximum capacity of em_pqueue
  long maxlen();

  //print structure
  void print_range();

  void print();

  //print the detailed size of empq (pq, buf_0, buff[i])
  void print_size();

  friend ostream& operator<<(ostream& s, const em_pqueue &empq) {
    s << "EM_PQ: pq size=" << empq.pqsize
      << ", buff_0 size=" << empq.bufsize 
      << ", ext_bufs=" << empq.crt_buf 
      << "(max " << empq.max_nbuf << ")\n";
    s << "IN_MEMORY PQ: \n" << *(empq.pq) << "\n";
    s << "IN_MEMORY BUFFER: \n" << *(empq.buff_0) << "\n";
    for (unsigned short i=0; i < empq.crt_buf; i++) {
      //s << "EM_BUFFER " << i << ":\n" ;
      s << *(empq.buff[i]);
    }
    return s;
  }


protected:
  //return the nb of active streams in the buffer
  int active_streams() {
    int totstr = 0;
    for (unsigned short i = 0; i< crt_buf; i++) {
      totstr+= buff[i]->get_nbstreams();
    }
    return totstr;
  }

  //called when buff_0 is full to empty it on external level_1 buffer;
  //can produce cascading emptying
  bool empty_buff_0();

  //sort and empty buffer i into buffer (i+1) recursively; 
  //called recursively by empty_buff_0() to empty subsequent buffers
  //i must be a valid (i<crt_buf) full buffer
  void empty_buff(unsigned short i);
  
  
  /* merge the first <K> elements of the streams of input buffer,
     starting at position <buf.deleted[i]> in each stream; there are
     <buf.arity> streams in total; write output in <outstream>; the
     items written in outstream are of type <merge_output_type> which
     extends T with the stream nb and buffer nb the item comes from;
     this information is needed later to distribute items back; do not
     delete the K merged elements from the input streams; <bufid> is the
     id of the buffer whose streams are being merged;
     
     the input streams are assumed sorted in increasing order of keys; */
  AMI_err merge_buffer(em_buffer<T,Key> *buf,
			      ExtendedMergeStream *outstr, long K);
  

  /* merge the first <K> elements of the input streams; there are
     <arity> streams in total; write output in <outstream>;
     
     the input streams are assumed sorted in increasing order of their
     keys; */
  AMI_err merge_streams(ExtendedMergeStream** instr, 
			       unsigned short arity,
			       ExtendedMergeStream *outstr, long K);
  
  //deletes one element from <buffer, stream>
  void delete_str_elt(unsigned short buf_id,
			     unsigned int stream_id);

  /* copy the minstream in the internal pqueue while merging with
     buff_0; the smallest <pqsize> elements between minstream and
     buff_0 will be inserted in internal pqueue; also, the elements
     from minstram which are inserted in pqueue must be marked as
     deleted in the source streams; */
  void merge_bufs2pq(ExtendedMergeStream *minstream); 

  //clean buffers in case some streams have been emptied 
  void cleanup();
  
  //called when pq must be filled from external buffers
  bool fillpq();

  //print the nb of elements in each stream
  void print_stream_sizes();
};



#endif
