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



#ifndef _AMI_SORT_H
#define _AMI_SORT_H

#include "ami_sort_impl.h"

#define SORT_DEBUG if(0)


/* ---------------------------------------------------------------------- */

// A version of AMI_sort that takes an input streamof elements of type
// T, creates an output stream and and uses the < operator to sort

// instream is allocated;  *outstream is created
// template<class T>
// AMI_err 
// AMI_sort(AMI_STREAM<T> *instream, AMI_STREAM<T> **outstream) {

//   cout << "Not implemented yet!\n";
//   exit(1);
//   return AMI_ERROR_NO_ERROR;
// }



/* ---------------------------------------------------------------------- */

// A version of AMI_sort that takes an input stream of elements of
// type T, creates an output stream, and a user-specified comparison
// function

// instream is allocated;  *outstream is created
// template<class T>
// AMI_err AMI_sort(AMI_STREAM<T> *instream, AMI_STREAM<T> **outstream,
//                  int (*cmp)(const T&, const  T&)) {

//   cout << "Not implemented yet!\n";
//   exit(1);
//   return AMI_ERROR_NO_ERROR;
// }



/* ---------------------------------------------------------------------- */
// A version of AMI_sort that takes an input stream of elements of
// type T, creates an output stream, and a user-specified comparison
// object. 

//The comparison object "cmp", of (user-defined) class represented by
//CMPR, must have a member function called "compare" which is used for
//sorting the input stream.




//  create  *outstream 
template<class T, class Compare>
AMI_err 
AMI_sort(AMI_STREAM<T> *instream, AMI_STREAM<T> **outstream, Compare *cmp, 
	 int deleteInputStream = 0) {
  char* name;
  queue<char*>* runList;
  int instreamLength;

  assert(instream && outstream && cmp); 
  instreamLength = instream->stream_len();

  if (instreamLength == 0) {
    *outstream = new AMI_STREAM<T>();
    if (deleteInputStream) {
      delete instream;
    }
    return AMI_ERROR_NO_ERROR;
  }
  
  SORT_DEBUG {
    instream->name(&name);
    cout << "AMI_sort: sorting stream" << name <<", len=" 
	 << instreamLength << endl;
    delete name;
    MM_manager.print();
  }
  
  //run formation
  runList = runFormation(instream, cmp);
  assert(runList && runList->length() > 0); 

  if (deleteInputStream) {
    delete instream;
  }

  if (runList->length() == 1) {
    //if 1 run only
    runList->dequeue(&name);
    *outstream = new AMI_STREAM<T>(name);
    delete name; //should be safe, stream makes its own copy
  } else {
    *outstream = multiMerge<T,Compare>(runList,  cmp);
    //i thought the templates are not needed in the call, but seems to
    //help the compiler..laura
  }

  assert(runList->length() == 0);
  delete runList;
  
  SORT_DEBUG {
    cout << "AMI_sort: done" << endl << endl;
    MM_manager.print();
  }

  assert(*outstream);  
  assert((*outstream)->stream_len() == instreamLength);
  return AMI_ERROR_NO_ERROR;
  
}



template<class  T, class Compare>
int
isSorted(AMI_STREAM<T> *str, Compare cmp) {
  T *prev, *crt;
  AMI_err ae;   

  assert(str);
  str->seek(0);
  
  if (str->stream_len() <2) return 1;
  
  ae = str->read_item(&crt);
  cout << "reading: " << *crt << endl;
  prev = new T (*crt);
  ae = str->read_item(&crt);
  while (ae == AMI_ERROR_NO_ERROR) {
    cout << "reading: " << *crt << endl;
    if (cmp.compare(*prev, *crt) != -1)
      assert(0);
      return 0;
    prev = crt;
    ae = str->read_item(&crt);
  }
  return 1;
}
             
                          
#endif // _AMI_SORT_H 
