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


#ifndef _MEM_STREAM_H
#define _MEM_STREAM_H

#include <stdlib.h>
#include <assert.h>

#include <iostream>
using namespace std;

template<class T>
class MEM_STREAM {
private:
  T *data;
  T *curr;
  T *dataend;
  int len;

public:
  MEM_STREAM(T *data, int len);
  ~MEM_STREAM(void);

  // Read and write elements.
  AMI_err read_item(T **elt);

  AMI_err write_item(const T &elt);

  // Return the number of items in the stream.
  off_t stream_len(void);

  // Return the path name of this stream.
  AMI_err name(char **stream_name);

  // Move to a specific item in the stream.
  AMI_err seek(off_t offset);

  char *sprint();
};


/**********************************************************************/

template<class T>
MEM_STREAM<T>::MEM_STREAM(T *datap, int lenv) {

  data = datap;
  dataend = data + lenv;
  curr = datap;
  len = lenv;

};


/**********************************************************************/
// Return the number of items in the stream.
template<class T>
off_t MEM_STREAM<T>::stream_len(void) {

  return len;

};



/**********************************************************************/
// Return the path name of this stream.
template<class T>
AMI_err MEM_STREAM<T>::name(char **stream_name)  {

  char *path = "dummy";

  *stream_name = new char [strlen(path) + 1];
  strcpy(*stream_name, path);

  return AMI_ERROR_NO_ERROR;
};


/**********************************************************************/
// Move to a specific offset within the (sub)stream.
template<class T>
AMI_err MEM_STREAM<T>::seek(off_t offset) {

  assert(offset <= len);

  curr = data + offset;

  return AMI_ERROR_NO_ERROR;
}



/**********************************************************************/
template<class T>
MEM_STREAM<T>::~MEM_STREAM(void)  {
};



/**********************************************************************/
template<class T>
AMI_err MEM_STREAM<T>::read_item(T **elt)  {

  assert(data);

  if(curr == dataend) {
    return AMI_ERROR_END_OF_STREAM;
  }
  *elt = curr;
  curr++;
  return AMI_ERROR_NO_ERROR;
};




/**********************************************************************/

template<class T>
AMI_err MEM_STREAM<T>::write_item(const T &elt) {

  assert(data);

  if(curr == dataend) {
    return AMI_ERROR_END_OF_STREAM;
  }
  *curr = elt;
  curr++;
  return AMI_ERROR_NO_ERROR;
};


/**********************************************************************/
// sprint()
// Return a string describing the stream
//
// This function gives easy access to the file name, length.
// It is not reentrant, but this should not be too much of a problem 
// if you are careful.
template<class T>
char *MEM_STREAM<T>::sprint()  {
  static char buf[BUFSIZ];
  sprintf(buf, "[MEM_STREAM %d]", stream_len());
  return buf;
};

#endif // _MEM_STREAM_H 
