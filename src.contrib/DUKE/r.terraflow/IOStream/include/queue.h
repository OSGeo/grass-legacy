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




#ifndef QUEUE_H
#define QUEUE_H

#include <iostream.h>
#include <assert.h>

template<class T> 
class queue {
private:
  T *data;
  int size;
  int head;  // first valid location (if data)
  int tail;  // next free location
  int len;
  void grow();
public:
  queue(int size=4096);
  ~queue();
  bool enqueue(T &);
  bool dequeue(T *);
  bool peek(int offset, T *);
  bool isEmpty() const { return len==0; };
  //int length() const { return len; };
  unsigned int length() const { return (unsigned int)len; };
};


template<class T> 
queue<T>::queue(int vsize) : size(vsize) {
  data = new T[size];
  head = 0;
  tail = 0;
  len = 0;
}


template<class T> 
queue<T>::~queue() {
  delete [] data;
}


template<class T> 
bool
queue<T>::enqueue(T &elt) {
  if(len==size) grow();
  assert(len<size);
  data[tail] = elt;
  tail = (tail+1)%size;
  len++;
  return true;
}

template<class T> 
bool 
queue<T>::dequeue(T *elt) {
  if(len>0) {
	*elt = data[head];	  
	head = (head+1)%size;
	len--;
	return true;
  } 
  return false;
}


template<class T> 
bool 
queue<T>::peek(int offset, T *elt) {
  if(len>offset) {
	int pos = (head+offset)%size;
	*elt = data[pos];	  
	return true;
  } 
  return false;
}

template<class T> 
void
queue<T>::grow() {
  T *data2 = new T[size*2];
  int k=head;
  for(int i=0; i<len; i++) {
	data2[i] = data[k];
	k = (k+1)%size;
  }
  head = 0;
  tail = len;
  delete [] data;
  data = data2;
  size *= 2;
}


#endif // QUEUE_H
