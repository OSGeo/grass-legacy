/*C
 * Original project: Lars Arge, Jeff Chase, Pat Halpin, Laura Toma, Dean
 *		     Urban, Jeff Vitter, Rajiv Wickremesinghe 1999
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


#ifndef _genericwindow_H
#define _genericwindow_H

#include <stdio.h>

#include <ami.h>
#include "types.h"


/* ************************************************************* *
 * class 'genericWindow' implements a 3x3 window in a grid; 
 * ************************************************************* */

template<class T>
class genericWindow {
protected:
  T data[9]; 
public:

  /***************************************************************/
  /* initialize a window to 0 */
  genericWindow() {
    for (int i=0; i<9;i++) {
	  data[i] = T();
	}
  }

  /***************************************************************/
  /* initialize a window from an array of 9 values */
  genericWindow(T* a) {
    assert(a);
    for (int i=0; i<9;i++) {
	  data[i] = a[i];
	}
  }

  /***************************************************************/
  /* initialize a window from 3 arrays of 3 elements each */
  genericWindow(T *a, T *b, T*c) {
    int i;
    assert(a); assert(b); assert(c);
    for (i=0;i<3;i++) {
	  data[i] = a[i];
	  data[i+3] = b[i];
	  data[i+6] = c[i];	  
    }
  }

  /***************************************************************/
  /* initialize a window from 3 arrays of 3 elements each */
  template<class C>
  genericWindow(C *a, C *b, C*c) {
    int i;
    assert(a); assert(b); assert(c);
    for (i=0;i<3;i++) {
	  data[i] = a[i];
	  data[i+3] = b[i];
	  data[i+6] = c[i];	  
    }
  }

  /***************************************************************/
  genericWindow(const genericWindow<T> &win) {
    for (int i=0;i<9;i++) {
	  data[i] = win.data[i];
    }
  }

  /***************************************************************/
  /* get specified neighbour di,dj in {-1,0,1} */
  T get(short di, short dj) const {
    assert (di>=-1 && di<=1);
    assert(dj>=-1 && dj<=1);
	return data[4+dj+di*3];
  }

  /***************************************************************/
  /* get specified neighbour i in 0..8 */
  T get(unsigned short i=4) const {
	assert(i <= 8);
	return data[i];
  }

  /***************************************************************/
  /* set specified neighbour i in 0..8 */
  void set(unsigned short i, T val) {
    assert(i <= 8);
	data[i] = val;
  }
  
  /***************************************************************/
  /* set specified neighbour di,dj in {-1,0,1} */
  void set(int di, int dj, T val) {
    assert (di>=-1 && di<=1);
    assert(dj>=-1 && dj<=1);
	data[4+dj+di*3] = val;
  }

  /***************************************************************/
  /*  multiply all elements by a scalar */
  void scalarMultiply(T mult) {
	for(int i=0; i<9; i++) {
	  data[i] *= mult;
	}
  }

  /***************************************************************/
  inline friend ostream& operator<<(ostream& s, const genericWindow<T> &x) {
	s << "[" << x.data[0] << "," << x.data[1] << "," << x.data[2] << "]\n";
	s << "[" << x.data[3] << "," << x.data[4] << "," << x.data[5] << "]\n";
	s << "[" << x.data[6] << "," << x.data[7] << "," << x.data[8] << "]\n";
	return s;
  }

};

typedef genericWindow<elevation_type> ElevationWindow;

void fillPit(ElevationWindow& win);

#endif

