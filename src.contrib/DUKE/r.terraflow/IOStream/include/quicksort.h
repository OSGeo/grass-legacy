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



#ifndef _QUICKSORT_H
#define _QUICKSORT_H
 
#include <stdlib.h> //for random()


// The class represented by CMPR, must have a member function called
// "compare" which is used for sorting



/* ---------------------------------------------------------------------- */
// On return from partition(), everything at or below pivot will be
// less that or equal to everything above it.  Furthermore, it will
// not be 0 since this will leave us to recurse on the whole array
// again.
template<class T, class CMPR>
void partition(T *data, size_t n, size_t &pivot, CMPR &cmp) {
    T *ptpart, tpart;
    T *p, *q;
    T t0;
    
    // Try to get a good partition value and avoid being bitten by already
    // sorted input.
    ptpart = data + (random() % n);
    tpart = *ptpart;
    *ptpart = data[0];
    data[0] = tpart;
    
    // Walk through the array and partition it.
    for (p = data - 1, q = data + n; ; ) {
      
      do {
	q--;
      } while (cmp.compare(*q, tpart) > 0);
      do {
	p++;
      } while (cmp.compare(*p, tpart) < 0);
      
      if (p < q) {
	t0 = *p;
	*p = *q;
	*q = t0;
      } else {
	pivot = q - data;            
	break;
      }
    }
}




/* ---------------------------------------------------------------------- */
template<class T, class CMPR>
void insertionsort(T *data, size_t n, CMPR &cmp) {
  T *p, *q, test;
  
  for (p = data + 1; p < data + n; p++) {
    for (q = p - 1, test = *p; (cmp.compare(*q, test) > 0); q--) {
      *(q+1) = *q;
      if (q==data) {
	q--; // to make assignment below correct
	break;
      }
    }
    *(q+1) = test;
  }
}




/* ---------------------------------------------------------------------- */
template<class T, class CMPR>
void quicksort(T *data, size_t n, CMPR &cmp, size_t min_len = 20)  {

  size_t pivot;
  if (n < min_len) {
    insertionsort(data, n, cmp);
    return;
  }
  //else
  partition(data, n, pivot, cmp);
  quicksort(data, pivot + 1, cmp, min_len);
  quicksort(data + pivot + 1, n - pivot - 1, cmp, min_len);
}




#endif // _QUICKSORT_H 














































