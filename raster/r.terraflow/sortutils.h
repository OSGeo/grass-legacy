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


#ifndef SORTUTILS_H
#define SORTUTILS_H

#include <fstream.h>

#include <ami.h>
#include "common.h"




/* ********************************************************************** */
/* deletes input stream *str and replaces it by the sorted stream */

template<class T, class FUN>
void
sort(AMI_STREAM<T> **str, FUN fo) {
  Rtimer rt;
  AMI_STREAM<T> *sortedStr;

  stats->recordLength("pre-sort", *str);
  rt_start(rt);

  /* let AMI_sort create its output stream */
  int eraseInputStream = 1;
  AMI_sort(*str,&sortedStr, &fo, eraseInputStream);
  rt_stop(rt);
  /* delete *str; */
  
  stats->recordLength("sort", sortedStr);
  stats->recordTime("sort", rt);
  
  sortedStr->seek(0);
  *str = sortedStr;

}





/* ********************************************************************** */

/* warning - creates a new stream!! */
template<class T, class FUN>
AMI_STREAM<T> *
sort(AMI_STREAM<T> *strIn, FUN fo) {
  Rtimer rt;
  AMI_STREAM<T> *strOut;

  stats->recordLength("pre-sort", strIn);
  rt_start(rt);

  /* #ifdef TPIE
	 strOut = new AMI_STREAM<T>();
 
	 #ifdef FO_SORT
	 AMI_sort(strIn, strOut, &fo);
	 #else
	 AMI_sort(strIn, strOut, FUN::compare);
	 #endif

	 #else
  */ 
  AMI_sort(strIn, &strOut, &fo);
  assert(strOut);
  /* #endif */
  
  rt_stop(rt);
  stats->recordLength("sort", strOut);
  stats->recordTime("sort", rt);
  
  strOut->seek(0);
  return strOut;
}

#endif

