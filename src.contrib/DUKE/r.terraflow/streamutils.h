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


#ifndef STREAMUTILS_H
#define STREAMUTILS_H

#include <fstream.h>

#include <ami.h>
#include "types.h"
#include "common.h"




template<class T>
void
printStream(ostream &s, AMI_STREAM<T> *str) {
  T *elt;
  AMI_err ae;

  str->seek(0);
  while((ae = str->read_item(&elt)) == AMI_ERROR_NO_ERROR) {
	s << *elt << endl;
  }
  str->seek(0);
}



/* laura note: this works that class T has an empty contructor which
   initializes it to the nodata value */ 
template<class T, class FUN>
void
printStream2Grid(AMI_STREAM<T> *str, 
		 dimension_type nrows, dimension_type ncols,
		 const char *name,
		 FUN fmt) {
  T *elt, nodata;
  AMI_err ae;
  ofstream fstrm(name);

  stats->comment("saving grid: ", name);

  fstrm << "rows=" << nrows << endl;
  fstrm << "cols=" << ncols << endl;
  
  str->seek(0);
  ae = str->read_item(&elt);
  assert(ae == AMI_ERROR_NO_ERROR || ae == AMI_ERROR_END_OF_STREAM);
  for(dimension_type i=0; i<nrows; i++) {
    for(dimension_type j=0; j<ncols; j++) {
    
      if(ae == AMI_ERROR_NO_ERROR && elt->i == i && elt->j == j) {
	fstrm << " " << fmt(*elt);
	ae = str->read_item(&elt);
	assert(ae == AMI_ERROR_NO_ERROR || ae == AMI_ERROR_END_OF_STREAM);
      } else {
	fstrm << " " << fmt(nodata);
      }
    } /* for j */
    fstrm << endl;
  }
  assert(ae == AMI_ERROR_END_OF_STREAM); /* stream must have finished */
  str->seek(0);
}


template<class T, class FUN>
void printGridStream(AMI_STREAM<T> *str, 
		     dimension_type nrows, dimension_type ncols,
		     char *name,
		     FUN fmt) {
  T *elt;
  AMI_err ae;
  ofstream fstrm(name);

  stats->recordLength("saving grid", str);
  fstrm << "rows=" << nrows << endl;
  fstrm << "cols=" << ncols << endl;

  assert(str->stream_len() == nrows * ncols);
  str->seek(0);
  for(dimension_type i=0; i<nrows; i++) {
	for(dimension_type j=0; j<ncols; j++) {
	  ae = str->read_item(&elt);
	  assert(ae == AMI_ERROR_NO_ERROR);
	  fstrm << " " << fmt(*elt);
	}
	fstrm << endl;
  }
  str->seek(0);
}





/* ********************************************************************** */

/* assume sorted... */
template<class T, class FUN>
AMI_STREAM<T> *
removeDuplicates(AMI_STREAM<T> *str, FUN fo) {
  AMI_err ae;

  AMI_STREAM<T> *newStr = new AMI_STREAM<T>();
  if(str->stream_len() == 0) return newStr;	/* empty stream */

  str->seek(0);
  T prev, *elp;
  ae = str->read_item(&elp);
  assert(ae == AMI_ERROR_NO_ERROR);
  prev = *elp;
  while((ae = str->read_item(&elp)) == AMI_ERROR_NO_ERROR) {
	if(fo.compare(*elp, prev)) {	/* differ */
	  newStr->write_item(prev);
	  prev = *elp;
	} else {
	  /* cout << "duplicate: " << *elp << " of " << prev << endl; */
	}
  }
  newStr->write_item(prev);		/* last one */
  return newStr;
}

/* ********************************************************************** */

template<class T, class FUN>
void
removeDuplicatesEx(AMI_STREAM<T> **str, FUN fo) {
  AMI_STREAM<T> *tmp = removeDuplicates(*str, fo);
  delete *str;
  *str = tmp;
}



/* ********************************************************************** */

/* 
 * merge a grid and a stream together to form an new grid of the original type
 * str should be sorted in ij order 
 */
template<class T, class TT, class FUN>
AMI_STREAM<T> *
mergeStream2Grid(AMI_STREAM<T> *grid, 
		 dimension_type rows, dimension_type cols, 
		 AMI_STREAM<TT> *str,
		 FUN fo) {
  AMI_err ae, aeS;
  T *gep;						/* grid element */
  TT *sep;						/* stream element */

  AMI_STREAM<T> *mergeStr = new AMI_STREAM<T>();
  str->seek(0);
  grid->seek(0);
  aeS = str->read_item(&sep);
  assert(aeS == AMI_ERROR_NO_ERROR || aeS == AMI_ERROR_END_OF_STREAM);

  for(dimension_type i=0; i<rows; i++) {
	for(dimension_type j=0; j<cols; j++) {
	  ae = grid->read_item(&gep);
	  assert(ae == AMI_ERROR_NO_ERROR);
	  if((aeS == AMI_ERROR_NO_ERROR) && (sep->i == i) && (sep->j == j)) {
		ae = mergeStr->write_item(fo(*sep));
		assert(ae == AMI_ERROR_NO_ERROR);
		aeS = str->read_item(&sep);
		assert(aeS == AMI_ERROR_NO_ERROR || aeS == AMI_ERROR_END_OF_STREAM);
	  } else {
		ae = mergeStr->write_item(fo(*gep));
		assert(ae == AMI_ERROR_NO_ERROR);
	  }
	}
  }

  return mergeStr;
}




#endif
