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



#include "direction.h"
#include "nodata.h"

/***************************************************************/
/* returns the direction corresponding to the window */
/* directions:
   32 64 128
   16 *   1
   8  4   2
*/
direction_type 
encodeDirection(const genericWindow<elevation_type>& elevwin,
				const dimension_type nrows, const dimension_type ncols,
				dimension_type row, 
				dimension_type col) {
  
  direction_type dir = DIRECTION_UNDEF;
  
  if(!is_nodata(elevwin.get())) {
    dir = 0;
    if (elevwin.get(5) < elevwin.get() && !is_void(elevwin.get(5))) dir |= 1;
    if (elevwin.get(3) < elevwin.get() && !is_void(elevwin.get(3))) dir |= 16;
    for(int i=0; i<3; i++) {
      if(elevwin.get(i) < elevwin.get() && !is_void(elevwin.get(i))) dir |= 32<<i;
      if(elevwin.get(i+6) < elevwin.get() && !is_void(elevwin.get(6+i))) dir |= 8>>i;
    }
  }
  
  /* if no direction, check for boundary */
  if(dir==0 || dir==DIRECTION_UNDEF) {
    if(row==0) {
      dir = 32 | 64 | 128;
    }
    if(row==nrows-1) {
      dir = 2 | 4 | 8;
    }
    if(col==0) {
      if(row==0) dir = 32;
      else if(row==nrows-1) dir = 8;
      else dir = 8 | 16 | 32;
    }
    if(col==ncols-1) {
      if(row==0) dir = 128;
      else if(row==nrows-1) dir = 2;
      else dir = 128 | 1 | 2;
    }
  }
  return dir;
}



direction_type
findDominant(direction_type dir) {
  switch(dir) {
  case 1:
  case 2:
  case 4:
  case 8:
  case 16:
  case 32:
  case 64:
  case 128:
	return dir;

  case 1+2:
  case 128+1:
	return 1;
  case 2+4:
  case 4+8:
	return 4;
  case 8+16:
  case 16+32:
	return 16;
  case 32+64:
  case 64+128:
	return 64;

  case 1+2+4:
	return 2;
  case 2+4+8:
	return 4;
  case 4+8+16:
	return 8;
  case 8+16+32:
	return 16;
  case 16+32+64:
	return 32;
  case 32+64+128:
	return 64;
  case 64+128+1:
	return 128;
  case 128+1+2:
	return 1;

  case 128+1+2+4:
  case 64+128+1+2:
	return 1;
  case 1+2+4+8:
  case 2+4+8+16:
	return 4;
  case 8+16+32+64:
  case 4+8+16+32:
	return 16;
  case 32+64+128+1:
  case 16+32+64+128:
	return 64;
	
  case 64+128+1+2+4:
	return 1;
  case 128+1+2+4+8:
	return 2;
  case 1+2+4+8+16:
	return 4;
  case 2+4+8+16+32:
	return 8;
  case 4+8+16+32+64:
	return 16;
  case 8+16+32+64+128:
	return 32;
  case 16+32+64+128+1:
	return 64;
  case 32+64+128+1+2:
	return 128;
  }


  return dir;
}


char
directionSymbol(direction_type dir) {
  char c='?';
  int cnt=0;
  char *symbols = ">\\v/<\\^/";

  if(dir == 0) return '.';
  
  dir = findDominant(dir);

  for(int i=0; i<8; i++) {
	if(dir & (1<<i)) {
	  cnt++;
	  c = symbols[i];
	}
  }  
  if(cnt>1) c = 'X';

  switch(dir) {
  case 1+16:
  case 128+1+2+8+16+32:
	c = '-';
	break;
  case 1+2+8+16+32:
  case 128+1+8+16+32:
	c = '<';
	break;
  case 128+1+2+16+32:
  case 128+1+2+8+16:
	c = '>';
	break;
  case 4+64:
  case 2+4+8+32+64+128:
	c = '|';
	break;
  case 4+8+32+64+128:
  case 2+4+32+64+128:
	c = '^';
	break;
  case 2+4+8+64+128:
  case 2+4+8+32+64:
	c = 'v';
	break;
  case 255:
	c = '*';
	break;
  default:
	break;
  }
  return c;
} 


