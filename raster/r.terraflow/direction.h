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


#ifndef _direction_H
#define _direction_H


#include <stdio.h>

#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
#include <ostream>
#else
#include <ostream.h>
#endif

using namespace std;

#include <ami.h>

#include "types.h"
#include "genericWindow.h"
#include "nodata.h"



/***************************************************************/
/***************************************************************/
class directionWindow: public genericWindow<bool> {

public:
  int numdir;

  /***************************************************************/
  /* Modifies the window by setting to 1 only the neighbors to which
     direction point. This is the inverse function of encodeDirection().
     
     direction:   
     32 64 128  
     16 *   1   
     8  4   2 */
  directionWindow(direction_type dir)
    : genericWindow<bool>() {

    /* first set everything to 0 */
    numdir = 0;
    int i;
    for (i=0; i<9; i++) {
      set(i, false);
    }

    if (dir == 0 || dir == DIRECTION_UNDEF) {
      return;
    }
    assert(dir > 0 && dir < 256);
    if (dir & 1) {
      set(5, true); numdir++;
    }
    if (dir & 2) {
      set(8, true); numdir++;
    }
    if (dir & 4) {
      set(7, true); numdir++;
    }
    if (dir & 8) {
      set(6, true); numdir++;
    }
    if (dir & 16) {
      set(3, true); numdir++;
    }
    if (dir & 32) {
      set(0, true); numdir++;
    }
    if (dir & 64) {
      set(1, true); numdir++;
    }
    if (dir & 128) {
      set(2, true); numdir++;
    }
  }

  /***************************************************************/
  /* Check direction consistency. */
  void checkDirection(short di, short dj, int skipit,
		      elevation_type el, elevation_type elneighb) const {
#ifndef NDEBUG 
    if (skipit == 1) {
      assert(get(di,dj) ==  false);
    } else {
      if ((el > elneighb) && !is_nodata(elneighb) && !is_nodata(el)) {
	assert(get(di,dj) ==  true);
      }
    }
#endif   
  }
  
  
  /***************************************************************/
  /* Correct direction (di,dj). If direction points to invalid
     neighbor which must be skipped, set it to 0; if direction does
     not point to valid downslope neighbor, set it to 1. */
  void correctDirection(short di, short dj, int skipit, 
			dimension_type i, dimension_type j,
			elevation_type elev_crt, direction_type dir,
			elevation_type elev_neighb) {
    
    if (skipit && get(di,dj) == true) {
      cout << "WARNING:  at (" 
	   << i << "," << j << " , h=" << elev_crt << ", dir=" << dir << ")"
	   << "direction points to non-valid neighbor ("
	   <<  i + di << ","
	   <<  j + dj << ", h="
	   << elev_crt - elev_neighb
	   << ")\n";
      set(di,dj, false); /* correct it */
    }
    if (!skipit && elev_crt > elev_neighb &&  !is_nodata(elev_neighb)
	&& get(di,dj) == 0) {
      set(di,dj, true); /* correct it */
    }
  }
};


direction_type encodeDirection(const genericWindow<elevation_type>& elevwin,
			       const dimension_type nrows, 
			       const dimension_type ncols,
			       dimension_type row, dimension_type col);

direction_type findDominant(direction_type dir);
char directionSymbol(direction_type dir);


#endif

