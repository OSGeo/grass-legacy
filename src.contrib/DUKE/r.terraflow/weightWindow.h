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


#ifndef _weight_H
#define _weight_H

#include <stdio.h>
#include <iostream.h>

#include "types.h"         
#include "common.h"
#include "genericWindow.h"  
#include "direction.h"  



class weightWindow {
  
public:
  float cell_dx, cell_dy;       /* dimension of cell in the grid */
  float celldiag;     /* diagonal of a cell in the grid */
  float sumweight, sumcontour;
  genericWindow<float> weight;  /* weights */

protected:
 
  /* initialize all weights to 0 */
  void init();
  /* set weight of neighbor (di,dj) */
  void computeWeight(const short di, const short dj, 
		     const elevation_type elev_crt,
		     const elevation_type elev_neighb);
  /* normalize weights */
  void normalize();

  /* computes the contour corresponding to this direction  */
  double computeContour(const short di, const short dj);
 
  /* computes the distance corresponding to this direction */
  double computeDist(const short di, const short dj); 

  /* compute the tanB corresponding to the elevation window and
     neighbor di,dj. */
  double computeTanB(const short di,const short dj, 
		     const genericWindow<elevation_type>& elevwin);
		    

public:

  weightWindow(const float gdx, const float gdy);
  
  ~weightWindow(){};

  /***************************************************************/
  /* Compute the weights of the neighbors of a cell given an elevation
     window and a direction window; if trustdir = 1 then trust
     directions; otherwise compute the downslope neighbors and use
     direction only for cells which do not have downslope neighbors */
  /***************************************************************/
  void compute(const dimension_type i, const dimension_type j,
	       const genericWindow<elevation_type>& elevwin, 
	       const direction_type dir,
	       const int trustdir);
  
  /* Find the dominant direction. Set corresponding weight to 1, and
     sets all other weights to 0. Set sumweight and sumcontour.*/
  void makeD8(const dimension_type i, const dimension_type j,
	      const genericWindow<elevation_type>& elevwin, 
	      const direction_type dir,
	      const bool trustdir);
  
  /* get specified weight di,dj in {-1,0,1} */
  float get(const short di, const short dj) const {
    return weight.get(di,dj);
  }
  
  /* get specified weight i in 0..8 */
  float get(const unsigned short i) const {
    return weight.get(i);
  }

  /* return the total contour */
  float totalContour() const {
    return sumcontour;
  }
  
  float totatWeight() const {
    return sumweight;
  }

  float dx() const {
    return cell_dx;
  }

  float dy() const {
    return cell_dy;
  }
  
  friend ostream& operator<<(ostream& s, const weightWindow &x) {
    return s << x.weight;
  }
};

#endif

