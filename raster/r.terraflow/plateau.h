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



#ifndef PLATEAU_H
#define PLATEAU_H

#include <iostream.h>
#include <assert.h>

#include <ami.h>

#include "types.h"
#include "direction.h"
#include "genericWindow.h"

/* ---------------------------------------------------------------------- */
class plateauType : public ijBaseType {
public: /* struct, so members public */
  cclabel_type cclabel;
  direction_type dir;
  bool valid;
public:
  plateauType(dimension_type gi, dimension_type gj, direction_type gdir, 
	      cclabel_type gcclabel=LABEL_UNDEF) :
    ijBaseType(gi,gj), cclabel(gcclabel), dir(gdir), valid(true) {};
 
  plateauType() : valid(false) {};
  
  ~plateauType() {}
  
  void invalidate() { valid = false; }
  
  static char *printLabel(const plateauType &p) {
    static char buf[3];
    sprintf(buf, CCLABEL_FMT, p.cclabel);
    return buf;
  }

  friend ostream& operator << (ostream& s, const plateauType &p) {
    if(p.valid) {
      return s << "[" << (ijBaseType)p
	       << ": dir=" << p.dir 
	       << "; lbl=" << p.cclabel << "]";
    } else {
      return s << "[invalid]";
    }
  }
};

class ijCmpPlateauType {
public:
  static int compare(const plateauType &a, const plateauType &b) {
    return ijBaseType::compare(a, b);
  }
};

class labelCmpPlateauType {
public:
  static int compare(const plateauType &a, const plateauType &b) {
	if(a.cclabel < b.cclabel) return -1;
	if(a.cclabel > b.cclabel) return 1;
	return 0;
  }
};





/* ********************************************************************** */
class plateauStats {
public:
  dimension_type iMin, iMax, jMin, jMax;
  long size;
  cclabel_type label;
  bool hasSpill;
public:
 
  plateauStats() : label(LABEL_UNDEF) {}
  
  plateauStats(cclabel_type l) : 
    iMin(dimension_type_max), iMax(0), 
    jMin(dimension_type_max), jMax(0),
    size(0), label(l), hasSpill(false) {};
  
  void add(plateauType &pt) {
    assert(pt.cclabel == label);
    if(pt.i < iMin) iMin = pt.i;
    if(pt.i > iMax) iMax = pt.i;
    if(pt.j < jMin) jMin = pt.j;
    if(pt.j > jMax) jMax = pt.j;
    if(pt.dir > 0) hasSpill = true;
    size++;
  }

  SHALLOW_OP_EQ(plateauStats);
  
  friend ostream& operator << (ostream& s, const plateauStats &p) {
    return s << "[" << p.label << ": "
	     << "(" << p.iMin << "," << p.jMin << ")-"
	     << "(" << p.iMax << "," << p.jMax << "); "
	     << p.size << " " 
	     << (p.hasSpill?"S":".") << "]";
  }

};



/* ********************************************************************** */

AMI_STREAM<plateauType> *
findPlateaus(AMI_STREAM<elevation_type> *elstr,
			 const dimension_type nrows, const dimension_type ncols,
			 const elevation_type nodata_value,
			 AMI_STREAM<ElevationWindow > *winstr,
			 AMI_STREAM<direction_type> *dirStr,
			 AMI_STREAM<plateauStats> *statStr);

#endif

