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



#ifndef NODATA_H
#define NODATA_H

#include <iostream.h>
#include <assert.h>

#include <ami.h>
#include "types.h"
#include "option.h"



/* somewhat of a hack. should read the GRASS nodata value instead
   (which normally is MAX_INT), but, is it worth it? */
#define TERRAFLOW_INTERNAL_NODATA_VALUE -9999



int is_nodata(elevation_type el);
int is_nodata(int x);
int is_nodata(float x);
int is_void(elevation_type el);


class nodataType : public ijBaseType {
public: /* struct, so members public */
  cclabel_type label;
  bool valid;
  static elevation_type ELEVATION_BOUNDARY; /* means this cell is on	
					     * the grid boundary, 
					     * directly, or via
					     * connect nodata cells */

  static elevation_type ELEVATION_NODATA; /* means we have no data for	
					   * this cell */
public:
  nodataType(dimension_type gi, dimension_type gj, cclabel_type glab) :
	ijBaseType(gi,gj), label(glab), valid(true) {};
  nodataType() : valid(false) {};
  void invalidate() { valid = false; }
  elevation_type getElevation() {
	return (label == LABEL_BOUNDARY ? ELEVATION_BOUNDARY : ELEVATION_NODATA);
  }

  static char *printLabel(const nodataType &p) {
	static char buf[3];
	sprintf(buf, CCLABEL_FMT, p.label);
	return buf;
  }

  static void init(elevation_type nodata) {
	/*  somewhat of a hack... */
	ELEVATION_NODATA = nodata;
	ELEVATION_BOUNDARY = nodata+1;
	/* ELEVATION_BOUNDARY = ELEVATION_MIN; */
	/* assert(ELEVATION_NODATA != ELEVATION_MIN); */
  }
  
  /* LAURA: i added this polymorph because Terraflow has now a FIXED
     internal value for nodata; it does not read GRASS nodata value */
  static void init() {
	/* somewhat of a hack... */
	ELEVATION_NODATA = TERRAFLOW_INTERNAL_NODATA_VALUE;
	ELEVATION_BOUNDARY = TERRAFLOW_INTERNAL_NODATA_VALUE + 1;
	/* ELEVATION_BOUNDARY = ELEVATION_MIN; */
	/* assert(ELEVATION_NODATA != ELEVATION_MIN); */
  }
  
  friend ostream& operator << (ostream& s, const nodataType &p) {
	if(p.valid) {
	  return s << "[" << p.i << "," << p.j 
			   << "; lbl=" << p.label << "]";
	} else {
	  return s << "[invalid]";
	}
  }
  
};

class nodataType2elevation_type {
public:
  elevation_type operator()(nodataType n) { return n.getElevation(); }
  elevation_type operator()(elevation_type n) { return n; }
};

class labelCmpNodataType {
public:
  static int compare(const nodataType &a, const nodataType &b) {
	if(a.label < b.label) return -1;
	if(a.label > b.label) return 1;
	return 0;
  }
};

class ijCmpNodataType {
public:
  static int compare(const nodataType &a, const nodataType &b) {
	return ijBaseType::compare(a, b);
  }
};


AMI_STREAM<elevation_type> *
classifyNodata(AMI_STREAM<elevation_type> *elstr);
			   

#endif
