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


#ifndef _types_H
#define _types_H

#include <limits.h>
#include <iostream.h>



/* input parameters type */
/* ------------------------------------------------------------ */

typedef short dimension_type; /* represent dimension of the grid */
static const dimension_type dimension_type_max=SHRT_MAX;

typedef short direction_type;  /* represent the direction of a cell */
static const direction_type DIRECTION_UNDEF=-1;

/* one of these options must be defined at compile time */
/* #define ELEV_SHORT */
/* #define ELEV_FLOAT */

#if (!defined ELEV_SHORT && !defined ELEV_FLOAT)
#error Must define ELEVATION TYPE 
#endif

#ifdef ELEV_SHORT
typedef short elevation_type;  /* represent the elevation of a cell */
static const elevation_type elevation_type_max = SHRT_MAX;
#endif
#ifdef ELEV_FLOAT
typedef  float elevation_type;  /* represent the elevation of a cell */
static const elevation_type elevation_type_max = 1e+15;
#endif
/* static const elevation_type ELEVATION_UNDEF=SHRT_MAX;
   static const elevation_type ELEVATION_MIN=SHRT_MIN;
   also see nodata.H for ELEVATION_BOUNDARY and ELEVATION_NODATA
*/


/* represent the topological rank of a cell */
typedef int toporank_type; 



/* output parameter types */
/* ------------------------------------------------------------ */
typedef float flowaccumulation_type;    
static const flowaccumulation_type MAX_ACCU = 1e+15;
typedef float tci_type;       




typedef int cclabel_type;
#define CCLABEL_FMT "%3d"
/* the following are not arbitrary. LABEL_UNDEF should be the
 * most-negative value */
static const cclabel_type LABEL_UNDEF=-1;
static const cclabel_type LABEL_BOUNDARY=0;
static const cclabel_type LABEL_NODATA=1;
static const cclabel_type LABEL_START=1; /* the next label will be used */

typedef int bfs_depth_type;
static const bfs_depth_type DEPTH_INITIAL=1;

#define IS_BOUNDARY(i,j,nr,nc) (((i)==0) || ((i)==((nr)-1)) || \
							  ((j)==0) || ((j)==((nc)-1)))


/* ---------------------------------------------------------------------- */

class labelFactory {
protected:
  static cclabel_type label;
public:
  static cclabel_type getNewLabel() { return ++label; }
  static cclabel_type getCurrentLabel() { return label; }
  static const cclabel_type getLabelInit() { 
	return cclabel_type(LABEL_START);
  }
  static const cclabel_type getLabelCount() {
	return label+1;
  }
  static void setLabelCount(int n) {
	label = n-1;
  }
  static void reset() {	label = getLabelInit(); }
};

/* ---------------------------------------------------------------------- */

class ijBaseType {
public:
  dimension_type i,j;
  ijBaseType() : i(-1), j(-1) {};
  ijBaseType(dimension_type gi, dimension_type gj) : i(gi), j(gj) {};
  friend int operator==(const ijBaseType &a, const ijBaseType &b) {
	return (compare(a,b) == 0);
  }
  friend int operator!=(const ijBaseType &a, const ijBaseType &b) {
	return (compare(a,b) != 0);
  }
  friend ostream& operator << (ostream& s, const ijBaseType &p);
  static int compare(const ijBaseType &a, const ijBaseType &b);
};

#define SHALLOW_OP_EQ(_cls)			\
  friend int					\
  operator==(const _cls &a, const _cls &b) {	\
    const int n = sizeof(_cls);			\
    const char *ap = (const char *)&a;		\
    const char *bp = (const char *)&b;		\
    for(int i=0; i<n; i++) {			\
      if(ap[i] != bp[i]) return 0;		\
    }						\
    return 1;					\
  }



#endif

