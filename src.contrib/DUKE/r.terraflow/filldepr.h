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


#ifndef __FILL_DEPR_H
#define __FILL_DEPR_H

#include <ami.h>
#include "types.h"
#include "water.h"


/************************************************************/
/* INPUT: edgelist of watershed adjacency graph E={(u,v,h)}, 1 \le u,v
\le W; the maximum number of watersheds

h is the smallest height on the boundary between watershed u and
watershed v;

E contains the edges between the watersheds on the boundary and the
outside face; 

the outside face is assumed to be watershed number W+1

E is sorted increasingly by (h,u,v)

OUTPUT: an array raise[1..W], raise[i] is the height to which the
watershed i must be raised in order to have a valid flow path to the
outside watershed; 
*/
/************************************************************/

elevation_type* fill_depression(AMI_STREAM<boundaryType>*boundaryStr,
				cclabel_type maxWatersheds);

elevation_type*  inmemory_fill_depression(AMI_STREAM<boundaryType>*boundaryStr,
					  cclabel_type maxWatersheds);

elevation_type*  ext_fill_depression(AMI_STREAM<boundaryType>*boundaryStr,
				     cclabel_type maxWatersheds);



/************************************************************/
/* returns the amount of mmemory allocated by
   inmemory_fill_depression() */
/************************************************************/
size_t inmemory_fill_depression_mmusage(cclabel_type maxWatersheds);


/************************************************************/
/* produce a new stream where each elevation e inside watershed i is
   replaced with max(raise[i], e) */
/************************************************************/
void commit_fill(AMI_STREAM<labelElevType>* labeledGrid, 
		 elevation_type* raise, cclabel_type maxWatersheds, 
		 AMI_STREAM<elevation_type>* filledGrid);

#endif

