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


#include <time.h>
#include <ctype.h>

#include "flow.h"
#include "sweep.h"
#include "option.h"
#include "common.h"
#include "sortutils.h"
#include "streamutils.h"
#include "water.h"
#include "3scan.h"

/* globals in common.H

extern statsRecorder *stats;       stats file 
extern userOptions *opt;           command-line options 
extern struct  Cell_head *region;  header of the region 
extern dimension_type nrows, ncols;
*/

/* defined in this module */
AMI_STREAM<sweepItem>* 
fillstr2sweepstr(AMI_STREAM<waterWindowBaseType>* flowStream);





/* ********************************************************************** */
/* deletes fillStream */
void
computeFlowAccumulation(AMI_STREAM<waterWindowBaseType>* fillStream,
			AMI_STREAM<sweepOutput> *& outstr) {
  Rtimer rt, rtTotal;
  AMI_STREAM<sweepItem> *sweepstr;

  rt_start(rtTotal);
  assert(fillStream && outstr == NULL);
  stats->comment("------------------------------");
  stats->comment("COMPUTING FLOW ACCUMULATION");
  
  { /* timestamp stats file and print memory */
    time_t t = time(NULL);
    char buf[BUFSIZ];
    if(t == (time_t)-1) {
      perror("time");
      exit(1);
    }
    ctime_r(&t, buf);
    buf[24] = '\0';
    stats->timestamp(buf);
    *stats << endl;  
    
    size_t mm_size = (opt->mem  << 20); /* (in bytes) */
    formatNumber(buf, mm_size);
    *stats << "memory size: " << buf << " bytes\n";
  }

  /* create sweepstream using info from  fillStream  */
  sweepstr = fillstr2sweepstr(fillStream); 
  /* fillStream is deleted inside fillstr2sweepstr */

  /* sweep and dump outputs into outStream; trustdir=1 */
  outstr = sweep(sweepstr, opt->d8cut, 1);
  assert(outstr->stream_len() == sweepstr->stream_len());
  delete sweepstr;

  /* sort output stream into a grid */
  rt_start(rt);
  stats->comment( "sorting sweep output stream");
  stats->recordLength("output stream", outstr);
  sort(&outstr, ijCmpSweepOutput());
  rt_stop(rt);
  stats->recordLength("output stream", outstr);
  stats->recordTime("sorting output stream", rt);

  rt_stop(rtTotal);
  stats->recordTime("compute flow accumulation", rtTotal);

#ifdef SAVE_ASCII
  printStream2Grid(outstr, nrows, ncols, "flowaccumulation.asc", 
		   printAccumulationAscii());
  printStream2Grid(outstr, nrows, ncols, "tci.asc", 
		   printTciAscii());
#endif
  return;
}



/****************************************************************/
class flow_waterWindower {
 private:
  AMI_STREAM<sweepItem> *sweep_str;
 public:
  flow_waterWindower(AMI_STREAM<sweepItem> *str) :
    sweep_str(str) {};
  void processWindow(dimension_type i, dimension_type j, 
                     waterWindowBaseType *a,
                     waterWindowBaseType *b,
                     waterWindowBaseType *c);
};


/****************************************************************/
void
flow_waterWindower::processWindow(dimension_type i, dimension_type j, 
				  waterWindowBaseType *a,
				  waterWindowBaseType *b,
				  waterWindowBaseType *c) {
  
  elevation_type el1[3], el2[3], el3[3];
  toporank_type ac1[3], ac2[3], ac3[3];
  
  if (is_nodata(b[1].el)) {
    /*sweep_str does not include nodata */
    return;
  }
  /*#ifdef  COMPRESSED_WINDOWS
	sweepItem win = sweepItem(i, j, a, b, c);
	#else
  */
  for (int k=0; k<3; k++) {
    el1[k] = a[k].el;
    ac1[k] = -a[k].depth; /*WEIRD */
    el2[k] = b[k].el;
    ac2[k] = -b[k].depth; /*WEIRD*/
    el3[k] = c[k].el;
    ac3[k] = -c[k].depth; /*WEIRD*/
  }
  /*
	genericWindow<elevation_type> e_win(el);
	genericWindow<toporank_type> a_win(ac);
	sweepItem win = sweepItem(i, j, b[1].dir, e_win, a_win);
  */
  sweepItem win = sweepItem(i, j, b[1].dir, el1, el2, el3, ac1, ac2, ac3);
  /* #endif */
 
  AMI_err ae = sweep_str->write_item(win);
  assert(ae == AMI_ERROR_NO_ERROR);
}



/****************************************************************/
void
waterWindowBaseType2sweepItem(AMI_STREAM<waterWindowBaseType> *baseStr, 
			      const dimension_type nrows, 
			      const dimension_type ncols,
			      const elevation_type nodata_value,
			      AMI_STREAM<sweepItem> *sweep_str) {
  flow_waterWindower winfo(sweep_str);
  waterWindowBaseType nodata((elevation_type)nodata_value, 
							 (direction_type)nodata_value, 
							 DEPTH_INITIAL);
  /* 
	 assert(baseStr->stream_len() > 0);
	 XXX - should check if it fits in memory technically don't need to
	 give the template args, but seems to help the compiler 
	 memoryScan(*baseStr, hdr, nodata,  winfo); 
  */
  memoryScan<waterWindowBaseType,flow_waterWindower>(*baseStr, nrows, ncols, nodata, winfo); 
  
}


/****************************************************************/
/* open fill's output stream and get all info from there; delete
   fillStream */
AMI_STREAM<sweepItem>*
fillstr2sweepstr(AMI_STREAM<waterWindowBaseType>* fillStream) {

  Rtimer rt;
  AMI_STREAM<sweepItem> *sweepstr;  

  rt_start(rt);
  
  stats->comment("creating sweep stream from fill output stream");
  
  assert(fillStream->stream_len() == nrows * ncols);
  
  /* create the sweep stream */
  sweepstr = new AMI_STREAM<sweepItem>();
  waterWindowBaseType2sweepItem(fillStream, nrows, ncols,
				nodataType::ELEVATION_NODATA, sweepstr);
  delete fillStream;

  if (opt->verbose) {
    fprintf(stderr,  "sweep stream size: %.2fMB",
			(double)sweepstr->stream_len()*sizeof(sweepItem)/(1<<20));
    fprintf(stderr,  " (%d items, item size=%d B\n ", 
			(int)sweepstr->stream_len(), sizeof(sweepItem));;
  }
  stats->recordLength("sweep stream", sweepstr);
  
  /* sort sweep stream by (increasing) priority */
  if (opt->verbose) {
    fprintf(stderr, "sorting sweep stream (%.2fMB) in priority order\n", 
	    (double)sweepstr->stream_len()*sizeof(sweepItem)/(1<<20));
  }
  stats->comment("sorting sweep stream");
  sort(&sweepstr, PrioCmpSweepItem());

  rt_stop(rt);
  
  stats->recordTime("create sweep stream", rt);
  stats->recordLength("(sorted) sweep stream", sweepstr);

  return sweepstr;
}


