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



#include <stdlib.h>
#include <iostream.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include <ami.h>

#include "option.h"
#include "stats.h"
#include "sweep.h"
#include "common.h"
#include "weightWindow.h"
#include "nodata.h"
#include "sortutils.h"


/* 
   #define CHECKPARAM        //output the parameters during sweeping 
   #define CHECK_WEIGHTS     //output weights as computed
   #define SWEEP_PRINT_PQSIZE      //output priority queue size during sweeping
   #define CHECK_MEMORY  //enables printing available memory in between steps
*/

/* frequency; used to print progress dots */
static const int DOT_CYCLE = 50;
static const int PQSIZE_CYCLE = 100;


/* globals in common.H

extern statsRecorder *stats;       stats file 
extern userOptions *opt;           command-line options 
extern struct  Cell_head *region;  header of the region 
extern dimension_type nrows, ncols;
*/



/* SELECT FLOW DATA STRUCTURE */
#ifdef IM_PQUEUE
typedef pqheap_t1<flowStructure> FLOW_DATASTR;
#endif
#ifdef EM_PQUEUE
typedef em_pqueue<flowStructure, flowPriority> FLOW_DATASTR;
#endif
#ifdef EMPQ_ADAPTIVE
typedef EMPQueueAdaptive<flowStructure, flowPriority> FLOW_DATASTR;
#endif


/* defined in this module */
void pushFlow(const sweepItem& swit, const flowValue &flow, 
	      FLOW_DATASTR* flowpq, const weightWindow &weight);




/* ------------------------------------------------------------*/
sweepOutput::sweepOutput() {
  i = (dimension_type) nodataType::ELEVATION_NODATA;
  j = (dimension_type) nodataType::ELEVATION_NODATA;
  accu = (flowaccumulation_type) nodataType::ELEVATION_NODATA;  
#ifdef OUTPUT_TCI
  tci = (tci_type) nodataType::ELEVATION_NODATA;
#endif
};


/* ------------------------------------------------------------ */
/* computes output parameters of cell (i,j) given the flow value, the
   elevation of that cell and the weights of that cell; */
void
sweepOutput::compute(elevation_type elev, 
		    dimension_type i_crt, dimension_type j_crt, 
		    const flowValue &flow, 
		    const weightWindow &weight, 
		    const  elevation_type nodata) {
  
  float correct_tci; /* this the correct value of tci; we're going to
  truncate this on current precision tci_type set by user in types.H*/

  assert(elev != nodata);
  assert(flow.get() >= 0);
  assert(weight.sumweight >= 0 && weight.sumcontour >= 0);

  i = i_crt;
  j = j_crt;
  
  if (weight.sumweight == 0 || weight.sumcontour == 0) {
    accu = (flowaccumulation_type)nodata;
#ifdef OUTPUT_TCI
    tci = (tci_type)nodata;
#endif
    
  } else {
    accu = flow.get();
#ifdef OUTPUT_TCI
    correct_tci = log(flow.get()*weight.dx()*weight.dy()/weight.totalContour());
    /* assert(correct_tci > 0); //is this true? */
    if (correct_tci < 0) {
      fprintf(stderr, "warning: tci negative, [flow=%f,dx=%f,dy=%f,cont=%f]\n",
	      flow.get(), weight.dx(), weight.dy(), weight.totalContour());
    }
    tci = (tci_type)correct_tci;
#endif
  }
  
  return;
}




FLOW_DATASTR* 
initializePQ() {
  
  stats->comment("sweep:initialize flow data structure", opt->verbose);
  
  FLOW_DATASTR *flowpq;
#ifdef IM_PQUEUE
  stats->comment("FLOW_DATASTRUCTURE: in-memory pqueue");
  flowpq = new FLOW_DATASTR(PQ_SIZE);
  *stats << form("initialized to %.2fMB\n", (float)PQ_SIZE / (1<<20));
#endif
#ifdef EM_PQUEUE
  stats->comment("FLOW_DATASTRUCTURE: ext-memory pqueue");
  flowpq = new FLOW_DATASTR(nrows * ncols);  
#endif
#ifdef EMPQ_ADAPTIVE
  if (opt->verbose) stats->comment("FLOW_DATASTRUCTURE: adaptive pqueue");
  flowpq = new FLOW_DATASTR(); 
#endif
  return flowpq;
}
  

#define INIT_PRINT_PROGRESS()						\
  long out_frequency, pqsize_frequency;					\
  out_frequency = nrows*ncols / DOT_CYCLE;		\
  pqsize_frequency = nrows*ncols / PQSIZE_CYCLE;	\
  assert(out_frequency);						\
  assert(pqsize_frequency);

#define PRINT_PROGRESS(k) 			\
  if ((k) % out_frequency == 0) {		\
    fprintf(stderr,"."); 				\
    fflush(stderr);				\
  };

#ifdef SWEEP_PRINT_PQSIZE
#define PRINT_PQSIZE(k,flowpq)			\
  if ((k) % pqsize_frequency == 0) {		\
    fprintf(stderr," %ld ", (long)(flowpq)->size());	\
    fflush(stderr);				\
  }
#else
#define PRINT_PQSIZE(k,flowpq)
#endif



/***************************************************************/
/* Read the points in order from the sweep stream and process them.
   If trustdir = 1 then trust and use the directions contained in the
   sweep stream. Otherwise push flow to all downslope neighbors and
   use the direction only for points without downslope neighbors. */
/***************************************************************/

AMI_STREAM<sweepOutput>* 
sweep(AMI_STREAM<sweepItem> *sweepstr, const flowaccumulation_type D8CUT,
      const int trustdir) {
  flowPriority prio;
  flowValue flow;
  sweepItem* crtpoint;
  AMI_err ae;
  flowStructure x;	
  long nitems;
  Rtimer rt;
  AMI_STREAM<sweepOutput>* outstr;

  /* INIT_PRINT_PROGRESS(); */

  rt_start(rt);

  assert(sweepstr);

  *stats << "sweeping\n";
  fprintf(stderr,  "sweeping: ");
  /* create and initialize flow data structure */
  FLOW_DATASTR *flowpq;
  flowpq = initializePQ();
  
  /* create output stream */
  outstr = new AMI_STREAM<sweepOutput>();
  
  /* initialize weights and output */
  weightWindow weight(region->ew_res, region->ns_res);
  sweepOutput output;
  nitems = sweepstr->stream_len();

#ifndef NDEBUG
  flowPriority prevprio = flowPriority(SHRT_MAX);	/* XXX      */
#endif
  /* scan the sweep stream  */
  ae = sweepstr->seek(0);
  assert(ae == AMI_ERROR_NO_ERROR);
  for (long k = 0; k < nitems; k++) {
    
    /* cout << k << endl; cout.flush(); */
    /* read next sweepItem = (prio, elevwin, topoRankwin, dir) */
    ae = sweepstr->read_item(&crtpoint);
    if (ae != AMI_ERROR_NO_ERROR) {
      cerr << form("sweep: k=%ld: cannot read next item..\n", k);
      exit(1);
    }
    /* cout << "k=" << k << " prio =" << crtpoint->getPriority() << "\n"; */
    /* nodata points should not be in sweep stream */
    assert(!is_nodata(crtpoint->getElev()));
#ifndef NDEBUG    
    assert(crtpoint->getPriority() > prevprio); /* XXX */
    prevprio = crtpoint->getPriority(); /* XXX */
#endif
    
    
    /* compute flow accumulation of current point; initial flow value
       is 1 */
    flowValue flowini((double)1);
    /* add flow which was pushed into current point by upslope
       neighbours */
    assert(flowpq->is_empty() || 
		   (flowpq->min(x), x.getPriority() >= crtpoint->getPriority())); /* XXX */
    assert(flowpq->is_empty() != flowpq->min(x));	/* XXX */
    if (flowpq->min(x) && ((prio=x.getPriority()) == crtpoint->getPriority())) {
      flowpq->extract_all_min(x);
      /* cout << "EXTRACT: " << x << endl; */
      flow = x.getValue();
      flow = flow + flowini;
    } else {
      flow = flowini;
    }
    assert(flowpq->is_empty() || 
		   (flowpq->min(x), x.getPriority() > crtpoint->getPriority())); /* XXX */
	


    /* compute weights of current point given its direction */
    if (flow > D8CUT) {
      /* consider just the dominant direction */
      weight.makeD8(crtpoint->getI(), crtpoint->getJ(), 
		    crtpoint->getElevWindow(), crtpoint->getDir(), trustdir);
    } else {
      /* consider multiple flow directions */
      weight.compute(crtpoint->getI(), crtpoint->getJ(), 
		     crtpoint->getElevWindow(), crtpoint->getDir(), trustdir);
    }    
    
    
    /* distribute the flow to its downslope neighbours  */
    pushFlow(*crtpoint, flow, flowpq, weight);

    
    /* compute parameters  */
    output.compute(crtpoint->getElev(), crtpoint->getI(), crtpoint->getJ(),
		   flow, weight, nodataType::ELEVATION_NODATA);
#ifdef CHECKPARAM   
    printf("%7ld: (%5d, %5d, %5d) flow: %7.3f, weights:[",
	   k, crtpoint->getElev(), crtpoint->getI(),crtpoint->getJ(), 
	   flow.get());
    for (int l=0;l<9;l++) cout << form("%2.1f ",weight.get(l));
    cout <<"] ";
    cout << output << "\n";
#endif        

    /* write output to sweep output stream */
    ae = outstr->write_item(output);
    assert(ae == AMI_ERROR_NO_ERROR);
    
    /* PRINT_PROGRESS(k); */
    /* PRINT_PQSIZE(k, flowpq); */
    
    G_percent(k, nitems, 2);
  } /* for k  */
  
  fprintf(stderr, "\n");
  *stats << "sweeping done\n";
  *stats << form("pqsize = %ld \n", (long)flowpq->size());
  
  assert(outstr->stream_len() == nitems);
  delete flowpq; 

  rt_stop(rt);
  stats->recordTime("sweeping", rt);
  stats->recordLength("sweep output stream", outstr);

  return outstr;
}







/***************************************************************/
/* push flow to neighbors as indicated by flow direction and reflected
   by the weights of the neighbors; flow is the accumulated flow value
   of current point; The neighbours which receive flow from current
   point are inserted in the FLOW_DATASTR */
/***************************************************************/
void 
pushFlow(const sweepItem& swit, const flowValue &flow, 
	 FLOW_DATASTR *flowpq, 
	 const weightWindow &weight) {
  
  dimension_type  i_crt, j_crt, i_neighb, j_neighb;
  short di, dj;
  elevation_type elev_crt, elev_neighb;
  toporank_type toporank_crt;
 
  assert(flow >= 0);
  /* get current coordinates, elevation, topological rank */
  i_crt = swit.getI(); 
  j_crt = swit.getJ();
  elev_crt = swit.getElev();
  toporank_crt = swit.getTopoRank();
  assert(!is_nodata(elev_crt)); 

  for (di = -1; di <= 1; di++) {
    for (dj = -1; dj <= 1; dj++) {
      if (weight.get(di,dj) > 0) {

		/* push flow to this neighbor  */
	i_neighb = i_crt + di;  
	j_neighb = j_crt + dj;
	elev_neighb = swit.getElev(di,dj);
	
	/*assert(IS_BOUNDARY(i_crt,j_crt,hdr) || elev_neighb !=hdr.get_nodata());*/
	/* it's not simple to check what nodata is on boundary, so we'll
	just assume directions are correct even if they point to nodata
	elevation values. */

	if (!is_nodata(elev_neighb)) {
	  flowPriority prio(elev_neighb, swit.getTopoRank(di,dj), 
			    i_neighb, j_neighb);
	  flowPriority prio_crt(elev_crt,toporank_crt, i_crt, j_crt);
	  /* assert(prio >= prio_crt); */
#if (defined WARNING_FLAG)
	  if (prio < prio_crt) {
	    cout << form("\n(row=%d,col=%d,ele=%d): ",
			 i_crt, j_crt, elev_crt);
	    cout << "attempt to push flow uphill\n";
	  }
#endif
	  flowValue elt(weight.get(di,dj)*flow.get());
	  flowStructure x(prio, elt);
	  assert(x.getPriority() > swit.getPriority());
	  flowpq->insert(x);
	  /* cout << "INSERT: " << x << endl;  */
	} /* if (!is_nodata(elev_neighb)) */
      }
    } /* for dj */
  } /* for di */
  return;
}






