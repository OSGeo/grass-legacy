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

 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>

#ifdef HAVE_STATVFS_H
#include <sys/statvfs.h>
#endif


extern "C" {
#include "gis.h"
}

#include "option.h"
#include "common.h" /* declares the globals */
#include "fill.h"
#include "flow.h"
#include "nodata.h"
#include "grass2str.h"
#include "water.h"
#include "sortutils.h"


/* globals: in common.H
extern statsRecorder *stats; 
extern userOptions* opt;    
extern struct Cell_head region; 
*/


/* #define JUMP2FLOW */
/* define it only if you want to skip the flow direction computation
   and jump directly to computing flow accumulation; the flowstream
   must exist in /var/tmp/flowStream */


/* ---------------------------------------------------------------------- */
void 
parse_args(int argc, char *argv[]) {

  /* input elevation grid  */
  struct Option *input_elev;
  input_elev = G_define_option() ;
  input_elev->key        = "elev";
  input_elev->type       = TYPE_STRING;
  input_elev->required   = YES;
  input_elev->gisprompt  = "old,cell,raster" ;
  input_elev->description= "Input elevation grid" ;

  /* output filled elevation grid */
  struct Option *output_elev;
  output_elev = G_define_option() ;
  output_elev->key        = "filled";
  output_elev->type       = TYPE_STRING;
  output_elev->required   = YES;
  output_elev->gisprompt  = "new,cell,raster" ;
  output_elev->description= "Output (filled) elevation grid";
  
 /* output direction  grid */
  struct Option *output_dir;
  output_dir = G_define_option() ;
  output_dir->key        = "direction";
  output_dir->type       = TYPE_STRING;
  output_dir->required   = YES;
  output_dir->gisprompt  = "new,cell,raster" ;
  output_dir->description= "Output direction grid";

  /* output sinkwatershed  grid */
  struct Option *output_watershed;
  output_watershed = G_define_option() ;
  output_watershed->key        = "swatershed";
  output_watershed->type       = TYPE_STRING;
  output_watershed->required   = YES;
  output_watershed->gisprompt  = "new,cell,raster" ;
  output_watershed->description= "Output sinkwatershed grid";

  /* output flow accumulation grid */
  struct Option *output_accu;
  output_accu = G_define_option() ;
  output_accu->key        = "accumulation";
  output_accu->type       = TYPE_STRING;
  output_accu->required   = YES;
  output_accu->gisprompt  = "new,cell,raster" ;
  output_accu->description= "Output accumulation grid";

#ifdef OUTPUT_TCI
  struct Option *output_tci;
  output_tci = G_define_option() ;
  output_tci->key        = "tci";
  output_tci->type       = TYPE_STRING;
  output_tci->required   = YES;
  output_tci->gisprompt  = "new,cell,raster" ;
  output_tci->description= "Output tci grid";
#endif

  /* MFD/SFD flag */
  struct Flag *sfd_flag;
  sfd_flag = G_define_flag() ;
  sfd_flag->key        = 's';
  sfd_flag->description= "SFD (D8) flow (default is MFD)";
  /* sfd_flag->answer     = 'n'; */

  /* D8CUT value*/
  struct Option *d8cut;
  d8cut = G_define_option();
  d8cut->key  = "d8cut";
  d8cut->type = TYPE_DOUBLE;
  d8cut->required = NO;
  d8cut->answer = "infinity"; /* default value */
  d8cut->description = "If flow accumulation is larger than this value it is routed using SFD (D8) direction \n \t\t (meaningfull only  for MFD flow)";
  
  /* main memory */
  struct Option *mem;
  mem = G_define_option() ;
  mem->key         = "memory";
  mem->type        = TYPE_INTEGER;
  mem->required    = NO;
  mem->answer      = "300"; /* 300MB default value */
  mem->description = "Main memory size (in MB)";

  /* temporary STREAM path */
  struct Option *streamdir;
  streamdir = G_define_option() ;
  streamdir->key        = "STREAM_DIR";
  streamdir->type       = TYPE_STRING;
  streamdir->required   = NO;
  streamdir->answer     = "/var/tmp"; 
  streamdir->description= "Location of intermediate STREAMs";

  /* verbose flag */
  struct Flag *quiet;
  quiet = G_define_flag() ;
  quiet->key         = 'q' ;
  quiet->description = "Quiet" ;
  /* quiet->answer = 'n'; */

 /* stats file */
  struct Option *stats_opt;
  stats_opt = G_define_option() ;
  stats_opt->key = "stats";
  stats_opt->type       = TYPE_STRING;
  stats_opt->required   = NO;
  stats_opt->description= "Stats file";
  stats_opt->answer     = "stats.out";



  /* ************************* */
  if (G_parser(argc, argv)) {
    exit (-1);
  }
  
  /* ************************* */
  assert(opt);
  opt->elev_grid = input_elev->answer;
  opt->filled_grid = output_elev->answer;
  opt->dir_grid = output_dir->answer; 
  opt->watershed_grid = output_watershed->answer;
  opt->flowaccu_grid = output_accu->answer;
#ifdef OUTPUT_TCI
  opt->tci_grid = output_tci->answer;
#endif

  opt->d8 = sfd_flag->answer;
  if (strcmp(d8cut->answer, "infinity") == 0) {
    opt->d8cut = MAX_ACCU;
  } else {
    opt->d8cut = atoi(d8cut->answer);
  }

  opt->mem = atoi(mem->answer);
  opt->streamdir = streamdir->answer;
  opt->verbose = (!quiet->answer);
  opt->stats = stats_opt->answer;

  /* somebody should delete the options */
}


/* ---------------------------------------------------------------------- */
/* check compatibility of map header and region header */
void check_header(char* cellname) {

  char *mapset;
  mapset = G_find_cell(cellname, "");
  if (mapset == NULL) {
    G_fatal_error ("cell file [%s] not found", cellname);
  }
  /* read cell header */
  struct Cell_head cell_hd;
  if (G_get_cellhd (cellname, mapset, &cell_hd) < 0)
    G_fatal_error ("Cannot read header of [%s]", cellname);
  
  /* check compatibility with module region */
  if (!((region->ew_res == cell_hd.ew_res)
		&& (region->ns_res == cell_hd.ns_res))) {
    G_fatal_error("cell file %s resolution differs from current region",
				  cellname);
  } else {
    if (opt->verbose) { 
      fprintf(stderr, "cell %s header compatible with region header\n",
	      cellname);
      fflush(stderr);
    }
  }


  /* check type of input elevation raster and check if precision is lost */
    RASTER_MAP_TYPE data_type;
	data_type = G_raster_map_type(opt->elev_grid, mapset);
#ifdef ELEV_SHORT
	fprintf(stderr, "elevation stored as SHORT (%dB) ", 
			sizeof(elevation_type));
	if (data_type == FCELL_TYPE) {
	  fprintf(stderr, "WARNING: raster %s is of type FCELL_TYPE \
--precision may be lost.\n", opt->elev_grid); 
	}
	if  (data_type == DCELL_TYPE) {
	  fprintf(stderr, "WARNING: raster %s is of type DCELL_TYPE \
--precision may be lost.\n",  opt->elev_grid);
	}
	fprintf(stderr, "\n"); 
#endif 
#ifdef ELEV_FLOAT
	fprintf(stderr, "elevation stored as FLOAT (%dB) ", 
			sizeof(elevation_type));
	if (data_type == CELL_TYPE) {
	  fprintf(stderr, "WARNING: raster %s is of type CELL_TYPE \
--you should use r.terraflow.short\n", opt->elev_grid); 
	}
	if  (data_type == DCELL_TYPE) {
	  fprintf(stderr, "WARNING: raster %s is of type DCELL_TYPE \
--precision may be lost.\n",  opt->elev_grid);
	}
	fprintf(stderr, "\n"); 
#endif
	



}

/* ---------------------------------------------------------------------- */
void check_args() {

  /* check if filled elevation grid name is  valid */
  if (G_legal_filename (opt->filled_grid) < 0) {
    G_fatal_error ("[%s] is an illegal name", opt->filled_grid);
  }
  /* check if output grid names are valid */
  if (G_legal_filename (opt->dir_grid) < 0) {
    G_fatal_error ("[%s] is an illegal name", opt->dir_grid);
  }
  if (G_legal_filename (opt->filled_grid) < 0) {
    G_fatal_error ("[%s] is an illegal name", opt->filled_grid);
  }
  if (G_legal_filename (opt->flowaccu_grid) < 0) {
    G_fatal_error ("[%s] is an illegal name", opt->flowaccu_grid);
  }
  if (G_legal_filename (opt->watershed_grid) < 0) {
    G_fatal_error ("[%s] is an illegal name", opt->watershed_grid);
  }
#ifdef OUTPU_TCI
  if (G_legal_filename (opt->tci_grid) < 0) {
  G_fatal_error ("[%s] is an illegal name", opt->tci_grid);
  }
#endif
  
  /* check compatibility with region */
  check_header(opt->elev_grid);

  /* what else ? */  


}



/* ---------------------------------------------------------------------- */
void record_args(int argc, char **argv) {

  time_t t = time(NULL);
  char buf[BUFSIZ];
  if(t == (time_t)-1) {
    perror("time");
    exit(1);
  }

  ctime_r(&t, buf);
  buf[24] = '\0';
  stats->timestamp(buf);
  
  *stats << "Command Line: " << endl;
  for(int i=0; i<argc; i++) {
    *stats << argv[i] << " ";
  }
  *stats << endl;
  
  *stats << "input elevation grid: " << opt->elev_grid << "\n";
  *stats << "output (flooded) elevations grid: " << opt->filled_grid << "\n";
  *stats << "output directions grid: " << opt->dir_grid << "\n";
  *stats << "output sinkwatershed grid: " << opt->watershed_grid << "\n";
  *stats << "output accumulation grid: " << opt->flowaccu_grid << "\n";
#ifdef OUTPUT_TCI
  *stats <<  "output tci grid: " << opt->tci_grid << "\n";
#endif
  if (opt->d8) {
    stats ->comment("SFD (D8) flow direction");
  } else {
    stats->comment("MFD flow direction");
  }

  sprintf(buf, "D8CUT=%f", opt->d8cut);
  stats->comment(buf);

  size_t mm_size = opt->mem  << 20; /* (in bytes) */
  char tmp[100];
  formatNumber(tmp, mm_size);
  sprintf(buf,"memory size: %s bytes", tmp);
  stats->comment(buf);
}



/* ---------------------------------------------------------------------- */
void 
setFlowAccuColorTable(char* cellname) {
  struct  Colors colors;
  char   *mapset;
  struct Range r;

  mapset = G_find_cell(cellname, "");
  if (mapset == NULL) {
    G_fatal_error ("cell file [%s] not found", cellname);
  }
  if (G_read_range(cellname, mapset, &r) == -1) {
    G_fatal_error("cannot read range");
  }
  /*fprintf(stderr, "%s range is: min=%d, max=%d\n", cellname, r.min, r.max);*/
  int v[6];
  v[0] = r.min;
  v[1] = 5;
  v[2] = 30;
  v[3] = 100;
  v[4] = 1000;
  v[5] = r.max;
  

  G_init_colors(&colors);
 
  G_add_color_rule(v[0], 255,255,255,  v[1],     255,255,0, &colors);
  G_add_color_rule(v[1], 255,255,0,    v[2],       0,255,255, &colors);
  G_add_color_rule(v[2],   0,255,255,  v[3],       0,127,255, &colors);
  G_add_color_rule(v[3],   0,127,255,  v[4],       0,0,255,   &colors);
  G_add_color_rule(v[4],   0,0,255,  (CELL)v[5],   0,0,0,     &colors);

 
  if (G_write_colors(cellname, mapset, &colors) == -1) {
    G_fatal_error("cannot write colors");
  }
  G_free_colors(&colors);
}



/* print the largest interm file that will be generated during
   r.terraflow */
void
printMaxSortSize(long nodata_count) {
  char buf[BUFSIZ];
  long long  fillmaxsize = (long long)nrows*ncols*sizeof(waterWindowType);
  long long  flowmaxsize = (long long)(nrows*ncols - nodata_count)*sizeof(sweepItem);
  long long maxneed = (fillmaxsize > flowmaxsize) ? fillmaxsize: flowmaxsize;
  maxneed =  2*maxneed; /* need 2*N to sort */

  fprintf(stderr, "total elements=%ld, nodata elements=%ld\n", (long)nrows*ncols, nodata_count);
  fprintf(stderr, "largest temporary files: \n");
  fprintf(stderr, "\t\t FILL: %s [%d elements, %dB each]\n",
		  formatNumber(buf, fillmaxsize),
		  nrows * ncols, sizeof(waterWindowType));
  fprintf(stderr, "\t\t FLOW: %s [%ld elements, %dB each]\n",
		  formatNumber(buf, flowmaxsize),
		  (long)(nrows * ncols - nodata_count), sizeof(sweepItem));
  fprintf(stderr, "Will need at least %s space available in %s\n",
		  formatNumber(buf, maxneed),  		  /* need 2*N to sort */
		  getenv(STREAM_TMPDIR));
 
#ifdef HAVE_STATVFS_H
  fprintf(stderr, "Checking current space in %s: ", getenv(STREAM_TMPDIR));
  struct statvfs statbuf;
  statvfs(getenv(STREAM_TMPDIR), &statbuf);

  float avail = statbuf.f_bsize*statbuf.f_bavail;
  fprintf(stderr, "available %ld blocks x %ldB = %.0fB",
		  (long)statbuf.f_bavail, statbuf.f_bsize, avail);
  if (avail > maxneed) {
	fprintf(stderr, ". OK.\n");
  } else {
	fprintf(stderr, ". Not enough space available.\n");
	exit(1);
  }
#endif
}



/* ---------------------------------------------------------------------- */
int
main(int argc, char *argv[]) {
  struct GModule *module;
  Rtimer rtTotal;    
  char buf[BUFSIZ];

  fprintf(stderr, "r.terraflow December 2003\n");
  fflush(stderr);

  /* initialize GIS library */
  G_gisinit(argv[0]);

 
  module = G_define_module();
#ifdef ELEV_SHORT
  module->description ="Flow computation for massive grids (Integer version).";
#endif
#ifdef ELEV_FLOAT
  module->description ="Flow computation for massive grids (Float version).";
#endif

  /* get the current region and dimensions */  
  region = (struct Cell_head*)malloc(sizeof(struct Cell_head));
  assert(region);
  if (G_get_set_window(region) == -1) {
    G_fatal_error("r.terraflow: error getting current region");
  }
  int nr = G_window_rows();
  int nc = G_window_cols();
  if ((nr > dimension_type_max) || (nc > dimension_type_max)) {
    G_fatal_error("[nrows=%d, ncols=%d] dimension_type overflow -- change dimension_type and recompile\n", nr, nc);
  } else {
    nrows = (dimension_type)nr;
    ncols = (dimension_type)nc;
  }
  fprintf(stderr, "region size is %d x %d\n", nrows, ncols);
  fflush(stderr);  
  
  /* read user options; fill in global <opt> */  
  opt = (userOptions*)malloc(sizeof(userOptions));
  assert(opt);
  
  parse_args(argc, argv);
  check_args();
  
  /* check STREAM path (the place where intermediate STREAMs are placed) */
  sprintf(buf, "%s=%s",STREAM_TMPDIR, opt->streamdir);
  putenv(buf);
  if (getenv(STREAM_TMPDIR) == NULL) {
    fprintf(stderr, "%s:", STREAM_TMPDIR);
    G_fatal_error("not set");
  } else {
    fprintf(stderr, "STREAM temporary files in %s  ",
	    getenv(STREAM_TMPDIR)); 
	fprintf(stderr, "(THESE INTERMEDIATE STREAMS WILL NOT BE DELETED IN CASE OF ABNORMAL TERMINATION OF THE PROGRAM. TO SAVE SPACE PLEASE DELETE THESE FILES MANUALLY!)\n");
  }
  
  /* open the stats file */
  stats = new statsRecorder(opt->stats);
  record_args(argc, argv);
  {
    char buf[BUFSIZ];
    long grid_size = nrows * ncols;
    *stats << "region size = " <<  formatNumber(buf, grid_size) << " elts "
	   << "(" << nrows << " rows x " << ncols << " cols)\n";

    stats->flush();
  }

  /* set up STREAM memory manager */
  size_t mm_size = opt->mem << 20; /* opt->mem is in MB */
  MM_manager.set_memory_limit(mm_size);
  if (opt->verbose) {
	MM_manager.warn_memory_limit();
  } else {
	MM_manager.ignore_memory_limit();
  }
  MM_manager.print_limit_mode();


  /* initialize nodata */
  nodataType::init();
  *stats << "internal nodata value: " << nodataType::ELEVATION_NODATA << endl;
   
  /* start timing -- after parse_args, which are interactive */
  rt_start(rtTotal);

#ifndef JUMP2FLOW 
  /* read elevation into a stream */
  AMI_STREAM<elevation_type> *elstr=NULL;
  long nodata_count;
  elstr = cell2stream<elevation_type>(opt->elev_grid, elevation_type_max,
									  &nodata_count);
  /* print the largest interm file that will be generated */
  printMaxSortSize(nodata_count);
  

  /* -------------------------------------------------- */
  /* compute flow direction and filled elevation (and watersheds) */
  AMI_STREAM<direction_type> *dirstr=NULL;
  AMI_STREAM<elevation_type> *filledstr=NULL;
  AMI_STREAM<waterWindowBaseType> *flowStream=NULL;
  AMI_STREAM<labelElevType> *labeledWater = NULL;

  flowStream=computeFlowDirections(elstr, filledstr, dirstr, labeledWater);

  delete elstr;

  /* write streams to GRASS cell files */
  stream2_CELL(dirstr, nrows, ncols, opt->dir_grid);
  delete dirstr; 
  stream2_CELL(filledstr, nrows, ncols, opt->filled_grid);
  delete filledstr; 
  stream2_CELL(labeledWater, nrows, ncols, labelElevTypePrintLabel(), 
			   opt->watershed_grid);
  delete labeledWater;
  
#else 
  AMI_STREAM<waterWindowBaseType> *flowStream;
  flowStream = new AMI_STREAM<waterWindowBaseType>("/var/tmp/flowStream");
  fprintf(stderr, "flowStream opened: len=%d\n", flowStream->stream_len());
  fprintf(stderr, "jumping to flow accumulation computation\n");
#endif
  
  /* -------------------------------------------------- */
  /* compute flow accumulation (and tci) */
  AMI_STREAM<sweepOutput> *outstr=NULL;
  
  computeFlowAccumulation(flowStream, outstr);
  /* delete flowStream -- deleted inside */

  /* write output stream to GRASS cell files */
#ifdef OUTPUT_TCI
  stream2_FCELL(outstr, nrows, ncols, printAccumulation(), printTci(),
		opt->flowaccu_grid, opt->tci_grid);
#else 
  stream2_FCELL(outstr, nrows, ncols, printAccumulation(), opt->flowaccu_grid);
#endif

  setFlowAccuColorTable(opt->flowaccu_grid);

  delete outstr;
  
  rt_stop(rtTotal);
  stats->recordTime("Total running time: ", rtTotal);
  stats->timestamp("end");
  
  fprintf(stderr, "r.terraflow done\n");
  
  /* free the globals */
  free(region);
  free(opt);
  delete stats;

  return 0;
}



 
