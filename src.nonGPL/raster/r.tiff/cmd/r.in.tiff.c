/* Updated to FP map writing functions 11/99 Markus Neteler */

/* Updated 9/99 to true 24bit support.
 * The old version was limited to 256 colors.
 * 
 *  Incorporated color quantization to speed up GRASS.
 *   (GRASS is getting extremely slow with more than
 *     a few thousand categories/colors)
 *    
 *    Stefano Merler
 *    merler@irst.itc.it
 */       

/* r.in.tiff - Converts from a Tagged Image File Format image to a Grass Raster.
 *
 * tif2ras.c - Converts from a Tagged Image File Format image to a Sun Raster.
 * Portions Copyright (c) 1990 by Sun Microsystems, Inc.
 *
 * Author: Patrick J. Naughton
 * naughton@wind.sun.com
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.
 *
 *   This program takes a MicroSoft/Aldus "Tagged Image File Format" image or
 * "TIFF" file as input and writes a GRASS cell file.
 */
/*typedef unsigned short u_short;
typedef unsigned long u_long;
typedef unsigned char u_char;
	*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include "tiffio.h"
#include "gis.h"

#include "config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

int quantize(int, int *);
int get_tif_colors2(int, int *);
int get_tif_colors( TIFF *, int ,int , u_char *);
CELL lookup_color2(int, int, int, int, int *);
CELL lookup_color(int, int, int, int);
int count_colors( TIFF *, int,int, u_char *);

typedef struct {
  int	type;
  int	length;
  u_char	*map[3];
} colormap_t;

#define MAXCOLOR 16777216

struct mycolor {
  int red;
  int grn;
  int blu;
}colortable[MAXCOLOR];


typedef int boolean;
#define RMT_NONE 0
#define RMT_EQUAL_RGB 1
#define RT_BYTE_ENCODED 2

#define	CVT(x)		((uint16)(((x) * 255) / ((1L<<16)-1)))

boolean     Verbose;
char       *pname;		/* program name (used for error messages) */
static	char  *inf = NULL ;
static	char  *outf = NULL ;

int main (int argc, char *argv[])
{
  int         depth=0,
    numcolors;
  register TIFF *tif;
  register u_char *inp, *outp='\0';
  register int col,
    row,
    i;
  u_char     *Map = NULL;
  u_char     *buf;
  uint16 *redcmap, *greencmap, *bluecmap;

  colormap_t  Colormap;	/* The Pixrect Colormap */
  u_char      red[256],
    green[256],
    blue[256];

  int cellfp;
  CELL *cell,*cellptr;
  struct Cell_head cellhd;
  struct Colors cellcolor;
  struct Colors bwcolors, *pbwcolr;
  int cellcol, cellnrows, cellncols;
  struct Option *inopt, *outopt, *nlevopt;
  struct Flag *vflag, *bflag;

  u_short	bitspersample,
    samplesperpixel,
    photometric;
  u_long	width,
    height;
  int ncolors;
  long tif_pos;
  int nlev;
  int Bands = 0;
  int maxcolors;
  int *levels;
  int Red, Grn, Blu;
  int num_colors;
  int x;
  int outred, outgrn, outblu;
  char mapred[300], mapgrn[300], mapblu[300];
  CELL *cellr, *cellg, *cellb;
  
  pbwcolr=&bwcolors;
  G_gisinit(argv[0]);

  inopt = G_define_option();
  inopt->key		= "input";
  inopt->type		= TYPE_STRING;
  inopt->required		= YES;
  inopt->description	= "Name on TIFF file to input.";

  outopt = G_define_option();
  outopt->key		= "output";
  outopt->type		= TYPE_STRING;
  outopt->required	= YES;
  outopt->gisprompt	= "new,cell,raster";
  outopt->description	= "Name of new raster file.";

  nlevopt = G_define_option();
  nlevopt->key             = "nlev";
  nlevopt->type   	 = TYPE_INTEGER;
  nlevopt->required        = NO;
  nlevopt->description     = "Max number of levels for R/G/B.";
  nlevopt->answer         = "20 (= 8000 colors)";
  nlevopt->options         = "1-256";
	
  vflag = G_define_flag();
  vflag->key		= 'v';
  vflag->description	= "Verbose mode on.";

  bflag = G_define_flag();
  bflag->key              = 'b';
  bflag->description      = "Raster map of the (true) R/G/B levels.";

  if(G_parser(argc, argv))
    exit(-1);

  Verbose = vflag->answer ;
  Bands = bflag->answer;

  nlev=atoi(nlevopt->answer);
  maxcolors=nlev*nlev*nlev;
	
  inf = inopt->answer;
  tif = TIFFOpen(inf, "r");
  if (tif == NULL)
    G_fatal_error("Error opening TIFF file.");
  if (Verbose)
    fprintf(stderr, "Reading %s...", inf);

  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
  TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
  if (Verbose)
    TIFFPrintDirectory(tif, stderr, 0l);
  if (bitspersample > 8)
    G_fatal_error("Can't handle more than 8-bits per sample");

  switch (samplesperpixel) {
  case 1:
    if (bitspersample == 1)
      depth = 1;
    else
      depth = 8;
    break;
  case 3:
  case 4:
    depth = 24;
    break;
  default:
    G_fatal_error("Only handle 1-channel gray scale or 3-channel color");
  }

  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH,&width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH,&height);

  cellhd.zone = G_zone();
  cellhd.proj = G_projection();
  cellnrows = cellhd.rows = cellhd.north = height;
  cellncols = cellhd.cols = cellhd.east = width;
  cellhd.south = cellhd.west = 0.0;
  cellhd.ns_res = cellhd.ew_res = 1;
  cellhd.format = 0;
  cellhd.compressed = 0;
  if(G_set_window(&cellhd) < 0)
    G_fatal_error("couldn't set cellhd.");
  cell = G_allocate_cell_buf();
  outf = outopt->answer;
  G_set_cell_format(0);
  if (Verbose)
    fprintf(stderr, "%ldx%ldx%d image\n ", width, height, depth);
  if (Verbose)
    fprintf(stderr, "%d bits/sample, %d samples/pixel \n",
	    bitspersample, samplesperpixel);
  numcolors = (1 << bitspersample);

  TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photometric);
  if(photometric == PHOTOMETRIC_RGB){
    if (!(cellfp = G_open_cell_new( outf )))
      G_fatal_error("New raster file couldn't be opened for writing.");
    if(Bands){
      sprintf(mapred,"%s.r",outf);
      if((outred = G_open_cell_new (mapred)) < 0)
	G_fatal_error("Can't open new raster file.");
      cellr = G_allocate_cell_buf();
      sprintf(mapgrn,"%s.g",outf);
      if((outgrn = G_open_cell_new (mapgrn)) < 0)
	G_fatal_error("Can't open new raster file.");
      cellg = G_allocate_cell_buf();
      sprintf(mapblu,"%s.b",outf);
      if((outblu = G_open_cell_new (mapblu)) < 0)
	G_fatal_error("Can't open new raster file.");
      cellb = G_allocate_cell_buf();
    }
  }
  else{
      /*if (!(cellfp = G_open_cell_new_random( outf )))*/
      if (!(cellfp = G_open_cell_new( outf )))
       G_fatal_error("New raster file couldn't be opened for writing.");
     if(Bands){
       G_warning("-b flag ignored: only works with 24bit images");
       Bands = 0;
     }
  }
  
  G_init_colors(&cellcolor);

  buf = (u_char *) malloc(TIFFScanlineSize(tif));
  if (buf == NULL)
    G_fatal_error("Can't allocate memory for scanline buffer...");
	
  if (numcolors == 2) {
    if (Verbose)
      fprintf(stderr, "monochrome\n ");
    G_set_color((CELL)(0), 0, 0, 0, &cellcolor);
    G_set_color((CELL)(1), 255, 255, 255, &cellcolor);
    Colormap.map[0] = Colormap.map[1] = Colormap.map[2] = NULL;
  } else {
    switch (photometric) {
    case PHOTOMETRIC_MINISBLACK:
      if (Verbose)
	fprintf(stderr, "%d graylevels (min=black)\n ", numcolors);
      Map = (u_char *) malloc(numcolors * sizeof(u_char));
      for (i = 0; i < numcolors; i++){
	Map[i] = (255 * i) / numcolors;
	G_set_color((CELL)(i), Map[i], Map[i], Map[i], &cellcolor);
      }
      break;
    case PHOTOMETRIC_MINISWHITE:
      if (Verbose)
	fprintf(stderr, "%d graylevels (min=white)\n ", numcolors);
      Map = (u_char *) malloc(numcolors * sizeof(u_char));
      for (i = 0; i < numcolors; i++){
	Map[i] = 255 - ((255 * i) / numcolors);
	G_set_color((CELL)(i), Map[i], Map[i], Map[i], &cellcolor);
      }
      break;
    case PHOTOMETRIC_RGB:
/*      tif_pos = ftell((FILE *)tif); */ /* commented 5/2000 */
      ncolors=count_colors(tif,height,width,buf);
      if (Verbose)fprintf (stdout,"Total colors = %d\n",ncolors);
      if(ncolors > maxcolors){
	G_warning("Color levels quantization...\n");
	levels=(int*)G_calloc(nlev,sizeof(int));
	quantize(nlev,levels);
	num_colors=get_tif_colors2(nlev,levels);
	if (Verbose)fprintf (stdout,"Total used colors = %d\n", num_colors);
      }
      else{
	tif_pos = ftell((FILE *)tif);
	num_colors=get_tif_colors(tif,height,width,buf)	;
	if (Verbose)fprintf (stdout,"Total used colors = %d\n", num_colors);
/*	fseek((FILE *)tif, tif_pos, 0);*/ /* commented 5/2000 */
      }
      break;
    case PHOTOMETRIC_PALETTE:
      if (Verbose)
	fprintf(stderr, " %d colormapped\n ", numcolors);
      memset(red, 0, sizeof(red));
      memset(green, 0, sizeof(green));
      memset(blue, 0, sizeof(blue));
      if (!TIFFGetField(tif,TIFFTAG_COLORMAP,&redcmap,
			&greencmap,&bluecmap)
	  ) {
	G_fatal_error("Missing required \"Colormap\"");
	break;
      }
      for (i = 0; i < numcolors; i++) {
	red[i] = CVT(redcmap[i]);
	green[i] = CVT(greencmap[i]);
	blue[i] = CVT(bluecmap[i]);
	G_set_color((CELL)(i), red[i], green[i], blue[i], &cellcolor);
      }
      break;
    case PHOTOMETRIC_MASK:
      G_fatal_error("Don't know how to handle PHOTOMETRIC_MASK");
      break;
    default:
      G_fatal_error("Unknown photometric.");
    }
  }


  for (row = 0; row < height; row++) {
    if (TIFFReadScanline(tif, buf, row, 0) < 0){
      fprintf(stderr, "Bad data read on line: %d\n", row);
      exit(-1);
    }
    inp = buf;
    switch (photometric) {
    case PHOTOMETRIC_RGB:
      cellptr=cell;
      for (col = 0; col < width; col++) {
	Red = (int) *inp++;		/* Red */
	Grn = (int) *inp++;		/* Green */
	Blu = (int) *inp++;		/* Blue */
	if(ncolors > maxcolors)
	  cellptr[col]=lookup_color2(Red,Grn,Blu,nlev,levels);
	else
	  cellptr[col]=lookup_color(Red,Grn,Blu,num_colors);
	if(Bands){
	  cellr[col] = (CELL)Red;
	  cellg[col] = (CELL)Grn;
	  cellb[col] = (CELL)Blu;
	}
      }
      G_put_c_raster_row(cellfp, cellptr);
      if(Bands){
	if (G_put_c_raster_row(outred, cellr) < 0 )
	  G_fatal_error("Can't write new raster row!!");
	if (G_put_c_raster_row(outgrn, cellg) < 0 )
	  G_fatal_error("Can't write new raster row!!");
	if (G_put_c_raster_row(outblu, cellb) < 0 )
	  G_fatal_error("Can't write new raster row!!");
      }
      break;
    case PHOTOMETRIC_MINISWHITE:
    case PHOTOMETRIC_MINISBLACK:
      switch (bitspersample) {
      case 1: {
	int	sz_col = width/8;

	if ( width%8 ) ++sz_col;
	cellcol = 0;
	for (col=0; col<sz_col; col++){
	  if ( cellcol<width ) {
	    cell[cellcol++] = (CELL)((*inp >> 7) & 0x01);
	    if ( cellcol<width ) {
	      cell[cellcol++] = (CELL)((*inp >> 6) & 0x01);
	      if ( cellcol<width ) {
		cell[cellcol++] = (CELL)((*inp >> 5) & 0x01);
		if ( cellcol<width ) {
		  cell[cellcol++] = (CELL)((*inp >> 4) & 0x01);
		  if ( cellcol<width ) {
		    cell[cellcol++] = (CELL)((*inp >> 3) & 0x01);
		    if ( cellcol<width ) {
		      cell[cellcol++] = (CELL)((*inp >> 2) & 0x01);
		      if ( cellcol<width ) {
			cell[cellcol++] = (CELL)((*inp >> 1) & 0x01);
			if ( cellcol<width )
			  cell[cellcol++] = (CELL)(*inp++ & 0x01);
		      }
		    }
		  }
		}
	      }
	    }
	  }
	}
	/*G_put_map_row_random(cellfp, cell, row, 0, width);*/
	G_put_c_raster_row(cellfp, cell);
	/* *outp++ = *inp++;*/
	break;
      }
      case 2:
	for (col = 0; col < ((width + 3) / 4); col++) {
	  *outp++ = (*inp >> 6) & 3;
	  *outp++ = (*inp >> 4) & 3;
	  *outp++ = (*inp >> 2) & 3;
	  *outp++ = *inp++ & 3;
	}
	break;
      case 4:
	cellcol = 0;
	for (col = 0; col < width / 2; col++) {
	  /* *outp++ = *inp >> 4;
	   *outp++ = *inp++ & 0xf;*/
	  cell[cellcol++] = (CELL) (*inp >> 4);
	  cell[cellcol++] = (CELL) (*inp++ & 0xf);
	}
	/*G_put_map_row_random(cellfp, cell, row, 0, width);*/
	G_put_c_raster_row(cellfp, cell);
	break;
      case 8:
	for (col = 0; col < width; col++)
	  cell[col] = (CELL) *inp++;
	/*G_put_map_row_random(cellfp, cell, row, 0, width);*/
	G_put_c_raster_row(cellfp, cell);
	break;
      default:
	fprintf(stderr, "%s: bad bits/sample: %d\n",
		G_program_name(),bitspersample);
	exit(-1);
      }
      break;
    case PHOTOMETRIC_PALETTE:
      for (col = 0; col < width; col++)
	cell[col] = (CELL) *inp++;
      /*G_put_map_row_random(cellfp, cell, row, 0, width);*/
      G_put_c_raster_row(cellfp, cell);
      break;
    default:
      fprintf(stderr, "%s: unknown photometric (write): %d\n",
	      G_program_name(), photometric);
      exit(-1);
    }
  }

  free((char *) buf);

  if (Verbose)
    fprintf(stderr, "Creating SUPPORT Files for %s\n", outf);
  G_close_cell( cellfp );
  if(Bands){
    G_close_cell(outred);
    G_close_cell(outgrn);
    G_close_cell(outblu);
  }
	
  if(photometric == PHOTOMETRIC_RGB){
    for(x=0;x<num_colors;x++){
      G_set_color((CELL)x, colortable[x].red, colortable[x].grn,
		  colortable[x].blu, &cellcolor);
    }
  }
  G_write_colors( outf, G_mapset(), &cellcolor);
  if(Bands){
    G_init_colors(pbwcolr);
    for(x=0;x<256;x++){
      G_set_color((CELL)x, x, x,x, pbwcolr);
    }
    if(G_write_colors(mapred, G_mapset(), pbwcolr) < 0)
      G_fatal_error("Can't write color table!!");
    if(G_write_colors(mapgrn, G_mapset(), pbwcolr) < 0)
      G_fatal_error("Can't write color table!!");
    if(G_write_colors(mapblu, G_mapset(), pbwcolr) < 0)
      G_fatal_error("Can't write color table!!");
  }

  if (Verbose)
    fprintf(stderr, "done.\n");

  exit(0);
}

int count_colors( TIFF *tif, int height,int width, u_char *buf)
{
  int i,j,ncolors;
  int *total_color;
  int red, grn, blu;	
  u_char *inp;
    
  total_color=(int*)G_calloc(MAXCOLOR,sizeof(int));

  for (i=0; i < height; i++){
    if (TIFFReadScanline(tif, buf, i, 0) < 0){
      fprintf(stderr, "Bad data read on line: %d\n", i);
      exit(-1);
    }
    inp = buf;
    
    for (j = 0; j < width; j++) {
      red = (int) *inp++;	
      grn = (int) *inp++;	
      blu = (int) *inp++;
      total_color[red*256*256+grn*256+blu]=1;
    }
		
  }
  ncolors = 0;
  for(i=0;i<MAXCOLOR;i++)
    ncolors += total_color[i];
  free(total_color);
  return(ncolors);
}

int quantize (int colors_for_chanell, int *levels) 
{
  int i;
  int step;
  
  levels[0] = 0;
  for(i=1;i<colors_for_chanell-1;i++){
    step = (int)((255.-levels[i-1]) / (double)(colors_for_chanell-i));
    levels[i] = levels[i-1] + step;
  }
  levels[colors_for_chanell-1] = 255;
  return 0;
}

int get_tif_colors2 (int colors_for_chanell, int *levels)
{
  int i,j,k;
  int actual;
  
  
  actual = 0;
  for(i=0;i<colors_for_chanell;i++)
    for(j=0;j<colors_for_chanell;j++)
      for(k=0;k<colors_for_chanell;k++){
	colortable[actual].red = levels[i];
	colortable[actual].grn = levels[j];
	colortable[actual].blu = levels[k];
	actual ++;
      }
  return(actual);
}

CELL lookup_color2 (int r, int g, int b, int nlev, int *levels)
{
  int x;
  double mindist, tmpdist;
  int bestlevR;
  int bestlevG;
  int bestlevB;
  int index;
	

  mindist=255.;
  for (x=0;x<nlev;x++){
    tmpdist=abs((double)(r - levels[x]));
    if(tmpdist < mindist){
      mindist = tmpdist;
      bestlevR = x;
    }
  }
	
  mindist=255.;
  for (x=0;x<nlev;x++){
    tmpdist=abs((double)(g - levels[x]));
    if(tmpdist < mindist){
      mindist = tmpdist;
      bestlevG = x;
    }
  }
	
  mindist=255.;
  for (x=0;x<nlev;x++){
    tmpdist=abs((double)(b - levels[x]));
    if(tmpdist < mindist){
      mindist = tmpdist;
      bestlevB = x;
    }
  }
  index = bestlevR*nlev*nlev + bestlevG*nlev + bestlevB;
  return((CELL)index);
}

CELL lookup_color (int r, int g, int b, int num)
{
  int x;

  for (x=0;x<num;x++){
    if (colortable[x].red == r && 
	colortable[x].grn == g &&
	colortable[x].blu == b){
      break;
    }
  }
  return((CELL)x);
}

int get_tif_colors( TIFF *tif, int height,int width, u_char *buf)
{
  int x,maxcolor,match;
  int red, grn, blu;
  int i,j;
  u_char *inp;
	

  match = maxcolor = 0;
  for (i=0; i < height; i++){
    if (TIFFReadScanline(tif, buf, i, 0) < 0){
      fprintf(stderr, "Bad data read on line: %d\n", i);
      exit(-1);
    }
    inp = buf;
    
    for (j = 0; j < width; j++) {
      red = (int) *inp++;	
      grn = (int) *inp++;	
      blu = (int) *inp++;
      match=0;
      for (x=0;x<maxcolor;x++){
	if (colortable[x].red == red && 
	    colortable[x].grn == grn &&
	    colortable[x].blu == blu){
	  match = 1;
	  break;
	}
      }	
      if (match == 0){
	colortable[maxcolor].red = red;
	colortable[maxcolor].grn = grn;
	colortable[maxcolor].blu = blu;
	maxcolor++;
	if (maxcolor == MAXCOLOR)
	  G_fatal_error("Exceeded maximum colors!!");
      }
		
    }
  }
  return(maxcolor);
	
}
