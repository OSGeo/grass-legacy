/*
 * $Id$
 *
 * Remove quantization code, force bands for RGB.  Removes
 * a big chunk of stack memory that caused the program
 * not to run on some implementations.  Simplify World file
 * support a little.
 *
 * Added support for Tiled TIFF input
 * Luca Cristelli (luca.cristelli@ies.it) 2/2001
 *
 * Added TIFF World file support, 
 * Fixed one segfault bug (tif_pos = ftell(...)
 * Eric G. Miller 4-Nov-2000
 *
 * removed LZW support 5/2000
 *
 * Updated to FP map writing functions 11/99 Markus Neteler
 *
 * Updated 9/99 to true 24bit support.
 * The old version was limited to 256 colors.
 * 
 *  Incorporated color quantization to speed up GRASS.
 *   (GRASS is getting extremely slow with more than
 *     a few thousand categories/colors)
 *    
 *    Stefano Merler
 *    merler@irst.itc.it
 *
 * This r.tiff version uses the standard libtiff from your system.
 *  8. June 98 Marco Valagussa <marco@duffy.crcc.it>
 *
 * Original version: 
 * r.in.tiff - Converts from a Tagged Image File Format image to a Grass Raster.
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include "tiffio.h"
#include "gis.h"

#include "config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static void
set_cellhd(char *fname, struct Cell_head *head, 
    u_long height, u_long width, int verbose);

typedef struct {
  int	type;
  int	length;
  u_char	*map[3];
} colormap_t;

typedef int boolean;

#ifdef __CYGWIN__
#define uint16 unsigned short
#endif
#define	CVT(x)		((uint16)(((x) * 255) / ((1L<<16)-1)))

#define READ_IMAGE_BY_TILE 1
#define READ_IMAGE_BY_STRIPE 2
#define READ_IMAGE_BY_SCANLINE 3
#define READ_WHOLE_IMAGE 4

static boolean     Verbose;
static	char  *inf = NULL ;
static	char  *outf = NULL ;

int main (int argc, char *argv[])
{
  int         depth=0, numcolors;
  TIFF *tif;
  u_char *inp, *outp = NULL;
  int col, row, i;
  u_char     *Map = NULL;
  u_char     *buf, *tilebuf;
  uint16 *redcmap, *greencmap, *bluecmap;

  colormap_t  Colormap;	/* The Pixrect Colormap */
  u_char      red[256], green[256], blue[256];
  
  int readmode;

  int cellfp;
  CELL *cell;
  struct Cell_head cellhd;
  struct Colors cellcolor;
  struct Colors bwcolors, *pbwcolr;
  int cellcol, cellnrows, cellncols;
  struct Option *inopt, *outopt;
  struct Flag *vflag;
  struct GModule *module;

  u_short	bitspersample,
    samplesperpixel,
    photometric;
  u_long	width,
    height;
  u_long tilewidth = 0, tilelength = 0, tilesize = 0, tilerowsize = 0;
  int Bands = 0;
  int x;
  int outred, outgrn, outblu;
  char mapred[300], mapgrn[300], mapblu[300];
  CELL *cellr, *cellg, *cellb;
  
  G_gisinit(argv[0]);
  
  /* Set description */
  module              = G_define_module();
  module->description = ""\
  "Imports a TIFF (8 or 24 bit) raster file into GRASS raster file(s)";

  vflag = G_define_flag();
  vflag->key		= 'v';
  vflag->description	= "Verbose mode on.";

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
  outopt->description	= "Name of new raster file or band prefix";

  if(G_parser(argc, argv))
    exit(-1);

  Verbose = vflag->answer;
  pbwcolr=&bwcolors;


  /* default image tipe is SCANLINE */
  readmode = READ_IMAGE_BY_SCANLINE;
  
  inf = inopt->answer;
  tif = TIFFOpen(inf, "r");
  if (tif == NULL)
    G_fatal_error("Error opening TIFF file.");

  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
  TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
  if (Verbose)
    TIFFPrintDirectory(tif, stderr, 0l);
  if (bitspersample > 8)
    G_fatal_error("Can't handle more than 8-bits per sample.");

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
    G_fatal_error("Only handle 1-channel gray scale or 3-channel color.");
  }

  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH,&width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH,&height);

  cellnrows  = height;
  cellncols  = width;
  
  set_cellhd(inf, &cellhd, height, width, Verbose);
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
  if(photometric == PHOTOMETRIC_RGB)
  {
    Bands = 1;
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
  else
  {
    /*if (!(cellfp = G_open_cell_new_random( outf )))*/
    if (!(cellfp = G_open_cell_new( outf )))
     G_fatal_error("New raster file couldn't be opened for writing.");
  }
  
  G_init_colors(&cellcolor);

  if (TIFFIsTiled(tif)) {
    
    if (Verbose) fprintf (stderr, "\ntiff file is TILED ");
    readmode = READ_IMAGE_BY_TILE;
    
    TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tilewidth); 
    TIFFGetField(tif, TIFFTAG_TILELENGTH, &tilelength);
    
    tilesize = TIFFTileSize(tif);
    tilerowsize = TIFFTileRowSize(tif);

    tilebuf = G_malloc(tilesize * (width / tilewidth + 1));
    if (tilebuf == NULL)
      G_fatal_error("Can't allocate memory for tiles buffer...");
    
    if (Verbose) fprintf (stderr, " (tiles are %ld x %ld)\n", tilewidth, tilelength);
  } 
  
  buf = (u_char *) malloc(TIFFScanlineSize(tif)+tilerowsize);
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
      if (Verbose) fprintf (stdout, "RGB photometric; 3 band files...\n"); 
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
    
    /* to support more tiff types (tiled and striped) 
     * we need to read some more data and then buil a scanline.
     * This way we don-t need to change the rest of the code.
     */
  if (Verbose)
    G_percent (row, height, 2);
 
    switch (readmode) 
    {
      case READ_IMAGE_BY_TILE:
        if (row % tilelength == 0) 
        {
        /* read all tiled required to build a pixel row */
          for(i = 0; (i * tilewidth) < width; i++) 
          {
            if (TIFFReadTile(tif, tilebuf + tilesize * i, 
                  i * tilewidth, row, 0, 0) < 0 )
            {
              fprintf(stderr, "Bad data read on tile: x = %ld, y = %d\n", 
                  i * tilewidth, row);
              exit(-1);
            }
          }
        }
        /* build row */
        for(i = 0; ( i * tilewidth ) < width; i++) 
        {
          memcpy(buf + i * tilerowsize, 
              tilebuf + i * tilesize + ( row % tilelength ) * tilerowsize, 
              tilerowsize );
        }  
        break;
      case READ_IMAGE_BY_STRIPE:
      case READ_IMAGE_BY_SCANLINE:
        if (TIFFReadScanline(tif, buf, row, 0) < 0)
        {
          fprintf(stderr, "Bad data read on line: %d\n", row);
          exit(-1);
        }
        break;
      default:
        G_fatal_error("Unknown read mode.");
    }
    
    inp = buf;
    switch (photometric) 
    {
      case PHOTOMETRIC_RGB:
        for (col = 0; col < width; col++) 
        {
          cellr[col] = (int) *inp++;		/* Red */
          cellg[col] = (int) *inp++;		/* Green */
          cellb[col] = (int) *inp++;		/* Blue */
        }
        if (G_put_raster_row(outred, cellr, CELL_TYPE) < 0 )
          G_fatal_error("Can't write new raster row!!");
        if (G_put_raster_row(outgrn, cellg, CELL_TYPE) < 0 )
          G_fatal_error("Can't write new raster row!!");
        if (G_put_raster_row(outblu, cellb, CELL_TYPE) < 0 )
          G_fatal_error("Can't write new raster row!!");
        break;
      case PHOTOMETRIC_MINISWHITE:
      case PHOTOMETRIC_MINISBLACK:
        switch (bitspersample) 
        {
          case 1:
            {
              int	sz_col = width/8;

              if ( width%8 ) ++sz_col;
              cellcol = 0;
              for (col=0; col<sz_col; col++)
              {
                if ( cellcol<width ) 
                {
                  cell[cellcol++] = (CELL)((*inp >> 7) & 0x01);
                  if ( cellcol<width ) 
                  {
                    cell[cellcol++] = (CELL)((*inp >> 6) & 0x01);
                    if ( cellcol<width ) 
                    {
                      cell[cellcol++] = (CELL)((*inp >> 5) & 0x01);
                      if ( cellcol<width ) 
                      {
                        cell[cellcol++] = (CELL)((*inp >> 4) & 0x01);
                        if ( cellcol<width ) 
                        {
                          cell[cellcol++] = (CELL)((*inp >> 3) & 0x01);
                          if ( cellcol<width ) 
                          {
                            cell[cellcol++] = (CELL)((*inp >> 2) & 0x01);
                            if ( cellcol<width ) 
                            {
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
              G_put_raster_row(cellfp, cell, CELL_TYPE);
              /* *outp++ = *inp++;*/
              break;
            }
          case 2:
            for (col = 0; col < ((width + 3) / 4); col++) 
            {
              *outp++ = (*inp >> 6) & 3;
              *outp++ = (*inp >> 4) & 3;
              *outp++ = (*inp >> 2) & 3;
              *outp++ = *inp++ & 3;
            }
            break;
          case 4:
            cellcol = 0;
            for (col = 0; col < width / 2; col++) 
            {
              /* *outp++ = *inp >> 4;
               *outp++ = *inp++ & 0xf;*/
              cell[cellcol++] = (CELL) (*inp >> 4);
              cell[cellcol++] = (CELL) (*inp++ & 0xf);
            }
            G_put_raster_row(cellfp, cell, CELL_TYPE);
            break;
          case 8:
            for (col = 0; col < width; col++)
              cell[col] = (CELL) *inp++;
            G_put_raster_row(cellfp, cell, CELL_TYPE);
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
        G_put_raster_row(cellfp, cell, CELL_TYPE);
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

  if(Bands){
    G_close_cell(outred);
    G_close_cell(outgrn);
    G_close_cell(outblu);
  }
  else
  {
    G_close_cell( cellfp );
  }
	
  if(Bands)
  {
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
  else
  {
    G_write_colors( outf, G_mapset(), &cellcolor);
  }      

  if (Verbose)
    fprintf(stderr, "done.\n");

  exit(0);
}


#define  AFFINE_XSCALE  (0)
#define  AFFINE_XSHIFT  (1)
#define  AFFINE_YSHIFT  (2)
#define  AFFINE_YSCALE  (3)
#define  AFFINE_XORIGIN (4)
#define  AFFINE_YORIGIN (5)
#define  AFFINE_NPARAMS (6)

static void
set_cellhd(char *fname, struct Cell_head *head, 
    u_long height, u_long width, int verbose)
{
  int i, len, has_tfw = 0;
  char *tfw, *dir, *ptr, atiff[4][5] = {".tif", ".tiff", ".TIFF", ".TIF"},
                  atifw[4][5] = {".tfw", ".tifw", ".TIFW", ".TFW"};
  double params[AFFINE_NPARAMS];
  FILE *ftfw;

  if (fname == NULL || head == NULL)
    G_fatal_error("Got NULL filename or Cell_head structure");
  
  if(verbose)
    fprintf(stderr, "  Looking for TIFF World file ...   ");

  len = strlen(fname) + 6;
  tfw = G_calloc(len,1);
  strncpy(tfw, fname, len - 6);

  /* Make sure our file name search doesn't get a directory */
  dir = strrchr(tfw, '/');  /* TODO: Make this OS independent */
  if (dir == NULL)
    dir = tfw;
  else
    dir++;

  /* Find the extension (if exists) */
  for (i = 0; i < 4; i++)
  {
    ptr = strstr(dir, atiff[i]);
    if (ptr != NULL)
      break;
  }

  /* If it wasn't found, move ptr to the end of the string */
  if(i == 4)
    ptr = strchr(tfw,'\0');

  /* Find the world file if exists */

  for (i = 0; i < 4; i++)
  {
    strncpy(ptr, atifw[i], 5);
    ftfw = fopen(tfw, "r");
    if (ftfw != NULL) {
      has_tfw = 1;
      break;
    }
  }
  
  if (has_tfw) {
    if (fscanf (ftfw, "%lf%lf%lf%lf%lf%lf",
                &params[AFFINE_XSCALE],
                &params[AFFINE_XSHIFT],
                &params[AFFINE_YSHIFT],
                &params[AFFINE_YSCALE],
                &params[AFFINE_XORIGIN],
                &params[AFFINE_YORIGIN]) != AFFINE_NPARAMS)
    {
        G_fatal_error ("reading TIFF world file");
    }

    if (params[AFFINE_XSHIFT] != 0.0
        || params[AFFINE_YSHIFT] != 0.0)
    {
      G_warning ("TIFF world file specifies shear factors, but\n"
                   "they are not supported.  Importing TIFF as XY...\n");
      goto force_xy;
    }
    else
    {
      head->ew_res = params[AFFINE_XSCALE];
      head->ns_res = -params[AFFINE_YSCALE];
      head->west = params[AFFINE_XORIGIN] - head->ew_res / 2.0;
      head->east = head->west + head->ew_res * width;
      head->north = params[AFFINE_YORIGIN] + head->ns_res / 2.0;
      head->south = head->north - head->ns_res * height;
      head->zone = G_zone();
      head->proj = G_projection();
      fclose(ftfw);
      if (verbose)
        fprintf(stderr, "Found it!\n");
    }
  }
  else
  {
force_xy:
    head->north = (double) height;
    head->east  = (double) width;
    head->south = head->west = 0.0;
    head->ns_res = head->ew_res = 1;
    head->zone = 0;
    head->proj = PROJECTION_XY;
    if (verbose)
      fprintf(stderr, "Not Found!\n");
  }
  head->rows = height;
  head->cols = width;
  head->format = 0;
  head->compressed = 0; /* Is this necessary? */

  G_free(tfw);

}
  
    
/* vim: set softtabstop=2 shiftwidth=2 expandtab: */
