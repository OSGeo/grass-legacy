/* This file draws the data from the output file written to by quickprop */

#include <stdio.h>
#include "globals.h"

draw(NTestPatterns) 
int NTestPatterns;
{
extern int nrows, ncols, **FLAG, Noclass, *actual;
extern float **OUT;
char outname[25], *G_ask_cell_new(), *mapset, *temp, tmpstr[20];
FILE *fopen(), *fclose();
CELL *pcell, *G_allocate_cell_buf();
int min, max, i, row, col, fout, save, j;
long **cellout, clss;
struct Colors outcolors;
extern struct Cell_head cellhd;

  if(OUT == NULL) {
	G_warning("Didn't propagate input data");
	return(-1);
  }

  mapset = (char *)malloc(sizeof(char)*20);
  mapset = G_mapset();
  pcell = G_allocate_cell_buf();

  G_set_window(&cellhd);
  Curses_prompt_gets("Enter the map name for output : ",outname);
  Curses_write_window (PROMPT_WINDOW, 1, 1, "");
  if(G_find_cell(outname,"")) {
    G_warning("File already exists");
    Curses_prompt_gets("Overwrite ? (Yes = 1) (No = 0) ",tmpstr);
    if(atoi(tmpstr)) 
      fout = G_open_cell_new(outname, mapset);
    else return(0);
  }
  else
    fout = G_open_cell_new(outname, mapset);

  cellout = (long **) malloc(sizeof(long)*nrows);
  for(i=0;i < nrows;i++) 
    cellout[i] = (long *) malloc(sizeof(long)*ncols);

  if(NTestPatterns > 0) {
    for(row=0,i=0;row < nrows;row++) {
      for(col=0;col < ncols;col++) {
        if(FLAG != NULL) {
          if(FLAG[row][col] == 1 && i < NTestPatterns) {
	    for(j=1;j <= Noclass;j++) {
	      if(OUT[i][j] > 0.5) {
	        clss = j; /* Break @ first class */
	        break;
	      }
	    }
/*	    cellout[row][col] = (nint)((clss*1./Noclass)*100.); */
	    cellout[row][col] = actual[clss];
            i++;
          }
	  else cellout[row][col] = 0;
        }
        else {
/*	printf("disclaimer!\n"); */
          for(j=1;j <= Noclass;j++) {
            if(OUT[i][j] > 0.5) {
              clss = j; /* Break @ first class */
              break;
            } 
           }
/*           cellout[row][col] = (nint)(clss*1./Noclass*100); */
	     cellout[row][col] = actual[clss];
           i++;
        }
      }
      G_put_map_row(fout,cellout[row]);
    }

    min = 0; max = 100;
    G_close_cell(fout);
    G_init_colors(&outcolors);
    make_grn_yel_red(&outcolors,min,max);
    G_set_color((CELL)0,0,0,0,&outcolors);
    G_write_colors(outname,mapset,&outcolors);
    G_adjust_window_to_box(&cellhd,&VIEW_MAP1_ZOOM->cell.head,
                VIEW_MAP1_ZOOM->nrows, VIEW_MAP1_ZOOM->ncols);
    Configure_view(VIEW_MAP1_ZOOM, outname, mapset, cellhd.ns_res,
                       cellhd.ew_res);
    mydraw(VIEW_MAP1_ZOOM,OVER_WRITE,outname);
  }
}

make_grn_yel_red(pcolr,min,max)
struct Colors *pcolr;
CELL min, max;
{
int i, j, num, n, red, grn, blu;

  G_init_colors(pcolr);
  if(max < min)
    return -1;

  if(min == 1) min=0;
  if(max == -1) max=0;
  num = max - min + 1;
  n = num/2;
  red = 256; blu=0;
  for(i=1;i <= n;i++) {
    grn = ((float)i/(float)n)*256;
    G_set_color((CELL)(i+min),red,grn,blu,pcolr);
  }
  grn = 256; blu=0;
  j=0;
  for(;i<num;i++) {
    red = 256 - ((float)(j++)/(float)n)*256;
    G_set_color((CELL)(i+min),red,grn,blu,pcolr);
  }
  G_set_color((CELL)(0),256,256,256,pcolr);
  return 1;
}
