#include "globals.h"
#define YES 1
#define NO 0

int *attr, maxcategory;

lump()
{
extern int nrows, ncols;
char lumpedfile[20], lumpfile[20], *mapset, tmpstr[20];
int lumprow, lumpcol, i, **lumpdata, **compress, flumped, resolve;
int **temparray, flump, j, k, ii, jj, jl, il, jcol;
int t, b, l, r;
CELL *pcell, *G_allocate_cell_buf();
struct Cell_head new_window;
extern struct Cell_head window, cellhd;
struct Colors colors;

      mapset = G_mapset();
      Curses_prompt_gets("Enter the map layer for lumping : ",lumpfile);
      Curses_prompt_gets("Name of new cell file ? ", lumpedfile);
      flump = G_open_cell_old(lumpfile, mapset);
      G_read_colors(lumpfile,mapset,&colors);

      nrows = G_window_rows();
      ncols = G_window_cols();
      Curses_prompt_gets("Cells ( > 0!) to lump row-wise ? ",tmpstr);
      lumprow = atoi(tmpstr);
      Curses_prompt_gets("Cells ( > 0!) to lump column-wise ? ",tmpstr);
      lumpcol = atoi(tmpstr);
      lumpdata = (int **) malloc(sizeof(int)*nrows);
      temparray = (int **) malloc(sizeof(int)*lumprow);
 
      for(i=0;i < lumprow;i++)
        temparray[i] = (int *) malloc(sizeof(int)*lumpcol);
 
      maxcategory = 0;
      for(i=0;i < nrows;i++) {
        lumpdata[i] = (int *) malloc(sizeof(int)*ncols);
        pcell = G_allocate_cell_buf();
        G_get_map_row(flump,pcell,i);
        for(j=0;j < ncols;j++) {
          lumpdata[i][j] = pcell[j];
          if(lumpdata[i][j] > maxcategory)
                maxcategory = lumpdata[i][j];
        }
      }
      attr = (int *) malloc(sizeof(int)*(maxcategory+2));

      compress = (int **)malloc(sizeof(int)*((int)(nrows*1./lumprow)+2));
      for(i=0,k=0;i < nrows - lumprow;i+=lumprow,k++) {
        compress[k]=(int *)malloc(sizeof(int)*((int)(ncols*1./lumpcol)+2));
        for(j=0,jcol=0;j < ncols - lumpcol;j+=lumpcol,jcol++) {
          for(ii=i,il=0;ii < i+lumprow;ii++,il++)
            for(jj=j,jl=0;jj < j+lumpcol;jj++,jl++)
              temparray[il][jl] = lumpdata[ii][jj];
          compress[k][jcol] = checkcategory(lumprow,lumpcol,temparray);
        }
        for(il=0;il < lumprow;il++)
          for(jl=0;jl < lumpcol;jl++)
            temparray[il][jl] = 0;
 
        for(ii=i,il=0;ii < i+lumprow;ii++,il++)
          for(j=ncols - lumpcol,jl=0;j < ncols;j++,jl++)
            temparray[il][jl] = lumpdata[ii][j];
        compress[k][jcol] = checkcategory(il,jl,temparray);
      }

      compress[k]=(int *) malloc(sizeof(int)*((int)(ncols*1./lumpcol)+2));
      for(j=0,jcol=0;j < ncols - lumpcol;j+=lumpcol,jcol++)
        for(jj=j,jl=0;jj < j+lumpcol;jj++,jl++)
          for(i=nrows-lumprow,il=0;i < nrows;i++,il++) {
              temparray[il][jl] = lumpdata[i][jj];
          compress[k][jcol] = checkcategory(lumprow,lumpcol,temparray);
        }
          
      for(il=0;il < lumprow;il++)
        for(jl=0;jl < lumpcol;jl++)
          temparray[il][jl] = 0;
 
      for(j=ncols - lumpcol,jl=0;j < ncols;j++,jl++)
        for(i=nrows-lumprow,il=0;i < nrows;i++,il++)
          temparray[il][jl] = lumpdata[i][j];
      compress[k][jcol] = checkcategory(il,jl,temparray);
      G_close_cell(flump);

      new_window.format = window.format;
      new_window.compressed = window.compressed;
      new_window.rows = (int)((nrows*1.)/lumprow);
      new_window.cols = (int)((ncols*1.)/lumpcol);
      new_window.proj = window.proj;
      new_window.zone = window.zone;
      new_window.ew_res = window.ew_res*lumpcol;
      new_window.ns_res = window.ns_res*lumprow;
      new_window.north = window.north;
      new_window.south = window.north - new_window.rows*new_window.ns_res;
      if(new_window.south > window.south)
		new_window.south = window.south;
      new_window.west = window.west;
      new_window.east = window.west + new_window.cols*new_window.ew_res;
      if(new_window.east > window.east)
		new_window.east = window.east;
      G_put_window(&new_window);
      G_set_window(&new_window);
      flumped = G_open_cell_new(lumpedfile, mapset);
      nrows = G_window_rows();
      ncols = G_window_cols();
 
      for(i=0;i < nrows;i++)
        G_put_map_row(flumped,compress[i]);
 
      G_close_cell(flumped);
      G_write_colors(lumpedfile,mapset,&colors);
      G_adjust_window_to_box(&new_window,&VIEW_MAP1_ZOOM->cell.head, 
		VIEW_MAP1_ZOOM->nrows, VIEW_MAP1_ZOOM->ncols);
      Configure_view(VIEW_MAP1_ZOOM, lumpedfile, mapset, (double)new_window.ns_res,
                       (double)new_window.ew_res);
      mydraw(VIEW_MAP1_ZOOM,OVER_WRITE,lumpedfile);
 
      Curses_prompt_gets("Keep new lumped resolution (NO = 0) (YES=1) ? ",
				tmpstr);
      resolve = atoi(tmpstr);
      if(resolve == NO) {
        G_put_window(&window);
        G_set_window(&window);
        nrows = G_window_rows(); ncols = G_window_cols();
	G_adjust_window_to_box(&window,&VIEW_MAP1_ZOOM->cell.head,
			VIEW_MAP1_ZOOM->nrows, VIEW_MAP1_ZOOM->ncols);
	Configure_view(VIEW_MAP1_ZOOM, lumpfile, mapset,(double)window.ns_res,
			(double)window.ew_res);
	G_adjust_window_to_box(&window,&VIEW_MAP1->cell.head, VIEW_MAP1->nrows, 
				VIEW_MAP1->ncols);
	Configure_view(VIEW_MAP1, lumpfile, mapset,(double)window.ns_res,
			(double)window.ew_res);
        mydraw(VIEW_MAP1,OVER_WRITE,lumpfile);
      }
      else {
	G_put_window(&new_window);
	G_set_window(&new_window);
	nrows = G_window_rows(); ncols = G_window_cols();
	G_adjust_window_to_box(&new_window,&VIEW_MAP1->cell.head,
					VIEW_MAP1->nrows, VIEW_MAP1->ncols);
	Configure_view(VIEW_MAP1, lumpedfile, mapset, (double)new_window.ns_res,
				(double)new_window.ew_res);
	mydraw(VIEW_MAP1,OVER_WRITE,lumpedfile);
	cellhd = new_window;
      }
      Erase_view(VIEW_MAP1_ZOOM);
      use_mouse_msg();
      driver();
}

/* checkcategory goes through a local neighborhood to find the
"dominant" category value.  Right now, an attribute array indexed
all the way to maxcategory value is used to count the number of
cells of each category in the neighborhood.  A better scheme exists!! */

checkcategory(row,column,array)
int row, column, **array;
{
int k,l, localvalue, localnum;

        for(k=0;k < maxcategory+2;k++)
          attr[k] = 0;

        for(k=0;k < row;k++)
          for(l=0;l < column;l++)
            attr[array[k][l]]++;

        localnum = 0;
        for(k=0;k < maxcategory;k++)
          if(attr[k] > localnum) {
            localnum = attr[k];
            localvalue = k;
           }
            
        return(localvalue);
}
