
/* 
 * FILE: seg2ras.c 
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * SegmentToRaster()
 * -----------------
 * This function writes the current segment file, color-table, 
 * category-table and range out to a new raster map layer 
 * named Global.rname. 
 *
 */

#include <stdio.h>
#ifdef SVR4
#include <malloc.h>
#include <sys/types.h>
#else
#include <alloca.h>
#include <ctype.h>
#endif
#include <sys/file.h>
/*#include <dirent.h>*/
#include <gis.h>
#include "xgre.h"

/***********************/
/*** SegmentToRaster ***/
/***********************/

int SegmentToRaster()
{
   char  chpath[100];   /* cell file header path */
   FILE *chfp;          /* cell header file pointer */
   CELL *cbuf;          /* cell row buffer */
   int   rfd;           /* raster map file descriptor */
   int   irow;          /* for-loop row counter */

#  ifdef DEBUG
   printf("seg2ras: saving %s@%s\n",Global.rname,G_mapset());
#  endif

/*** SET REGION TO MATCH SEGMENT FILE HEADER ***/

   if (G_set_window(&(Global.seghd)) == -1)
      {
      XgError(Global.applShell,"Unable to set region");
      return(-1);
      }

#  ifdef DEBUG
   printf("seg2ras: region set\n");
#  endif

/*** OPEN RASTER ***/

   if ((rfd = G_open_cell_new(Global.rname)) == NULL)
      {
      XgError(Global.applShell,"Unable to open raster map for writing.");
      return(-1);
      }

/*** COPY SEGMENT FILE TO RASTER MAP ROW BY ROW ***/

   XmUpdateDisplay(Global.applShell);

   segment_flush(&(Global.seg));
   cbuf = (CELL *)G_malloc(Global.seghd.cols*sizeof(CELL));
   for (irow=0; irow<Global.seghd.rows; irow++)
      {
      if ((segment_get_row(&(Global.seg),(char*)cbuf,irow)) < 0)
         {
         XgError(Global.applShell,"Error reading segment file row.");
         return(-1);
         }
      if ((G_put_map_row(rfd,(CELL*)cbuf)) < 0)
         {
         XgError(Global.applShell,"Error writing raster map row.");
         return(-1);
         }
      }
   G_close_cell(rfd);

#  ifdef DEBUG
   printf("seg2ras: raster written\n");
#  endif

   XmUpdateDisplay(Global.applShell);

   /* create raster map categories file */
   if (G_write_cats(Global.rname,&(Global.cats)) < 0)
      {
      XgError(Global.applShell,"Unable to write raster map categories file.");
      return(-1);
      }

#  ifdef DEBUG
   printf("seg2ras: cats written\n");
#  endif

   /* create raster map color file */
   if (G_write_colors(Global.rname,G_mapset(),&(Global.colors)) < 0)
      {
      XgError(Global.applShell,"Unable to write raster map color file.");
      return(-1);
      }

#  ifdef DEBUG
   printf("seg2ras: colors written\n");
#  endif

   /* create raster map range file */
   if (G_write_range(Global.rname,&(Global.range)) < 0)
      {
      XgError(Global.applShell,"Unable to write raster map range file.");
      return(-1);
      }

return(0);
}

