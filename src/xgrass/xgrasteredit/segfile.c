
/* 
 * FILE: openseg.c
 * 
 * PROGRAMMER: David M. Johnson
 * 
 * FUNCTIONS:
 *
 * OpenSegmentFile()
 * -----------------
 * Open and initialize the segment file (Global.segname).  Read 
 * its header (Global.seghd), get its file desciptor (Global.segfd), 
 * and initialize the segment structure (Global.seg).
 * 
 * CloseSegmentFile()
 * ------------------
 * Flush, release and close the current segment file.  This 
 * function flushes all changes out to the current segment
 * file and closes it.  Color-table, category and range 
 * information are also written out to the appropriate files.  
 * The segment file will not be removed and may be opened for 
 * further editing at a later time.
 *
 * RemoveSegmentFile()
 * -------------------
 * This function will close and remove the current segment file
 * along with all of its support files: the color-table file, 
 * the categories file and the range information file.
 * 
 */

#include "xgre.h"

/***********************/
/*** OpenSegmentFile ***/
/***********************/

int OpenSegmentFile()
{
   char spath[200];
   char shpath[200];
   int status;
   FILE *seghdfp;

#  ifdef DEBUG
   printf("OpenSegmentFile:%s\n",spath);
#  endif

/*** READ SEGMENT HEADER FILE AND SET REGION ***/

   sprintf(shpath,"%s/%s/%s/%s",
      G_location_path(),G_mapset(),XGRE_SEGHD,Global.segname); 
   if ((seghdfp = fopen(shpath,"r")) == NULL)
     {
     XgError(Global.applShell,"Unable to open segment header for reading");
     return(-1);
     }
   G__read_Cell_head(seghdfp,&(Global.seghd),0);
   fclose(seghdfp);

   if (G_set_window(&(Global.seghd)) < 0)
     {
     XgError(Global.applShell,"Unable to set region");
     return(-1);
     }

/*** OPEN AND INIT SEGMENT FILE ***/

   sprintf(spath,"%s/%s/%s/%s",
      G_location_path(),G_mapset(),XGRE_SEG,Global.segname); 
   if ((Global.segfd = open(spath,2)) < 0)
      {
      XgError(Global.applShell,"Unable to open segment file for reading");
      return(-1);
      }

   XmUpdateDisplay(Global.applShell);
   if ((segment_init(&(Global.seg),Global.segfd,XGRE_MEM_SEGS)) < 0)
      {
      XgError(Global.applShell,"Unable to initialize segment file");
      return(-1);
      }

#  ifdef DEBUG
   printf("   segfd = %d\n",Global.segfd);
   printf("   nrows = %d\n",Global.seg.nrows);
   printf("   ncols = %d\n",Global.seg.ncols);
   printf("   srows = %d\n",Global.seg.srows);
   printf("   scols = %d\n",Global.seg.scols);
   printf("   size  = %d\n",Global.seg.size);
   printf("   len   = %d\n",Global.seg.len);
#  endif

/*** READ SEGMENT COLORTABLE FILE ***/

   if (read_colors(XGRE_SEGCOLR,Global.segname,G_mapset(),&(Global.colors)) < 0)
     {
     XgError(Global.applShell,"Unable to read segment color file");
     return(-1);
     }

/*** READ SEGMENT CATEGORIES FILE ***/

   if (G__read_cats(XGRE_SEGCATS,Global.segname,G_mapset(),&(Global.cats),1)<0)
     {
     XgError(Global.applShell,"Unable to read segment category file");
     return(-1);
     }

/*** READ SEGMENT RANGE FILE ***/

   if (read_range(XGRE_SEGRANGE,Global.segname,G_mapset(),&(Global.range))<0)
     {
     XgError(Global.applShell,"Unable to read segment range file");
     return(-1);
     }
   return(0);
}

/************************/
/*** CloseSegmentFile ***/
/************************/

int CloseSegmentFile()
{
   char shpathname[400];
   FILE *segcolrfp;

   /* flush, release and close the current segment file */
   segment_flush(&(Global.seg));
   segment_release(&(Global.seg));
   close(Global.segfd);

   /* write out the color-table */
   sprintf(shpathname,"%s/%s/%s/%s",
      G_location_path(),G_mapset(),XGRE_SEGCOLR,Global.segname);
   if ((segcolrfp = fopen(shpathname,"w")) == NULL)
      {
      XgError(Global.applShell,"Unable to create segment color file.");
      return(-1);
      }
   G__write_colors(segcolrfp,&(Global.colors));
   fclose(segcolrfp);

   /* write out the category info */
   if (G__write_cats(XGRE_SEGCATS,Global.segname,&(Global.cats)) < 0)
      {
      XgError(Global.applShell,"Unable to write segment categories file.");
      return(-1);
      }
   
   /* write out the range info */
   if(write_range(XGRE_SEGRANGE,Global.segname,&(Global.range))<0)
      {
      XgError(Global.applShell,"Unable to create segment range file.");
      return(-1);
      }

   strcpy(Global.segname,"");
   return(0);
}

/*************************/
/*** RemoveSegmentFile ***/
/*************************/

int RemoveSegmentFile()
{
   /* flush, release and close the current segment file */
   segment_flush(Global.seg);
   segment_release(Global.seg);
   close(Global.segfd);

   /* remove category file */
   G_remove(XGRE_SEGCATS,Global.segname);
   
   /* remove color file */
   G_remove(XGRE_SEGCOLR,Global.segname);
   
   /* remove range file */
   G_remove(XGRE_SEGRANGE,Global.segname);
   
   /* remove segment file itself */
   G_remove(XGRE_SEG,Global.segname);

   strcpy(Global.segname,"");
   return(0);
}
   
