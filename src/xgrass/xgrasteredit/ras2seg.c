
/* 
 * FILE: ras2seg.c 
 *
 * PROGRAMMER: David M. Johnson
 *
 * RasterToSegment()
 * -----------------
 * This function creates a segment file and segment support 
 * files based on an existing raster map.  The raster map 
 * named Global.rname in mapset Global.mapset will be used.  
 *
 * If the global flag Global.segtype is set to XGRE_REGION_REG
 * then the segment file will be created using the raster map's
 * region.  If Global.segtype is set to XGRE_CURRENT_REG then
 * the current GRASS region will be used.
 *
 * Because a user may create several segment file for one 
 * raster map layer, a special scheme is used to name segment
 * files.   The name will be created by taking the raster map
 * name and adding a suffix to it.  The first segment file
 * created for a raster map will be named with a suffix or
 * ".s0" and the second will be ".s1" and the third will
 * be ".s2" and so on. 
 *
 * The segment support files that are created are the header
 * file, the categories file, the color-table file, and the 
 * range file.  The directories in which these files are
 * placed are determined by the following C Pre-Processor
 * variables which are defined in the raster editor's 
 * header file xgre.h:
 *
 *    XGRE_SEG   
 *    XGRE_SEGHD  
 *    XGRE_SEGCATS 
 *    XGRE_SEGCOLR  
 *    XGRE_SEGRANGE  
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
/*** RasterToSegment ***/
/***********************/

int RasterToSegment()
{
   FILE    *segcolrfp;       /* segment color file descriptor */
   FILE    *seghdfp;         /* segment header file descriptor */
   char     spathname[100];  /* segment file path name */
   CELL    *rowbuf;          /* raster row buffer */
   char     shpathname[100]; /* segment header file path name */
   int      srows, scols,    /* rows & columns per segment */ 
            hsegs,           /* horizontal no. of segments */
            vsegs,           /* vertical no. of segments */
            nsegs,           /* no. of segments to be held in memory */
            tsegs,           /* total no. of segments */
            i=0,
            row,
            ret,
            len,
            rfd;              /* raster file descriptor */

/*** GET AND SET REGION FOR SEGMENT FILE ***/

   if (Global.segtype == XGRE_RASTER_REG)
      {
      /* get region of raster map */
      if (G_get_cellhd(Global.rname,Global.rmapset,&(Global.seghd)) < 0) 
         {
         XgError(Global.applShell,"Unable to read raster map header.");
         return(-1);
         }
      }
   else if (Global.segtype == XGRE_CURRENT_REG)
      {
      /* get current GRASS region */
      if (G_get_set_window(&(Global.seghd)) < 0) 
         {
         XgError(Global.applShell,"Unable to read current region.");
         return(-1);
         }
      }

   if (G_set_window(&(Global.seghd)) < 0) 
      {
      XgError(Global.applShell,"Unable to set region.");
      return(-1);
      }

/*** OPEN RASTER ***/

   if ((rfd = G_open_cell_old(Global.rname,Global.rmapset)) <0 ) 
      {
      XgError(Global.applShell,"Unable to open raster map for reading.");
      return(-1);
      }

/*** OPEN SEGMENT FILE ***/  

   /* build segment file name */
   do {
      sprintf(spathname,"%s/%s/seg/%s.s%d",
         G_location_path(),G_mapset(),Global.rname,i);
      sprintf(Global.segname,"%s.s%d",Global.rname,i++);
      }
   while ((ret = access(spathname,0)) == 0);

   XmUpdateDisplay(Global.applShell);

   /* open segment file */
   G__make_mapset_element("seg");
   sprintf(spathname,"%s/%s/seg/%s",
      G_location_path(),G_mapset(),Global.segname);
   if ((Global.segfd = open(spathname,O_RDWR|O_CREAT,0664)) < 0)
      {
      XgError(Global.applShell,"Unable to create segment file.");
      return(-1);
      }

   /* format segment file */
   srows = scols = XGRE_SEG_EDGE;          /* segment size */
   nsegs = XGRE_MEM_SEGS;                  /* no. to be held in memory */
   hsegs = (Global.seghd.cols-1)/scols+1;  /* vertical no. of segments */ 
   vsegs = (Global.seghd.rows-1)/srows+1;  /* horizontal no. of segments */
   tsegs = hsegs*vsegs;                    /* total no. of segments */ 
#  ifdef DEBUG
   printf("ras2seg: segfile=%s hsegs=%d vsegs=%d tsegs=%d bytes=%d\n",
   Global.segname,hsegs,vsegs,tsegs,tsegs*srows*scols*sizeof(CELL));
#  endif

   XmUpdateDisplay(Global.applShell);
   if ((ret=segment_format_nofill(Global.segfd,Global.seghd.rows,
      Global.seghd.cols,srows,scols,sizeof(CELL))) < 1 ) 
         {
         XgError(Global.applShell,"Unable to format segment file.");
         return(-1);
         }

   /* initialize segment file */
   XmUpdateDisplay(Global.applShell);
   if((ret=segment_init(&(Global.seg),Global.segfd,nsegs))<0) 
      {
      XgError(Global.applShell,"Unable to initialize segment file.");
      return(-1);
      }

/*** COPY RASTER MAP DATA INTO SEGMENT FILE ***/

   if ((rowbuf = (CELL*)G_allocate_cell_buf()) < 0) 
      {
      XgError(Global.applShell,"Unable to allocate raster row buffer.");
      return(-1);
      }

   XmUpdateDisplay(Global.applShell);
   for(row = 0; row < (Global.seghd.rows); ++row) 
      {
      if ((ret = G_get_map_row_nomask(rfd,rowbuf,row)) < 0) 
         {
         XgError(Global.applShell,"Error reading raster map row.");
         return(-1);
         }
      if ((ret = segment_put_row(&(Global.seg),rowbuf,row)) < 0)
         {
         XgError(Global.applShell,"Error writing segment file row.");
         return(-1);
         }
      } 

   for (len = 0; len < srows-(Global.seghd.rows)%srows; ++len, ++row) 
      {
      if((ret = segment_put_row(&(Global.seg),rowbuf,row))<0) 
         {
         XgError(Global.applShell,"Error writing segment file row.");
         return(-1);
         }
      }
   XmUpdateDisplay(Global.applShell);
   segment_flush(&(Global.seg));
   G_close_cell(rfd);

/*** CREATE SEGMENT SUPPORT FILES ***/

   /* create segment file header */
   G__make_mapset_element(XGRE_SEGHD);
   sprintf(shpathname,"%s/%s/%s/%s",
      G_location_path(),G_mapset(),XGRE_SEGHD,Global.segname);
   if ((seghdfp = fopen(shpathname,"w")) == NULL)
      {
      XgError(Global.applShell,"Unable to create segment file header.");
      return(-1);
      }
   G__write_Cell_head(seghdfp,&(Global.seghd),1);
   fclose(seghdfp);

   /* create segment file category file */
   G__make_mapset_element(XGRE_SEGCATS);
   if (G_read_cats(Global.rname,Global.rmapset,&(Global.cats)) == 0)
      {
      if (G__write_cats(XGRE_SEGCATS,Global.segname,&(Global.cats)) < 0)
         {
         XgError(Global.applShell,"Unable to create segment categories file.");
         return(-1);
         }
      }

   /* create segment file color-table file */
   G__make_mapset_element(XGRE_SEGCOLR);
   if (G_read_colors(Global.rname,Global.rmapset,&(Global.colors)) != -1)
      {
      sprintf(shpathname,"%s/%s/%s/%s",
         G_location_path(),G_mapset(),XGRE_SEGCOLR,Global.segname);
      if ((segcolrfp = fopen(shpathname,"w")) == NULL)
         {
         XgError(Global.applShell,"Unable to create segment color file.");
         return(-1);
         }
      G__write_colors(segcolrfp,&(Global.colors));
      fclose(segcolrfp);
      }

   /* create segment file range file */
   G__make_mapset_element(XGRE_SEGRANGE);
   if (G_read_range(Global.rname,Global.rmapset,&(Global.range)) != -1)
     {
     if(write_range(XGRE_SEGRANGE,Global.segname,&(Global.range))<0)
        {
        XgError(Global.applShell,"Unable to create segment range file.");
        return(-1);
        }
     }

   return(0);
}
