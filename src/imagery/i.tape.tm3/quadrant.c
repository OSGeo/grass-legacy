/*======================================================================
Filename:   quadrant.c
Module:	    i.landsat.tm
Author:	    Christopher Lesher

$Id$

Even though the name of this file is quadrant.c, the routines here handle
the import of quadrant scenes and full scenes.  The file formats for full
scene and quadrant scene are almost identical.
======================================================================*/

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "landsat.h"
#include "local_proto.h"

/*======================================================================
                            ImportQuadrant

Files have been determined to be in quadrant scene format.  The volume
directory file is in header.  The file stream is positioned at the end
of the first file.

Return Value:
   Return bit mask containing bit for every band imported.  If band b was
   imported successfully, then ImportQuadrant() | b != 0.
   If the files were examined but not imported, return -1.
======================================================================*/
long 
ImportQuadrant (Landsat *landsat, unsigned char *header, int length)
{
   unsigned char leader[4321];
   unsigned char sc_header[4321];
   unsigned char map_proj[4321];
   int band, filenum, k;
   long bands_done;  /* bitmask for bands successfully imported. */
   FileSequence *fileseq = &landsat->fileseq;
   TMinfo tminfo;


   /**  mbaba **/
   /*** if(ExamineQuadrant(landsat, header, length, ! landsat->examine, &tminfo)) **/
   /*** get all of tminfo from ExamineQuadrant **/

   if(ExamineQuadrant(landsat, header, length, ! landsat->examine, &tminfo))
      return(-1);
   if(landsat->examine) {
      PrintTMinfo(&tminfo);
      return(-1);
   }


   /* Fill in the Tape_info structure. */
   sprintf(landsat->info.id[0], "Thematic Mapper. Scene ID %s",
      tminfo.scene_id);
   sprintf(landsat->info.id[1], "");
   sprintf(landsat->info.desc[0], ""); 	  /** clesher 20 Oct 93 **/
   sprintf(landsat->info.desc[1], "Mission %d", tminfo.mission);
   sprintf(landsat->info.desc[2], "Path %d Row %d ",
      tminfo.path, tminfo.row_number);
   if(tminfo.quadrant)
      sprintf(landsat->info.desc[2], "%s Quadrant %d", landsat->info.desc[2],
      	 tminfo.quadrant);
   else
      sprintf(landsat->info.desc[2], "%s Full Scene", landsat->info.desc[2]);
   sprintf(landsat->info.desc[3], "Date:  %s", SPrintDate(tminfo.image_date));
   switch(tminfo.correction) {
      case 4:
      sprintf(landsat->info.desc[4], "Geometrically uncorrected data"); break;

      case 5:
      sprintf(landsat->info.desc[4],
      	 "Geometrically corrected data (without geodetic corrections)"); break;

      case 6:
      sprintf(landsat->info.desc[4],
      	 "Geometrically corrected data (with geodetic corrections)"); break;
   }


   /* Read files until we find a band the user wants. */
   bands_done = 0;
   while(landsat->bands_to_do) {
      int record_type;

      if(FileSeqNext(fileseq, -1))
      	 goto quit;
      if(FileSeqRead(fileseq, leader+1, 4320) == -1)
      	 goto quit;
      record_type = RecordType(leader);
      if(record_type == VOLUME_DESCRIPTOR)
         continue;
      else if(record_type == FILE_DESCRIPTOR) {
         filenum = atoi(Field(leader, 45, 48));

	 /** mbaba **/
      	 /* Get the Scene Header Record */
	 if(FileSeqRead(fileseq, sc_header+1, 4320) == -1)
	   goto quit;

	 /** mbaba **/
      	 /* Get the Map Projection Record */
	 if(FileSeqRead(fileseq, map_proj+1, 4320) == -1)
	   goto quit;

         /* filenum must be one of 1, 4, 7, 11, ... */
	 /** mbaba **
         /** if( (filenum -1) % 3)
         /**    continue;
	  **/


      	 /* Skip records to radiometric calibration ancillary record. */
      	 for(k=0; k<1; ++k)
            if(FileSeqRead(fileseq, leader+1, 4320) == -1)
      	       goto quit;
         band = atoi(Field(leader, 13, 16));
      	 fprintf(stderr, "Found Band %d\n", band);
         if( landsat->bands_to_do & (1 << band)) {

	   /** mbaba **/
	   if( 1 ) {
	     tminfo.full_scene_geo.lat = atof(Field(sc_header, 53, 68));
	     tminfo.full_scene_geo.lon = atof(Field(sc_header, 69, 84));
	     tminfo.full_scene_pixel.row = atoi(Field(sc_header, 85, 100));
	     tminfo.full_scene_pixel.col = atoi(Field(sc_header, 101, 116));
	     tminfo.quad_scene_geo.lat = atof(Field(sc_header, 213, 228));
	     tminfo.quad_scene_geo.lon = atof(Field(sc_header, 229, 244));
	     tminfo.quad_scene_pixel.row = atoi(Field(sc_header, 245, 260));
	     tminfo.quad_scene_pixel.col = atoi(Field(sc_header, 261, 276));
	   }

	   if(ImportBand(landsat, band, &tminfo))
	     goto quit;
	   else {
	     landsat->bands_to_do ^= (1 << band);
	     bands_done |= (1 << band);
	   }
         }
      }
      else if(record_type != TRAILER)
      	 fprintf(stderr, "Could not recognize file %s\n", fileseq->name);
   }

quit:
   /* Dont bother changing region if nothing was imported. */
   if( bands_done)
      G_put_window(&landsat->window);

   /* put control points */
   if (bands_done)
     put_landsat_control (landsat, &tminfo, sc_header, map_proj);


   return(bands_done);
}



/*======================================================================
                              ImportBand

Read files until imagery file found.  A band is extracted
from the image file.  File stream is left at current file.

Return Value:
   0 if successful.
   1 if error.
======================================================================*/
int 
ImportBand (Landsat *landsat, int band, TMinfo *tminfo)
{
   int                  fd=0, k, count;
   unsigned char        buffer[7201], *pixel;
   char                 bandname[500];
   FileSequence         *fileseq = &landsat->fileseq;
   struct Cell_head     window;
   CELL                 *cellbuf = NULL, *cell;
   int                  lines, columns, top, bottom, left, right;
   struct Histogram     histogram;
   struct Colors        colr;
   struct Ref           ref;
   int	       	     	error=1;
   int	       	     	recordsize; /* for quad=4300, for full=7200 */
   char	       	     	title[100];
   struct History    	hist;

   do {
      if(FileSeqNext(fileseq, -1))
      	 goto quit;
      if(FileSeqRead(fileseq, buffer +1, 4320) == -1)
      	 goto quit;
   } while(RecordType(buffer) != FILE_DESCRIPTOR);

   /* lines & columns are dimensions of band. */
   lines = atoi(Field(buffer, 237, 244));
   columns = atoi(Field(buffer, 245, 248)) + atoi(Field(buffer, 249, 256)) +
      atoi(Field(buffer, 257, 260));
   recordsize = atoi(Field(buffer, 187, 192));

   top = landsat->top ? landsat->top : 1;
   bottom = landsat->bottom ? landsat->bottom : lines;
   left = landsat->left ? landsat->left : 1;
   right = landsat->right ? landsat->right : columns;

   if(bottom<1 || top>lines) {
      fprintf(stderr, "The range of rows is completely outside image.\n");
      return(1);
   }
   if(right<1 || left>columns) {
      fprintf(stderr, "The range of columns is completely outside image.\n");
      return(1);
   }

   /* Truncate region to band dimensions if necessary */
   if(top<1) { 	  /* This should never happen */
      top=1;
      fprintf(stderr, "Top row set to 1\n");
   }
   if(bottom>lines) {
      bottom=lines;
      fprintf(stderr, "Bottom row set to %d\n", lines);
   }
   if(left<1) {	  /* This should never happen */
      left=1;
      fprintf(stderr, "Left column set to 1\n");
   }
   if(right>columns) {
      right=columns;
      fprintf(stderr, "Right column set to %d\n", columns);
   }

   landsat->window.format = 0;
   landsat->window.compressed = 1;
   landsat->window.north = -top + 0.5;
   landsat->window.south = -bottom -0.5;
   landsat->window.west = left - 0.5;
   landsat->window.east = right + 0.5;
   landsat->window.cols = right - left +1;
   landsat->window.rows = bottom-top+1;
   landsat->window.ns_res = 1;
   landsat->window.ew_res = 1;
   landsat->window.proj = 0;
   landsat->window.zone = 0;

/***
   fprintf(stderr, "lines=%d columns=%d\n", lines, columns);
   fprintf(stderr, "top=%d bottom=%d left=%d right=%d\n", top,bottom,left,right);
***/

   if(G_set_window(&landsat->window) < 0)
      goto quit;

   /* Open cell file AFTER window is set. */
   sprintf(bandname, "%s.%d", landsat->outfile, band);
   fd = G_open_cell_new(bandname);
   if( fd == 0)
      goto quit;
   cellbuf = G_allocate_cell_buf();
   if( cellbuf == 0)
      goto quit;

   FileSeqFF(fileseq, recordsize * (top-1));
   StartProgress(landsat, band);
   fflush(stdout);
   for(k=top; k<=bottom; ++k) {
      int result;
      if(k%100==0)
      	 PrintProgress(landsat, k, bottom);
      result = FileSeqRead(fileseq, buffer, recordsize);
      if(result <= 0) {
      	 EndProgress();
      	 if(result == -1)
      	    fprintf(stderr, "Error while reading tape.\n");
      	 else if(result == 0)
      	    fprintf(stderr, "End of file reached on tape.\n");
      	 fprintf(stderr, "%d of %d lines for band %d written.\n", k-top,
      	    bottom-top+1, band);
      	 break;
      }
      
      /* mbaba -- save left and right fills of first and last line */
      /* TODO -- fix for windowed reads */
      if (k == top) {
	 /* Top Left corner */
	tminfo->NWpix.col = FieldBinary(buffer, 25, 28);
	tminfo->NWpix.row = lines - top +1 - 0.5;

	/* Top Right corner */
	tminfo->NEpix.col = right - FieldBinary(buffer, 29, 32);
	tminfo->NEpix.row = lines - top +1 - 0.5;
      }

      if (k == bottom) {
	/* Bottom Left corner */
	tminfo->SWpix.col = FieldBinary(buffer, 25, 28);
	tminfo->SWpix.row = lines - bottom +1 - 0.5;
	
	/* Bottom Right corner */
	tminfo->SEpix.col = right - FieldBinary(buffer, 29, 32);
	tminfo->SEpix.row = lines - bottom +1 - 0.5;
      }
      

      cell = cellbuf;
      pixel = buffer + 32 + left-1;
      count = landsat->window.cols;
      while(count--)
         *cell++ = *pixel++;
      G_put_map_row(fd, cellbuf);
   }
   EndProgress();

   G_close_cell(fd);
   fd=0;

   
   sprintf(title, "%s (band %d)", landsat->info.title, band);
   G_put_cell_title(bandname, title);

   I_get_histogram(bandname, G_mapset(), &histogram);
   I_grey_scale(&histogram, &colr);
   G_write_colors(bandname, G_mapset(), &colr);
   G_free_histogram(&histogram);
   G_free_colors(&colr);

   /* Write history file using fields from landsat->info */
   G_short_history(bandname, "imagery", &hist);
   strcpy(hist.datsrc_1, landsat->info.id[0]);
   strcpy(hist.datsrc_2, landsat->info.id[1]);
   sprintf(hist.edhist[0], "extracted window: rows %d-%d cols %d-%d",
      top, bottom, left, right);
   hist.edlinecnt = 1;
   for(k=0; k<5 && hist.edlinecnt < MAXEDLINES; ++k)
      if( landsat->info.desc[k][0])
      	 G_strncpy(hist.edhist[hist.edlinecnt++], landsat->info.desc[k],
      	    RECORD_LEN -1);
   G_write_history(bandname, &hist);


   I_get_group_ref(landsat->group, &ref);
   I_add_file_to_group_ref(bandname, G_mapset(), &ref);
   I_put_group_ref(landsat->group, &ref);
   I_free_group_ref(&ref);
   I_put_group(landsat->group);
   error = 0;

quit:
   if(fd) G_close_cell(fd);
   if(cellbuf) free(cellbuf);
   return(error);
}



/*======================================================================
                           ExamineQuadrant

File stream is positioned at start of the second file, the first
band's file descriptor.  The entire first file, the volume directory,
is pointed to by header.  This function serves two purposes:

If info==True:
   Fill in landsat->info with fields from header.  File sequence pointer is
   left unchanged.

If info==False:
   Print fields from header and first band.  File sequence is left inside
   the first image file.  This is meant for the -e examine option.

Return Value:
   0 if successful.  Either landsat info structure was filled in successfully
      or all info for examination were printed.
   1 on error.  Text record could not be found, or next file could not be
      found.
======================================================================*/
int 
ExamineQuadrant (
    Landsat *landsat,
    unsigned char *header,	/* Volume descriptor */
    int length,    	     	/* length of header. */
    int info,
    TMinfo *tminfo
)
{
   int numrecs;
   unsigned char filedesc[4321];
   unsigned char image[4321];
   unsigned char *p;
   FileSequence *fileseq = &landsat->fileseq;
   int quadrant;

   strcpy(tminfo->file_format, Field(header, 149, 154));
   tminfo->vol_creation_date.month = atoi(Field(header, 117, 118))-1;
   tminfo->vol_creation_date.day = atoi(Field(header, 119, 120));
   tminfo->vol_creation_date.year = atoi(Field(header, 113, 116));
   tminfo->vol_creation_time.hours = atoi(Field(header, 121, 122));
   tminfo->vol_creation_time.mins = atoi(Field(header, 123, 124));
   tminfo->vol_creation_time.secs = atoi(Field(header, 125, 126));
   tminfo->vol_creation_time.decimal = atoi(Field(header, 127, 128));

   /* Look through records in the volume directory until text record found. */
   numrecs = 1;
   p = header +360;
   while(p+360 <= header+length && RecordType(p) == FILE_POINTER &&
      ++numrecs <= 23)
      p += 360;
   if(numrecs > 23 || RecordType(p) != TEXT) {
      fprintf(stderr, "Volume directory has no volume text record.\n");
      return(1);
   }

   /* Determine if this is quadrant or full scene.  Volume text record
      seems to be different for each. */
   quadrant = strcmp(Field(p, 17, 18), "TM");
   if( strcmp(Field(p, 17, 18), "TM") == 0)
      tminfo->type = FullScene;
   else {
      tminfo->type = QuadrantScene;
      tminfo->quadrant = atoi(Field(p, 128, 130));
   }

   if( quadrant) {
      /* For quadrant scene, image date is taken from date/time field */
      tminfo->mission = atoi(Field(p, 34, 34));
      tminfo->path = atoi(Field(p, 36, 38));
      tminfo->row_number = atoi(Field(p, 39, 41));
      tminfo->correction = atoi(Field(p, 47, 48));
      strcpy(tminfo->product_id, Field(p, 29, 49));
      tminfo->image_date.year = 1900 + atoi(Field(p, 78, 79));
      MonthAndDay(tminfo->image_date.year, atoi(Field(p, 80, 82)),
      	 &tminfo->image_date.month, &tminfo->image_date.day);
      tminfo->image_time.hours = atoi(Field(p, 83, 84));
      tminfo->image_time.mins = atoi(Field(p, 85, 86));
      tminfo->image_time.secs = atoi(Field(p, 87, 88));
      tminfo->image_time.decimal = 0;
      strcpy(tminfo->scene_id, Field(p, 107, 117));
      strcpy(tminfo->interleave, Field(p, 150, 152));
      tminfo->generation_date.year = 1900 + atoi(Field(p, 274, 275));
      MonthAndDay(tminfo->generation_date.year, atoi(Field(p, 276, 278)),
      	 &tminfo->generation_date.month, &tminfo->generation_date.day);
   } else {
      /* Some full scenes are a little different from others, try to
      	 handle this. */
      if(strcmp(Field(header, 17, 18), "TM") == 0) {
      	 /* For full scene, image date is taken from product id */
	 tminfo->mission = atoi(Field(p, 22, 22));
	 tminfo->path = atoi(Field(p, 24, 26));
	 tminfo->row_number = atoi(Field(p, 27, 29));
	 tminfo->correction = atoi(Field(p, 35, 36));
	 tminfo->image_date.year = 1900 + atoi(Field(p, 30, 31));
	 MonthAndDay(tminfo->image_date.year, atoi(Field(p, 32, 34)),
	    &tminfo->image_date.month, &tminfo->image_date.day);
      } else if(strcmp(Field(header, 17, 23), "PRODUCT") == 0) {
      	 tminfo->mission = atoi(Field(p, 34, 34));
      }
   }

   if(info) return(0);


   /* Get next file, which must be the first band's leader */
   if(FileSeqNext(fileseq, -1))
      return(1);
   FileSeqRead(fileseq, filedesc+1, 4320); 
   /* Get scene header record. */
   FileSeqRead(fileseq, filedesc+1, 4320); 

   /* Get next file, which must the the first band's image */
   if(FileSeqNext(fileseq, -1))
      return(1);
   FileSeqRead(fileseq, image+1, 4320);

   /* Add borders to rows and columns */
   tminfo->rows = atoi(Field(image, 261, 264)) +
      atoi(Field(image, 237, 244)) + atoi(Field(image, 265, 268));
   tminfo->cols = atoi(Field(image, 245, 248)) +
      atoi(Field(image, 249, 256)) + atoi(Field(image, 257, 260));

   if( quadrant) {
      tminfo->full_scene_geo.lat = atof(Field(filedesc, 53, 68));
      tminfo->full_scene_geo.lon = atof(Field(filedesc, 69, 84));
      tminfo->full_scene_pixel.row = atoi(Field(filedesc, 85, 100));
      tminfo->full_scene_pixel.col = atoi(Field(filedesc, 101, 116));
      tminfo->quad_scene_geo.lat = atof(Field(filedesc, 213, 228));
      tminfo->quad_scene_geo.lon = atof(Field(filedesc, 229, 244));
      tminfo->quad_scene_pixel.row = atoi(Field(filedesc, 245, 260));
      tminfo->quad_scene_pixel.col = atoi(Field(filedesc, 261, 276));
   } else {
      tminfo->full_scene_geo.lat = atof(Field(filedesc, 213, 228));
      tminfo->full_scene_geo.lon = atof(Field(filedesc, 229, 244));
      tminfo->full_scene_pixel.row = atoi(Field(filedesc, 245, 260));
      tminfo->full_scene_pixel.col = atoi(Field(filedesc, 261, 276));
   }
   return(0);
}



/*======================================================================
			     MonthAndDay

Given a year (full 4 digits) and julian day (1-366), return the
month (1-12) and day (1-31).
======================================================================*/
void 
MonthAndDay (int year, int julian, int *month, int *day)
{
   static int lengths[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
   int m;

   if( year % 4 == 0 && year % 400 != 0)
      lengths[1] = 29;
   else
      lengths[1] = 28;

   m=0;
   while(julian)
      if(julian > lengths[m])
      	 julian -= lengths[m++];
      else
      	 break;
   *month = m;
   *day = julian;
}


