/*======================================================================
Filename:   fast.c
Module:	    i.landsat.tm
Author:	    Christopher Lesher
  Mods based on Bernard's stuff  6/17/99 
Routines for importing and examining fast format TM imagery.
======================================================================*/

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "landsat.h"

#ifdef _NO_PROTO
static int ExamineFast();
static int ExamineFast_A();
static int ExamineFast_B();
static void ParseGeo();
static void ParseUTM();
static int ImportFastBand();
#else
static int ExamineFast(Landsat*, unsigned char*, int, boolean, TMinfo*);
static int ExamineFast_A(Landsat*, unsigned char*, int, boolean, TMinfo*);
static int ExamineFast_B(Landsat*, unsigned char*, int, boolean, TMinfo*);
static void ParseGeo(TMgeo*, unsigned char*, int, int);
static void ParseUTM(TMutm*, unsigned char*, int, int);
static int ImportFastBand(Landsat*, int band, TMinfo*);
#endif

/*======================================================================
			      ImportFast
======================================================================*/
int ImportFast (Landsat *landsat, unsigned char *header, int length)
{
   TMinfo tminfo;
   FileSequence *fileseq = &landsat->fileseq;
   long bands_done;
   int k;

   if(ExamineFast(landsat, header, length, ! landsat->examine, &tminfo))
      return(-1);
   if(landsat->examine) {
      PrintTMinfo(&tminfo);
      return(-1);
   }

   /* Fill in the Tape_info structure. */
   sprintf(landsat->info.id[0], "Thematic Mapper. Scene ID %s",
      tminfo.scene_id);
   *landsat->info.id[1] = '\0';
   sprintf(landsat->info.desc[1], "Mission %d", tminfo.mission);
   sprintf(landsat->info.desc[2], "Path %d Row %d  Fast Format",
      tminfo.path, tminfo.row_number);
   sprintf(landsat->info.desc[3], "Date:  %s", SPrintDate(tminfo.image_date));
   *landsat->info.desc[4] = '\0';


   bands_done = 0;
   if( landsat->fileseq.type == REG_FILE) {
      for(k=0; k<landsat->numbands; ++k) {
      	 if(FileSeqNext(fileseq, landsat->bands[k]))
      	    goto quit;
      	 if(ImportFastBand(landsat, landsat->bands[k], &tminfo))
      	    goto quit;
      	 landsat->bands_to_do ^= (1 << landsat->bands[k]);
      	 bands_done |= (1 << landsat->bands[k]);
      }
   } else { /* type == SPECIAL_FILE */
      int num_present;	   /* Num bands on this volume. */
      int bands_present[7]; /* Band numbers on this volume. */

      while (True) {
	 for(num_present=0; num_present<7; ++num_present) {
      	    int b = 1361+num_present;
      	    if( header[b] == 'P')
      	       bands_present[num_present] = 8;
      	    else if( header[b] != ' ')
      	       bands_present[num_present] = atoi(Field(header, b, b));
      	    else break;
      	 }
	 for(k=0; k<num_present && landsat->bands_to_do; ++k) {
	    if(FileSeqNext(fileseq, -1))
      	       goto quit;
	    if( landsat->bands_to_do & (1 << bands_present[k])) {
      	       if(ImportFastBand(landsat, bands_present[k], &tminfo))
      	       	  goto quit;
      	       landsat->bands_to_do ^= (1 << bands_present[k]);
      	       bands_done |= (1 << bands_present[k]);
      	    } else if(! landsat->quiet) {
      	       if( bands_present[k] == 8)
      	       	  fprintf(stderr, "Looking for band P\n");
      	       else
      	       	  fprintf(stderr, "Looking for band %d\n", bands_present[k]);
      	    }
	 }
      	 if( ! landsat->bands_to_do) break;
      	 /* After reading bands, next file must be a volume directory.
      	    FileSeqRead() may force user to mount next tape. */
      	 if(FileSeqNext(fileseq, -1))
      	    goto quit;
      	 if(FileSeqRead(fileseq, header+1, length) <= 0)
      	    goto quit;
      	 /* Need to re-examine header.  Header info will be different for
      	    spectral bands and panchromatic bands from the same volume set. */
      	 ExamineFast(landsat, header, length, ! landsat->examine, &tminfo);
      } 
   }

quit:
   /* If nothing was imported then dont change window. */
   if( bands_done)
      G_put_window(&landsat->window);
   return(bands_done);
}



/*======================================================================
			    ImportFastBand

Return Value:
   0 if successful
   1 if error.
======================================================================*/
static int 
ImportFastBand (Landsat *landsat, int band, TMinfo *tminfo)
{
   int top, bottom, left, right;
   unsigned char *buffer = NULL, *pixel;
   CELL *cellbuf = NULL, *cell;
   struct Cell_head  window;
   struct Histogram  histogram;
   struct History    hist;
   struct Colors     colr;
   struct Ref  	     ref;
   int	       	     recordsize;
   int	       	     fd=0;
   int	       	     k, count;
   char	       	     bandname[500];
   FileSequence	     *fileseq = &landsat->fileseq;
   char	       	     title[100];
   int	       	     error=1;

   buffer = (unsigned char *)malloc(tminfo->cols);
   if(buffer == NULL) return(1);

   recordsize = tminfo->cols;
   top = landsat->top ? landsat->top : 1;
   bottom = landsat->bottom ? landsat->bottom : tminfo->rows;
   left = landsat->left ? landsat->left : 1;
   right = landsat->right ? landsat->right : tminfo->cols;

   if(top<1) top=1;
   if(bottom>tminfo->rows) bottom=tminfo->rows;
   if(left<1) left=1;
   if(right>tminfo->cols) right=tminfo->cols;

   landsat->window.south = tminfo->rows - bottom +1- 0.5;
   landsat->window.north = tminfo->rows - top +1 + 0.5;
   landsat->window.west = left - 0.5;
   landsat->window.east = right + 0.5;
   landsat->window.cols = right - left +1;
   landsat->window.rows = bottom-top+1;
   landsat->window.ns_res = 1;
   landsat->window.ew_res = 1;
   landsat->window.proj = 0;
   landsat->window.zone = 0;

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
      if( result <= 0) {
      	 EndProgress();
      	 if(result == -1)
      	    fprintf(stderr, "Error while reading tape.\n");
      	 else if(result == 0)
      	    fprintf(stderr, "End of file reached on tape.\n");
      	 fprintf(stderr, "%d of %d lines for band %d written.\n", k-top,
      	    bottom-top+1, band);
      	 break;
      }
      cell = cellbuf;
      pixel = buffer +left-1;
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
			     ExamineFast
======================================================================*/
static int ExamineFast (
    Landsat *landsat,
    unsigned char *header,	/* Volume descriptor */
    int length,    	     	/* length of header. */
    boolean info,
    TMinfo *tminfo
)
{
   if(strncmp(header+1, "SCENE ID", 8) == 0)
      return(ExamineFast_A(landsat, header, length, info, tminfo));
   else if(strncmp(header+1, "PRODUCT", 7) == 0)
      return(ExamineFast_B(landsat, header, length, info, tminfo));
   else {
      fprintf(stderr, "Unknown fast format file type.\n");
      return(1);
   }
}


/*======================================================================
			    ExamineFast_A
======================================================================*/
static int ExamineFast_A (
    Landsat *landsat,
    unsigned char *header,	/* Volume descriptor */
    int length,    	     	/* length of header. */
    boolean info,
    TMinfo *tminfo
)
{
   tminfo->type = FastA;
   ParseGeo(&tminfo->NWgeo, header, 1117, 1131);
   ParseGeo(&tminfo->NEgeo, header, 1175, 1189);
   ParseGeo(&tminfo->SEgeo, header, 1233, 1247);
   ParseGeo(&tminfo->SWgeo, header, 1291, 1305);
   ParseUTM(&tminfo->NWutm, header, 1144, 1158);
   ParseUTM(&tminfo->NEutm, header, 1202, 1216);
   ParseUTM(&tminfo->SEutm, header, 1260, 1274);
   ParseUTM(&tminfo->SWutm, header, 1318, 1332);

   /* Version A header may or may not have scene center utm at end of file.
      Documentation doesn't show it but some files have it.  Corresponding
      pixel coordinates of center are not given, however. */
   if(strcmp(Field(header, 1447, 1458), "SCENE CENTER") == 0)
      ParseUTM(&tminfo->full_scene_utm, header, 1461, 1475);
   else {
      tminfo->full_scene_utm.east = -1;
      tminfo->full_scene_utm.north = -1;
   }

   tminfo->path = atoi(Field(header, 27, 29));
   tminfo->row_number = atoi(Field(header, 31, 33));
   tminfo->mission = atoi(Field(header, 76, 76));
   tminfo->rows = atoi(Field(header, 1109, 1112));
   tminfo->cols = atoi(Field(header, 1087, 1090));
   strcpy(tminfo->scene_id, Field(header, 11, 20));
   tminfo->image_date.year = atoi(Field(header, 55, 58));
   tminfo->image_date.month = atoi(Field(header, 59, 60)) -1;
   tminfo->image_date.day = atoi(Field(header, 61, 62));

    return 0;
}



/*======================================================================
			    ExamineFast_B
======================================================================*/
static int ExamineFast_B (
    Landsat *landsat,
    unsigned char *header,	/* Volume descriptor */
    int length,    	     	/* length of header. */
    boolean info,
    TMinfo *tminfo
)
{
   tminfo->type = FastB;
   ParseGeo(&tminfo->NWgeo, header, 1117, 1131);
   ParseGeo(&tminfo->NEgeo, header, 1175, 1189);
   ParseGeo(&tminfo->SEgeo, header, 1233, 1247);
   ParseGeo(&tminfo->SWgeo, header, 1291, 1305);
   ParseGeo(&tminfo->full_scene_geo, header, 1454, 1466);
   ParseUTM(&tminfo->NWutm, header, 1144, 1158);
   ParseUTM(&tminfo->NEutm, header, 1202, 1216);
   ParseUTM(&tminfo->SEutm, header, 1260, 1274);
   ParseUTM(&tminfo->SWutm, header, 1318, 1332);
   ParseUTM(&tminfo->full_scene_utm, header, 1481, 1495);
   tminfo->full_scene_pixel.row = atoi(Field(header, 1514, 1519));
   tminfo->full_scene_pixel.col = atoi(Field(header, 1508, 1513));

   tminfo->path = atoi(Field(header, 27, 29));
   tminfo->row_number = atoi(Field(header, 31, 33));
   tminfo->mission = atoi(Field(header, 76, 76));
   tminfo->rows = atoi(Field(header, 1108, 1112));
   tminfo->cols = atoi(Field(header, 1086, 1090));
   strcpy(tminfo->product_id, Field(header, 10, 20));
   tminfo->image_date.year = atoi(Field(header, 55, 58));
   tminfo->image_date.month = atoi(Field(header, 59, 60)) -1;
   tminfo->image_date.day = atoi(Field(header, 61, 62));
   return(0);
}


static void ParseGeo (TMgeo *geo, unsigned char *header, int lon, int lat)
{
   double degrees;

   degrees = (double)atoi(Field(header, lon, lon+2));
   degrees += (double)atoi(Field(header, lon+3, lon+4)) / 60.0;
   degrees += (double)atoi(Field(header, lon+5, lon+11)) / 3600.0;
   if( header[lon+12] == 'W') degrees = - degrees;
   geo->lon = degrees;
   degrees = (double)atoi(Field(header, lat, lat+1));
   degrees += (double)atoi(Field(header, lat+2, lat+3)) / 60.0;
   degrees += (double)atoi(Field(header, lat+4, lat+10)) / 3600.0;
   if( header[lat+11] == 'S') degrees = - degrees;
   geo->lat = degrees;
}

static void ParseUTM (TMutm *utm, unsigned char *header, int east, int north)
{
   utm->east = atof(Field(header, east, east+12));
   utm->north = atof(Field(header, north, north+12));
}
