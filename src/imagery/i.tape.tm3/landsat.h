/*======================================================================
Filename:   landsat.h
Module:	    i.landsat.tm
Author:	    Christopher Lesher
======================================================================*/

#ifndef _landsat_h
#define _landsat_h

#include "gis.h"

#ifndef  Ref_Files
#include "imagery.h"
#endif

#include "fileseq.h"

/* long atoi(); Please don't ....    WBH 02/05/1999 */

#define TM_BANDS 7

#define VOLUME_DESCRIPTOR			1
#define FILE_POINTER				2
#define TEXT					3
#define FILE_DESCRIPTOR				4
#define SCENE_HEADER				5
#define MAP_PROJECTION_ANCILLARY		6
#define RADIOMETRIC_CALIBRATION_ANCILLARY	7
#define IMAGE_DATA				8
#define TRAILER					9
#define SCENE_DEFINITION			10
#define UNPROCESSED_SCD				11
#define CONTROL_POINT_DATA			12
#define GEOMETRIC_MODELING_DATA			13
#define HIGH_FREQUENCY_MATRICES			14
#define ANNOTATION				15
#define BAND_QUALITY_DATA			16
#define NULL_VOLUME_DESCRIPTOR			17

#define DEFAULT_LANDSAT_TITLE 	 "Landsat TM Imagery"

typedef struct {
   char *infile;  /* File to read landsat from, regular or special */
   char *group;	  /* Group to create for these landsat bands. */
   char *outfile; /* Prefix for raster maps created.  Band number is appended*/
   int	 bands[TM_BANDS];  /* Band numbers in increasing order to import.
      	       	     	      For fast format, band 8 is panchromatic */
   long	 bands_to_do;	      	 /* Bit mask of bands to import. */
   int	 numbands;   	      	 /* Number of bands in bands[] */
   int	 top, bottom, left, right;  /* Window within bands to import */
   int	 blocksize;
   boolean examine;  	      	 /* True if -e flag spccified. */
   boolean percent;  	      	 /* True if -p flag specified. */
   boolean quiet;    	      	 /* True if -q flag specified. */
   boolean fileprompt;	      	 /* True if we must prompt for filenames. */
   FileSequence fileseq;
   struct Tape_Info info;
   struct Cell_head window;
} Landsat;

typedef struct {
   int	 month, day, year; /* Month is 0-11, Year is > 1900 */
} TMdate;

typedef struct {
   int	 hours, mins, secs, decimal;
} TMtime;

typedef struct {
   double   lat, lon;
} TMgeo;

typedef struct {
   double   east, north;
} TMutm;

typedef struct {
   int	    row, col;
} TMpixel;

typedef enum { QuadrantScene, FullScene, FastA, FastB } TMFileFormat;

typedef struct {
   TMFileFormat type;
   int	       quadrant;      	       	     /* 1-4 for quadrant scenes. */
   char	       file_format[15];
   int	       mission;
   int	       path;
   int	       row_number;
   int	       correction;
   TMdate      vol_creation_date;
   TMtime      vol_creation_time;
   char	       product_id[22];
   TMdate      image_date;
   TMtime      image_time;
   char	       scene_id[22];
   char	       interleave[4]; 	       	     /* Always "BSQ" */
   TMdate      generation_date;
   int	       rows, cols;    	       	     /* size of bands in pixels. */
   TMgeo       full_scene_geo;	       	     /* quadrant, full, fast B */
   TMutm       full_scene_utm;	       	     /* fast B, fast A maybe */
   TMpixel     full_scene_pixel;       	     /* quadrant, full, fast B */
   TMgeo       quad_scene_geo;	       	     /* quadrant */
   TMpixel     quad_scene_pixel;       	     /* quadrant */
   TMgeo       NWgeo, NEgeo, SWgeo, SEgeo;   /* fast A,B */
   TMutm       NWutm, NEutm, SWutm, SEutm;   /* fast A,B */
   TMpixel     NWpix, NEpix, SWpix, SEpix;   /*  */
} TMinfo;

extern int errno;
 
#ifdef _NO_PROTO
int   Ask();
int   Continue();
void  EndProgress();
void  EnterFilename();
void  Error();
int   ExamineQuadrant();
char* Field();
void  FoundBand();
int   ImportFast();
long  ImportQuadrant();
void  MonthAndDay();
int   MountTape();
void  PrintDate();
void  PrintProgress();
void  PrintTMinfo();
int   RecordType();
char* SPrintDate();
char* SPrintGeo();
char* SPrintPixel();
char* SPrintTime();
char* SPrintUTM();
void  StartProgress();
#else
int   Ask(char *prompt);
int   Continue();
void  EndProgress();
void  EnterFilename(FileSequence*);
void  Error(char *);
int   ExamineQuadrant(Landsat*, unsigned char *, int, int, TMinfo*);
char* Field(char *buffer, int start, int end);
void  FoundBand(int band);
int   ImportFast(Landsat*, unsigned char *header, int length);
long  ImportQuadrant(Landsat*, unsigned char *header, int length);
void  MonthAndDay(int year, int julian_day, int *month, int *day);
int   MountTape(boolean first);
void  PrintDate(char *date);
void  PrintProgress(Landsat*, int line, int totallines);
void  PrintTMinfo(TMinfo*);
int   RecordType(unsigned char *buffer);
char* SPrintDate(TMdate);
char* SPrintGeo(TMgeo);
char* SPrintPixel(TMpixel);
char* SPrintTime(TMtime);
char* SPrintUTM(TMutm);
void  StartProgress(Landsat*, int band);
#endif

#endif

