/* r.out.pov v1.0 beta 
   convert grass raster to tga height-field for POVray.
   
   Persistence of Vision (POV) raytracer can use a height-field
   defined in a Targa (.TGA) image file format where the RGB pixel
   values are 24 bits (3 bytes). A 16 bit unsigned integer height-field
   value is assigned as follows:
     RED    high byte
     GREEN  low byte
     BLUE   empty

   This code is based on code by W.D. Kirby who wrote DEM2XYZ in 1996.
   
   Author: Klaus Meyer, GEUM.tec GbR, eMail: GEUM.tec@geum.de
   Date: July 1998
*/

#include "gis.h"
#include <stdio.h>

void writeHeader(FILE *outf);
void processProfiles(int inputFile, FILE *outputF);

#define     YMAX     65536          /*  max length scan line */
#define     XMAX     65536          /*  max # of scan lines */

#define MIN(x,y)     (((x) < (y)) ? (x) : (y))
#define MAX(x,y)     (((x) > (y)) ? (x) : (y))

#ifndef M_PI
#define M_PI        3.14159265358979323846
#endif

#define SW     0
#define NW     1
#define NE     2
#define SE     3


int             base[XMAX];           /* array of base elevations */

double          defaultElev = 0;      /* elevation for empty points */
int             width, height;        /* height-field image width and height */
int             hfType = 0;           /* height-field type */
double          hfBias, hfNorm = 1.0;

double          verticalScale=1.0;    /* to stretch or shrink elevations */


double          minValue=50000., maxValue=-50000.;

char            mapLabel[145];
int             DEMlevel, elevationPattern, groundSystem, groundZone;
double          projectParams[15];
int             planeUnitOfMeasure, elevUnitOfMeasure, polygonSizes;
double          groundCoords[4][2], elevBounds[2], localRotation;
int             accuracyCode;
double          spatialResolution[3];
int             profileDimension[2];
int             firstRow, lastRow;
int             wcount = 0, hcount;


double          deltaY;
char            inText[24], **junk;

double          eastMost, westMost, southMost, northMost;
int             eastMostSample,  westMostSample, southMostSample, northMostSample;
int             rowCount, columnCount, r, c;
long int        cellCount = 0 ;
int             rowStr,rowEnd, colStr, colEnd;



int main(int argc, char *argv[]) 
{
    struct Cell_head region; 
    struct Range range;
    CELL range_min, range_max;
    FILE *outf, *fp;
    char *outfilename;

    CELL *cell;
    char *name; 
    char *mapset;
    int fd;
    int row,col;
    int nrows, ncols;
    int number;
    double bias;
    char fmt[20];
	struct GModule *module;
    struct
    {
	struct Option *map ;
        struct Option *tga ;
	struct Option *hftype ;
        struct Option *bias ;
        struct Option *scaleFactor ;
    } parm;

	module = G_define_module();
    module->description =
		"Converts a raster map layer into a height-field file for POVRAY.";

/* Define the different options */

    parm.map = G_define_option() ;
    parm.map->key        = "map";
    parm.map->type       = TYPE_STRING;
    parm.map->required   = YES;
    parm.map->gisprompt  = "old,cell,raster" ;
    parm.map->description= "Name of an existing raster map" ;

    parm.tga = G_define_option() ;
    parm.tga->key        = "tga";
    parm.tga->type       = TYPE_STRING;
    parm.tga->required   = YES;
    parm.tga->description= "Name of output povray file (TGA height field file)" ;

    parm.hftype = G_define_option() ;
    parm.hftype->key        = "hftype";
    parm.hftype->type       = TYPE_INTEGER;
    parm.hftype->required   = NO;
    parm.hftype->description= "Height-field type (0=actual heights 1=normalized)";

    parm.bias = G_define_option() ;
    parm.bias->key        = "bias";
    parm.bias->type       = TYPE_DOUBLE;
    parm.bias->required   = NO;
    parm.bias->description= "Elevation bias";

    parm.scaleFactor = G_define_option() ;
    parm.scaleFactor->key        = "scale";
    parm.scaleFactor->type       = TYPE_DOUBLE;
    parm.scaleFactor->required   = NO;
    parm.scaleFactor->description= "Vertical scaling factor";


    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
       	exit (-1);

    strcpy (fmt, "%ld ");
    if (parm.hftype->answer != NULL)
    {
        sscanf (parm.hftype->answer, "%ld", &hfType);
        if (hfType > 0)
            sprintf (fmt, "%%%dld ", hfType);
	else
	    sprintf (fmt, "%%%dld", hfType);
    }

    strcpy(fmt, "%lf ");
    if (parm.bias->answer != NULL)
    {
        sscanf (parm.bias->answer, "%lf", &bias);
        if (bias > 0)
            sprintf (fmt, "%%%dlf ", bias);
	else
	    sprintf (fmt, "%%%dlf", bias);
    }
	
    strcpy(fmt, "%lf ");
    if (parm.scaleFactor->answer != NULL)
    {
        sscanf (parm.scaleFactor->answer, "%lf", &verticalScale);
        if (verticalScale > 0)
            sprintf (fmt, "%%%dlf ", verticalScale);
	else
	    sprintf (fmt, "%%%dlf", verticalScale);
    }

    name = parm.map->answer;
    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
        char msg[100];	
		
	sprintf (msg, "%s: <%s> cellfile not found\n", G_program_name(), name);
		G_fatal_error (msg);
        exit(1);
    }

    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    	exit(1);


    outfilename = parm.tga->answer;
    if (outfilename == NULL)
    {
        char msg[100];	
		
	sprintf (msg, "%s: <%s> invalid outputfilename\n", G_program_name(), outfilename);
		G_fatal_error (msg);
        exit(1);
    }
   
    if (NULL == (outf = fopen (outfilename, "wb")))
    {
        char msg[100];	
		
	sprintf (msg, "%s: <%s> cannot open outputfile\n", G_program_name(), outfilename);
		G_fatal_error (msg);
        exit(1);
    }  

    cell = G_allocate_cell_buf();

    nrows = G_window_rows();
    ncols = G_window_cols();
    if (nrows > YMAX || ncols > XMAX)
    {
        char msg[100];	
		
	sprintf (msg, "%s: cellfile exceeds %ld columns or %ld rows\n", 
          G_program_name(), outfilename, XMAX, YMAX);
		G_fatal_error (msg);
        exit(1);
    }

    columnCount = ncols;
    rowCount = nrows;
    width = ncols;
    height = nrows;

    /* SW, NW, NE, SE corners */
    G_get_window(&region);

    eastMost = region.east;
    westMost = region.west;
    northMost = region.north;
    southMost = region.south;

    G_init_range(&range);
    G_read_range(name, mapset, &range);
    G_get_range_min_max (&range, &range_min, &range_max);
    if (range.min < 0 || range.max < 0) 
    {
      fprintf(stderr, "Warning negative elevation values in input\n");
    }
    elevBounds[0] = range.min;
    elevBounds[1] = range.max; 

    /* Normalize using max value of unsigned short integer (16 bit) */ 
    if (hfType == 1) hfNorm = 65535.0 / (elevBounds[1]+hfBias);

    spatialResolution[0] = region.ew_res;
    spatialResolution[1] = region.ew_res;
    spatialResolution[2] = region.ns_res;

    /* write TGA image file header */
    (void) writeHeader(outf);

    /* have to read everything */
    (void) processProfiles(fd,outf);

    fclose (outf);
    G_close_cell(fd);

    exit(0);
}




void writeHeader(outputF)
FILE *outputF;
{
   int i;

   /* Write TGA image header */
   for (i = 0; i < 10; i++)      /* 00, 00, 02, then 7 00's... */
      if (i == 2)
	 putc((short)i, outputF);
      else
	 putc(0, outputF);

   putc(0, outputF); /* y origin set to "First_Line" */
   putc(0, outputF);

   putc((short)(width % 256), outputF);  /* write width and height */
   putc((short)(width / 256), outputF);
   putc((short)(height % 256), outputF);
   putc((short)(height / 256), outputF);
   putc(24, outputF);  /* 24 bits/pixel (16 million colors!) */
   putc(32, outputF);  /* Bitmask, pertinent bit: top-down raster */
}

/*
 * read profiles
 */

void processProfiles(inputFile,outputF)
int             inputFile;
FILE           *outputF;
{
   CELL *cell;
   int             c, r, mod ;
   int             count, tempInt, lastProfile = 0;

   int             profileID[2], profileSize[2];
   double          planCoords[2], tempFloat;


   cell = G_allocate_cell_buf();
   for (r = 0; r < rowCount; r++)
   {
     if (G_get_map_row (inputFile, cell, r) < 0)
	exit(1); 
        /* break; */
     for (c = 0; c < columnCount; c++)
     {   
         G_percent (r, rowCount, 2);
         tempFloat = ((float) cell[c] * verticalScale) + hfBias;

         /* Normalize */
         tempFloat *= hfNorm;
         if(tempFloat < 0.0) tempFloat = 0.0;
         if(tempFloat > 65535.0) tempFloat = 65535.0;
   
         if(tempFloat>maxValue) maxValue = tempFloat;
         if(tempFloat<minValue) minValue = tempFloat;
   
         /* write pixel */
         putc((char)0, outputF);                       /* Blue  empty     */
         putc((char)((int) tempFloat % 256), outputF); /* Green low byte  */
         putc((char)((int) tempFloat / 256), outputF); /* Red   high byte */
     } 
    }

}
