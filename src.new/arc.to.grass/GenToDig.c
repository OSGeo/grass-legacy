
/* GenToDig.c
 *
 * functions defined:
 *
 * GenToDigArea(ascii,binary,neatline)
 * FILE *ascii;     - pointer to gen file to be read
 * FILE *binary;    - pointer to dig file to be written 
 * int  neatline;   - true if neatline is desired
 *
 * GenToDigLine(ascii,binary,neatline)
 * FILE *ascii;     - pointer to gen file to be read
 * FILE *binary;    - pointer to dig file to be written 
 * int  neatline;   - true if neatline is desired
 *
 * GenToDig(type,ascii,binary,neatline)
 * char type;	    - either AREA or LINE
 * FILE *ascii;     - pointer to gen file to be read
 * FILE *binary;    - pointer to dig file to be written 
 * int  neatline;   - true if neatline is desired
 * 
 * PURPOSE:
 * 
 * NOTES: 
 *
 * 1) the only values set in the dig file header are the N,S,E,
 * and W boundary coordinates.  These boundary coordinates are 
 * determined by finding the max and min coordiate values in the
 * gen file.
 *
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#include "digit.h"
#include "values.h"
#include "dig_head.h"

/**********************************************************************/
GenToDigArea(ascii,binary,neatline)
FILE *binary, *ascii ;
int neatline;
{
#ifdef DEBUG 
printf("GenToDigArea\n");
#endif
GenToDig((char)AREA,ascii,binary,neatline);
}

/**********************************************************************/
GenToDigLine(ascii,binary,neatline)
FILE *binary, *ascii ;
int neatline;
{
#ifdef DEBUG 
printf("GenToDigLine\n");
#endif
GenToDig((char)LINE,ascii,binary,neatline);
}

/**********************************************************************/
GenToDig(type, ascii, binary, neatline)
char type; 
FILE *binary, *ascii;
int  neatline;
{
char   inbuf[1024];
char   tmpbuf[1024];
int    id;
int    done=0;
int    almost_done=0;
int    itmp;
int    vertex_count=0;
int    vertex_max=-MAXINT;
int    n_points=0;
int    n_dig_lines=0;
double xtmp, ytmp;
double xmin, 
       xmax;
double ymin, 
       ymax;
int    first=1;
double *xarray,
       *yarray,
       *x, *y;

/* read through the lines file to find max/min coordinate values
 * and max number of vertices in a line
 */
do {
   fgets(inbuf,1024,ascii);
   if (sscanf(inbuf,"%lf %lf",&xtmp,&ytmp)==2)
      {
      if (first)
         {
         xmax=xmin=xtmp;
         ymax=ymin=ytmp;
         first=0;
         }
      if (xtmp > xmax) xmax=xtmp;
      if (xtmp < xmin) xmin=xtmp;
      if (ytmp > ymax) ymax=ytmp;
      if (ytmp < ymin) ymin=ytmp;
      vertex_count++;
      almost_done = 0;
      }
   else if (sscanf(inbuf,"%d",&itmp)==1)
      {
      if (vertex_count > vertex_max)
         vertex_max = vertex_count;
      vertex_count = 0;
      }
   else if (strcmp(inbuf,"END") && almost_done)
      done = 1;
   else if (strcmp(inbuf,"END"))
      almost_done = 1;
   }
while (!done);
rewind(ascii);

/* build a dig header from the min and max information */
head.orig_scale = 1;
head.W = xmin;
head.E = xmax;
head.S = ymin;
head.N = ymax;
 
/* write the dig header to the binary file */
dig_write_head_binary(binary, &head);

xarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;
yarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;

if (neatline)
   {
   x=xarray; y=yarray;
   n_points = 5;
   *x++ = xmin; 
   *y++ = ymin; 
   *x++ = xmin; 
   *y++ = ymax; 
   *x++ = xmax; 
   *y++ = ymax; 
   *x++ = xmax; 
   *x++ = xmin; 
   *y++ = ymin; 
   dig_Write_line(binary,(char)type,xarray,yarray,n_points);
   }

done = 0;
do {
   /* read until next line id (or and END marker) is found */
   do {
      fgets(inbuf,1024,ascii);
      sscanf(inbuf,"%s",tmpbuf);
      if (strcmp(tmpbuf,"END")==0)
         done = 1;
      }
   while (sscanf(inbuf,"%d",&id)!=1 && !done); 
   
   if (!done)
      {
      /* read line's points until an END marker is found */
      x = xarray;
      y = yarray;
      almost_done = 0;
      n_points = 0;
      do {
         fgets(inbuf,1024,ascii);
         sscanf(inbuf,"%s",tmpbuf);
         if (strcmp(tmpbuf,"END")==0)
            almost_done=1;
         else if (sscanf(inbuf,"%lf %lf",&xtmp,&ytmp)==2)
            {
            *x++ = xtmp;
            *y++ = ytmp;
            n_points++;
            }
         }
      while (!almost_done);
   
      /* write line to binary dig file */ 
      if (n_points > 0)
         {
         n_dig_lines++;
         dig_Write_line(binary,(char)type,xarray,yarray,n_points);
         }
      }
   }
while (!done);

if (n_dig_lines > 0)
    return(0);             /* normal exit */
else
    return(-1);            /* error - no lines written to dig file */
}
