
/* GenToLabelledDigLines.c 
 *
 * function defined:
 *
 * GenToLabelledDigLines(lin_file,txt_file,dig_file,atts_file,cats_filename) 
 *
 * NOTES: 
 *
 * 1) the only values set in the dig file header are the N,S,E,
 * and W boundary coordinates.  These boundary coordinates are 
 * determined by finding the max and min coordiate values in the
 * gen file.
 * 
 * 2) Only lines will be written to the dig file, no area edges. 
 *
 *
 * Dave Johnson 
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#include "digit.h"
#include "gis.h"
#include "dig_head.h"

GenToLabelledDigLines(lin_file,txt_file,dig_file,atts_file,cats_filename) 
FILE *lin_file,
     *txt_file,
     *dig_file,
     *atts_file;
char *cats_filename;
{
double *xarray, *yarray, *x, *y;
int   id;
int   done=0;
int   almost_done=0;
double xtmp, ytmp;
int    first=1;
double xmin,xmax;
double ymin,ymax;
int   itmp;
struct Categories new_cats;
int   vertex_count=0;
int   vertex_max=0;
char  inbuf[1024];
int   n_points=0;
int   n_dig_lines=0;
int   type = LINE;
char  tmpbuf[1024];
int   CatStat;
int   CatNum;
int   NumCats;
int   NumCols,
      NumLines,
      IDCol,
      AttCol,
      CatCol;
char  AttText[512];
char  txtbuf[512];

#ifdef DEBUG
printf("GenToLabelledDigLines\n");
#endif

AttText[0]=0;
 
/* count the columns in the text_file */
rewind(txt_file);
fgets(txtbuf,512,txt_file);
NumCols = CountColumns(txtbuf);
NumLines = CountLines(txt_file);
 
/* determine which column in the txt_file is the line ID
 * number.  this is done by looking for the second column in the
 * file that has a 1 on the 1st line, 2 on the 2nd line, 3 on the
 * 3rd line, and so on...
rewind(txt_file);
if ((IDCol=FindIDColumn(txt_file)) < 0)
   {
   printf("\n\nThe LABEL-TEXT file has been scanned.\n");
   printf("There is not enough information in the file to assign\n");
   printf("to create GRASS attribute and category files\n");
   return(-1);
   }
*/

/* tell the user how many columns were found in the txt_file */
printf("\n\nThe LABEL-TEXT file has been scanned. There a are %d\n",NumLines);
printf("lines in the file and %d columns in the file\n",NumCols);
 
if (NumCols == 2)
   {
   CatCol = 2;
   AttCol = -1;
   printf("\nBecause there are only 2 columns, column 2 is assumned to be\n");
   printf("the category number column\n");
   }
else if (NumCols < 2)
   {
   G_fatal_error("Too few columns in label-text file");
   exit(-1);
   }
else
   {
   /* ask the user which column to use for GRASS category values,
    * this column must contain integers only.
    */
   done=0;
   do {
      printf("\nEnter the number of the column that should be used as\n");
      printf("for line IDs: ");
      gets(txtbuf);
      IDCol = atoi(txtbuf);
      if (IDCol<1 || IDCol>NumCols)
         printf("That is not a valid column number, please try again\n");
      else
         done=1;
      }
   while (!done);
     
   /* ask the user which column to use for GRASS category values */
   done=0;
   do {
      printf("\nEnter the number of the column that should be used as\n");
      printf("for GRASS category values: ");
      gets(txtbuf);
      CatCol = atoi(txtbuf);
      if (CatCol<1 || CatCol>NumCols)
         printf("That is not a valid column number, please try again\n");
      else
         done=1;
      }
   while (!done);

   /* ask the user which column to use for GRASS attribute text */
   done=0;
   do {
      printf("\nEnter the number of the column that should be used as\n");
      printf("for GRASS attribute text: ");
      gets(txtbuf);
      AttCol = atoi(txtbuf);
      if (AttCol<1 || AttCol>NumCols)
         printf("That is not a valid column number, please try again\n");
      else
         done=1;
      }
   while (!done);
   }  
 
G_init_cats(0,"",&new_cats);

printf("\nWorking...\n");

/* read through the lines file to find max/min coordinate values
 * and max number of vertices in a line
 */
rewind(lin_file);
first=1;
done=0;
do {
   fgets(inbuf,1024,lin_file);
   if (sscanf(inbuf,"%lf %lf",&xtmp,&ytmp)==2)
      {
      if (first==1)
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
rewind(lin_file);

/* build a dig header from the min and max information */
head.orig_scale = 1;
head.W = xmin;
head.E = xmax;
head.S = ymin;
head.N = ymax;
 
/* write the dig header to the dig_file */
dig_write_head_binary(dig_file, &head);

xarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;
yarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;

done = 0;
do {
   /* read until next line id (or and END marker) is found */
   do {
      fgets(inbuf,1024,lin_file);
      sscanf(inbuf,"%s",tmpbuf);
      if (strcmp(tmpbuf,"END")==0)
         done = 1;
      }
   while (sscanf(inbuf,"%d",&id)!=1 && !done); 

   if (!done)
      {
      /* find the category number for that line-id */
      CatStat = FindAttAndCat(txt_file,AttCol,CatCol,IDCol,id,AttText,&CatNum);
      if (CatStat < -1)
         {
         G_fatal_error("Reading label-text file");
         exit(-1);
         } 

      /* read line's points until an END marker is found */
      x = xarray;
      y = yarray;
      almost_done = 0;
      n_points = 0;
      do {
         fgets(inbuf,1024,lin_file);
         sscanf(inbuf,"%s",tmpbuf);
         if (strcmp(tmpbuf,"END")==0)
            almost_done=1;
         else if (sscanf(inbuf,"%lf %lf",&xtmp,&ytmp)==2)
            {
            *x++ = xtmp;
            *y++ = ytmp;
#           ifdef DEBUG
            printf("(%lf %lf) ",xtmp,ytmp);
#           endif
            n_points++;
            }
         if (CatStat>-1 && n_points == 2)
            fprintf(atts_file,"L %12.2lf %12.2lf %d\n",xtmp,ytmp,CatNum);
         }
      while (!almost_done);

      /* write line to the dig file */ 
      if (n_points > 0)
         {
         n_dig_lines++;
         dig_Write_line(dig_file,(char)type,xarray,yarray,n_points);
         }

      /* set the attribute string in the category structure */
      if (AttCol != -1)
         {
         if (G_set_cat((CELL)NumCats,AttText,&new_cats) != 1)
            G_fatal_error("gen_to_dig_atts_and_cats: call to G_set_cats");
         }
      }
   }
while (!done);

/*if (AttCol != -1)
   if (G_write_vector_cats(cats_filename,&new_cats) != 1)
      G_fatal_error("GenToDigLabelledLines: writing dig_cats file");*/

if (n_dig_lines > 0)
    return(0);             /* normal exit */
else
    return(-1);            /* error - no lines written to dig file */
}
