
/* main.c
 *
 * function defined: main routine for arc.to.grass program
 *
 * PURPOSE: Import ARC/INFO Generate files into GRASS.
 *          Results in the creation of a GRASS vector 
 *          file consisting of dig, dig_atts, and
 *          dig_cats components.  This routine gets
 *          file names from the user and opens files
 *          necessary for importation. 
 *
 * NOTES: 
 *
 * 1) ARC/INFO Generate files must respresent either
 *    a polygon or a line coverage.  No mixing of lines
 *    and polygons in one import operation.
 *
 * 2) Program support.vect must be run on the resulting GRASS
 *    vector file and then some cleaning-up may have to be
 *    done using program digit.
 *
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#include  "gis.h"
#include  "stdio.h"

main()
{
FILE  *fopen();
char  tmpbuf[80];
int   neatline=0;
int   done,
      errflag,
      try_again=0;
char  *mapset ;
char  cov_type[80];          /* coverage type */
char  lines_filename[80] ;   /* ungenerate format lines file */
FILE  *lines_file;
char  pts_filename[80] ;     /* ungenerate format point-labels file */ 
FILE  *pts_file;
char  txt_filename[80] ;     /* label-text file */ 
FILE  *txt_file;
char  dig_filename[80] ;     /* GRASS vector (dig) file */  
char  dig_filepath[80] ;  
FILE  *dig_file;           
char  atts_filename[80] ;     /* GRASS vector (dig_atts) file */  
char  atts_filepath[80] ;  
FILE  *atts_file;           

G_gisinit("ARC/INFO Generate Format Import");

do { 

   try_again=0;

   /* GET COVERAGE TYPE */

   printf("\n COVERAGE TYPE\n");
   printf("Enter \"polygon\" or \"line\"\n");
   printf("Hit RETURN to cancel request\n");
   printf("> ");
   gets(cov_type);

   /* EXIT IF USER HIT RETURN */

   if (strcmp(cov_type,"") == 0)
      exit(0);

   /* GET FILE NAMES FOR COVERAGE TYPE "POLYGON" */

   else if (strcmp(cov_type,"polygon")==0)      
      {
      do {
         printf("\n NEATLINE\n");
         printf("Do you want a neatline ?\n");
         printf("Enter \"yes\" or \"no\"\n");
         printf("> ");
         gets(tmpbuf);
         }
      while (strcmp(tmpbuf,"yes")!=0 && strcmp(tmpbuf,"no")!=0);
      if (strcmp(tmpbuf,"yes")==0)
         neatline = 1;
      
      /* LINES FILENAME */
      done = 0;
      do {
         printf("\n LINES FILENAME\n");
         printf("Enter the name of the a created with the LINES\n");
         printf("option of the ARC/INFO Ungenerate command\n");
         printf("Hit RETURN to cancel request\n");
         printf("> ");
         gets(lines_filename);
         if (strcmp(lines_filename,"") == 0)
            exit(0);
         else 
            {
            if ((lines_file=fopen(lines_filename,"r")) == NULL)
               G_warning("File Not Found");
            else
               done = 1;
            }
         }
     while (!done);

      /* LABEL-POINTS FILENAME */
      done = 0;
      do {
         printf("\n LABEL-POINTS FILENAME\n");
         printf("Enter the name of a file created with the POINTS\n");
         printf("option of the ARC/INFO Ungenerate command\n");
         printf("Hit RETURN if there is no such file\n");
         printf("> ");
         gets(pts_filename);
         if (strcmp(pts_filename,"") == 0)
            {
            pts_file = NULL; 
            txt_file = NULL; 
            done = 1;
            }
         else 
            {
            if ((pts_file=fopen(pts_filename,"r")) == NULL)
               G_warning("File Not Found");
            else
               done = 1;
            }
         }
      while (!done);

      /* LABEL-TEXT FILENAME */
      if (pts_file != NULL)
         {
         done = 0;
         do {
            printf("\n LABEL-TEXT FILENAME\n");
            printf("Enter the name of a file that associates\n");
            printf("label-point ID numbers with text label strings\n");
            printf("Hit RETURN if there is no such file\n");
            printf("> ");
            gets(txt_filename);
            if (strcmp(txt_filename,"") == 0)
               {
               txt_file = NULL;
               done = 1;
               }
            else 
               {
               if ((txt_file=fopen(txt_filename,"r")) == NULL)
                  G_warning("File Not Found");
               else
                  done = 1;
               }
            }
         while (!done);
         }
      }

   /* ELSE - GET FILE NAMES FOR COVERAGE TYPE "LINE" */

   else if (strcmp(cov_type,"line")==0)
      {
      /* LINES FILE */
      done = 0;
      do {
         printf("\n LINES FILENAME\n");
         printf("Enter the name of a file created with the LINES\n");
         printf("option of the ARC/INFO Ungenerate command\n");
         printf("Hit RETURN to cancel request\n");
         printf("> ");
         gets(lines_filename);
         if (strcmp(lines_filename,"") == 0)
            exit(0);
         else 
            {
            if ((lines_file=fopen(lines_filename,"r")) == NULL)
               G_warning("File Not Found");
            else
               done = 1;
            }
         }
      while (!done);
      
      /* LABEL-TEXT FILENAME */
      done = 0;
      do {
         printf("\n LABEL-TEXT FILENAME\n");
         printf("Enter the name of a file that associates\n");
         printf("line ID numbers with text label strings\n");
         printf("Hit RETURN if there is no such file\n");
         printf("> ");
         gets(txt_filename);
         if (strcmp(txt_filename,"") == 0)
            {
            pts_file = NULL;
            txt_file = NULL;
            done = 1;
            }
         else 
            {
            if ((txt_file=fopen(txt_filename,"r")) == NULL)
               G_warning("File Not Found");
            else
               done = 1;
            }
         }
      while (!done);
      } 
   else try_again = 1;
   } 
while (try_again);

/* get name for grass vector file from user */
mapset = G_ask_new( " FILENAME TO STORE BINARY VECTOR",dig_filename,"dig",
                    "binary vector") ;

/* open a new GRASS dig file */
if ( ! mapset) exit(0) ;
G__make_mapset_element("dig") ;
G__file_name(dig_filepath,"dig",dig_filename,mapset) ;
if ((dig_file=fopen(dig_filepath,"w"))==NULL)
   {
   printf("Not able to open dig file <%s>\n",dig_filepath);
   exit(-1);
   }

if (pts_file != NULL)
   {
   /* open a new GRASS dig_atts file */
   G__make_mapset_element("dig_att") ;
   G__file_name(atts_filepath,"dig_att",dig_filename,mapset) ;
   if ((atts_file=fopen(atts_filepath,"w"))==NULL)
      {
      printf("Not able to open dig_atts file <%s>\n",atts_filepath);
      exit(-1);
      }
   }

if (errflag=BuildDig(cov_type,neatline,lines_file,pts_file,txt_file,
    dig_file, atts_file,dig_filename)<0)
   {
   switch (errflag)
     {
     case -1:G_fatal_error("Reading LINES file"); exit(-1);
     case -2:G_fatal_error("Reading LABEL-TEXT file"); exit(-1);
     case -3:G_fatal_error("Reading LABEL-TEXT or LABEL-POINTS file"); exit(-1);
     case -4:G_fatal_error("Reading LINES file"); exit(-1);
     case -5:G_fatal_error("Invalid coverage type"); exit(-1);
     }
   }

fprintf(stderr, "\n\nBefore this vector file can be used in the 'digit' program:\nRun the program support.vect to build the  needed support files.\n") ;

exit(0) ;
}
