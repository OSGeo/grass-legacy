/* %W% %G% */
/* main.c    1.0   10/01/89
/* main.c    1.1   1/30/91
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of generating vector (digit) files
*             from an existing vector maplayer. Re-classifies vector
*             vector data from user provided conversion list.
*
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.select.vect [-d]  input=vector file to read 
*                                 output=vector file to create
*                                 type=area,line, or site
*                                 file=category conversion file
*
*    flags:
*         -d      : dissolve common boundaries
*
*/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include  "gis.h"
#include "Vect.h"

#define		B_DIR		"dig"
#define		A_DIR		"dig"
#define		ATT_DIR		"dig_att"
#define		CAT_DIR		"dig_cats"

struct Map_info Map;
int cat_array[5000];
char  buf[1024] ;
struct dig_head Head;


main (argc,argv)
int argc;
char *argv[];

{
	int i, ier, cat_index, new_cat, max_att;
	int dissolve=0, cnt, x, y;
        char buffr[80], file_name[80], text[80];
        char *input, *output, *mapset, *conv_file;
        struct Categories cats;
        struct Option *inopt, *outopt, *fileopt, *typopt;
        struct Flag *d_flag;
        FILE *in, *catf;


    G_gisinit (argv[0]);

            /* set up the options and flags for the command line parser */

    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";
    
    typopt = G_define_option();
    typopt->key              = "type";
    typopt->type             =  TYPE_STRING;
    typopt->required         =  YES;
    typopt->options          =  "area,line,site";
    typopt->description      =  "Select area, line, or site "; 

    inopt = G_define_option();
    inopt->key             = "input";
    inopt->type            = TYPE_STRING;
    inopt->required        = YES;
    inopt->gisprompt       = "old,dig,vect";
    inopt->description     = "vector input map name ";

    outopt = G_define_option();
    outopt->key             = "output";
    outopt->type            =  TYPE_STRING;
    outopt->required        =  YES;
    outopt->gisprompt       = "any,dig,vect";
    outopt->description     = "vector output map name ";

    fileopt = G_define_option();
    fileopt->key             = "file";
    fileopt->type            =  TYPE_STRING;
    fileopt->required        =  YES;
    fileopt->description     = "Text file name for category conversion";

       /* heeeerrrrrre's the   PARSER */
    if (G_parser (argc, argv))
        exit (-1);

       /* start checking options and flags */
    

       /* set input vector file name and mapset */
    input = inopt->answer;
    mapset = G_find_vector (input, "") ;
    if (mapset == NULL)
	{
		sprintf(buffr,"Vector file [%s] not available in search list",
		    input);
		G_fatal_error(buffr) ;
	}
      
       /* set output vector file name */
    output = outopt->answer;

       /* set conversion file name */
    conv_file = fileopt->answer;

    if ( d_flag->answer ) dissolve = 1;

    if (*typopt->answer == 'a')
       {
       max_att = rclas_area(input,output,conv_file,dissolve);
       if ( 0 > max_att)
          {
          fprintf(stderr," Error in area re-class processing\n");
          exit(1);
          }
       }
    else
       {
       max_att = rclas_line(input,output,conv_file);
       if ( 0 > max_att)
          {
          fprintf(stderr," Error in line/site re-class processing\n");
          exit(1);
          }
       }
                     /* Open output "dig_cats" file*/
    G__file_name(file_name, CAT_DIR, output, G_mapset()) ;
    if ( (catf = fopen (file_name, "w")) == NULL)
       {
       fprintf(stderr,"Can't create output dig_cats file <%s> \n", file_name) ;
       return (-1);
       }

    fprintf (stdout,"\n");
    fprintf (stdout,"    Making category file\n");
                      /* make a cats file */
    
    sprintf(buffr,"# %d categories\nTitle %s\n",max_att,output);
    fputs(buffr,catf);
    sprintf(buffr,"\n0.00 0.00 0.00 0.00\n0:no data\n");
    fputs(buffr,catf);
           /* build an empty cat file */
    for ( i = 1; i <= max_att; i++)
	{
        sprintf(buffr,"%d:\n",i);
        fputs(buffr,catf);
	}
    fclose(catf) ;

    sprintf( buffr, "%s/etc/v.build  map=%s  thresh=no",G_gisbase(),output);
    system(buffr);

		       /* give the user this message  */
    fprintf(stderr, 
    "\n\nRe-classified vector file <%s> has been created in the 'dig' directory\n\n",
		    output);

    exit(0);
}
