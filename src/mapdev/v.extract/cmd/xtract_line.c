/* %W% %G% */
/*  @(#)xtract_lines.c    1.0  9/29/89   
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector line records, outputting lines
 * which match the user list of names/categories.
 * The resulting map attribute is arbitarily set to first category
 * of the user list or a user selected category number (cat_new).
 */

#include <string.h>
#include <stdlib.h>
#include  "gis.h"
#include "Vect.h"
#include "local_proto.h"

#define		DIG_DIR		"dig"
#define		ATT_DIR		"dig_att"
#define         DEBUG           0

struct Map_info Map;
struct Map_info Out_Map;
struct line_pnts *Points;
char  buf[1024] ;

int 
xtract_line (int num_index, int num_array[], char *in_name, char *out_name, int cat_new, int select)
{
	FILE *outa;
	int cat, cat1;
	int max_att=0;

        int day, yr, vect_read;
	register int i, ii, jj;
	char  name[150], date[40], mon[4] ;
	char  *mapset ;


        P_LINE *Lines;


        if ((mapset = G_find_file(DIG_DIR,in_name,"")) == NULL)
                {
		G_fatal_error("Can't find input file <%s>.",in_name) ;
		return(-1) ;
                }

        fprintf(stderr,"\nLoading vector information.\n");

                     /* Do initial read of input DIGIT file */
	if ((vect_read = Vect_open_old(&Map,in_name, mapset)) < 0 )
                {
		G_fatal_error("Reading input file.") ;
		return(-1) ;
                }
        if (vect_read < 2)
                {
		G_fatal_error("You must run v.support on this file.") ;
		return(-1) ;
                }

                     /* Open output "dig" and "dig_att" files */
	G__make_mapset_element(DIG_DIR) ;
                     /* Open output "dig" file */
	if ( Vect_open_new(&Out_Map, out_name) < 0)
	   {
	   fprintf(stderr,"Can't create output vector file <%s> \n", out_name) ;
	   return (-1);
	   }

	G__make_mapset_element(ATT_DIR) ;
	G__file_name(name, ATT_DIR, out_name, G_mapset()) ;
	if ( (outa = fopen (name, "w")) == NULL)
	   {
	   fprintf(stderr,"Can't create output attribute file <%s> \n", name) ;
	   return (-1);
	   }

                     /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();

                     /* Read and write header info */
        sprintf(date,"%s",G_date());
        sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
        if (yr < 2000) yr = yr - 1900;
        else yr = yr - 2000;
        sprintf(date,"%s %d %d",mon,day,yr);
        Vect_copy_head_data(&Map.head, &Out_Map.head);
	strcpy( Out_Map.head.date,date);
	strcpy( Out_Map.head.your_name,out_name);

/* --------------------- Lines Section ------------------------------- */
                     /* Cycle through all lines */
        for (ii=1; ii <= Map.n_lines; ii++)
	     {
	     Lines = &(Map.Line[ii]);
	            /* skip anything other than the selected line type */
	     if (Lines->type != select)   continue;

	            /* get the category */
	     cat = Map.Att[Lines->att].cat;

	       /* check against the user category list */
             for ( i = 0 ; i < num_index ; i++)
	       {
               if (cat == num_array[i])
                 {  
if (DEBUG == 1) fprintf (stdout,"\tline qualified, label= %d\n",
                                    cat_new ? cat_new : num_array[0]);

                                 /* read and write line */
if (DEBUG == 2) fprintf (stdout,"\t\t\twrite line # %d, offset= %ld\n",ii,Map.Line[ii].offset);
                 if (0 > V1_read_line(&Map, Points, Map.Line[ii].offset))
			    fprintf (stderr, "Out of Memory\n"), exit (-1);
		 Vect_write_line (&Out_Map, Map.Line[ii].type, Points);
                  
		         /* put out the attribute info for this line */
                 jj = Map.Line[ii].att;
                 cat1 = cat_new ? cat_new : num_array[i];
if (DEBUG == 2) fprintf (stdout,"\t\t\tatt : %c    %10.2f   %10.2f    %6d\n", 
                     codes(Map.Att[jj].type), Map.Att[jj].x,
                     Map.Att[jj].y,cat1);
	         fprintf( outa, "%c    %10.2f   %10.2f    %6d\n", 
                     codes(Map.Att[jj].type), Map.Att[jj].x,
                     Map.Att[jj].y,cat1);

                       /* capture the highest attribute value */
                 if (cat1 > max_att) max_att = cat1;
                 break;
	         }
               } /* end for num_index */
             }  /* end lines section */

	fclose (outa);
	Vect_close (&Out_Map);
	Vect_close (&Map);
	return(max_att) ;
}
