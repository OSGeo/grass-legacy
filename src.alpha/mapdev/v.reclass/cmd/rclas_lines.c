static char rcsid[]="$Header: /usr3/4.0/src.contrib/SCS/vector/v.reclass/cmd/RCS/rclas_lines.c,v 1.2 1992/12/13 10:36:19 grass Exp $";
/* @(#)rclas_lines.c	1.1.2.1 1/14/93 */
/*  @(#)rclas_lines.c    1.0  9/29/89   
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector line records, outputting lines
 * whose attribute appears in the conversion file, with the
 * line attribute is re-set to the conversion category value
 */


#include  "gis.h"
#include "Vect.h"

#define		DIG_DIR		"dig"
#define		ATT_DIR		"dig_att"
#define         DEBUG           0

struct Map_info Map;
struct Map_info Out_Map;
struct line_pnts *Points;
char  buf[1024] ;

rclas_line(in_name, out_name, new)
char  *in_name, *out_name;
struct Reclass *new;
{
	FILE *out, *outa;
	int cat_num, cat, line, got_one, lcat, cat1;
	int linea, latt, att_cnt=0, max_att=0;
        int did_sel, did_draw;
        int vect_read, day, yr;
	register int area_num, i, ii, jj, kk;
	char  name[150], bufr[1024], date[40], mon[4] ;
	char  *ptr, *mapset ;
	char *X, *Y;
	int n_points;
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
	strcpy( Out_Map.head.map_name,"Created by v.reclass");

        fprintf(stderr,"\n\tProcessing ");

/* --------------------- Lines Section ------------------------------- */
                     /* Cycle through all lines */
        for (ii=1; ii <= Map.n_lines; ii++)
	     {
             G_percent(ii, Map.n_lines, 10);
	     Lines = &(Map.Line[ii]);
	            /* skip anything other than area lines */
	     if (Lines->type == AREA)   continue;

	            /* get the category */
	     cat1 = cat_num = Map.Att[Lines->att].cat;

		       /* compare to the conversion list */
                cat1 = (cat_num > new->max)||(cat_num < new->min) ? 0 : (new->table[cat_num - new->min]);
		if (!cat1) continue;
                                 /* read and write line */
if (DEBUG == 2) printf("\t\t\twrite line # %d, offset= %d\n",ii,Map.Line[ii].offset);
             if (0 > V1_read_line(&Map, Points, Map.Line[ii].offset))
			    fprintf (stderr, "Out of Memory\n"), exit (-1);
	     Vect_write_line (&Out_Map, Map.Line[ii].type, Points);
                  
		         /* put out the attribute info for this line */
             jj = Map.Line[ii].att;
if (DEBUG == 2) printf("\t\t\tatt : %c    %10.2lf   %10.2lf    %6d\n", 
                     codes(Map.Att[jj].type), Map.Att[jj].x,
                     Map.Att[jj].y,cat1);
	     fprintf( outa, "%c %14.7lf %14.6lf   %8d\n", 
                     codes(Map.Att[jj].type), Map.Att[jj].x,
                     Map.Att[jj].y,cat1);

                       /* capture the highest attribute value */
             if (cat1 > max_att) max_att = cat1;
             }  /* end lines section */

	fclose (outa);
	Vect_close (&Out_Map);
	Vect_close (&Map);
	return(max_att) ;
}
