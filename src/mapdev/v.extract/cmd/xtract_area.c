/* updated for GRASS 5 9/99 */
/*  @(#)select_area.c     1.0  9/29/89   
 *  @(#)                  1.1  2/15/90    
 *  @(#)                  1.2  3/15/90    
 *  @(#)xtract_area.c     1.3  1/30/91  for 4.0
 *  From preliminary work (pull.vect) by Dave Gerdes - CERL
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector area records, outputting area lines
 * for any areas which match the user list of names/categories.
 * for lines which have left and right categories in the list
 * will optionally(-d flag) NOT write to output (common remove).
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
#define		CAT_DIR		"dig_cats"

struct Map_info Map;
struct Map_info Out_Map;
struct line_pnts *Points;
struct Plus_head Plus ;
struct Categories cats ;
char  buf[1024] ;

static	int   snapped = 0 ;

int 
xtract_area (int num_index, int num_array[], char *in_name, char *out_name, int optiond, int cat_new)
{
	FILE *outa ;
	int left, right, lcat, rcat, cat1;
	int l_isle, r_isle, larea, rarea, max_att=0;
        int non_cat, cat_no;
	int day, yr, typ;
        int vect_read;
	register int area_num, ii, jj;
	char  name[150], date[40];
	char  mon[4], *mapset ;

        double xx, yy;

        P_LINE *Lines;


        if ((mapset = G_find_file(DIG_DIR,in_name,"")) == NULL)
                {
		G_fatal_error("Can't find <%s> vector dig file.",in_name) ;
		return(-1) ;
                }

        fprintf(stderr,"\nLoading <%s> vector information.\n",in_name);
   
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

		     /* create dig directory, if not existing */
 	G__make_mapset_element("dig") ;
 	G__make_mapset_element("dig_plus") ;
 	G__make_mapset_element("dig_cats") ;
 	G__make_mapset_element("dig_att") ;

                     /* Open output "dig" file */
	if ( Vect_open_new(&Out_Map, out_name) < 0)
	   {
	   fprintf(stderr,"Can't create output vector file <%s> \n", out_name) ;
	   return (-1);
	   }

                     /* Open output "dig_att" file */
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
	strcpy( Out_Map.head.map_name,"Created by v.extract");
        

        fprintf(stderr,"\n\tProcessing ");

/* --------------------- Lines Section ------------------------------- */
                     /* Cycle through all lines */
        for (ii=1; ii <= Map.n_lines; ii++)
	     {
             G_percent(ii, Map.n_lines, 10);
	     Lines = &(Map.Line[ii]);
	            /* skip anything other than area lines */
	     if (Lines->type != AREA)   continue;

	            /* get the category for areas left & right */
		left = right = 0;
                lcat = rcat = 0;
		l_isle = r_isle = -1;

                if (Lines->left != 0) 
		   {
                   if (Lines->left > 0) 
		      {
                      lcat = Map.Att[Map.Area[Lines->left].att].cat;
		      larea = Lines->left;
		      }
                   else
                      {
                      larea = abs(Lines->left);
                      l_isle = Map.Isle[larea].area;
                      lcat = Map.Att[Map.Area[l_isle].att].cat;
		      larea = l_isle;
                      }
		   }

                if (Lines->right != 0) 
		   {
                   if (Lines->right > 0) 
		      {
                      rcat = Map.Att[Map.Area[Lines->right].att].cat;
		      rarea = Lines->right;
		      }
                   else
                      {
                      rarea = abs(Lines->right);
                      r_isle = Map.Isle[rarea].area;
                      rcat = Map.Att[Map.Area[r_isle].att].cat;
		      rarea = r_isle;
                      }
		   }

		       /* compare to the category list */
                for ( jj = 0 ; jj < num_index ; jj++)
                   {
                   if (lcat == num_array[jj]) left = 1;
                   if (rcat == num_array[jj]) right = 1;
                   }

/*if (left || right)
     {
     fprintf (stdout,"line= %d\n",ii);
     fprintf (stdout,"\tlcat= %d, left= %d, Line_left= %ld, l_isle= %d\n",lcat,left,Lines->left,l_isle);
     fprintf (stdout,"\trcat= %d, rite= %d, Line_rite= %ld, r_isle= %d\n",rcat,right,Lines->right,r_isle);
     }
*/
                if (optiond)  /* if user requested -d option */
		    if (left && right) continue;

                           /* check the flags, if either create this line */
                if (left || right)
                    {             /* read and write line */
                    if (0 > V1_read_line(&Map, Points, Map.Line[ii].offset))
			    fprintf (stderr, "Out of Memory\n"), exit (-1);
		    Vect_write_line (&Out_Map, Map.Line[ii].type, Points);
                    }

             }  /* end lines section */
	Vect_close (&Out_Map);
	 
        if (optiond) non_cat = cat_new ? cat_new : num_array[0];
        else non_cat = cat_new;      /* without optiond, cat_new = 0 */

/* --------------------- Attributes Section -------------------------- */
		/* Cycle through all areas */
	for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
	     {
			/* get the category number for area "area_num" */
	     cat_no = Map.Att[Map.Area[area_num].att].cat;

		       /* compare to the category list */
             for ( jj = 0 ; jj < num_index ; jj++)
{
/*fprintf(stderr,"num_array[%d]= %d, cat_no= %d\n",jj,num_array[jj],cat_no);*/
                 if (cat_no == num_array[jj]) 
                    {
                    if (non_cat != 0) cat1 = non_cat;
                    else cat1 = cat_no;
                    xx = Map.Att[Map.Area[area_num].att].x;
                    yy = Map.Att[Map.Area[area_num].att].y;
                    typ = Map.Att[Map.Area[area_num].att].type;
/*fprintf(stderr,"\t Create new att info, from left for %d\n", cat1);*/
		       /* capture the highest attribute value */
                    if (cat1 > max_att) max_att = cat1;
/*fprintf(stderr, "%c    %10.2f   %10.2f    %6d\n", codes(typ), xx, yy, cat1);*/
                    fprintf(outa, "%c    %10.2f   %10.2f    %6d\n", 
                              codes(typ), xx, yy, cat1);
                    }
}
             }  /* end attributes section */

	fclose (outa);
	Vect_close (&Map);
        return(max_att) ;
}

int 
codes (int type)
{
    switch (type) {
	case LINE:
	    return ('L');
	    break;
	case AREA:
	    return ('A');
	    break;
	case DOT:
	    return ('P');
	    break;
	case DEAD_LINE:
	    return ('l');
	    break;
	case DEAD_AREA:
	    return ('a');
	    break;
	case DEAD_DOT:
	    return ('p');
	    break;
	default:
	    return ('X');
    }
}
