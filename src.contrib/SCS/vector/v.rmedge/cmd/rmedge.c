/*  @(#)rmedge.c     1.2  3/12/90   
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector area records, outputting area lines
 * for any areas which  are NOT on the outer edge of the map.
 * The resulting map attributes are copied to the result map.
 */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include  "gis.h"
#include "Vect.h"

#define		DIG_DIR		"dig"
#define		ATT_DIR		"dig_att"
#define		CAT_DIR		"dig_cats"

struct Map_info Map;
struct Map_info Out_Map;
struct line_pnts *Points;
char  buf[1024] ;

int 
rmedge (char *in_name, char *out_name)
{
	FILE *out, *outa;
	register int line_num, i, ii, jj, kk;
	int ret, line, left, right, vect_read;
	int larea, rarea, l_isle, r_isle, lcat, rcat;
	char  name[150], command1[128], command2[128] ;
	char  *mapset ;
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
	if ((vect_read = Vect_open_old(&Map,in_name,mapset)) < 0)
                {
		G_fatal_error("Reading input file.") ;
		return(-1) ;
                }
                      /* check for topology on the  vector file */
        if (vect_read < 2)
		{
		G_fatal_error("You must run v.support on this file.") ;
		return(-1) ;
		}
 
	G__make_mapset_element(DIG_DIR) ;
	G__make_mapset_element(ATT_DIR) ;
	G__make_mapset_element(CAT_DIR) ;

                     /* Open output "dig" file */
	if (Vect_open_new(&Out_Map, out_name) < 0)
	   {
	   fprintf(stderr,"Can't create output vector file <%s> \n", name) ;
	   return (-1);
	   }
		  /* setup to copy "dig_att" file */
	G__file_name(name, ATT_DIR, in_name, mapset) ;
	command1[0] = '\0';
        sprintf(command1,"cp %s %s/%s/dig_att/",name,G_location_path(),G_mapset());
        strcat(command1,out_name);
		  /* and setup to copy "dig_cats" file */
	G__file_name(name, CAT_DIR, in_name, mapset) ;
	command2[0] = '\0';
        if ((ret = access(name,0)) != -1)
           {            /* the category file exists, copy categ. file */
           sprintf(command2,
	       "cp %s %s/%s/dig_cats/",name,G_location_path(),G_mapset());
           strcat(command2,out_name);
	   }


		     /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();

                     /* Read and write header info */
	Vect_copy_head_data(&Map.head, &Out_Map.head);

                     /* Cycle through all areas */
	for (line_num = 1 ; line_num <= Map.n_lines ; line_num++)
	  { 
	  Lines = &(Map.Line[line_num]);

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

          if (lcat != 0) left = 1;
          if (rcat != 0) right = 1;

      /* check the flags, if either create this line */
          if (left && right)
             {             /* read and write line */
    	     if (0 > V1_read_line(&Map, Points, Map.Line[line_num].offset))
			    fprintf (stderr, "Out of Memory\n"), exit (-1);
	     Vect_write_line (&Out_Map, Map.Line[line_num].type, Points);
             }
          } /* end for Map.Lines */

	Vect_close (&Out_Map);
	Vect_close (&Map);
        system (command1);
	if (strlen(command2) != 0) system (command2);

		       /* give the user this message */
        fprintf(stderr, "\nEdgeless vector file <%s> has been created in the 'dig' directory\n\n",out_name);
  	fprintf(stderr, "\n\nRunning the program 'v.support' will produce labeling errors.\n") ;

	return(0) ;
}

