
/*
 * write_lines.c  
 * 
 */

#include <unistd.h>
#include "Vect.h"
#include "gis.h"
#include "Vect.h"
#include "v_out_arc.h"
#include "gtoa.h"

int 
write_lines (char *name, char *mapset, struct Map_info *map, FILE *lines_file, FILE *text_file)
{
   P_LINE *Line;
   P_AREA *Area;
   int line, left, right;
   int type;
   int count=0,num;
   char *att;
   int n_points, n_atts;
   int attflag;
   static struct line_pnts *GPoints;
   int i, n, catflag;
   struct Categories cats;

#  ifdef DEBUG
   fprintf (stdout,"write_lines\n");
#  endif
   
   GPoints = Vect_new_line_struct ();

   catflag=G_read_vector_cats(name,mapset,&cats);
	  /*  n = cats.count; */

   for (line = 1 ; line <= map->n_lines ; line++)
   {
    	Line = &(map->Line[line]);

        if (Line->att || line == 1)
            attflag=1;
        else
            attflag=0;
        
	/* if reach the DOTs, then we are done [this is ERRONEOUS*/
	/* [since line->type is NOT ordered, plus  we probably   */
	/* [want to keep the dots as degenerate lines anyhow?? - dks*/

	/***
		if (Line->type == DOT)
			break;
    */

        if (0 > V1_read_line (map, GPoints, Line->offset))
		{
			fprintf (stderr, "ERROR reading line %d from file\n", line);
			exit (-1);
        }

	
	fprintf (lines_file,"           %d\n",line);
	start_coords ();
	write_coords (lines_file, GPoints->n_points, GPoints->x, GPoints->y);

	/*WHAT DOES THIS DO??? end_coords (lines_file); */
	lin_flg=1;

	fprintf (lines_file,"END\n");
        
        /* write category number and attribute text to text_file */
        if (attflag==1 && catflag!=-1)
        {
           /* get attribute text */
	   att = G_get_cat(map->Att[Line->att].cat, &cats);
	   if(space) 
	   {
	     i = 0;
	     while(att[i])
	     {
		if(att[i]==' ') att[i] = '_';
		i++;
             }
           }

           fprintf(text_file,"%d%c%d%c%d%c%s\n",
                   line, separator, map->Att[Line->att].cat, separator, line, 
						 separator, att);
           txt_flg=1;
        }
	    else if (attflag==1)
	    {
	        fprintf(text_file,"%d%c%d%c%d\n", line, separator, map->Att[Line->att].cat,
	                	      separator, line);
		    txt_flg=1;
        } 
       
    }

	Vect_destroy_line_struct (GPoints);

	fprintf (lines_file,"END\n");	

  return 0;
}
