
/*
 * write_lines.c  
 * 
 */

#include <stdio.h>
#include "gtoa.h"
#include "gis.h"
#include "digit.h"

write_lines (name,mapset,map,lines_file,text_file)
    char   *name,
           *mapset;
    struct Map_info *map;
    FILE   *lines_file,
           *text_file;
{
   P_LINE *Line;
   P_AREA *Area;
   int line, left, right;
   int count=0,num;
   char att[1000];
   int n_points, n_atts;
   int attflag;
   static struct line_pnts Gpoints;
   int i, n, catflag;
   struct Categories cats;

#  ifdef DEBUG
   printf("write_lines\n");
#  endif
   
   if ((catflag=G_read_vector_cats(name,mapset,&cats)) != -1)
       {
       n = cats.count;
       G_sort_cats (&cats);
       }

   for (line = 1 ; line <= map->n_lines ; line++)
   {
	Line = &(map->Line[line]);

        if (Line->att || line == 1)
            attflag=1;
        else
            attflag=0;
        
	/* if reach the DOTs, then we are done */
	if (Line->type == DOT)
	    break;

	if (0 > dig__Read_line (&Gpoints, map->digit, Line->offset))
	{
 		fprintf (stderr, "ERROR reading line %d from file\n", line);
		exit (-1);
        }
	
	fprintf (lines_file,"%d\n",line);
        start_coords ();
	write_coords (lines_file, Gpoints.n_points, Gpoints.x, Gpoints.y);
	end_coords (lines_file);
        lin_flg=1;
	fprintf (lines_file,"END\n");
        
        /* write category number and attribute text to text_file */
        if (attflag==1 && catflag!=-1)
           {
           /* get attribute text */
           i=0; att[0]=0;
           do {
              if (cats.list[i].num == map->Att[Line->att].cat)
                 {
                 sscanf(cats.list[i].label,"%s",att);
                 i=n;
                 }   
              i++;
              }   
           while (i<n);
           fprintf(text_file,"%d %d %d %s\n",
                   line,map->Att[Line->att].cat,line,att);
           txt_flg=1;
           }
       else if (attflag==1)
           {
           fprintf(text_file,"%d %d %d\n",line,map->Att[Line->att].cat,line);
           txt_flg=1;
           } 
       
    }
fprintf (lines_file,"END\n");	
}
