#include "gis.h"
#include "Vect.h"

modCat (name, N_map, O_map, Points, vect_cat, newcat, attFile, curLine)
    char *name;
    struct Map_info *N_map;
    struct Map_info *O_map;
    struct line_pnts *Points;
    int vect_cat, newcat;
    FILE *attFile;
    int curLine;
{
    int i, n,  np;
    int att;
    int *list, count, idx;
    int line, nlines, lnCnt;
    int nareas, area_cnt, a_index;
    int *find_area(), *find_line();
    char lnType;
    double x,y ;
    P_ATT *O_Att, *N_Att;
    P_LINE *Line;
    P_AREA *Area; 
    char buf[1024];

    fflush (stdout);


    lnCnt = 0;
    nlines = V2_num_lines (O_map);
    nareas = V2_num_areas (O_map);

  if (list = find_line (vect_cat, &count, O_map) )
   {
    for (n = 0; n < count; n++)
    {
	idx = list[n] ;
	if (O_map->Line[idx].type == LINE)  {
			if(!read_vLine (O_map, Points, idx) ) return(1) ;
			Vect_write_line(N_map, O_map->Line[idx].type , Points);
			att = O_map->Line[idx].att;

			fprintf (attFile, "L ");
			G_format_easting (O_map->Att[att].x, buf, -1);
			fprintf (attFile, "%s ", buf);
			G_format_northing (O_map->Att[att].y, buf, -1);
			fprintf (attFile, "%s ", buf);
			fprintf (attFile, "%d\n", newcat);
		}
	  }
  }

  if (list = find_area (vect_cat, &count, O_map) ) {
	for (n=0; n < count; n++) {
		  idx = list[n] ;
			Line = &O_map->Line[idx];
               		a_index = O_map->Att[O_map->Area[idx].att].index;
			if (!read_vArea(O_map, a_index, Points) ) return 1;
		        Vect_write_line(N_map, O_map->Line[idx].type , Points);
			att = O_map->Area[idx].att; 

			fprintf (attFile, "A ");
			G_format_easting ( O_map->Att[O_map->Area[idx].att].x, buf, -1);
			fprintf (attFile, "%s ", buf);
			G_format_easting ( O_map->Att[O_map->Area[idx].att].y, buf, -1);
			fprintf (attFile, "%s ", buf);
			fprintf (attFile, "%d\n", newcat);
		  }
	  }

    return 0;
}



static read_vLine (O_map, Points, line)
	struct Map_info *O_map;
	struct line_pnts *Points;
	int line;
{
	int ret;

        if (0 > (ret = V2_read_line (O_map, Points, line)))
	{
	    if (ret == -2)
		G_warning ("Read error");
	    return 0;
	}
	return 1;

}


static read_vArea(O_map,index, Points)
	struct Map_info *O_map;
	int index;
	struct line_pnts *Points;

	
{
     if(Vect_get_area_points(O_map,index,Points) < 0)  {
		G_warning ("Read error");
		return(0);
	}
	return 1;
}
