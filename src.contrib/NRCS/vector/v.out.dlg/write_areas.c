/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include "digit.h"
#include "export_dlg.h"
#include "gis.h"


#define AREA_FMT "A%5d%12.2lf%12.2lf%6d%6d%6d%6d%6d%6d      \n"

write_dlg_areas (map, fp)
    struct Map_info *map;
    FILE *fp;
{
    P_AREA *Area;
    P_ISLE *Isle;
    double x, y;
    int n_atts;
    register int area, i;
    plus_t zero = 0;
    int n_lines;
    struct Categories cats;
    FILE *dlg_attfile;
    char buf[250], s[80], dlgprefix2[2];

    sprintf (dlgprefix2,".%d",dlgext);

    if (dlgtype){
	    G_strcpy(buf, dlgprefix);
	    switch (dlgtype){
	    case 5:
                strcat(buf, dlgprefix2);
		G_strcat(buf, "aa");
		break;
	    case 6:
                strcat(buf, dlgprefix2);
		G_strcat(buf, "sa");
		break;
	    case 7:
                strcat(buf, dlgprefix2);
		G_strcat(buf, "ha");
		break;
	    case 8:
                strcat(buf, dlgprefix2);
		G_strcat(buf, "ca");
		break;
	    case 1:
		G_strcat(buf, ".att");
		break;
	    }
   	dlg_attfile = fopen(buf, "w");
	    if (G_read_vector_cats( map->name, map->mapset, &cats) < 0){
			G_free_cats(&cats);
			G_init_cats((CELL)0, "", &cats);
	    }
	/*if (dlgtype >= 5){*/
	if (dlgtype == 5 || dlgtype == 6 || dlgtype == 7 || dlgtype == 8 ){
	    if (map->n_areas == 0){
	    fprintf(stderr,"\nError: No Area Found.  Vector file structure error for selected DLG type.\n");
	    fprintf(stderr,"Error: Vector file must contain area frame.\n");
	    fprintf(stderr,"Error: DLG write aborted.\n");
 	    sprintf (s, "rm %s.*",dlgprefix);
	    system(s);
	    exit(0);
	    }
	    Isle = &(map->Isle[1]);
		x = Isle->W - 1000.;
		y = Isle->S - 1000.;
	fprintf (fp, AREA_FMT, 
		1, 		 	/* index of element */
		x, y,			/* coordinates */
		0,			/* unused */
		Isle->n_lines,		/* number of lines */
		0,			/* unused */
		1,			/* # of att codes */
		0,			/* unused */
		0);			/* number of isles */

	start_ints ();
	   write_ints (fp, Isle->n_lines, Isle->lines);
	end_ints (fp);
	   start_att ();
	   write_dlg_att (fp, 0, 0);
	   end_att (fp);
	}

    }
    for (area = 1 ; area <= map->n_areas ; area++)
    {
	Area = &(map->Area[area]);

	if (Area->att || (area == 1 && dlgtype < 5 ))
	    n_atts=1;
	else
	    n_atts=0;

	if (AREA_LABELED (Area))
	{
		x = map->Att[Area->att].x;
		y = map->Att[Area->att].y;
	}
	else
	{
	    if ((area == 1 || area == 2) && dlgtype < 5 )
	    {
		/* dont really exist */
		x = (Area->E + Area->W) / 2.0;
		y = (Area->N + Area->S) / 2.0;
	    }
	    else
	    {
/*
	double totalarea;
	dig_find_area (map, Area, &totalarea, &x, &y, 0.0);
*/
                /* this returns a point inside area and outside all islands
                of the area */
                y = 0.0; x = 0.0;
                Vect_get_point_in_area(map, area , &x, &y);
                /* dbug:
                if((Vect_point_in_islands(map,area,x,y))
                  || ( dig_point_in_area(map,x,y, Area) == 0.0))
                    printf(" 2 point is not inside the area! %f  %f\n", x, y);
                */
	    }
	}

	/*
	** get total number of area_line entries.  This includes
	** the zero separators.
	*/
	n_lines = Area->n_lines + Area->n_isles;
	for (i = 0 ; i < Area->n_isles ; i++)
	{
	    n_lines += map->Isle[Area->isles[i]].n_lines;
	}

	if (dlgtype < 5 ){
	fprintf (fp, AREA_FMT, 
		area, 		 	/* index of element */
		x, y,			/* coordinates */
		0,			/* unused */
		n_lines,		/* number of lines */
		0,			/* unused */
		n_atts,			/* # of att codes */
		0,			/* unused */
		Area->n_isles);		/* number of isles */
	}else{
	fprintf (fp, AREA_FMT, 
		area + 1,	 	/* index of element */
		x, y,			/* coordinates */
		0,			/* unused */
		n_lines,		/* number of lines */
		0,			/* unused */
		n_atts,			/* # of att codes */
		0,			/* unused */
		Area->n_isles);		/* number of isles */
	}

	start_ints ();
	write_ints (fp, Area->n_lines, Area->lines);
	for (i = 0 ; i < Area->n_isles ; i++)
	{
	    Isle = &(map->Isle[Area->isles[i]]);
            write_ints (fp, 1, &zero);
	    write_ints (fp, Isle->n_lines, Isle->lines);
	}
	end_ints (fp);

/*rewrite this section not working correctly*/
if (area == 1 && dlgtype  )

	if (dlgtype < 6) {  /* not for SPECFEAT, HYDRO, CULTURE */
	fprintf( dlg_attfile, "%d	%d	%d	%s\n", 1, 0, 0 ,"UNIV");
	}

	if (area == 1 && dlgtype < 5) { /* special area has  0000 000 attr */
   	start_att ();
   	write_dlg_att (fp, 0, 0);
   	end_att (fp);
	}
/*rewrite this section not working correctly*/
else
	if (area == 2 && dlgtype == 1)
		fprintf( dlg_attfile, "%d			\n", area);
	else
	if (n_atts) {
	    start_att ();
	    write_dlg_att (fp, DEF_MAJOR, map->Att[Area->att].cat);
	    end_att (fp);
		switch (dlgtype){
		   case 5:
		   case 6:
		   case 7:
		   case 8:
	    		fprintf( dlg_attfile, "%d	%d	%d	%s\n",
			 area + 1, 999, map->Att[Area->att].cat,
			G_get_cat(map->Att[Area->att].cat, &cats));
			break;
		   case 1:
	    		fprintf( dlg_attfile, "%d	%d	%d	%s\n",
			 area, 999, map->Att[Area->att].cat,
			G_get_cat(map->Att[Area->att].cat, &cats));
			break;
		   default:
			break;
		}
	}
    }
}
