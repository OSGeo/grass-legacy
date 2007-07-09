#include <string.h>
#include "global.h"

#define BUFFSIZE 128

/* 
 * adopted from v.in.ascii code!
 *
 * add vector features to the map (input in
 * GRASS ASCII vector format)
 *
 * return number of added features
 *
 * id's of added features can be stored in List (if given)
 */
int asc_to_bin(FILE *ascii , struct Map_info *Map, struct ilist *List)
{
    char ctype;
    char buff[BUFFSIZE];
    double *xarray;
    double *yarray;
    double *zarray;	
    double *x, *y, *z;
    int i, n_points, n_coors, n_cats;
    int type, newline;
    int alloc_points;
    int end_of_file;
    int catn;
    int cat;
    int nlines;

    struct line_pnts *Points;
    struct line_cats *Cats;	
    
    nlines = 0;

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();	
    
    if (List) {
	Vect_reset_list(List);
    }

    end_of_file = 0 ;
    /*alloc_points     = 1000 ; */
    alloc_points     = 1;
    xarray = (double *) G_calloc(alloc_points, sizeof(double)) ;
    yarray = (double *) G_calloc(alloc_points, sizeof(double)) ;
    zarray = (double *) G_calloc(alloc_points, sizeof(double)) ;
    
    while( G_getl2(buff,BUFFSIZE-1,ascii) != 0 )
    {
	n_cats=0;
	if (buff[0] == '\0') {
	    G_debug(3, "a2b: skipping blank line");
	    continue;
	}

	if (  sscanf(buff, "%1c%d%d", &ctype, &n_coors, &n_cats) < 2  || n_coors < 0 || n_cats < 0 ) {
	    if (ctype == '#') {
		G_debug(2, "a2b: skipping commented line");
		continue;
	    }
	    G_warning (_("Error reading ASCII file: %s"), buff);
	    return -1;
	}
	if (ctype == '#') {
	    G_debug(2, "a2b: Skipping commented line");
	    continue;
	}
	
	switch(ctype){
	case 'A':
	    type = GV_BOUNDARY ;
	    break ;
	case 'B':
	    type = GV_BOUNDARY ;
	    break ;
	case 'C':
	    type = GV_CENTROID ;
	    break ;			
	case 'L':
	    type = GV_LINE ;
	    break ;
	case 'P':
	    type = GV_POINT ;
	    break ;
	case 'F':
	    type = GV_FACE ;
	    break ;
	case 'K':
	    type = GV_KERNEL ;
	    break ;
	case 'a':
	case 'b':
	case 'c':
	case 'l':
	case 'p':
	    type = 0; /* dead -> ignore */
	    break;
	default:
	    G_warning (_("Error reading ASCII file: %s"), buff) ;
	    return -1;
	}
	G_debug(5, "feature type = %d", type);
	
	n_points = 0 ;
	x = xarray ;
	y = yarray ;
	z = zarray ;		
	
	/* Collect the points */
	for(i = 0; i < n_coors; i++)
	{
	    if ( G_getl2 (buff, BUFFSIZE-1, ascii) == 0 ) {
		G_warning (_("End of ASCII file reached before end of coordinates")) ;
		return -1;
	    }
	    if (buff[0] == '\0') {
		G_debug(3, "a2b: skipping blank line while reading vertices");
		i--;
		continue;
	    }
	    
	    *z=0;
	    if ( sscanf(buff, "%lf%lf%lf", x, y, z) < 2 ) {			
		G_warning (_("Error reading ASCII file: %s"), buff) ;
		return -1;
	    }    
	    G_debug( 5, "coor in: %s -> x = %f y = %f z = %f", G_chop(buff), *x, *y, *z);
	    
	    n_points++;
	    x++;
	    y++;
	    z++;			
	    
	    if (n_points >= alloc_points)
	    {
		alloc_points = n_points + 1000 ;
		xarray = (double *) G_realloc((void *)xarray, alloc_points * sizeof(double) );
		yarray = (double *) G_realloc((void *)yarray, alloc_points * sizeof(double) );
		zarray = (double *) G_realloc((void *)zarray, alloc_points * sizeof(double) );
		x = xarray + n_points ;
		y = yarray + n_points ;
		z = zarray + n_points ;
	    }
	}
	
	/* Collect the cats */
	for( i=0; i<n_cats; i++)
	{
	    if ( G_getl2(buff,BUFFSIZE-1,ascii) == 0 ) {
		G_warning (_("End of ascii file reached before end of categories.")) ;
		return -1;
	    }
	    if (buff[0] == '\0') {
		G_debug(3, "a2b: skipping blank line while reading category info");
		i--;
		continue;
	    }
	    
	    if ( sscanf(buff, "%u%u", &catn, &cat) != 2 ) {
		G_warning (_("Error reading categories: %s"), buff) ;
		return -1;
	    }
	    Vect_cat_set ( Cats, catn, cat );
	}
	
	/* Allocation is handled for line_pnts */
	if (0 > Vect_copy_xyz_to_pnts (Points, xarray, yarray, zarray, n_points))
	    G_fatal_error(_("Out of memory"));
	
	if ( type > 0 ) {
	    newline = Vect_write_line ( Map, type, Points, Cats );
	    if (List) {
		Vect_list_append(List, newline);
	    }
	    nlines++;

	    Vect_reset_cats ( Cats ); 
	}
    }

    G_message(_("%d features added"), nlines);

    return nlines;	
}

int 
read_head ( FILE * dascii, struct Map_info *Map )
{
    char buff[1024];
    char *ptr;
    
    for (;;)
    {
	if (0 == G_getl2(buff, sizeof(buff) -1, dascii))
	    return 0;
	
	/* Last line of header */
	if (strncmp (buff, "VERTI:", 6) == 0)	  
	    return 0;
	
	if (!(ptr = G_index (buff, ':')))
	    G_fatal_error(_("Unexpected data in vector head: %s"), buff);
	
	ptr++;			/* Search for the start of text */
	while (*ptr == ' ')
	    ptr++;
	
	if (strncmp (buff, "ORGANIZATION:", 12) == 0)
	    Vect_set_organization ( Map, ptr );  
	else if (strncmp (buff, "DIGIT DATE:", 11) == 0)
	    Vect_set_date ( Map, ptr );  
	else if (strncmp (buff, "DIGIT NAME:", 11) == 0)
	    Vect_set_person ( Map, ptr );  
	else if (strncmp (buff, "MAP NAME:", 9) == 0)
	    Vect_set_map_name ( Map, ptr );  
	else if (strncmp (buff, "MAP DATE:", 9) == 0)
	    Vect_set_map_date ( Map, ptr );  
	else if (strncmp (buff, "MAP SCALE:", 10) == 0)
	    Vect_set_scale ( Map, atoi (ptr) );  
	else if (strncmp (buff, "OTHER INFO:", 11) == 0)
	    Vect_set_comment ( Map, ptr );  
	else if (strncmp (buff, "ZONE:", 5) == 0 || strncmp (buff, "UTM ZONE:", 9) == 0)
	    Vect_set_zone ( Map, atoi (ptr) );  
	else if (strncmp (buff, "WEST EDGE:", 10) == 0) {}
	else if (strncmp (buff, "EAST EDGE:", 10) == 0) {}
	else if (strncmp (buff, "SOUTH EDGE:", 11) == 0) {}
	else if (strncmp (buff, "NORTH EDGE:", 11) == 0) {}
	else if (strncmp (buff, "MAP THRESH:", 11) == 0)
	    Vect_set_thresh ( Map, atof (ptr) );  
	else
        {
	  G_warning(_("Unknown keyword <%s> in vector head"), buff);
	}
    }
    /* NOTREACHED */
}

/* 
 * close lines (boudaries) with regard to threshold distance
 * if threshold distance < 0.0 -> no limit
 *
 * return number of modified lines
 */
int do_close(struct Map_info *Map, int ltype,
	     double thresh)
{
    int nlines, line, type, nlines_modified, newline;
    int npoints;
    double *x, *y, *z;
    double dist;

    struct line_pnts *Points;
    struct line_cats *Cats;

    Points = Vect_new_line_struct();
    Cats   = Vect_new_cats_struct();

    nlines_modified = 0;

    Vect_build_partial (Map, GV_BUILD_BASE, NULL);
    nlines = Vect_get_num_lines (Map);

    for (line = 1; line <= nlines; line++) {
	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

	if (!(type & ltype))
	    continue;

	npoints = Points -> n_points - 1;
	x = Points -> x;
	y = Points -> y;
	z = Points -> z;

	dist = Vect_points_distance (x[npoints], y[npoints], z[npoints],
				     x[0], y[0], z[0], WITHOUT_Z);

	if (dist > 0 && (thresh < 0.0 || dist <= thresh)) {
	    Vect_line_delete_point (Points, npoints);
	    Vect_append_point (Points, x[0], y[0], z[0]); 
	    
	    newline = Vect_rewrite_line (Map, line, type, Points, Cats);
	    if (newline < 0)  {
		G_warning(_("Cannot rewrite line %d"), line);
		return -1;
	    }
	    nlines_modified++;
	}
    }

    G_message (_("%d lines closed"), nlines_modified);

    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);

    return nlines_modified;
}

/*
 * snap lines given in List to the nearest feature
 *
 * return number of snapped lines
 */
int do_snapping(struct Map_info *Map, struct ilist* List, int layer,
		double thresh, int to_vertex)
{
    int i, line, type, npoints, node;
    int line_to_snap;
    int nlines_modified;

    struct line_pnts *Points;
    struct line_cats *Cats;
    struct ilist *List_snap, *List_updated;

    nlines_modified = 0;

    Points = Vect_new_line_struct();
    Cats   = Vect_new_cats_struct();

    List_snap = Vect_new_list();
    List_updated = Vect_new_list();

    for(i = 0; i < List -> n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);
	npoints = Points -> n_points;

	for (node = 0; node < npoints; node += npoints - 1) {

	    line_to_snap = Vect_find_line(Map, Points -> x[node],
					  Points -> y[node], Points -> z[node],
					  -1, thresh, WITHOUT_Z, line);

	    if(line_to_snap > 0 && Vect_line_alive(Map, line_to_snap)) {
		Vect_list_append(List_snap, line_to_snap);
		Vect_list_append(List_snap, line); /* add line which should be snapped */
		if (do_snap(Map, List_snap, 0, layer, List_updated)) {
		    line = List_updated->value[0];
		    nlines_modified++;
		}
		Vect_reset_list(List_snap);
	    }
	    if (npoints == 1) {
		break;
	    }
	}
    }

    Vect_destroy_list (List_snap);
    Vect_destroy_list (List_updated);

    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);

    return nlines_modified;
}
