#include "externs.h"
#include "gis.h"

opencell (fullname, name, mapset)
char *fullname;
char *name;
char *mapset;
{
	char *m;
	int fd;

	strcpy (name, fullname);
	m = G_find_cell2 (name, "");
	if (m == NULL) {
	    fprintf (stderr, "warning: %s - raster file not found\n", name);
	    return -1;
	}
	if (strlen (m) == 0)
	    strcpy (mapset, G_mapset ());
	else
	    strcpy (mapset, m);
	fd = G_open_cell_old (name, mapset);
	if (fd < 0)
	    fprintf (stderr, "warning: unable to open [%s] in [%s]\n",
		    name, mapset);
	return fd;
}


show_cat (width, mwidth, name, mapset, cat, label, map_type)
    char *name, *mapset, *label;
    RASTER_MAP_TYPE map_type;
{
    char *fname;
    char buf[100];
    CELL cell_val;

    cell_val = cat;
    if(map_type != CELL_TYPE)
       sprintf(buf, ", quant  ");
    else
       sprintf(buf, " ");
    {
	if(G_is_c_null_value(&cell_val))
	{
	    if (!isatty(fileno(stdout)))
	        fprintf (stdout, "%*s in %-*s%s (Null)%s\n", width, name, mwidth, mapset, buf, label);
	    fprintf (stderr, "%*s in %-*s%s (Null)%s\n", width, name, mwidth, mapset, buf, label);
	}
	else
	{
	    if (!isatty(fileno(stdout)))
	        fprintf (stdout, "%*s in %-*s%s (%d)%s\n", width, name, mwidth, mapset, buf, cat, label);
	    fprintf (stderr, "%*s in %-*s%s (%d)%s\n", width, name, mwidth, mapset, buf, cat, label);
        }
    }
}

show_dval (width, mwidth, name, mapset, dval, label)
    char *name, *mapset ;
    DCELL dval;
    char  *label;
{
    DCELL dcell_val;
    char *fname;

    dcell_val = dval;
    {
	if(G_is_d_null_value(&dcell_val))
        {
	    if (!isatty(fileno(stdout)))
	        fprintf (stdout, "%*s in %-*s, actual  (Null)%s\n", width, name, mwidth, mapset, label);
	    fprintf (stderr, "%*s in %-*s, actual  (Null)%s\n", width, name, mwidth, mapset, label);
	}
	else
        {
	    if (!isatty(fileno(stdout)))
	        fprintf (stdout, "%*s in %-*s, actual  (%lf)%s\n", width, name, mwidth, mapset, dval, label);
	    fprintf (stderr, "%*s in %-*s, actual  (%lf)%s\n", width, name, mwidth, mapset, dval, label);
	}
    }
}

show_utm (north, east, window)
    double north, east;
    struct Cell_head *window;
{
    char e[50], n[50];

    if (window->proj == PROJECTION_LL && !isatty (fileno (stdout)))
    {
	/* format in decimal rather than d.m.s */
	G_format_northing (north, n, -1);
	G_format_easting  (east,  e, -1);
    }
    else
    {
	G_format_northing (north, n, window->proj);
	G_format_easting  (east,  e, window->proj);
    }

    {
	if (window->proj != PROJECTION_LL)
	{
	    strcat (n, "(N)");
	    strcat (e, "(E)");
	}

	if (!isatty(fileno(stdout)))
	    fprintf (stdout, "\n%s %s\n", e, n);
	fprintf (stderr, "\n%s %s\n", e, n);
    }
}

show_what (east, north, elevation)
    double east, north, elevation;
{
    struct Categories Cats[4];
    int Fd[4];
    int Nlayers;
    char Name[4][80];
    char Mapset[4][80];
    int width, mwidth;
    int i;
    int row, col;
    int nrows, ncols;
    CELL *buf, null_cell;
    DCELL *dbuf, null_dcell;
    RASTER_MAP_TYPE map_type[4];

    nrows = wind.rows;
    ncols = wind.cols;
    row = (wind.north - north) / wind.ns_res ;
    col = (east - wind.west) / wind.ew_res ;
    if (row < 0 || row >= nrows) return;
    if (col < 0 || col >= ncols) return;

    buf = G_allocate_c_raster_buf();
    dbuf = G_allocate_d_raster_buf();

    width = 0;
    mwidth = 0;

    /* open cells & set number of layers */
    Nlayers = 0;
    if ((Fd[Nlayers] = 
		opencell (Elevname, Name[Nlayers], Mapset[Nlayers])) >= 0)
	Nlayers++;
    if ((Fd[Nlayers] = 
		opencell (Cellname[0], Name[Nlayers], Mapset[Nlayers])) >= 0)
	Nlayers++;
    if (Three_map){
	if ((Fd[Nlayers] = 
	        opencell (Cellname[1], Name[Nlayers], Mapset[Nlayers])) >= 0)
	    Nlayers++;
	if ((Fd[Nlayers] = 
	        opencell (Cellname[2], Name[Nlayers], Mapset[Nlayers])) >= 0)
	    Nlayers++;
    }

    /*  get cats & map_types */

    for (i=0; i < Nlayers; i++)
    {
        int n;

        n = strlen (Name[i]);
        if (n > width) width = n;

        n = strlen (Mapset[i]);
        if (n > mwidth) mwidth = n;

	if (G_read_cats (Name[i], Mapset[i], &Cats[i]) < 0)
	    Cats[i].ncats = -1;

	map_type[i] = G_raster_map_type(Name[i], Mapset[i]);
    }

    {
        show_utm (north, east, &wind);
	fprintf(stderr,"row: %d\tcol:%d\n", row, col);
	fprintf(stderr,"interpolated elevation: %f\n", elevation);
	fprintf(stderr,"ELEVATION FILE:\n");

	G_set_c_null_value(&null_cell,1);
	G_set_d_null_value(&null_dcell,1);

        for (i = 0; i < Nlayers; i++) {
	    
	    if(i == 1)
		fprintf(stderr,"COLOR FILE%c:\n", Nlayers > 2 ? 'S': ' ' );

            if(map_type[i] == CELL_TYPE){
		if (G_get_c_raster_row (Fd[i], buf, row) < 0)
		    show_cat (width, mwidth, Name[i], Mapset[i], null_cell,
			"ERROR reading cell file", map_type[i]);
		else if (-1 == Cats[i].ncats)
		    show_cat (width, mwidth, Name[i], Mapset[i], buf[col],
			    "No category", map_type[i]);
                else
		    show_cat (width, mwidth, Name[i], Mapset[i], buf[col],
		     G_get_c_raster_cat (&buf[col], &Cats[i]), map_type[i]);
            }

	    else /* fp map */ {
		if (G_get_d_raster_row (Fd[i], dbuf, row) < 0)
		    show_dval (width, mwidth, Name[i], Mapset[i], null_dcell,
			"ERROR reading fcell file");
		else if (-1 == Cats[i].ncats)
		    show_dval (width, mwidth, Name[i], Mapset[i], dbuf[col],
			    "No category", map_type[i]);
		else
		    show_dval (width, mwidth, Name[i], Mapset[i], dbuf[col],
			G_get_d_raster_cat(&dbuf[col], &Cats[i]));
	    }

        }
        for (i = 0; i < Nlayers; i++)
	    G_close_cell(Fd[i]);
    }

    free (buf);
    free (dbuf);
}


void
show_where_viewpt()
{
double east, north, elev, p[3];
char ebuf[50], nbuf[50];

    
    east = FROM_TO[FROM][X]/XYscale + wind.west;
    north = FROM_TO[FROM][Y]/XYscale + wind.south;
    if(Z_exag)
	elev = FROM_TO[FROM][Z]/Z_exag + Z_Min_real;
    else
	elev = 0.0;

    G_format_northing (north, nbuf, wind.proj);
    G_format_easting  (east,  ebuf, wind.proj);
   
    if (wind.proj != PROJECTION_LL)
    {
	strcat (nbuf, "(N)");
	strcat (ebuf, "(E)");
    }

    fprintf(stderr,"\n%s  %s\nelevation: %.2lf\n", ebuf,nbuf,elev);

    p[X] = FROM_TO[FROM][X];
    p[Y] = FROM_TO[FROM][Y];
    if(1 == viewcell_interp(p))
	fprintf(stderr,"height: %.2lf\n", elev - p[Z]);
}
