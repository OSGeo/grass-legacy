/* Written by Bill Brown, UIUC GIS Laboratory
 */

#include "enforce.h"
#define APP_VERSION 1.0

double atof();

main(argc,argv) 
    int argc;
    char *argv[];
{
    struct Option *inrast, *invect, *outrast, *outsite, *width, *depth;
    struct Flag *bequiet, *noflat;
    int n, ret;
    double swidth, sdepth, center[2];
    char defwidth[80], defdepth[80];

    char *vector_mapset, *rast_mapset, errbuf[128]; 
    FILEDESC infile, outfile;
    FILE *histfd;
    struct Map_info Map;
    RASTER_MAP_TYPE rtype;
    struct Cell_head  wind;

    G_gisinit (argv[0]);
    G_get_set_window (&wind);
    G_begin_distance_calculations();
    /* default width one cell at center */
    center[0] = (wind.east + wind.west) / 2.;
    center[1] = (wind.north + wind.south) / 2.;
    swidth = G_distance(center[0], center[1], 
			center[0] + wind.ew_res, center[1]);
    sprintf(defwidth,"%.6lf", swidth);
    sdepth = 0.0;
    sprintf(defdepth,"%.6lf", sdepth);

    inrast = G_define_option();
    inrast->key            = "rast";
    inrast->type           = TYPE_STRING;
    inrast->required       = YES;
    inrast->multiple       = NO;
    inrast->gisprompt      = "old,cell,raster";
    inrast->description    = "raster input file";

    invect = G_define_option();
    invect->key             = "vect";
    invect->type            = TYPE_STRING;
    invect->required        = YES;
    invect->multiple        = NO;
    invect->gisprompt       = "old,dig,vector";
    invect->description     = "vector input file";

    outrast = G_define_option();
    outrast->key            = "output";
    outrast->type           = TYPE_STRING;
    outrast->required       = YES;
    outrast->multiple       = NO;
    outrast->gisprompt      = "new,cell,raster";
    outrast->description    = "raster output file";

    outsite = G_define_option();
    outsite->key            = "site";
    outsite->type           = TYPE_STRING;
    outsite->required       = NO;
    outsite->multiple       = NO;
    outsite->gisprompt      = "new,site_lists,site";
    outsite->description    = "site output file for adjusted stream points";

    width = G_define_option();
    width->key            = "width";
    width->type           = TYPE_DOUBLE;
    width->required       = NO;
    width->multiple       = NO;
    width->description    = "width of streams (in meters)";
    width->answer         = defwidth;

    depth = G_define_option();
    depth->key            = "depth";
    depth->type           = TYPE_DOUBLE;
    depth->required       = NO;
    depth->multiple       = NO;
    depth->description    = "additional depth";
    depth->answer         = defdepth;

    bequiet = G_define_flag ();
    bequiet->key = 'q';
    bequiet->description = "quiet - do not show progress";

    noflat = G_define_flag ();
    noflat->key = 'n';
    noflat->description = "no flat areas allowed in flow direction";

    if (G_parser (argc, argv))
	exit (-1);

    if(width->answer)
        if(1 != sscanf(width->answer,"%lf", &swidth)) 
	    sscanf(defwidth,"%lf", &swidth);
    if(depth->answer)
        if(1 != sscanf(depth->answer,"%lf", &sdepth)) 
	    sscanf(defdepth,"%lf", &sdepth);

    /* Open files */

    vector_mapset = G_find_vector2 (invect->answer, "");
    if (vector_mapset == NULL)
    {
        sprintf (errbuf, "Vector map <%s> not found", invect->answer);
        G_fatal_error (errbuf);
    }
    if (2 > Vect_open_old (&Map, invect->answer, vector_mapset))
    {
        sprintf (errbuf, "You must run v.support on <%s> before running %s\n",
            invect->answer, G_program_name());
        G_fatal_error (errbuf);
    }

    rast_mapset = G_find_file2 ("cell", inrast->answer, "");
    if(!rast_mapset){
	sprintf(errbuf,"Couldn't find raster file %s", inrast->answer);
	G_fatal_error(errbuf);
    }
    if ((infile = G_open_cell_old(inrast->answer, rast_mapset)) == -1)
    {
	sprintf(errbuf,"Not able to open cellfile for [%s]", inrast->answer);
	G_fatal_error(errbuf);
    }
    rtype = G_raster_map_type(inrast->answer, rast_mapset);

    /* open new map */
    if ((outfile = G_open_raster_new(outrast->answer,  rtype)) < 0)
    {
        sprintf(errbuf,"Not able to open cellfile for [%s]", outrast->answer);
        G_fatal_error(errbuf);
    }
    ret = enforce_downstream(infile, outfile, outsite->answer, &Map, rtype, 
		    swidth, sdepth, noflat->answer, errbuf, bequiet->answer);

    G_close_cell(infile);
    G_close_cell(outfile);

    /* append command line to History file */
    if(NULL == (histfd = G_fopen_append("hist",outrast->answer)))
        fprintf(stderr,"Unable to update History file.\n");
    else{
        fprintf(histfd,"%s version: %.2f\n", argv[0], APP_VERSION);
        fprintf(histfd,"%s\n", G_recreate_command());
        fclose(histfd);
    }
    if(!bequiet->answer)
	fprintf(stderr,"%s\n", G_recreate_command());

    Vect_close(&Map);

    if(ret < 0)
	G_fatal_error (errbuf);
    if(ret > 0)
	G_warning (errbuf);

    return(ret);
}

/*
    Points = Vect_new_line_struct();
    outbuf = G_allocate_f_raster_buf();
    G_close_cell(cellfile);
    G_easting_to_col (east, region);
    G_northing_to_row (north, region);
    if (G_is_f_null_value (tf)) *tf = 0.0;

	switch (rtype){
	    case CELL_TYPE:
	    case FCELL_TYPE:
	    case DCELL_TYPE:
	}

*/
/*
    Note: Use rast input type for rast output 
    Read vect file, 
    for each line,
	use a shadow line struct to represent stream profile,
	    where x is distance along stream and y is elevation,
	    adding each point to lobf as it's created.
	find trend using lobf
	from high to low, lower any points forming dams
	    when next pnt elev increases,
	    find next point <= than last confirmed pt
	    just use linear interp for now
        write line to new raster  
	    Use orig line struct for XYs, shadow struct Y for cell val
	    if new raster already has value there, use lower?
		actually probably want to use val for trunk stream 
		and then verify branch in reverse
		- for now maybe create a conflict map
    After that's working, add width to lines? 
	or use r.grow
*/

