/* Written by Bill Brown, USA-CERL, NCSA, UI GMSL.
 */

#include <strings.h>
#include "gis.h"

typedef int FILEDESC;
double atof();

main(argc, argv)
    int argc;
    char *argv[];

{
    struct Option 	*rast, *sites;
    char		*cellmap, *sitesmap;
    FILEDESC    	cellfile = NULL;
    FILE                *sitesfile = NULL;
    CELL		*cellbuf; 
    FCELL		*fcellbuf; 
    DCELL		*dcellbuf; 
    char 		sitedesc[12], errbuf[100];
    int			row, col; 
    struct Cell_head    w;

    int                 mtype;
    Site                *s;

    G_gisinit (argv[0]);

    rast = G_define_option();
    rast->key            	   = "input";
    rast->type           	   = TYPE_STRING;
    rast->required     	           = YES;
    rast->gisprompt    		   = "old,cell,raster";
    rast->description  		   = "Name of raster file.";

    sites = G_define_option();
    sites->key                    = "output";
    sites->type                   = TYPE_STRING;
    sites->required               = YES;
    sites->gisprompt    	  = "new,site_lists,sites";
    sites->description            = "Name of new sites file.";

    if (G_parser (argc, argv))
	exit (-1);

    cellmap = G_find_file2 ("cell", rast->answer, "");
    if(!cellmap){
	sprintf(errbuf,"Couldn't find raster file %s", rast->answer);
	G_fatal_error(errbuf);
    }

    mtype = G_raster_map_type(rast->answer, cellmap);

    if ((cellfile = G_open_cell_old(rast->answer, cellmap)) == -1)
    {
	sprintf(errbuf,"Not able to open cellfile for [%s]", rast->answer);
	G_fatal_error(errbuf);
    }
 
    if ((sitesfile = G_fopen_sites_new(sites->answer)) == NULL)
    {
	sprintf(errbuf,"Not able to open sitesfile for [%s]", sites->answer);
	G_fatal_error(errbuf);
    }
    
    G_get_set_window (&w);

    if(NULL == (s = G_site_new_struct(mtype,2,0,0)))
	G_fatal_error("memory allocation failed for site");

    switch (mtype){
	case  CELL_TYPE:
	    cellbuf = G_allocate_c_raster_buf();
	    break;
	case  FCELL_TYPE:
	    fcellbuf = G_allocate_f_raster_buf();
	    break;
	case  DCELL_TYPE:
	    dcellbuf = G_allocate_d_raster_buf();
	    break;
    }
    
    {
	double east, north;

	for (row = 0; row < w.rows; row++) {

	    G_percent(row, w.rows - 1, 10);

	    s->north = G_row_to_northing((double)(row +.5), &w); 
	    switch (mtype){
		case  CELL_TYPE:
		     G_get_c_raster_row(cellfile,cellbuf,row);
		    break;
		case  FCELL_TYPE:
		     G_get_f_raster_row(cellfile,fcellbuf,row);
		    break;
		case  DCELL_TYPE:
		     G_get_d_raster_row(cellfile,dcellbuf,row);
		    break;
	    }

	    for(col=0; col < w.cols; col++){
		s->east = G_col_to_easting((double)(col +.5), &w); 
		switch (mtype){
		    case  CELL_TYPE:
			if (G_is_c_null_value (cellbuf+col)) continue;
			s->ccat = cellbuf[col];
			break;
		    case  FCELL_TYPE:
			if (G_is_f_null_value (fcellbuf+col)) continue;
			s->fcat = fcellbuf[col];
			break;
		    case  DCELL_TYPE:
			if (G_is_d_null_value (dcellbuf+col)) continue;
			s->dcat = dcellbuf[col];
			break;
		}
		G_site_put(sitesfile, s);
	    }

	}
    }

    switch (mtype){
	case  CELL_TYPE:
	    free(cellbuf);
	    break;
	case  FCELL_TYPE:
	    free(fcellbuf);
	    break;
	case  DCELL_TYPE:
	    free(dcellbuf);
	    break;
    }
    G_site_free_struct (s);
    G_close_cell(cellfile);
    fclose(sitesfile);
    
    return(1);

}





