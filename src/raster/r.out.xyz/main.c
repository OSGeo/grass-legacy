/* r.out.xyz
 * Written by Bill Brown, UI GMSL.
 * Wed Jun 25 1997
 */

#include "gis.h"

typedef int FILEDESC;

int main( int argc, char *argv[])
{

	struct GModule *module;
    struct Option 	*rast;
    char		*cellmap;
    FILEDESC    	cellfile = (FILEDESC) NULL;
    CELL		*cellbuf; 
    FCELL		*fcellbuf; 
    DCELL		*dcellbuf; 
    char 		errbuf[100];
    int			row, col; 
    struct Cell_head    w;

    int mtype;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Export GRASS raster files into xyz format.";

    rast = G_define_option();
    rast->key            	   = "input";
    rast->type           	   = TYPE_STRING;
    rast->required     	           = YES;
    rast->gisprompt    		   = "old,cell,raster";
    rast->description  		   = "Name of raster file.";

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
 
    G_get_set_window (&w);

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

	    north = G_row_to_northing((double)(row +.5), &w); 
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
		east = G_col_to_easting((double)(col +.5), &w); 
		switch (mtype){
		    case  CELL_TYPE:
			if (G_is_c_null_value (cellbuf+col)) 
			    fprintf(stdout,"%.6f %.6f NULL\n",east,north);
			else
			    fprintf(stdout,"%.6f %.6f %d\n",
				    east,north,cellbuf[col]);
			break;
		    case  FCELL_TYPE:
			if (G_is_f_null_value (fcellbuf+col))
			    fprintf(stdout,"%.6f %.6f NULL\n",east,north);
			else
			    fprintf(stdout,"%.6f %.6f %.6f\n",
				    east,north,fcellbuf[col]);
			break;
		    case  DCELL_TYPE:
			if (G_is_d_null_value (dcellbuf+col))
			    fprintf(stdout,"%.6f %.6f NULL\n",east,north);
			else
			    fprintf(stdout,"%.6f %.6f %.6f\n",
				    east,north,dcellbuf[col]);
			break;
		}
	    }

	}
    }

    switch (mtype){
	case  CELL_TYPE:
	    G_free(cellbuf);
	    break;
	case  FCELL_TYPE:
	    G_free(fcellbuf);
	    break;
	case  DCELL_TYPE:
	    G_free(dcellbuf);
	    break;
    }
    G_close_cell(cellfile);
    
    return(1);

}
