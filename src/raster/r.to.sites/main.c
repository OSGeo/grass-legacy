/* Written by Bill Brown, USA-CERL, NCSA, UI GMSL.
 */

#include <stdlib.h>
#include <strings.h>
#include "gis.h"
#include "site.h"

typedef int FILEDESC;
double atof();


int main(int argc, char *argv[])
{

	struct GModule *module;
    struct Option 	*rast, *sites, *lab;
    struct Flag 	*attr, *dim;
    char		*cellmap, *sitesmap;
    FILEDESC    	cellfile = (FILEDESC) NULL;
    FILE                *sitesfile = NULL;
    CELL		*cellbuf; 
    FCELL		*fcellbuf; 
    DCELL		*dcellbuf; 
    char 		errbuf[100];
    int			row, col; 
    struct Cell_head    w;

    int                 mtype;
    Site                *s, *sa, *sd;


    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
		"Converts point data in a GRASS raster map "
		"layer into a GRASS site_lists file.";
					        
    attr = G_define_flag ();
    attr->key = 'a';
    attr->description = "Output as decimal attribute rather than cat";

    dim = G_define_flag ();
    dim->key = 'z';
    dim->description = "Output as third dimension rather than cat";

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

    lab = G_define_option();
    lab->key                    = "label";
    lab->type                   = TYPE_STRING;
    lab->required               = NO;
    lab->description            = "Label for site values";
    lab->answer                 = "No Label";

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

    if(NULL == (sa = G_site_new_struct(-1,2,0,1)))
	G_fatal_error("memory allocation failed for site");

    if(NULL == (sd = G_site_new_struct(-1,3,0,0)))
	G_fatal_error("memory allocation failed for site");

    {
    Site_head shead;
    DateTime dt;
    struct TimeStamp ts;
    int tz, ret;
    char buf[320];

	shead.name = G_store(sites->answer);
	shead.desc = G_store(G_recreate_command());
	if(1 == G_read_raster_timestamp (rast->answer, cellmap, &ts))
/* 
 * changed Sep 1999 bb - should only write timestamp if original raster
 * had a timestamp 
*/
/*
	datetime_set_type (&dt, DATETIME_ABSOLUTE, DATETIME_YEAR, 
			   DATETIME_SECOND, 0);
	datetime_get_local_time (&dt);
        datetime_get_local_timezone (&tz);
	datetime_set_timezone (&dt, tz);
	G_set_timestamp (&ts, &dt);
*/
	shead.time = &ts;
	else 
	    shead.time = (struct TimeStamp*)NULL;

	shead.form = shead.labels = shead.stime = (char *)NULL;

	if(attr->answer){
	    shead.form = G_store("||%");
	    sprintf(buf,"Easting|Northing|%%%s",lab->answer);
	    shead.labels = G_store(buf);
	}
	else if (dim->answer){
	    shead.form = G_store("|||");
	    sprintf(buf,"Easting|Northing|%s|",lab->answer );
	    shead.labels = G_store(buf);
	}
	else{
	    shead.form = G_store("||#");
	    sprintf(buf,"Easting|Northing|#%s",lab->answer );
	    shead.labels = G_store(buf);
	}

	G_site_put_head (sitesfile, &shead);
	free (shead.name);
	free (shead.desc);
	free (shead.form);
	free (shead.labels);
    }


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

	    G_percent(row, w.rows - 1, 2);

	    if(attr->answer){
		sa->north = G_row_to_northing((double)(row +.5), &w); 
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
		    sa->east = G_col_to_easting((double)(col +.5), &w); 
		    switch (mtype){
			case  CELL_TYPE:
			    if (G_is_c_null_value (cellbuf+col)) continue;
			    sa->dbl_att[0] = cellbuf[col];
			    break;
			case  FCELL_TYPE:
			    if (G_is_f_null_value (fcellbuf+col)) continue;
			    sa->dbl_att[0] = fcellbuf[col];
			    break;
			case  DCELL_TYPE:
			    if (G_is_d_null_value (dcellbuf+col)) continue;
			    sa->dbl_att[0] = dcellbuf[col];
			    break;
		    }
		    G_site_put(sitesfile, sa);
		}
	    }

	    else if (dim->answer){
		sd->north = G_row_to_northing((double)(row +.5), &w); 
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
		    sd->east = G_col_to_easting((double)(col +.5), &w); 
		    switch (mtype){
			case  CELL_TYPE:
			    if (G_is_c_null_value (cellbuf+col)) continue;
			    sd->dim[0] = cellbuf[col];
			    break;
			case  FCELL_TYPE:
			    if (G_is_f_null_value (fcellbuf+col)) continue;
			    sd->dim[0] = fcellbuf[col];
			    break;
			case  DCELL_TYPE:
			    if (G_is_d_null_value (dcellbuf+col)) continue;
			    sd->dim[0] = dcellbuf[col];
			    break;
		    }
		    G_site_put(sitesfile, sd);
		}
	    }

	    else{
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
    G_site_free_struct (sa);
    G_site_free_struct (sd);
    G_close_cell(cellfile);
    fclose(sitesfile);
    
    return(1);

}





