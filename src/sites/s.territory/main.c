/* Written by Bill Brown, UIUC GIS Laboratory
 */

#include <stdlib.h>
#include <strings.h>
#include "gis.h"
#include "site.h"
#include "local_proto.h"

typedef int FILEDESC;

int main (int argc, char *argv[])
{

    struct Option 	*rast, *in, *out, *claim, *tfld, *ifld;
    char		*cellmap, *sitesmap;
    FILEDESC    	cellfile = (FILEDESC) 0;
    FILEDESC    	claimfile = (FILEDESC) 0;
    FILE                *in_sfd = NULL, *out_sfd = NULL;
    FCELL		*dbuf, *ibuf; 
    char 		errbuf[100];
    int			row, col, Thresh_fld=1, Icr_fld=0, shh=0; 
    struct Cell_head    w;


    G_gisinit (argv[0]);

    rast = G_define_option();
    rast->key            	   = "rast";
    rast->type           	   = TYPE_STRING;
    rast->required     	           = YES;
    rast->gisprompt    		   = "old,cell,raster";
    rast->description  		   = "Name of incidence or density file.";

    in = G_define_option();
    in->key                    = "sites";
    in->type                   = TYPE_STRING;
    in->required               = YES;
    in->gisprompt    	       = "old,site_lists,sites";
    in->description            = "Name of sites file with threshold locations.";

    out = G_define_option();
    out->key                    = "output";
    out->type                   = TYPE_STRING;
    out->required               = NO;
    out->gisprompt    	        = "new,site_lists,sites";
    out->description            = "Name of new sites file to contain radius.";

    claim = G_define_option();
    claim->key            	   = "claim";
    claim->type           	   = TYPE_STRING;
    claim->required     	   = NO;
    claim->gisprompt    	   = "new,cell,raster";
    claim->description  = "Simultaneous growth map. (ignored if site output given)";

    tfld = G_define_option();
    tfld->key                    = "thresh";
    tfld->type                   = TYPE_INTEGER;
    tfld->def                    = "1";
    tfld->required               = NO;
    tfld->description            = "Field in sites file containing thresholds.";

    ifld = G_define_option();
    ifld->key                    = "incr";
    ifld->type                   = TYPE_INTEGER;
    ifld->def                    = "2";
    ifld->required               = NO;
    ifld->description            = "Field in sites file containing radius increment (meters).";

    if (G_parser (argc, argv))
	exit (-1);

    cellmap = G_find_file2 ("cell", rast->answer, "");
    if(!cellmap){
	sprintf(errbuf,"Couldn't find raster file %s", rast->answer);
	G_fatal_error(errbuf);
    }
    if ((cellfile = G_open_cell_old(rast->answer, cellmap)) == -1)
    {
	sprintf(errbuf,"Not able to open cellfile for [%s]", rast->answer);
	G_fatal_error(errbuf);
    }
 
    sitesmap = G_find_file2 ("site_lists", in->answer, "");
    if(!sitesmap){
	sprintf(errbuf,"Couldn't find sites file %s", in->answer);
	G_fatal_error(errbuf);
    }
    if ((in_sfd = G_fopen_sites_old(in->answer, sitesmap)) == NULL)
    {
	sprintf(errbuf,"Not able to open sites for [%s]", in->answer);
	G_fatal_error(errbuf);
    }

    if(out->answer){
	if ((out_sfd = G_fopen_sites_new(out->answer)) == NULL)
	{
	    sprintf(errbuf,"Not able to open sites for [%s]", out->answer);
	    G_fatal_error(errbuf);
	}
    }
    else if(claim->answer){
	if ((claimfile = G_open_raster_new (claim->answer,  CELL_TYPE)) == -1)
	{
	    sprintf(errbuf,"Not able to open cellfile for [%s]", claim->answer);
	    G_fatal_error(errbuf);
	}
    }
    else
	shh = 1;

    if(tfld->answer){
	sscanf(tfld->answer,"%d",&Thresh_fld);
    }
    if(ifld->answer){
	sscanf(ifld->answer,"%d",&Icr_fld);
    }
    
    G_get_set_window (&w);
    ibuf = (FCELL *)G_malloc (w.cols * w.rows * sizeof (FCELL));  
    dbuf = (FCELL *)G_malloc (w.cols * w.rows * sizeof (FCELL));  
    /* dbuf could be CELL_TYPE for simultaneous, but is used to
       hold fp distances for sequential */
  
    if(!shh)
	fprintf(stderr,"Reading %s...",rast->answer);
    {
	FCELL *tf;

	tf = ibuf;
	for (row = 0; row < w.rows; row++) {

	    if(!shh)
		G_percent(row, w.rows - 1, 10);

	    G_get_f_raster_row(cellfile, tf, row);

	    for(col=0; col < w.cols; col++){
		if (G_is_f_null_value (tf)) *tf = 0.0;
		tf++;
	    }

	}
    }
    fprintf(stderr,"\n");

    {
	int ndim, nstrings, ndoubles, nsites;
	RASTER_MAP_TYPE rtype;
	Site *cursite, *outsite;
	Site **sitelist;
	double e_ing, n_ing, thresh, incr, dist;

	G_site_describe (in_sfd, &ndim, &rtype, &nstrings, &ndoubles);
	if(ndoubles < Thresh_fld) 
	    G_fatal_error("Threshold field not present in sites file.");
	if(ndoubles < Icr_fld) 
	    G_fatal_error("Radius increment field not present in sites file.");

	if(claim->answer){
	    sitelist = gu_load_cached_sites(in_sfd, 0, 1, shh, &w, &nsites);
	    if(nsites < 1) 
		G_fatal_error("Error reading sites file.");
	    calc_simultaneous_territory(sitelist, nsites, Thresh_fld, Icr_fld,
					ibuf, dbuf, &w, shh);
	    gu_free_cached_sites(sitelist, nsites);

	    write_imap_fromf(claimfile, dbuf, &w);
	    G_close_cell(claimfile);

	}
	else{
	    cursite = G_site_new_struct (rtype, ndim, nstrings, ndoubles);
	    if(out->answer)
		outsite = G_site_new_struct (rtype, ndim, nstrings, ndoubles+1);

	    while(G_site_get (in_sfd, cursite) >= 0){
		if( G_site_in_region (cursite, &w) ){
		    e_ing = G_adjust_easting (cursite->east, &w);
		    n_ing = cursite->north;
		    thresh = cursite->dbl_att[Thresh_fld-1];
		    incr = cursite->dbl_att[Icr_fld-1];

		    dist = calc_min_territory(e_ing, n_ing, thresh, incr,
					      ibuf, dbuf, &w, shh);
		}
		else dist = 0.0;
		
		if(out->answer){
		    copy_sitedata(outsite, cursite);
		    outsite->dbl_att[ndoubles] = dist;
		    G_site_put (out_sfd, outsite); /* , (rtype != -1)); */
		}
		else
		    fprintf(stdout,"%f\n", dist);
	    }
	}

    }

    free(ibuf);
    free(dbuf);
    G_close_cell(cellfile);
    fclose(in_sfd);
    fclose(out_sfd);
    
    return(1);

}

int copy_sitedata (Site *to, Site *from)
{
int i;

   to->east = from->east;
   to->north = from->north;

   for (i=0; i<to->dim_alloc && i<from->dim_alloc; i++){
       to->dim[i] = from->dim[i];
   }
   for (i=0; i<to->str_alloc && i<from->str_alloc; i++){
       strcpy(to->str_att[i], from->str_att[i]);
   }
   for (i=0; i<to->dbl_alloc && i<from->dbl_alloc; i++){
       to->dbl_att[i] = from->dbl_att[i];
   }

   to->ccat = from->ccat;
   to->fcat = from->fcat;
   to->dcat = from->dcat;

   return 0;
}



