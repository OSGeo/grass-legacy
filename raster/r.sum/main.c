/* Written by Bill Brown, UIUC GIS Laboratory
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "glocale.h"

typedef int FILEDESC;

int main(
    int argc,
    char *argv[])
{

	struct GModule *module;
    struct Option 	*rast;
    char		*cellmap;
    FILEDESC    	cellfile = 0;
    FCELL		*dbuf, *ibuf; 
    char 		errbuf[100];
    int			row, col, shh=0; 
    struct Cell_head    w;


    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
		_("Sums up the raster cell values.");

    rast = G_define_option();
    rast->key            	   = "rast";
    rast->type           	   = TYPE_STRING;
    rast->required     	           = YES;
    rast->gisprompt    		   = "old,cell,raster";
    rast->description  		   = _("Name of incidence or density file.");


    if (G_parser (argc, argv))
	exit (-1);

    cellmap = G_find_file2 ("cell", rast->answer, "");
    if(!cellmap){
	G_fatal_error(_("Couldn't find raster file %s"), rast->answer);
    }
    if ((cellfile = G_open_cell_old(rast->answer, cellmap)) == -1)
    {
	G_fatal_error(_("Not able to open cellfile for [%s]"), rast->answer);
    }
 
    
    G_get_set_window (&w);
    ibuf = (FCELL *)G_malloc (w.cols * w.rows * sizeof (FCELL));  
    dbuf = (FCELL *)G_malloc (w.cols * w.rows * sizeof (FCELL));  
  
    if(!shh)
	fprintf(stderr,"Reading %s...",rast->answer);
    {
	FCELL *tf;
	double dsum=0.0;

	tf = ibuf;
	for (row = 0; row < w.rows; row++) {

	    if(!shh)
		G_percent(row, w.rows - 1, 10);

	    G_get_f_raster_row(cellfile, tf, row);

	    for(col=0; col < w.cols; col++){
		if (G_is_f_null_value (tf)) *tf = 0.0;
		dsum += *tf;
		tf++;
	    }

	}
    fprintf(stdout,"SUM = %f\n", dsum);
    }
    fprintf(stderr,"\n");


    free(ibuf);
    free(dbuf);
    G_close_cell(cellfile);
    
    return(0);

}
