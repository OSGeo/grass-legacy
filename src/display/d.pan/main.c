/*
 *   d.pan
 *
 *   Change region through graphics - select new center
 */

#include "gis.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
    int stat;
    char **rast, **vect;
    int nrasts, nvects;
    char command[128];
    struct Flag *quiet;
    struct Option *rmap, *vmap, *zoom;
    double magnify;
    char *mapset;
    int i;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    R_open_driver();

    if(D_get_cell_list (&rast, &nrasts) < 0)
               rast = NULL;
    if(D_get_dig_list (&vect, &nvects) < 0)
               vect = NULL;
    R_close_driver();
                    
    rmap = G_define_option();
    rmap->key = "rast";
    rmap->type = TYPE_STRING;
    rmap->multiple = YES;
    if (rast)
          rmap->answers = rast;
    rmap->required = NO;
    rmap->gisprompt = "old,cell,raster" ;
    rmap->description = "Name of raster map";
                                                        
    vmap = G_define_option();
    vmap->key = "vector";
    vmap->type = TYPE_STRING;
    vmap->multiple = YES;
    if (vect)
          vmap->answers = vect;
    vmap->required = NO;
    vmap->gisprompt = "old,dig,vector" ;
    vmap->description = "Name of vector map";
                                                        
    if(!rast && !vect)
    {
	    rmap->required = YES;
    }

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet";

    zoom = G_define_option() ;
    zoom->key        = "zoom" ;
    zoom->type       = TYPE_DOUBLE ;
    zoom->required   = NO ;
    zoom->answer     = "1.0" ;
    zoom->options    = "0.001-1000.0" ;
    zoom->description= "magnification: >1.0 zooms in, <1.0 zooms out" ;

    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    sscanf(zoom->answer,"%lf", &magnify); 

/* Make sure map is available */
    if (rmap->required == YES && rmap->answers == NULL)
	exit(0);

    if (rmap->answers)
    {
	if (!nrasts)
	{
		for(i=0; rmap->answers[i]; i++);
		nrasts = i;
	}
	for(i=0; i<nrasts; i++){
    		mapset = G_find_cell2 (rmap->answers[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Raster file [%s] not available", rmap->answers[i]);
			G_fatal_error(msg) ;
		}
	}
    }

    if (vmap->required == YES && vmap->answers == NULL)
	exit(0);

    if (vmap->answers)
    {
	if (!nvects)
	{
		for(i=0; vmap->answers[i]; i++);
		nvects = i;
	}
	for(i=0; i<nvects; i++){
    		mapset = G_find_vector2 (vmap->answers[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Vector file [%s] not available", vmap->answers[i]);
			G_fatal_error(msg) ;
		}
	}
    }

    /* if map was found in monitor: */
    if (rast || vect) 
       quiet->answer=1;
        
    R_open_driver();

    D_setup(0);

    if (G_projection() == PROJECTION_LL)
    {
    }

/* Do the pan */
    stat = pan(quiet->answer,magnify) ;

    R_close_driver();

    sprintf(command, "d.erase");
    system(command);

/* Redraw raster map */
    if (rast)
    {
      for(i=0; i<nrasts; i++)
      {
      	sprintf(command, "d.rast -o map=%s", rmap->answers[i]);
      	system(command);
      }
      R_pad_freelist(rast, nrasts);
    }

/* Redraw vector map */
    if (vect)
    {
      for(i=0; i<nvects; i++)
      {
      	sprintf(command, "d.vect map=%s", vmap->answers[i]);
      	system(command);
      }
      R_pad_freelist(vect, nvects);
    }
    

    exit(stat);
}

