#include "local_proto.h"

int redraw(void)
{
	int i;
	char command[128];

	R_close_driver();

        sprintf(command, "d.erase");
        system(command);
    
    /* Redraw raster map */
        if (rast)
        {
          for(i=0; i<nrasts; i++)
          {
          	sprintf(command, "d.rast -o map=%s", rast[i]);
          	system(command);
          }
        }
    
    /* Redraw vector map */
        if (vect)
        {
          for(i=0; i<nvects; i++)
          {
          	sprintf(command, "d.vect map=%s", vect[i]);
          	system(command);
          }
        }
        
    /* Redraw site map */
        if (site)
        {
          for(i=0; i<nsites; i++)
          {
          	sprintf(command, "d.sites sitefile=%s", site[i]);
          	system(command);
          }
        }

	R_open_driver();

	return 0;
}
