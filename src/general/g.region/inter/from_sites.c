#include "glob.h"
#include "local_proto.h"
int 
from_sites(void)
{
    char name[30];
    char *mapset;
    struct Cell_head window;
    FILE *fp;
    int i, rtype, ndim, nstr, ndec;
    Site *mysite;

    mapset = G_ask_sites_old ("", name);
    if (!mapset) return 1;
    if (NULL == (fp = G_fopen_sites_old (name, mapset)))
    	fprintf (stderr, "can't read sites list file [%s in %s]. ",
		name, mapset);
    else
    {
        rtype = -1;
        G_site_describe(fp, &ndim, &rtype, &nstr, &ndec);
        mysite = G_site_new_struct(rtype, ndim, nstr, ndec);

        G_copy (&window, &cur_window, sizeof(window));
        
        for (i = 0; G_site_get (fp, mysite) == 0; i++)
        {
        	if (i==0)
        	{
        		window.east = window.west = mysite->east;
        		window.north = window.south = mysite->north;
        	}
        	else
        	{
        		if (mysite->east > window.east) 
        			window.east = mysite->east;
        		if (mysite->east < window.west) 
        			window.west = mysite->east;
        		if (mysite->north > window.north) 
        			window.north = mysite->north;
        		if (mysite->north < window.south) 
        			window.south = mysite->north;
        	}
        }
        G_free(mysite);
        fclose (fp);
        if (i)
        {
             window.east += 100;
             window.west -= 100;
             window.south -= 100;
             window.north += 100;
        
             if(window.north == window.south)
             {
                   window.north = window.north + 0.5 * cur_window.ns_res;
                   window.south = window.south - 0.5 * cur_window.ns_res;
             }
             if(window.east==window.west)
             {
                   window.west = window.west - 0.5 * cur_window.ew_res;
                   window.east = window.east + 0.5 * cur_window.ew_res;
             }
        }
        G_align_window (&window, &cur_window);

	if(!edit_window (&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}

