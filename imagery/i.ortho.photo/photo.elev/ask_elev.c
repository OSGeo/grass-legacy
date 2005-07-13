/* ask_elev .c */

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "vask.h"
#include "elev.h"

int ask_elev (char *group, char *location, char *mapset)
{
    char buf[100];
    char t1[80]; 
    char t2[80], t3[80];
    char cur_location[30], cur_mapset[30];

    sprintf (t1, "Please select the elevation raster file for group <%s>", group);
    sprintf (t2, "Elevation raster file: ");

    V_clear();
    V_line (1, t1);
    V_line (4, t2);
    V_line (6, "(enter list for a list of existing raster files)");
    V_ques (elev_layer, 's',  4, 28, 20);

    for(;;)
    {
	V_intrpt_ok();
	if (!V_call())
	    exit(0);
	if (*elev_layer == 0)
	    exit(0);

	if (strcmp (elev_layer, "list") == 0)
	{
	  G_set_list_hit_return(1);
	  G_list_element ("cell","raster","",0);
        }

        else if ((mapset_elev = G_find_cell(elev_layer,"")) == NULL) 
	{  
	  sprintf (buf,"\n\nraster-file %s not found - select another file\n",elev_layer);
	  G_warning (buf);
	  *elev_layer = 0;
	  continue;
	}
        else break;
    }
    /* mod_elev_data(); */
    return 0;
}




