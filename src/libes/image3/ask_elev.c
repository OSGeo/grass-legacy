/* ask_elev.c */
#include "gis.h"
#include <string.h>
#include "vask.h"

/*======================================================================
   ask_elevation

   Prompt the user to enter the name of an elevation raster map.  The
current environment is used to search for the map.  Return the name of
the elevation map in elev_map and the mapset it is in in elev_mapset.
elev_map and elev_mapset must be large enough to hold 30 character
strings.

   Return Value:
   0 if user entered name of an existing raster map.  Name returned
      in *elev_map, mapset returned in *elev_mapset.
   1 if user did not enter anything.
======================================================================*/
int 
ask_elevation (char *group, char *elev_map, char *elev_mapset, char *elev_units)
{
    char buf[100];
    char t1[80];
    char t2[80], t3[80], t4[80], t5[80], t6[80];
    char *mapset;
    char u_meters[3], u_feet[3];
    int status;
    
    *t2=0;
    sprintf (t1,"Please select the elevation cell file for group <%s>", group);
    sprintf (t2,"Elevation cell-file: ");
    sprintf (t3,"(enter list for a listing of available cell-files)");
    sprintf (t4,"Select either Meters or Feet as the elevation unit:");
    sprintf (t5,"                                Meters");
    sprintf (t6,"                                Feet");
    sprintf (u_meters,"x");
    sprintf (u_feet,  " ");

    /** End_curses(); */
    V_clear();
    V_line (1, t1);
    V_line (4, t2);
    V_line (6, t3);
    V_ques (elev_map, 's',  4, 28, 30);

    V_line (10, t4);
    V_line (11, t5);
    V_line (12, t6);
    V_ques (u_meters, 's', 11, 28, 2);
    V_ques (u_feet,   's', 12, 28, 2);


    status=1;
    for(;;)
    {
	V_intrpt_ok();
	if (!V_call())
      	  goto end;
	if (*elev_map == 0)
      	  goto end;
	if (strcmp (elev_map, "list") == 0) {
	  G_set_list_hit_return (1);
	  G_list_element ("cell","","",0);
	  G_set_list_hit_return (0);
          continue;
        }

        else if ((mapset = G_find_cell(elev_map,"")) == NULL) 
	{  
	  sprintf (buf,"\n\ncell-file %s not found - select another file\n",
		   elev_map);
	  G_warning (buf);
	  *elev_map = 0;
	  continue;
	}

        /* check that only one of the units selected */
        if ((strncmp (u_meters, "x", 1) == 0) &&
            (strncmp (u_feet, "x", 1) == 0))  continue;
        
        else if ((strncmp (u_meters, " ", 1) == 0) &&
                 (strncmp (u_feet, " ", 1) == 0))  continue;
        
        else if (strncmp (u_meters, "x", 1) == 0) {
	     strcpy (elev_units, "METERS"); 
	     break;
        }
        else if (strncmp (u_feet , "x", 1) == 0)  {
	     strcpy (elev_units, "FEET"); 
             break;
        }
    }

    /* mod_elev_data(); */
    G_strncpy(elev_mapset, mapset, 29);
    status=0;

end:
    /* Resume_curses(); */
    return(status);
}




