
#include "gis.h"
ask_camera (block, location, mapset)
    char *block;
    char *location;
    char *mapset;
{
    char buf[100], camera[100];
    char t1[80], t2[80], t3[80];
    char cur_location[30], cur_mapset[30];

    G_suppress_warnings(1);
    if (!I_get_block_camera (block, camera)) 
       strcpy (camera, "list");
    G_suppress_warnings(0);

    sprintf (t1, "Please select the camera reference file for block <%s>", block);
    sprintf (t2, "Camera reference file: ");

    V_clear();
    V_line (1, t1);
    V_line (4, t2);
    V_line (6, "(enter list for a list of existing camera reference files)");
    V_ques (camera, 's',  4, 28, 20);

    for(;;)
    {
	V_intrpt_ok();
	if (!V_call())
	    exit(0);
	if (*camera == 0)
	    exit(0);

        if (I_find_camera (camera)) break;

	if (strcmp (camera, "list") == 0)
	    I_list_cameras();
	/*printf ("Hit RETURN -->"); G_gets(buf);*/
    }
    I_put_block_camera(block,camera);
}










