#include "tape.h"
ask_info ()
{
    sprintf (tape_info.id[0],"%s %s %s", mission,date, scene_id);
    sprintf (tape_info.desc[1],"SUN ANGLES: %s", sun_angle);
    /*
    if (*image_scale)
	sprintf (tape_info.desc[2],"IMAGE SCALE: %s", image_scale);
    if (*projection)
	sprintf (tape_info.desc[3],"PROJECTION: %s", projection);
    */

    I_edit_tape_info (&tape_info);
}
