/*
 *   d.frame [-cps] [frame=name] [at=bottom,top,left,right]
 *
 *   -c create a new frame
 *   -s select a frame
 *   -p print name of the the current frame
 *
 *   frame=name   create/choose with this name
 *   at=...       create frame here (implies -c)
 *       top, bottom, left, and right are % coordinates of window;
 *       0,0 is lower left; 100,100 is upper right
 */

#include "gis.h"
#include "display.h"
#include "D.h"
#include <string.h>
#include <stdlib.h>
#include "raster.h"


int 
main (int argc, char *argv[])
{
    int check_at();
    char buf[1024];
    int create, select, print, debug;
	struct GModule *module;
    struct
    {
	struct Option *frame, *at;
    } parm;
    struct
    {
	struct Flag *debug;
	struct Flag *select;
	struct Flag *print;
	struct Flag *create;
	struct Flag *erase;
    } flag;

	module = G_define_module();
	module->description =
		"Manages display frames on the user's graphics monitor.";

    flag.create = G_define_flag();
    flag.create->key = 'c';
    flag.create->description = "Create a new frame";

    flag.erase = G_define_flag();
    flag.erase->key = 'e';
    flag.erase->description = "Remove all frames and erase the screen";

    flag.print = G_define_flag();
    flag.print->key = 'p';
    flag.print->description = "Print name of current frame";

    flag.select = G_define_flag();
    flag.select->key = 's';
    flag.select->description = "Select a frame";

    flag.debug = G_define_flag();
    flag.debug->key = 'D';
    flag.debug->description = "Debugging output";

    parm.frame = G_define_option();
    parm.frame->key = "frame";
    parm.frame->type = TYPE_STRING;
    parm.frame->required = NO;
    parm.frame->multiple = NO;
    parm.frame->description = "Frame to be created/selected";

    parm.at = G_define_option();
    parm.at->key = "at";
    parm.at->key_desc = "bottom,top,left,right";
    parm.at->type = TYPE_DOUBLE;
    parm.at->required = NO;
    parm.at->multiple = NO;
    parm.at->description = "Where to place the frame (implies -c)";
    parm.at->checker = check_at;

    if (G_parser(argc,argv))
	exit(1);

    R_open_driver();

    create = flag.create->answer;
    print  = flag.print->answer;
    select = flag.select->answer;
    debug  = flag.debug->answer;

    if (parm.at->answer)
    {
	create = 1;
    }
    if (flag.erase->answer)
    {
	if (!create)
	    Dscreen();
	else
	    Dclearscreen();
    }
    R_close_driver();
    if (create)
    {
	select = 0;
	sprintf (buf, "%s/etc/frame.create", G_gisbase());
	if (parm.frame->answer)
	{
	    strcat (buf, " frame='");
	    strcat (buf, parm.frame->answer);
	    strcat (buf, "'");
	}
	if (parm.at->answer)
	{
	    strcat (buf, " at='");
	    strcat (buf, parm.at->answer);
	    strcat (buf, "'");
	}
	if(system(buf)) exit(1);
    }
    if (select)
    {
	sprintf (buf, "%s/etc/frame.select", G_gisbase());
	if (parm.frame->answer)
	{
	    strcat (buf, " frame='");
	    strcat (buf, parm.frame->answer);
	    strcat (buf, "'");
	}
	if(system(buf)) exit(1);
    }

    if (debug)
    {
	sprintf (buf, "%s/etc/frame.dumper", G_gisbase());
	if(system (buf)) exit(1);
    }

    if (print)
    {
	R_open_driver();
	D_get_cur_wind(buf) ;
	D_set_cur_wind(buf) ;
	R_close_driver() ;
	fprintf (stdout,"%s\n", buf) ;
    }
    exit(0) ;
}

int 
check_at (char *s)
{
    float top, bottom, left, right;


    if (s == NULL) return 0;

    if(4 != sscanf(s,"%f,%f,%f,%f", &bottom, &top, &left, &right)
    ||    bottom < 0.0 || top > 100.0 || bottom >= top
    ||    left < 0.0 || right > 100.0 || left >= right)
    {
	fprintf (stderr, "<at=%s> invalid request\n", s);
	return 1;
    }
    return 0;
}
