/*
 *   d.frame.new [frame=name] [at=top,bottom,left,right]
 *
 *   Establish a new window on the screen
 *   top, bottom, left, and right are % coordinates of window;
 *   0,0 is lower left; 100,100 is upper right
 */

#include "gis.h"
#include "raster.h"
#include "display.h"
#include "D.h"

int 
main (int argc, char *argv[])
{
	int stat;
	char name[256] ;
	float top, bottom, left, right;
	struct Option *frame, *at;

	frame = G_define_option();
	frame->key = "frame";
	frame->type = TYPE_STRING;
	frame->required = NO;
	frame->description = "Name to give to the new frame";
	frame->answer = NULL;

	at = G_define_option();
	at->key = "at";
	at->key_desc = "bottom,top,left,right";
	at->type = TYPE_STRING;
	at->required = NO;
	at->description = "Where to place the frame";
	at->answer = NULL;

	if (R_open_driver () != 0)
		G_fatal_error ("No graphics device selected");

	if (argc > 1 && G_parser(argc,argv))
		exit(1);
	if (at->answer)
	{
		if (4 != sscanf(at->answer,"%f,%f,%f,%f",
		    &bottom, &top, &left, &right))
		{
			G_usage();
			exit(1);
		}
		if (  bottom < 0.0 || top > 100.0 || bottom >= top
		    ||    left < 0.0 || right > 100.0 || left >= right)
		{
			fprintf (stderr, "** %s=%s ** illegal values\n",
			    at->key, at->answer);
			G_usage();
			exit(1);
		}
	}
	else
	{
		get_win_w_mouse(&top, &bottom, &left, &right) ;
	}

	if (frame->answer == NULL)
		R_pad_invent(frame->answer = name) ;

	stat = Dnew(frame->answer, bottom, top, left, right) ;
	if (stat == 0)
		Dchoose(frame->answer);

	R_close_driver() ;

	exit(stat) ;
}
