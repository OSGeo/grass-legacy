/*  %W%  %G%  */

#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

extern Display* the_display;

graphics(infile)
	FILE *infile ;
{
	char buff[128] ;
	char *got_new ;
	char *fgets() ;
	char *do_poly() ;

	got_new = fgets(buff, 128, infile) ;

	while(got_new)
	{
		switch (*buff & 0177)
		{
		case 't':
			do_text(buff) ;
			XFlush(the_display);
			got_new = fgets(buff, 128, infile) ;
			break ;
		case 's':
			do_size(buff) ;
			got_new = fgets(buff, 128, infile) ;
			break ;
		case 'p':
			got_new = do_poly(buff, infile) ;
			XFlush(the_display);
			break ;
		case 'c':
			do_color(buff) ;
			got_new = fgets(buff, 128, infile) ;
			break ;
		case 'm':
			do_move(buff) ;
			got_new = fgets(buff, 128, infile) ;
			break ;
		case 'd':
			do_draw(buff) ;
			XFlush(the_display);
			got_new = fgets(buff, 128, infile) ;
			break ;
		case 'i':
			do_icon(buff) ;
			XFlush(the_display);
			got_new = fgets(buff, 128, infile) ;
			break ;
		case '#':
			got_new = fgets(buff, 128, infile) ;
			break ;
		default:
			printf("Don't understand: %s", buff) ;
			got_new = fgets(buff, 128, infile) ;
			break ;
		}
	}
}
