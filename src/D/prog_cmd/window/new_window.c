/*  %W%  %G%  */

/*
 *   Dnew
 *
 *   Usage:  Dnew name bottom top left right
 *           Dnew name=name bottom=num top=num left=num right=num
 *           Dnew name   (for using mouse at tty)
 *           Dnew        (for using mouse at tty)
 *
 *   Establish a new window on the screen
 *   top, bottom, left, and right are % coordinates of window;
 *   0,0 is lower left; 100,100 is upper right
 */

#include <stdio.h>
#define MAIN
#include "opt_new.h"

#define USAGE "name=name bottom=num top=num left=num right=num"

main(argc, argv)
	char *argv[];
{
	int iTOP, iBOTTOM, iLEFT, iRIGHT;
	int n;
	int stat;
	int no_cur_win ;
	char **list ;
	int count ;
	char buff[256];
	char item[64] ;
	char cur_win[64] ;
	int stash_away() ;

	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	R_open_driver ();

	if (  bottom < 0. || top > 100. || bottom >= top
	||    left < 0. || right > 100. || left >= right)
	{
		if (isatty(0) && 
		    top == 0. && bottom == 0. && left == 0. && right == 0.)
		{
			get_win_w_mouse(&top, &bottom, &left, &right) ;
		}
		else
		{
			fprintf(stderr,"Illegal request:\n") ; ;
			fprintf(stderr,"bottom: %5.2f top: %5.2f left: %5.2f right: %5.2f\n",
				name, bottom, top, left, right) ;
			fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
			exit(1);
		}
	}

	if (strlen(name) == 0)
		R_pad_invent(name) ;

	stat = Dnew(name, bottom, top, left, right) ;

	R_close_driver() ;

	printf("%s\n", name) ;

	exit(stat) ;
}
