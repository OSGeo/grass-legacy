/*  %W%  %G%  */

/*
 *   Dwhich
 *
 *   Usage:  Dwhich horizontal vertical (screen % coordinates)
 *           Dwhich hori=num vert=num
 *
 *   Identify a window by screen % coordinates
 */

#include <stdio.h>
#define MAIN
#include "opt_which.h"

#define USAGE "hori=num vert=num"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char **list;
	char **pads;
	char cur_pad[64] ;
	int count;
	int i;
	int latest ;
	int n ;
	int new_time ;
	int npads;
	int p;
	int scr_top, scr_bot, scr_left, scr_rite ;
	int stat ;
	int x, y, t, b, l, r ;
	extern int stash_away() ;

/* Parse input */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	if (  X < 0.0 || X > 100.0
	||    Y < 0.0 || Y > 100.0)
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	R_open_driver();

/* Convert to screen coordinates */
	scr_top  = R_screen_top ();
	scr_bot  = R_screen_bot ();
	scr_left = R_screen_left ();
	scr_rite = R_screen_rite ();
	x = scr_left + (scr_rite - scr_left) * X / 100.0 + 1 ;
	y = scr_top + (scr_bot - scr_top) * Y / 100.0 + 1 ;

/* Set cur_win and time */
	latest = 0 ;
	strcpy(cur_pad, "") ;

	latest = 0 ;

/* Get list of pads (windows) */
	R_pad_list (&pads, &npads);

	for (p = 0; p < npads; p++)
	{
		stat = R_pad_select(pads[p]) ;
		if (stat)
		{
			R_pad_perror ("ERROR", stat);
			continue;
		}

/* Check each window's "d_win" */
		stat = R_pad_get_item ("d_win", &list, &count);
		if (stat)
		{
			R_pad_perror ("ERROR", stat);
			continue;
		}
		sscanf(list[0],"%d %d %d %d", &t, &b, &l, &r) ;
		R_pad_freelist(list, count) ;

/* If chosen point is outside pad window, continue */
		if ( x < l || x > r
		  || y < t || y > b)
			continue ;

/* If D_timestamp later than current, save pad name */
		stat = R_pad_get_item ("time", &list, &count);
		if (stat)
		{
			R_pad_perror ("ERROR", stat);
			continue;
		}
		sscanf(list[0],"%d",&new_time) ;
		R_pad_freelist(list, count) ;

		if (new_time > latest)
		{
			latest = new_time ;
			strcpy(cur_pad, pads[p]) ;
		}
	}

	printf("%s\n", cur_pad) ;

	R_close_driver();
	exit(0);
}
