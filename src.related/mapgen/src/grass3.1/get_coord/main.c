/*  %W%  %G%  */

/*
 *   get_cord
 *
 *   Usage:  get_cord [window]
 *
 */

#include <stdio.h>
#include "mdef.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
double x,y,x1,y1;
char c;

if (argc == 2)
	c = *argv[1];
else c = get_pwin();
if (c == 'd'){
	get_margin(&x,&y,NULL);
	d_get_cord(x,y,&x1,&y1,1);
	}
else
        d_get_cord(0.0,0.0,&x1,&y1,1);
}
