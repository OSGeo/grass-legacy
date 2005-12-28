/*@(#)ply_to_cll.h	2.1   6/26/87*/
#include <stdio.h>

#define POSITIVE	1
#define NEGATIVE	-1
#define ZERO		0
#define INFINITE	2
#define AREA	0
#define LINE	1
#define DOTS	2

#define MAX_VERTICIES	12288

struct element
{
	int row ;
	float col ;
} ;

/* quick and dirty declaration - module will be discontinued if nobody supports it*/
void do_dots();
void write_record();
line();
line_initialize();
line_flush();
save_line();
yadjust();
set_limits();
find_area();
save_area();
do_line();
write_end_record();

