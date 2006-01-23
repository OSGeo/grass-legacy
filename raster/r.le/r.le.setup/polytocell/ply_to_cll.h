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
void line(int, int, int, int, int);
void line_initialize(void);
void line_flush(void);
void yadjust();
void save_line();
void set_limits();
int find_area();
void save_area();
int do_line();
int write_end_record(int, int, int, int);

