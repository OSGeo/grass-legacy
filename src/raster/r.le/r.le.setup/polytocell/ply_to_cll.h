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
