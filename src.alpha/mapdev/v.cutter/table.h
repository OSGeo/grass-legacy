/**** table.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#ifndef CUTTER__TABLE_H__
#define CUTTER__TABLE_H__

#ifdef FORWARD
#undef FORWARD
#endif
#ifdef REVERSE
#undef REVERSE
#endif
#ifdef IN
#undef IN
#endif
#ifdef OUT
#undef OUT
#endif
#ifdef LEFT
#undef LEFT
#endif
#ifdef RIGHT
#undef RIGHT
#endif


#define FORWARD  1
#define REVERSE -1
#define FOR FORWARD
#define REV REVERSE

#define IN_USE	2
#define USED	1
#define NOTUSED	0

#define IN  1
#define OUT -1

#define RIGHT 1
#define LEFT -1

#define A_CODE 0
#define B_CODE 1

#define BASE_POLY 0


#define METHOD_MIDLINE 1
#define METHOD_VERTEX  2

/*
**  Keep track of areas that do and don't intersect
*/
#define POLY_INTERSECT   55
#define POLY_NOINTERSECT 54

/*
**  detailed Info about each intersection 
**   Used exclusively by t_data in the TABLE
*/
struct i_info {
    plus_t poly;	/* polygon of interest 1->N  */
    int    line;	/* order of line in polygon  0->N-1 always positive  */
    int    segment;	/* segment of line 1->N???  in polygon order */
    plus_t l_index;	/* real line number 1->N  +/-  */
    char   dir;		/* direction of line wrt poly.  not needed */
    char vertex;	/* intersect at vertex?  (not just NODE's) */
    int subpoly;	/* sub-polygon it is on  0: main  1-n: islands */
			/* for lines, reuse this as float distance */
};

struct table_base {
    struct t_data *Table;
    char *t_link;
};

/*
** Table data.  This is the table structure. containing almost
**  all the info needed to represent each intersection
**
**   This structure shares a lot of information with struct poly_vert_info
**
*/
struct t_data {
    struct i_info i[2];		/* 0 == A_CODE  1 == B_CODE */
    double x;
    double y;
    struct t_data *next;
    char used;		/* tri-state  USED,NOTUSED,IN_USE */
    char in_out;	/* -1 OUT  1 IN */
    int inter;		/* Intersection index */
};


/* structure of complete info for vertex w/in polygon */
struct poly_vert_info {
    int poly_code;	/* 0 A_CODE / 1 B_CODE  of poly of active line */
    plus_t poly;	/* active poly */
    int line;		/* # line in poly vertex is on (note, not nec. unique)*/
			    /*[0-N] this is not line index, but order in poly */
    int pos;		/* position w/in line [0-N] wrt polygon */
			/* if midline, then segment 0-N else vertex 0-N */

    int ppos;		/* absolute position w/in polygon (sub-poly)*/
    int midline;	/* is a mid-line intersection? */
    int inter;		/* intersection # if applicable else 0 for nodes */

    int subpoly;	/* sub-polygon it is on  0: main  1-n: islands */
    double x, y;
};


/******************************************************************************/
/*
**  This structure exists to provide a way to represent 
**   an area polygon including each of its island polygons
**   (in reverse direction of course)
**  This allows me to write generic code that treats a polygon
**   like a polygon, and the intersection just needs to record which
**   (sub)poly polygon it exists on.
*/

struct sub_poly {
    P_AREA *Area;		/* this is for P_AREA and P_ISLE */
				/* so can only use common elements */
    struct line_pnts *Points;	/* array of vertices of polygon */
    struct poly_info *info;	/* array 0 -> N-1 */
    int n_alloced;	       /* poly_info array alloced info */
    int num;		       /* total number of lines */
};

/* array of info per line in polygon 
**
**  Used by struct poly_t
*/
struct poly_info {
    int n_verts;	/* total number of verts in line */
    int dir;		/* direction of line  (not needed?) */
};

struct poly_t {
/*    P_AREA *Area;	*/
    struct sub_poly *spoly;	/* Array  0 -> N-1 */
    int n_polys;		/* Number of sub-polygons  1->N */
    int n_alloced;		/* number of spoly structures allocated */
};
/******************************************************************************/


/*
**  Structure to build network of intersections and record
**  arcs which have been already written out.
*/

struct span_t {
    int from;		/* From intersection */
    int to;		/* To   intersection */
    char written; 	/* Has span been written already? */
};


/*
** structures below are for code in label.c to find point in poly
**  these support the line intersection points stuff
*/
struct intersects {
    struct ipoints *points;
    int n_points;
    int n_alloced;
};

struct ipoints {
    double x;
    int poly;
};
	
#endif /* CUTTER__TABLE_H__ */
