/**** build.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


/* for vertex intersections */
#define A1B1 1
#define A2B2 2
#define A1B2 4
#define A2B1 8

/* Simple structure for point info */
struct point_t {
    double x;
    double y;
};

/* Simple structure for line segment info */
struct line_t {
    struct point_t p1;
    struct point_t p2;
};


/*
** A tribble is simply a 3 vertex line.  I had nothing better to call
**   it and it was late; what do you expect?
**
** It is used for analysing the line when there is a vertex/vertex intersection
** 
*/   

struct tribble {
    struct point_t p1;
    struct point_t p2;		/* vertex of intersection */
    struct point_t p3;
};

/*
**  identical to struct array_t except for 'data'
**
**  used for maintaining Active lists of intersections for A and B  polygons
**   in build.c
*/
struct array_p {
    struct t_data **data;
    int n_alloced;
    int num;
    int size;
};

struct bbox {
    double N, S, E, W;
};
