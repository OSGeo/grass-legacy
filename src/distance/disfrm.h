/* %W% %G% */

#define	  ZERO		0
#define   ONE		1
#define   TWO		2
#define	  FROM_MAXPTS	4000
#define	  FROM_MAXCELL  500000
#define   MAXDIST 255
#define   POSDISTS	MAXDIST

#define TO	1
#define FROM	2

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL int *table;
GLOBAL int table_len;
GLOBAL int dist[MAXDIST];
