/* %W% %G% */

/******************************************************************/
/* Subroutine map
 *
 * fromcell -|
 *           |- Readmap
 *           |- Fixpoints
 *           |- Fixmap -|- Arcdef
 *           |          |- Fastline
 *           |- Writemap 
 */
/******************************************************************/
#include   <stdio.h>
#include   <math.h>
#include   "disfrm.h"
#include "gis.h"

/* Note:
 * MAPTYPE mut be able to handle the number of distance zones requested
 */
#define MAPTYPE unsigned char

struct   COORD  { int row, col; } ;
struct   COORD   *PNTLIST ;

MAPTYPE *MAP ;

int  NUMPTS = 0 ;
int  NCOLS	;
int  NROWS	;
int  NUMDIST  ;
int  NUMPOINTS  ;
int *DISTANCE ;
char buffer[256] ;
char *malloc() ;
char *calloc() ;

fromcell (name, _NROWS, _NCOLS, _DISTANCE, _NUMDIST) 
    char *name ;
    int  _NCOLS	;
    int  _NROWS	;
    int *_DISTANCE ;
    int  _NUMDIST  ;

{
    int file ;
    unsigned mapsize ;
    MAPTYPE *mapptr ;
    CELL *cellptr ;
    CELL *cellbuf ;
    int k ;
    int j ;

#ifdef ERROR
    setbuf(stderr, NULL);
#endif

    NCOLS	  =   _NCOLS	;
    NROWS	  =   _NROWS	;
    NUMDIST   =   _NUMDIST  ;

    mapsize = NCOLS * NROWS ;

/* Read in the map of interest ***************************************/
    if ( (file = G_open_cell_old(name, G_mapset()) ) < 0)
	    G_fatal_error("Cell file not available\n") ;

    cellbuf = G_allocate_cell_buf() ;
    MAP = (MAPTYPE *)G_calloc(mapsize, sizeof(MAPTYPE)) ;

/* copy each row read into the MAP */
    mapptr = MAP;
    for( k = 0 ; k < NROWS ; k++)
    {
	G_get_map_row(file, cellptr = cellbuf, k) ;

	for (j = 0; j < NCOLS; j++)
	    *mapptr++ = *cellptr++;
    }

/* Allocate space for point list */
    NUMPOINTS = 0 ;
    for(mapptr = MAP, k = 0; k < mapsize; k++)
	if (*mapptr++ == 1)
	    NUMPOINTS++ ;
    
#ifdef ERROR
    fprintf(stderr,"Allocating space for %d points\n", NUMPOINTS) ;
#endif
    PNTLIST = (struct COORD *)(G_calloc(NUMPOINTS, sizeof(struct COORD))) ;

#ifdef ERROR
    fprintf (stderr, "About to enter Fixpoints\n");
#endif
    Fixpoints (NUMDIST + 1) ;

#ifdef ERROR
    fprintf (stderr, "About to enter Fixmap\n");
#endif
    Fixmap (_DISTANCE) ;

/* Make final corrections ****************************************************/
#ifdef ERROR
    fprintf (stderr, "Final corrections\n");
#endif
    for( k = 0 ; k < NROWS ; k++)
    {
	G_get_map_row(file, cellbuf, k) ;
	mapptr = MAP+k*NCOLS ;
	cellptr = cellbuf ;

	for (j = 0 ; j < NCOLS; j++)
	{
	    if (*cellptr)
		*mapptr = 0 ;
	    cellptr++ ;
	    mapptr++ ;
	}
    }

/* Write map out ***********************************************************/
    G_close_cell(file) ;

    if ( (file = G_open_cell_new(name) ) < 0)
	G_fatal_error("Cell file not available for writing\n") ;

    mapptr = MAP;
    for( k = 0 ; k < NROWS ; k++)
    {
	cellptr = cellbuf;

	for (j = 0; j < NCOLS; j++)
	    *cellptr++ = *mapptr++;
	if (G_put_map_row(file, cellbuf) < 0)
		G_fatal_error("Wrting map") ;
    }
    
    G_close_cell(file) ;
}


/***************************************************************/
/* loads point table and sets map to initial value
 */

Fixpoints (baseval)
    register int baseval ;

{
    register MAPTYPE *mapptr ;
    register int row, col;

    for (row=0; row < NROWS; row++)
    {
	mapptr = MAP + row * NCOLS ;
	for (col=0; col < NCOLS; col++)
	{
	    if ( *mapptr == ONE)
		 Plop (row, col) ;
	    *mapptr++ = baseval ;
	}
    }
    return ;
}


/******************************************************************/
/* deposits an item in PNTLIST									*/

Plop (row,col)

    int row, col ;

{
#ifdef ERROR
    fprintf(stderr,"Plop point# %d: col %d   row %d\n", NUMPTS, col, row) ;
#endif
    PNTLIST[NUMPTS].row = row ;
    PNTLIST[NUMPTS].col = col ;
    NUMPTS += 1 ;
}

/**********************************************************/
/* Loops through all distances; largest distance first
 *	 Loops through all points (to calculate distances from)
 *		 Fills circles around points
 */

Fixmap (DISTANCE)
    int *DISTANCE ;

{
    struct COORD *table, *AllocArcTable();
    register int distance, atcol, atrow ;
    int number, atnum ;
    int atpoint, atdist ;
    MAPTYPE *map ;

    table = AllocArcTable (DISTANCE[NUMDIST-1]);

    for (atdist=NUMDIST-1; atdist>=0; atdist--)
    {
	distance = atdist + 1 ;
#ifdef ERROR
	fprintf(stderr,"Doing distance:  %d: %d\n", DISTANCE[atdist], distance);
#endif
	Arcdef(DISTANCE[atdist],table,&number) ;
	for (atpoint=0; atpoint<NUMPTS; atpoint++) 
	{
	    atcol = PNTLIST[atpoint].col ;
	    atrow = PNTLIST[atpoint].row ;
#ifdef ERROR
	    fprintf(stderr,"Point # %d: col %d   row %d\n", atpoint, atcol, atrow) ;
#endif
	    for (atnum=0; atnum < number; atnum++) 
	    {
		Fastline( atcol+table[atnum].col, 
			  atcol-table[atnum].col, 
			  atrow+table[atnum].row, distance ) ;
		Fastline( atcol+table[atnum].col, 
			  atcol-table[atnum].col, 
			  atrow-table[atnum].row, distance ) ;
		Fastline( atcol+table[atnum].row, 
			  atcol-table[atnum].row, 
			  atrow+table[atnum].col, distance ) ;
		Fastline( atcol+table[atnum].row, 
			  atcol-table[atnum].row, 
			  atrow-table[atnum].col, distance ) ;
	    }
	}
    }
/*
    for (atpoint=0; atpoint<NUMPTS; atpoint++) 
    {
	atcol = PNTLIST[atpoint].col ;
	atrow = PNTLIST[atpoint].row ;
	if (Onmap(atrow,atcol)) 
	{ 
	    map = (MAP+(atrow)*NCOLS+atcol) ;
	    *map = 1 ;
	}
    }
*/
}

/***************************************************************************/
/*  Sets up a table of coordinates describing one-eighth of a circle.  From
 *  this set, an entire Cartesian circle can be drawn. 
 */

Arcdef(radius,table,number) 
    int radius, *number ;
    struct COORD *table;
{
    register double atx, aty, Radius2 ;
    register int n;
    double sqrt() ;

    n = 0 ;
    Radius2 = radius * radius ;

    for (atx=(double)radius, aty=0; atx>=aty; )
    {
	table[n].col = (int)(atx+.5) ;
	table[n++].row = (int)(aty+.5) ; 
	aty = aty + 1 ;
	atx = sqrt(Radius2-aty*aty) ;
    }
    *number = n;
}

struct COORD *
AllocArcTable (maxdist)
{
    register double atx, aty, Radius2 ;
    double sqrt() ;
    struct COORD *table;
    int n;

    n = 0 ;
    Radius2 = maxdist * maxdist ;

    for (atx=(double)maxdist, aty=0; atx>=aty; )
    {
	n++ ;
	aty = aty + 1 ;
	atx = sqrt(Radius2-aty*aty) ;
    }

    table = (struct COORD *) G_calloc (n, sizeof(struct COORD));
    return table;
}

/***************************************************************************/
/* Checks to change working map one line at a time 
 */
#define MIN(x,y)		(( x<y ) ? x : y)
#define MAX(x,y)		(( x>y ) ? x : y)

Fastline(x1, x2, y, val ) 
    int x1, x2, y, val ;

{
    register int d ;
    register int last ;
    register MAPTYPE *map ;
    int first ;

    if ( (y < 0) || (y >= NROWS) )
	return ;

    first = MIN(x1,x2) ;
    last = MAX(x1,x2) ;
    if (first < 0)
	first = 0 ;
    if (last >= NCOLS)
	last = NCOLS-1 ;
    if (first > last)
	return ;
    last++ ;

    map = MAP + y * NCOLS + first ;
    for (d = first; d < last; d++) 
	*map++ = val ;
    return(0) ;
}

/********************************************************************/
/* checks to make sure a set of coordinates are on the map
*/
Onmap (col,row)

    int col,row ;

{
    if (col < 0) return(0) ;
    if (row < 0) return(0) ;
    if (col >= NCOLS) return(0) ;
    if (row >= NROWS) return(0) ;
    return(1) ;
}

showmap()
{
    int i ;
    int j ;

    for (i=0; i<NROWS; i++)
    {
	for (j=0; j<NCOLS; j++)
	    fprintf(stderr, "%1d", MAP[i*NCOLS + j]) ;
	fprintf(stderr,"\n") ;
    }
    fprintf(stderr,"\n") ;
}
