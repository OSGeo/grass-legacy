
#include "circle.h"

/***************************************************************************
 *  Sets up a table of coordinates describing one-eighth of a circle.  From
 *  this set, an entire Cartesian circle can be drawn. 
 *
 *  Returns:
 *     number    the total number of coordinates calcuated
 *     table     the table of coordinates
 *
 *  Note:  the one-eighth of a circle covers the area 0 to 45 degrees
 */

Arcdef(radius,table,number) 
	int radius, *number ;
	struct table **table ;
{
	register double atx, aty, Radius2 ;
	double sqrt() ;

	*number = 0 ;
	Radius2 = radius * radius ;

	*table = (struct table *)(G_realloc(*table, sizeof(struct table) * radius )) ;

	for (atx=(double)radius, aty=0; atx>=aty; )
	{
		(*table)[*number].col = (int)(atx+.5) ;
		(*table)[*number].row = (int)(aty+.5) ; 
		(*number)++ ;
		aty++ ;
		atx = sqrt(Radius2-aty*aty) ;
	}
}


Circledef(radius,table,number) 
	int radius, *number ;
	struct table **table ;
{
	struct table *arctable ;
	int arcnumber ;
	int i ;

	Arcdef(radius,&arctable,&arcnumber)  ;

	*table = (struct table *)(G_realloc(*table, sizeof(struct table) * arcnumber * 8) ) ;
	*number = 0 ;

/* 0 to 45 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = arctable[i].row ;
		(*table)[*number].col = arctable[i].col ;
		(*number)++ ;
	}

/* 45 to 90 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = arctable[i].col ;
		(*table)[*number].col = arctable[i].row ;
		(*number)++ ;
	}

/* 90 to 135 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = arctable[i].col ;
		(*table)[*number].col = - arctable[i].row ;
		(*number)++ ;
	}

/* 135 to 180 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = arctable[i].row ;
		(*table)[*number].col = - arctable[i].col ;
		(*number)++ ;
	}

/* 180 to 225 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = - arctable[i].row ;
		(*table)[*number].col = - arctable[i].col ;
		(*number)++ ;
	}

/* 225 to 270 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = - arctable[i].col ;
		(*table)[*number].col = - arctable[i].row ;
		(*number)++ ;
	}

/* 270 to 315 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = - arctable[i].col ;
		(*table)[*number].col = arctable[i].row ;
		(*number)++ ;
	}

/* 315 to 360 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = - arctable[i].row ;
		(*table)[*number].col = arctable[i].col ;
		(*number)++ ;
	}
	free (arctable);
}

draw_circle (map, dist, x, y)
    struct Map_info *map;
    int dist;
    double x, y;
{
	struct table *arctable ;
	int arcnumber ;
	int i ;

	arctable = NULL;
	Arcdef(dist,&arctable,&arcnumber)  ;

	*table = (dig_alloc_points(&Gpoints, arcnumber * 8)) ;
	*number = 0 ;

/* 0 to 45 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = arctable[i].row ;
		(*table)[*number].col = arctable[i].col ;
		(*number)++ ;
	}

/* 45 to 90 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = arctable[i].col ;
		(*table)[*number].col = arctable[i].row ;
		(*number)++ ;
	}

/* 90 to 135 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = arctable[i].col ;
		(*table)[*number].col = - arctable[i].row ;
		(*number)++ ;
	}

/* 135 to 180 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = arctable[i].row ;
		(*table)[*number].col = - arctable[i].col ;
		(*number)++ ;
	}

/* 180 to 225 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = - arctable[i].row ;
		(*table)[*number].col = - arctable[i].col ;
		(*number)++ ;
	}

/* 225 to 270 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = - arctable[i].col ;
		(*table)[*number].col = - arctable[i].row ;
		(*number)++ ;
	}

/* 270 to 315 degrees */
	for (i=0; i<arcnumber; i++)
	{
		(*table)[*number].row = - arctable[i].col ;
		(*table)[*number].col = arctable[i].row ;
		(*number)++ ;
	}

/* 315 to 360 degrees */
	for (i=arcnumber-1; i>=0; i--)
	{
		(*table)[*number].row = - arctable[i].row ;
		(*table)[*number].col = arctable[i].col ;
		(*number)++ ;
	}
	free (arctable);
}
