/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#include <stdio.h>
extern  char *calloc(); /* removed <malloc.h> alpha930100 parghi 1993-03-30 */
extern  int  free();
extern  char *malloc();
extern  char *realloc();
#include "data.h"
#include "externs.h"

struct triple *
point_new (double x, double y, double z)
{
    struct triple  *point;

    if (!(point = (struct triple *) malloc (sizeof (struct triple))))
    {
        return NULL;
    }

    point->x = x;
    point->y = y;
    point->z = z;

    return point;
}


struct quaddata *
data_new (double x_orig, double y_orig, int n_rows, int n_cols, int n_points)
{
    struct quaddata *data;
    int             i;

    if (!(data = (struct quaddata *) malloc (sizeof (struct quaddata))))
    {
        return NULL;
    }

    data->x_orig = x_orig;
    data->y_orig = y_orig;
    data->n_rows = n_rows;
    data->n_cols = n_cols;
    data->n_points = n_points;
    data->points = (struct triple *) malloc (sizeof (struct triple) * (KMAX + 1)) ;
    if (!data->points) return NULL;
    for (i = 0; i <= KMAX; i++)
    {
        data->points[i].x = 0;
        data->points[i].y = 0;
        data->points[i].z = 0;
    }

    return data;
}





int 
quad_compare (struct triple *point, struct quaddata *data)
/* returns the quadrant the point should be inserted in */
/* called by divide() */
{
    int             cond1, cond2, cond3, cond4, rows, cols;
    if (data == NULL)
	return -1;
    if (data->n_rows % 2 == 0)
    {
	rows = data->n_rows / 2;
    }
    else
    {
	rows = (int) (data->n_rows / 2) + 1;
    }

    if (data->n_cols % 2 == 0)
    {
	cols = data->n_cols / 2;
    }
    else
    {
	cols = (int) (data->n_cols / 2) + 1;
    }
    cond1 = (point->x >= data->x_orig);
    cond2 = (point->x >= data->x_orig + cols * ew_res);
    cond3 = (point->y >= data->y_orig);
    cond4 = (point->y >= data->y_orig + rows * ns_res);
    if (cond1 && cond3)
    {
	if (cond2 && cond4)
	    return NE;
	if (cond2)
	    return SE;
	if (cond4)
	    return NW;
	return SW;
    }
    else
	return 0;
}


int 
quad_add_data (struct triple *point, struct quaddata *data)
{
    int             n, i, cond;
    double          xx, yy, r;
    cond = 1;
    if (data==NULL) {
      fprintf(stderr,"add_data: data is NULL \n");
      return -5;
    }
    for (i = 0; i < data->n_points; i++)
    {
	xx = data->points[i].x - point->x;
	yy = data->points[i].y - point->y;
	r = xx * xx + yy * yy;
	if (r <= dmin)
	{
	    cond = 0;
	}
    }

    if (cond)
    {
	n = (data->n_points)++;
	data->points[n].x = point->x;
	data->points[n].y = point->y;
	data->points[n].z = point->z;
    }
  return cond;
}




int 
quad_intersect (double xmin, double xmax, double ymin, double ymax, struct quaddata *data)
{
    if (((data->x_orig >= xmin) && (data->x_orig <= xmax)
	 && (((data->y_orig >= ymin) && (data->y_orig <= ymax))
	     || ((ymin >= data->y_orig) && (ymin <= data->y_orig + data->n_rows * ns_res))
	     )
	 )
	    || ((xmin >= data->x_orig) && (xmin <= data->x_orig + data->n_cols * ew_res)
		&& (((ymin >= data->y_orig) && (ymin <= data->y_orig + data->n_rows * ns_res))
		    || ((data->y_orig >= ymin) && (data->y_orig <= ymax))
		    )
		)
	)
    {
	return 1;
    }
    else
	return 0;
}




int 
quad_division_check (struct quaddata *data)
{
    if (data->points == NULL) return -1;
    if (data->n_points < KMAX) return 0;
    else return 1;
}



struct quaddata **
quad_divide_data (struct quaddata *data)
{
    struct quaddata **datas;
    int             cols1, cols2, rows1, rows2, i;  /*j1, j2, jmin = 0;*/
    double          dx, dy;  /* x2, y2, dist, mindist; */
    if ((data->n_cols <= 1) || (data->n_rows <= 1))
    {
      fprintf(stderr,"Points are too concentrated -- please increase DMIN\n");
      exit (0);
    }

    if (data->n_cols % 2 == 0)
    {
	cols1 = data->n_cols / 2;
	cols2 = cols1;
    }
    else
    {
	cols2 = (int) (data->n_cols / 2);
	cols1 = cols2 + 1;
    }
    if (data->n_rows % 2 == 0)
    {
	rows1 = data->n_rows / 2;
	rows2 = rows1;
    }
    else
    {
	rows2 = (int) (data->n_rows / 2);
	rows1 = rows2 + 1;
    }
    dx = cols1 * ew_res;
    dy = rows1 * ns_res;
    if (!(datas=(struct quaddata **) malloc(sizeof(struct quaddata *) * 5)))
    {
        return NULL;
    }
    datas[NE] = data_new(data->x_orig+dx,data->y_orig+dy,rows2,cols2,0);
    datas[SW] = data_new(data->x_orig,data->y_orig,rows1,cols1,0);
    datas[SE] = data_new(data->x_orig+dx,data->y_orig,rows1,cols2,0);
    datas[NW] = data_new(data->x_orig,data->y_orig+dy,rows2,cols1,0);
    for (i=0;i<data->n_points;i++) {
      switch (quad_compare (data->points+i, data))
      {
        case SW:
        {
	  quad_add_data (data->points+i, datas[SW]);
          break;
        }
        case SE:
        {
	  quad_add_data (data->points+i, datas[SE]);
          break;
        }
        case NW:
        {
	  quad_add_data (data->points+i, datas[NW]);
          break;
        }
        case NE:
        {
	  quad_add_data (data->points+i, datas[NE]);
          break;
        }
      }  
    }
    data->points = NULL;
    return datas;
}




int 
quad_get_points (struct triple *points, struct quaddata *data, double xmin, double xmax, double ymin, double ymax, int MAX)
 
{
 int i;
 int n=0;
 int l = 0;
 struct triple *point;
    for (i = 0; i < data->n_points; i++)
    {
	point = data->points+i;
	if (l >= MAX)
	    return MAX + 1;
	if ((point->x > xmin) && (point->x <= xmax)
		&& (point->y > ymin) && (point->y <= ymax))
	{
	    points[l].x = point->x;
	    points[l].y = point->y;
            points[l].z = point->z;
            l=l+1; 

	}
    }
    n = l;
    return n;
}	
