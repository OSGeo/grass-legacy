/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"

#include "quad.h"
#include "surf.h"
#include "userextern.h"
#include "userglobs.h"
#include "user.h"


/*
       fi - tension parameter
       az- interpolated values z for output grid
       adx,ady, ... - estimation of derivatives for output grid
       nsizr,nsizc - number of rows and columns for output grid

       interp_call() - divides region on segments
*/



int translate_quad (struct quadtree *tree,
    double numberx, double numbery, double numberz)
{
    int total=0,i;
    if (tree == NULL)
	return 0;
    if (tree->data == NULL)
	return 0;
    if (tree->ne_leaf != NULL) 
    {
        ((struct quaddata *)(tree->data))->x_orig-=numberx;
        ((struct quaddata *)(tree->data))->y_orig-=numbery;
	total+=translate_quad (tree->se_leaf, numberx, numbery, numberz);
	total+=translate_quad (tree->sw_leaf, numberx, numbery, numberz);
	total+=translate_quad (tree->ne_leaf, numberx, numbery, numberz);
	total+=translate_quad (tree->nw_leaf, numberx, numbery, numberz);
    }
    else
    {
	((struct quaddata *)(tree->data))->x_orig -= numberx;
	((struct quaddata *)(tree->data))->y_orig -= numbery;
	for (i = 0; i < ((struct quaddata *)(tree->data))->n_points; i++)
	{
	    ((struct quaddata *)(tree->data))->points[i].x -= numberx;
	    ((struct quaddata *)(tree->data))->points[i].y -= numbery;
	    ((struct quaddata *)(tree->data))->points[i].z -= numberz;
        }
	return 1;
    }
    return total;
}

int interp_call (struct quadtree *root, struct quadtree *tree)
{
    double          xmn, xmx, ymn, ymx, distx, disty, distxp, distyp, temp1,
                    temp2;
    int             i, npt, nptprev, MAXENC;
    static struct triple *points = NULL;
    if (tree == NULL)
	return 1;
    if (tree->data == NULL)
	exit (0);
    if (((struct quaddata *)(tree->data))->points == NULL)
    {
	interp_call (root, tree->sw_leaf);
	interp_call (root, tree->se_leaf);
	interp_call (root, tree->nw_leaf);
	interp_call (root, tree->ne_leaf);
	return 1;
    }
    else
    {

	if (!points)
	{
	    points = (struct triple *) malloc (sizeof (struct triple ) * (KMAX2 + 1));

	}

	distx = (((struct quaddata *)(tree->data))->n_cols * ew_res) * 0.1;
	disty = (((struct quaddata *)(tree->data))->n_rows * ns_res) * 0.1;
	distxp = 0;
	distyp = 0;
	xmn = ((struct quaddata *)(tree->data))->x_orig;
	xmx = ((struct quaddata *)(tree->data))->x_orig + ((struct quaddata *) 
                                                           (tree->data))->n_cols * ew_res;
	ymn = ((struct quaddata *)(tree->data))->y_orig;
	ymx = ((struct quaddata *)(tree->data))->y_orig + ((struct quaddata *) 
                                                           (tree->data))->n_rows * ns_res;
	i = 0;
	MAXENC = 0;
	npt = QT_region_data (root,xmn-distx,xmx+distx,ymn-disty,ymx+disty,points, KMAX2);
	while ((npt < KMIN) || (npt > KMAX2))
	{
	    if (i >= 70)
	    {
                fprintf(stderr,"Warning: taking too long to find points for interpolation--please change the region to area where your points are\n");
		break;
	    }
	    i++;
	    if (npt > KMAX2)
	    {
		MAXENC = 1;
		nptprev = npt;
		temp1 = distxp;
		distxp = distx;
		distx = distxp - fabs (distx - temp1) * 0.5;
		temp2 = distyp;
		distyp = disty;
		disty = distyp - fabs (disty - temp2) * 0.5;
		npt = QT_region_data (root, xmn - distx, xmx + distx, ymn - disty, 
				   ymx + disty, points, KMAX2);
	    }
	    else
	    {
		nptprev = npt;
		temp1 = distyp;
		distyp = disty;
		temp2 = distxp;
		distxp = distx;
		if (MAXENC)
		{
		    disty = fabs (disty - temp1) * 0.5 + distyp;
		    distx = fabs (distx - temp2) * 0.5 + distxp;
		}
		else
		{
		    distx += distx;
		    disty += disty;
		}
		npt = QT_region_data (root, xmn - distx, xmx + distx, ymn - disty, 
				    ymx + disty, points, KMAX2);

	   }
	}
	{
	    static int first = 1;

	    if (first)
	    {
		first = 0;

		if (!(A = (double *) malloc (sizeof(double) * ((KMAX2+1) * (KMAX2+2) + 1))))
		{
		    fprintf (stdout,"Allocation problem!!! npt=%d \n", npt);
		    exit (0);
		}
		if (!(b = (double *) malloc (sizeof (double) * (KMAX2 + 3))))
		{
		    fprintf (stdout,"Allocation problem!!! npt=%d \n", npt);
		    exit (0);
		}
		if (!(w = (double *) malloc (sizeof (double) * (KMAX2 + 1))))
		{
		    fprintf (stdout,"Allocation problem!!! npt=%d \n", npt);
		    exit (0);
		}
	    }
	}
/*
for  (i=0;i<npt;i++)
  fprintf (stdout,"i=%d: %lf,%lf,%lf\n",i,points[i].x,points[i].y,points[i].z);
*/
	/* show before to catch 0% */
	if (totsegm != 0)
	{
	  G_percent (cursegm, totsegm, 1);
	}
         if (!COGRR1 (xmn, ymn, ((struct quaddata *)(tree->data))->n_rows,
                             ((struct quaddata *)(tree->data))->n_cols, npt, points))
	    return 0;
	/*
	free (points);
	*/
	/* free (A); */
	/* show after to catch 100% */
	cursegm++;
        if (totsegm != 0)
        {
          G_percent (cursegm, totsegm, 1);
        }

	return 1;
    }
}



