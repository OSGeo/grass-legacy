/*
* $Id$
*
****************************************************************************
*
* MODULE:       s.vol.rst: program for 3D(volume) interpolation and geometry
*               analysis from scattered point data using regularized spline
*               with tension
*
* AUTHOR(S):    Original program (1989) and various modifications:
*               Lubos Mitas
*
*               GRASS 4.2, GRASS 5.0 version and modifications:
*               H. Mitasova,  I. Kosinovsky, D. Gerdes, J. Hofierka
*
* PURPOSE:      s.vol.rst interpolates the values to 3-dimensional grid from
*               point data (climatic stations, drill holes etc.) given in a
*               sites file named input. Output grid3 file is elev. 
*               Regularized spline with tension is used for the
*               interpolation.
*
* COPYRIGHT:    (C) 1989, 1993, 2000 L. Mitas,  H. Mitasova,
*               I. Kosinovsky, D. Gerdes, J. Hofierka
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "gis.h"

#include "oct.h"
#include "surf.h"
#include "dataoct.h"
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



int 
translate_oct (tree, numberx, numbery, numberz, numberw)
    struct octtree *tree;
    double          numberx;
    double          numbery;
    double          numberz;
    double          numberw;
{
    int total=0,i;
    struct quadruple *point;
    if (tree == NULL)
	return 0;
    if (tree->data == NULL)
	return 0;
    if (tree->leafs != NULL) 
    {
      ((struct octdata *) (tree->data))->x_orig-=numberx;
      ((struct octdata *) (tree->data))->y_orig-=numbery;
      ((struct octdata *) (tree->data))->z_orig-=numberz;
      for (i=0;i<NUMLEAFS;i++) {
        total+=translate_oct(tree->leafs[i],numberx,numbery,numberz,numberw);
      }
    }
    else
    {
      ((struct octdata *) (tree->data))->x_orig -= numberx;
      ((struct octdata *) (tree->data))->y_orig -= numbery;
      ((struct octdata *) (tree->data))->z_orig-=numberz;
      for (i = 0; i < ((struct octdata *) (tree->data))->n_points; i++)
      {
	    ((struct octdata *) (tree->data))->points[i].x -= numberx;
	    ((struct octdata *) (tree->data))->points[i].y -= numbery;
	    ((struct octdata *) (tree->data))->points[i].z -= numberz;
	    ((struct octdata *) (tree->data))->points[i].w -= numberw;
      }
      return 1;
    }
    return total;
}





int 
interp_call (root, tree)
    struct octtree *root;
    struct octtree *tree;
{
    double          xmn,xmx,ymn,ymx,zmn,zmx,distx,disty,distz,distxp,
                                        distyp,distzp,temp1,temp2,temp3;
    int             i, npt, nptprev, MAXENC, k,j;
    static struct quadruple *points = NULL;
    if (tree == NULL)
	return;
    if (tree->data == NULL)
	return -1;
    if (((struct octdata *) (tree->data))->points == NULL)
    {
      for(k=0;k<NUMLEAFS;k++)
	if(!interp_call (root, tree->leafs[k])) return 0;
      return 1;
    }
    else
    {

	if (!points)
	{
	    if(!(points=(struct quadruple*)malloc(sizeof(struct quadruple)*(KMAX2+1))))
            clean_fatal_error("Not enough memory for points");
	}

	distx = (((struct octdata *) (tree->data))->n_cols * ew_res) * 0.1;
	disty = (((struct octdata *) (tree->data))->n_rows * ns_res) * 0.1;
	distz = (((struct octdata *) (tree->data))->n_levs * tb_res) * 0.1;
	distxp = 0;
	distyp = 0;
	distzp = 0;
	xmn = ((struct octdata *) (tree->data))->x_orig;
	xmx = ((struct octdata *) (tree->data))->x_orig + 
              ((struct octdata *) (tree->data))->n_cols * ew_res;
	ymn = ((struct octdata *) (tree->data))->y_orig;
	ymx = ((struct octdata *) (tree->data))->y_orig + 
              ((struct octdata *) (tree->data))->n_rows * ns_res;
	zmn = ((struct octdata *) (tree->data))->z_orig;
	zmx = ((struct octdata *) (tree->data))->z_orig + 
              ((struct octdata *) (tree->data))->n_levs * tb_res;
/*printf("\n %f", ((struct octdata *)(tree->data))->z_orig);*/
	i = 0;
	MAXENC = 0;
	npt = OT_region_data (root,xmn-distx,xmx+distx,ymn-disty,
                     ymx+disty,zmn-distz,zmx+distz,points, KMAX2);
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
		temp3 = distzp;
		distzp = distz;
		distz = distzp - fabs (distz - temp3) * 0.5;
	        npt = OT_region_data (root,xmn-distx,xmx+distx,ymn-disty,
                              ymx+disty,zmn-distz,zmx+distz,points, KMAX2);
	    }
	    else
	    {
		nptprev = npt;
		temp1 = distyp;
		distyp = disty;
		temp2 = distxp;
		distxp = distx;
		temp3 = distzp;
		distzp = distz;
		if (MAXENC)
		{
		    disty = fabs (disty - temp1) * 0.5 + distyp;
		    distx = fabs (distx - temp2) * 0.5 + distxp;
		    distz = fabs (distz - temp3) * 0.5 + distzp;
		}
		else
		{
		    distx += distx;
		    disty += disty;
		    distz += distz;
		}
	        npt = OT_region_data (root,xmn-distx,xmx+distx,ymn-disty,
                              ymx+disty,zmn-distz,zmx+distz,points, KMAX2);

	   }
        }	
   /*fprintf(stderr,"got %d points for interpolation\n",npt); */
	{
	    static int first = 1;

	    if (first)
	    {
		first = 0;

		if (!(A = (double *) malloc (sizeof(double) * ((KMAX2+1) * (KMAX2+2) + 1))))
		{
		    clean_fatal_error("Cannot allocate A");
		}
		if (!(b = (double *) malloc (sizeof (double) * (KMAX2 + 2))))
		{
		    clean_fatal_error("Cannot allocate b");
		}
		if (!(w = (double *) malloc (sizeof (double) * (KMAX2 + 1))))
		{
		    clean_fatal_error("Cannot allocate w");
		}
	    }
	}

/*for  (i=0;i<npt;i++)
  fprintf(stderr,"i=%d: %lf,%lf,%lf\n",i,points[i].x,points[i].y,points[i].z);
*/

	/* show before to catch 0% */

fprintf(stderr,"total segments = %d, Current = %d, npoints = %d\n",totsegm,cursegm+1,npt);

{
  G_percent (cursegm, totsegm, 1);
}
  

if (!COGRR1 (xmn, ymn, zmn, ((struct octdata *) (tree->data))->n_rows, 
                            ((struct octdata *) (tree->data))->n_cols,
		            ((struct octdata *) (tree->data))->n_levs, 
                            npt, points)) {
            fprintf(stderr,"Error in COGRR!\n");
	    return 0;
        }
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



