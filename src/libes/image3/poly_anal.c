/*======================================================================
                             poly_anal.c


poly_analyze (group, residuals)

======================================================================*/
#include <unistd.h>
#include "ortho_image.h"


/*-------------------------------------------------------------------*/
int 
poly_anal_points (Rectify_Group *group, Residuals *resid)
{
    Control_Points     *points;           

    int n, count;
    int xmax, ymax, gmax;

    double rms;
    double d,d1,d2,sum;
    double e1, e2, n1, n2, z1, z2;
    double xval, yval, gval;
    double sqrt();
    static int order_pnts[4] = { 0, 4, 7, 11}; /* min required + 1 */
    char   msg[40];
    int    poly_ord;



    /* check the transformation type */
    if ((group->trans_type < POLY1) || (group->trans_type > POLY3)) {
      /* TODO warning mes */
      return (-1);
    }

    /* make control points visiable */
    points = (Control_Points *) group->points;


    xmax = ymax = gmax = 0;
    xval = yval = gval = 0.0;

    poly_ord = (int) group->trans_type;
    group->stat = group->calculate_trans(group);

    if (group->stat <= 0) 
    {
       if(group->stat == 0)
       {
          sprintf(msg,"Not Enough Points -- %d are required.",
		  order_pnts[poly_ord]);
          Menu_msg(msg);
          sleep(3);
       }
       return 0;
     }


 /* compute the row,col error plus ground error 
  * keep track of largest and second largest error
  */
    sum = 0.0;
    rms = 0.0;
    count = 0;
    for (n = 0; n < points->points_temp.count; n++)
    {
	if (points->points_temp.status[n] <= 0) continue;
	count++;

/*** TODO  -- convert fromlat/lons to current systems **/

	group->forward_trans(group, 
			    points->points_temp.e1[n], 
			    points->points_temp.n1[n], 
			    (double) 0, 
			    &e2, &n2, &z2);

	if((d = resid->xres[n] = e2 - points->points_temp.e2[n]) < 0)
	    d = -d;
	if (d > xval) 
	{
	    resid->large_x = n;
	    xval = d;
	}

	if ((d = resid->yres[n] = n2 - points->points_temp.n2[n]) < 0)
	    d = -d;
	if (d > yval)
	{
	    resid->large_y = n;
	    yval = d;
	}

/* compute ground error (ie along diagonal) */
	d1 = e2 - points->points_temp.e2[n];
	d2 = n2 - points->points_temp.n2[n];

	d = d1*d1 + d2*d2;
	sum += d;                 /* add it to rms sum, before taking sqrt */
	d = sqrt(d);
	resid->gnd[n] = d;
	if (d > gval)             /* is this one the max? */
	{
	    resid->large_gnd = n;
	    gval = d;
	}


    }

 /* compute overall rms error */
    if (count)
	resid->gnd_rms = sqrt (sum/count);


    return 0;
}












