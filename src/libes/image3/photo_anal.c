/*======================================================================
                             photo_anal.c


photo_analyze (group, residuals)

======================================================================*/
#include <unistd.h>
#include "ortho_image.h"


/*-------------------------------------------------------------------*/
int 
photo_anal_points (Rectify_Group *group, Residuals *resid)
{
    Control_Points     *points;           
    Auxillary_Photo *auxil;
    Coeffs_Photo    *coefs;

    int n, count;
    int xmax, ymax, gmax;
    int target_elev;

    double rms;
    double d,d1,d2,sum;
    double e2, n2, z2;
    double xval, yval, gval;
    double sqrt();
    char   msg[40];



    /* check the transformation type */
    if (group->trans_type != PHOTO) { 
      return (-1);   /* wrong trans type, this shouldn't occur */
    }

    /* make control points visiable */
    points  = (Control_Points *)  group->points;
    auxil   = (Auxillary_Photo *) group->auxil;
    coefs   = (Coeffs_Photo *)    group->coefs;


    xmax = ymax = gmax = 0;
    xval = yval = gval = 0.0;

    /* do an initial calcuation of coeffients */
    group->stat = group->calculate_trans(group);

    if (group->stat <= 0) 
    {
       if(group->stat == 0)  {  /* Not enough points */
          sprintf(msg,"Not Enough Points, 4 are required.");
          Menu_msg(msg);
          sleep(3);
       }
       else { /* coundn't solve ortho equations */
          sprintf(msg,"Couldn't Solve Ortho Equations.");
          Menu_msg(msg);
          sleep(3);
       }

       return 0;
     }


   /* compute the target x and y errors plus hypotnose 
    * keep track of largest x,y, and grnd errors */
    sum = 0.0;
    rms = 0.0;
    count = 0;
    for (n = 0; n < points->points_temp.count; n++)
    {
	if (points->points_temp.status[n] <= 0) continue;
	count++;

	/** TODO  -- convert fromlat/lons to current systems **/

	/* get target elevation at the current points */
        read_elev(&target_elev, 
		  points->points_temp.e2[n],
		  points->points_temp.n2[n]);


	/* calc the target location (e2,n2,z2) */
	group->forward_trans(group, 
			    points->points_temp.e1[n], 
			    points->points_temp.n1[n], 
			    (double)  target_elev, 
			    &e2, &n2, &z2);

	/* x error = target (e2) - control points easting */
	if((d = resid->xres[n] = e2 - points->points_temp.e2[n]) < 0)
	    d = -d;

	if (d > xval) 	{  /* save largest x error */
	    resid->large_x = n;
	    xval = d;
	}

	/* y error = target (n2) - control points northing */
	if ((d = resid->yres[n] = n2 - points->points_temp.n2[n]) < 0)
	    d = -d;

	if (d > yval)	{ /* save largest y error */
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

    /* compute overall ground (diagonal) rms error */
    if (count)
	resid->gnd_rms = sqrt (sum/count);


    return 0;
}












