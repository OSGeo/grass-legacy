/*======================================================================
                             ltm_trans.c


ltm_trans_calculate(group)

ltm_trans_forward (group, e1, n1, z1, e2, n2, z2)
  Rectify_Group    *group;
  double            e1,  n1,  z1;
  double           *e2, *n2, *z2;

ltm_trans_inverse (group, e1, n1, z1, e2, n2, z2)
  Rectify_Group    *group;
  double           *e1, *n1, *z1;
  double            e2,  n2,  z2;


======================================================================*/

#include "ortho_image.h"


/*---------------------------------------------------------------------

 Returns:
    -1:   wrong trans type.
    -2:   fiducial equation error.
    -3    ortho_photo equation error.
     0:   not enough points (min of 4 needed)
     1:   ok.
----------------------------------------------------------------------*/
int 
ltm_trans_calculate (Rectify_Group *group)
{
    int    i;
    int    status;
    int    target_elev;

    double scan_x,  scan_y,  scan_z;    /* source image (scanned) coords */
    double photo_x, photo_y, photo_z;   /* photo coordinates */
    double tar_x,   tar_y,   tar_z;     /* target location coordinates */

    Control_Points   *points;           
    Auxillary_Ltm  *auxil;
    Coeffs_Ltm     *coefs;


    /* check transformation type */
    if (group->trans_type != LAND_TM) {
      /* TODO message */
      group->stat = -1;
      return (group->stat);
    }

    /* make control points visiable */
    points = (Control_Points *) group->points;

    /* make auxilary data visiable */
    auxil = (Auxillary_Ltm *) group->auxil;

    /* make coeffients visiable */
    coefs = (Coeffs_Ltm *) group->coefs;

    /* alloc and fill init control points */
    /* temp control points will contain photo-coords and target-coords */
    auxil->points_photo.count = 0;
    auxil->points_photo.status = NULL;
    auxil->points_photo.e1  = NULL;
    auxil->points_photo.n1  = NULL;
    auxil->points_photo.z1  = NULL;
    auxil->points_photo.e2  = NULL;
    auxil->points_photo.n2  = NULL;
    auxil->points_photo.z2  = NULL;


    /* calculate the row/col to photo-coords transformation */
    /* coeffients (using the fiducial points marked in i.points) */
/** TODO
**    if (I_compute_fiducial_equations(auxil, coefs) <= 0) {
**/
        /** TODO message about fiducials **/
/**       group->stat = -2;
**       return (group->stat);
**    }
**/

    /* convert each scanned imagery (row,col) pair to photo coordinates */
    for (i = 0; i < points->points_temp.count; i++)  {
        status = points->points_temp.status[i];

        scan_x = points->points_temp.e1[i];
        scan_y = points->points_temp.n1[i];

        tar_x = points->points_temp.e2[i];
        tar_y = points->points_temp.n2[i];

	/* target elevation read from elev file, not control points */
	read_elev (&target_elev, tar_x, tar_y);
	tar_z = (double) target_elev;


/** TODO convert from imagery(row, col) to (line,pixel) relative to full scene */ 
        /* do the row,col to photo coordinate transformation */
/**        I_fiducial_ref  (scan_x, scan_y, &photo_x, &photo_y, coefs);
**/

	/* make a new temporary control point */
/**        I_new_con_point (&auxil->points_photo, 
**			 photo_x, photo_y, photo_z,
**			 tar_x,   tar_y,   tar_z,
**			 status);
**/

	/* make a new temporary control point */
        I_new_con_point (&auxil->points_photo, 
			 scan_x, scan_y, tar_z,
			 tar_x,  tar_y,  tar_z,
			 status);


    }



    /* pass the temporary photo_points to the ortho transformation */
    status = I_compute_ltm_equations(&auxil->points_photo, 
				      auxil, coefs);
    


    if (status == -1) { /** couldn't solve equations **/ 
        group->stat = -3;			  
    }
    else if (status == 0) { /** not enough points  **/ 
        group->stat = 0;			  
    }
    else if (status == 1) { /** all ok. **/ 
        group->stat = 1;			  
    }

    return (group->stat);
}


/*-------------------------------------------------------------------*
 *  Given the imagery location (e1,n1) z1 = target elevation 
 *  Determine the target location cordinates (e2,n2,z2)
 *-------------------------------------------------------------------*/
int 
ltm_trans_forward (Rectify_Group *group, double e1, double n1, double z1, double *e2, double *n2, double *z2)
{
char           msg[100];
double         temp_e, temp_n, temp_z;
int            temp_elev;


/** TODO */
        /* compute the Landsat full scene coordinates */
/**      I_fiducial_ref (e1, n1, &temp_e, &temp_n, group->coefs);
**/

      /* Now form these coords, compute the groud (target) */
      /* coordinates from the inverse ortho equations */
      /* zx = focal length, and z2 = ground elevation - see above */

      temp_e = e1;
      temp_n = n1;
      temp_z = z1;
      I_inverse_ltm_ref (temp_e, temp_n, temp_z, e2, n2, z2, 
			   group->auxil, 
			   group->coefs);


      return 0;
}

/*-------------------------------------------------------------------*/
int ltm_trans_inverse (Rectify_Group *group,
	double *e1, double *n1, double *z1, double e2, double n2, double z2)
{
char           msg[100];
double         temp_e, temp_n, temp_z;


	      /* Given e2,n2,z2 - ground (target) points */ 
	      /* compute ex,nx                           */
	      /* which are landsat full scene coordinates of */
	      /* the imaged point                        */

	      I_ltm_ref (e2, n2, z2, &temp_e, &temp_n, &temp_z, 
		         group->auxil, group->coefs); 


	      /* Now convert ex,nx from full scene coordinates */
	      /* to scanned (row,col) coordinates */
	      /* the scanned coordinates are stored in ex, nx */

/** TODO 
 **	      I_inverse_fiducial_ref (e1, n1, temp_e, temp_n, group->coefs);
**/

              *e1 = temp_e;
              *n1 = temp_n;


      return 0;
}











