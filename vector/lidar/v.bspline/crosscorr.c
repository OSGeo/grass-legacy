/***********************************************************************
 *
 * MODULE:       v.surf.bspline
 *
 * AUTHOR(S):    Roberto Antolin
 *
 * PURPOSE:      Spline Interpolation and cross correlation
 *
 * COPYRIGHT:    (C) 2006 by Politecnico di Milano -
 *			     Polo Regionale di Como
 *
 *               This program is free software under the
 *               GNU General Public License (>=v2).
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************************/

/*INCLUDES*/
#include <stdlib.h> 
#include <string.h> 

#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>
#include <grass/config.h>

#include <grass/PolimiFunct.h>
#include "bspline.h"


#define NDATA_MAX 100
#define PARAM_LAMBDA 6
#define PARAM_SPLINE 0


/*-------------------------------------------------------------------------------------------*/
    int 
cross_correlation (struct Map_info* Map, double passWE, double passNS, double *lambda_min)
    /*
       Map: Map in which cross crorrelation will take values
       passWE: spline step in West-East direction
       passNS: spline step in North-South direction
       lambda_min: regularizator value which provides the minimum of the error's media.

       RETURN:
       TRUE on success
       FALSE on failure
     */

{
    int bilin=TRUE, crazy=FALSE;		/*booleans*/
    int nsplx, nsply, nparam_spl, ndata;
    double *mean, *rms, *stdev, rms_min, stdev_min; /*passNS, passWE;*/

    double lambda[PARAM_LAMBDA] = {0.0001, 0.001, 0.01, 0.1, 1.0, 10.0};  	/* Fixed values (by the moment) */

    double *TN, *Q, *parVect;			/* Interpolation and least-square vectors*/
    double **N, **obsVect;			/* Interpolation and least-square matrix*/

    struct Point *observ;
    struct Stats stat_vect;
    /*struct line_pnts *points;*/
    /*struct line_cats *Cats;*/
    struct Cell_head region;

    G_get_window (&region);

    extern int bspline_field;
    extern char *bspline_column;
    dbCatValArray cvarr;


    mean = G_alloc_vector (PARAM_LAMBDA);	/* Alloc as much mean, rms and stdev values as the total*/
    rms = G_alloc_vector (PARAM_LAMBDA);	/* number of parameter used used for cross validation*/
    stdev = G_alloc_vector (PARAM_LAMBDA);	

    G_debug (5, _("CrossCorrelation: Some tests using different lambda_i values will be done"));

    ndata = Vect_get_num_lines (Map);

    if (ndata > NDATA_MAX) {
	G_warning (_("CrossCorrelation: %d are too many points. The cross validation would take too much time"), ndata);
	crazy = TRUE;
    }

    /*points = Vect_new_line_struct ();*/
    /*Cats = Vect_new_cats_struct ();*/

    observ = P_Read_Vector_Region_Map (Map, &region, &ndata, 1024);
    G_debug (5, _("CrossCorrelation: %d points read in region. "), ndata);
    fprintf (stdout, _("CrossCorrelation: %d points read in region. "), ndata);
    
    if (ndata > 50) 
	G_warning (_("CrossCorrelation: Maybe, it takes too long. "
		    "It will depend on how many points you are considering"));
    else 
	G_debug (5, _("CrossCorrelation: I wouldn't take too long."));

    if (ndata > 0) {			/* If at least one point is in the region */	
	int i, j, lbd;			/* lbd: lambda index */
	int BW, lbd_min	;		/* lbd_min: index where minimun is found */
	double mean_reg;

	int nrec, ctype = 0;
	struct field_info *Fi;
	dbDriver *driver_cats;

	if (bspline_field > 0) {
	    db_CatValArray_init ( &cvarr );

	    Fi = Vect_get_field( Map, bspline_field);
	    if ( Fi == NULL )
		G_fatal_error (_("CrossCorrelation: Cannot read field info"));	

	    driver_cats = db_start_driver_open_database ( Fi->driver, Fi->database );
	    G_debug (1, _("CrossCorrelation: driver=%s db=%s"), Fi->driver, Fi->database);

	    if ( driver_cats == NULL )
		G_fatal_error(_("CrossCorrelation: Cannot open database %s by driver %s"), Fi->database, Fi->driver);

	    nrec = db_select_CatValArray ( driver_cats, Fi->table, Fi->key, bspline_column, NULL, &cvarr );
	    G_debug (3, "nrec = %d", nrec );

	    ctype = cvarr.ctype;
	    if ( ctype != DB_C_TYPE_INT && ctype != DB_C_TYPE_DOUBLE )
		G_fatal_error ( _("CrossCorrelation: Column type not supported") );

	    if ( nrec < 0 )
		G_fatal_error (_("CrossCorrelation: Cannot select data from table"));

	    G_message ( _("CrossCorrelation: %d records selected from table"), nrec);

	    db_close_database_shutdown_driver (driver_cats);
	}

	nsplx = ceil ((region.east - region.west)/passWE);
	nsply = ceil ((region.north - region.south)/passNS);
	nparam_spl = nsplx * nsply;

	if (nparam_spl > 22900)
	    G_fatal_error (_("CrossCorrelation: Too many splines (%d x %d). "
			"Consider changing spline steps \"sie=\" \"sin=\"."), nsplx, nsply);

	BW = P_get_BandWidth (bilin, nsply); 	/**/

	/*Least Squares system*/
	N = G_alloc_matrix (nparam_spl, BW);	/* Normal matrix */
	TN = G_alloc_vector (nparam_spl);		/* vector */
	parVect = G_alloc_vector (nparam_spl);	/* Parameters vector */
	obsVect = G_alloc_matrix (ndata, 3);	/* Observation vector */
	Q = G_alloc_vector (ndata);			/* "a priori" var-cov matrix */

	stat_vect = alloc_Stats (ndata);
	if (bspline_field <= 0)
	    mean_reg = P_Mean_Calc (&region, observ, ndata);

	for (lbd=0; lbd < PARAM_LAMBDA; lbd++) {	/* For each lambda value */

	    fprintf (stdout, _("CrossCorrelation: Begining cross validation with "
			"lambda_i=%.3f ...\n"), lambda[lbd]);

	    for (j=0; j<ndata; j++) {			/* Making the interp. with all points */
		for (i=0; i<ndata; i++) {		/* Setting obsVect vector & Q matrix */
		    double dval;
		    if (i==j) continue;			/* Not considering point "j"*/

		    Q[i] = 1;					/* Q=I */
		    obsVect[i][0] = observ[i].coordX;
		    obsVect[i][1] = observ[i].coordY;

		    if (bspline_field > 0){
			int cat, ival, ret;

			/*type = Vect_read_line (Map, points, Cats, observ[i].lineID);*/
			/*if ( !(type & GV_POINTS ) ) continue;*/

			/*Vect_cat_get ( Cats, bspline_field, &cat );*/
			cat = observ[i].cat;

			if ( cat < 0 ) continue;

			if ( ctype == DB_C_TYPE_INT ) {
			    ret = db_CatValArray_get_value_int ( &cvarr, cat, &ival );
			    obsVect[i][2] = ival;
			} else {		 /* DB_C_TYPE_DOUBLE */
			    ret = db_CatValArray_get_value_double ( &cvarr, cat, &dval );
			    obsVect[i][2] = dval;
			}
			if ( ret != DB_OK ) {
			    G_warning (_("CrossCorrelation: No record for point (cat = %d)"), cat );
			    continue;
			}
		    }
		    else obsVect[i][2] = observ[i].coordZ - mean_reg; 

		}

		if (bilin) {		/* Bilinear interpolation */
		    normalDefBilin (N, TN, Q, obsVect, passWE, passNS, nsplx, nsply, region.west, 
			    region.south, ndata, nparam_spl, BW);
		    nCorrectGrad (N, lambda[lbd], nsplx, nsply, passWE, passNS);
		} 
		else {			/* Bicubic interpolation */	
		    normalDefBicubic (N, TN, Q, obsVect, passWE, passNS, nsplx, nsply, region.west, 
			    region.south, ndata, nparam_spl, BW);
		    nCorrectGrad (N, lambda[lbd], nsplx, nsply, passWE, passNS);
		}

		/* 
		   if (bilin) interpolation (&interp, P_BILINEAR);
		   else interpolation (&interp, P_BICUBIC);
		 */
		tcholSolve (N, TN, parVect, nparam_spl, BW);

		/* Estimation of i-point */
		stat_vect.estima[j] = 
		    dataInterpolateBicubic (obsVect[j][0], obsVect[j][1], passWE, passNS, nsplx, nsply,
			    region.west, region.south, parVect);

		/*Difference between estimated and observated i-point*/
		stat_vect.error[j] = obsVect[j][2] - stat_vect.estima[j];
		G_debug (1, _("CrossCorrelation: stat_vect.error[%d]  =  %lf"), j, stat_vect.error[j]);
	    } 

	    mean[lbd] = calc_mean (stat_vect.error, stat_vect.n_points);
	    rms[lbd] = calc_root_mean_square (stat_vect.error, stat_vect.n_points);
	    stdev[lbd] = calc_standard_deviation (stat_vect.error, stat_vect.n_points);

	    fprintf (stdout, _("CrossCorrelation: Mean = %.5lf\n"), mean[lbd]);
	    fprintf (stdout, _("CrossCorrelation: Root Means Square (RMS) = %.5lf\n"), rms[lbd]);
	    fprintf (stdout, "\n---------------------o-o-------------------\n\n");
	}	/* ENDFOR each lambda value */

	G_free_matrix (N);
	G_free_vector (TN);
	G_free_vector (Q);
	G_free_matrix (obsVect);
	G_free_vector (parVect);
#ifdef nodef
	/*At this moment, consider rms only*/
	if (TRUE) rms_min = find_minimum (rms, &lbd_min);
	else stdev_min = find_minimum (stdev, &lbd_min);

	/* Writing some output*/
	fprintf (stdout, _("CrossCorrelation: Different number of splines and lambda_i values have " \
		    "been taken for the cross correlation\n"));
	fprintf (stdout, _("CrossCorrelation: The minimum value for the test (rms=%lf) was obtained with:\n"), rms_min);
	fprintf (stdout, _("CrossCorrelation: lambda_i = %.3f\n"), lambda[lbd_min]);

	*lambda_min = lambda[lbd_min];
#endif

	*lambda_min = 1.0;

	/*fprintf (stdout, _("%-10s|%-10s|%-10s|"), "lambda", "mean", "rms");*/
	fprintf (stdout, _("Now, the results into a table:\n"));
	fprintf (stdout, _(" lambda    | mean (m)    | rms (m)     |\n"));
	for  (lbd=0; lbd < PARAM_LAMBDA; lbd++) {
	    fprintf (stdout, _(" %-10.5f| %-12.3f| %-12.3f|\n"),lambda[lbd], mean[lbd], rms[lbd]);
	}
	fprintf (stdout, _("\nResults are over.\n"));

    }	/* ENDIF (ndata > 0) */
    G_free (observ);
    return TRUE;
}

#ifdef nodef
void interpolation (struct ParamInterp *interp, boolean bilin) {
    if (bilin == P_BILINEAR) {	/* Bilinear interpolation */
	normalDefBilin (interp->N, interp->TN, interp->Q, interp->obsVect, interp->passoE, 
		interp->passoN, interp->nsplx, interp->nsply, interp->region.west, 
		interp->region.south, interp->ndata, interp->nparam_spl, interp->BW);

	nCorrectGrad (interp->N, interp->lambda[lbd], interp->nsplx, interp->nsply, 
		interp->passoE, interp->passoN);
    } 
    else {			/* Bicubic interpolation */	
	normalDefBicubic (interp->N, interp->TN, interp->Q, interp->obsVect, interp->passoE, 
		interp->passoN, interp->nsplx, interp->nsply, interp->region.west, 
		interp->region.south, interp->ndata, interp->nparam_spl, interp->BW);

	nCorrectGrad (interp->N, interp->lambda[lbd], interp->nsplx, interp->nsply, 
		interp->passoE, interp->passoN);
    }
    return TRUE; 
}
#endif

double
calc_mean (double *values, int nvalues) {
    int i;
    double sum=.0;

    if (nvalues == 0) return .0;
    for (i=0; i < nvalues; i++)
	sum += values[i];
    return sum/nvalues;
}


double 
calc_root_mean_square (double *values, int nvalues) {
    int i;
    double rms, sum=.0;

    if (nvalues == 0) return .0;

    for (i=0; i < nvalues; i++)
	sum += pow (values[i], 2)/nvalues;

    rms = sqrt(sum);
    return  rms;

}

double 
calc_standard_deviation (double *values, int nvalues) {
    double mean, rms, stdev;

    if ( nvalues == 0) return .0;

    rms = calc_root_mean_square (values, nvalues);
    mean = calc_mean (values, nvalues);

    stdev = sqrt (pow(rms, 2) - pow(mean, 2));
    return stdev;
}

struct  
Stats alloc_Stats (int n) {
    double *err, *stm; 
    struct Stats stat;

    stat.n_points = n;
    err = (double *) G_calloc (n, sizeof(double));
    stm = (double *) G_calloc (n, sizeof(double));

    stat.error = err;
    stat.estima = stm;

    return stat;
}

double
find_minimum (double *values, int *l_min) {
    int l; 
    double min;

    min =  values[0];

    for (l=0; l<PARAM_LAMBDA; l++) {
	if (min > values[l]) {
	    min = values[l];
	    *l_min = l;
	}
    }
    return min;
}

