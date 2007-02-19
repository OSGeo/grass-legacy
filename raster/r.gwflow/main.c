
/****************************************************************************
*
* MODULE:       r.gwflow 
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert <at> gmx <dot> de
* 		27 11 2006 Berlin
* PURPOSE:      Calculates confiend and unconfined transient two dimensional groundwater flow
*
* COPYRIGHT:    (C) 2006 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/N_pde.h>
#include <grass/N_gwflow.h>


/*- Parameters and global variables -----------------------------------------*/
typedef struct
{
    struct Option *output, *phead, *status, *kf_x, *kf_y, *q, *s, *r, *top,
	*bottom, *vector, *type, *dt, *maxit, *error, *solver;
    struct Flag *sparse;
} paramType;

paramType param;		/*Parameters */

/*- prototypes --------------------------------------------------------------*/
void set_params();		/*Fill the paramType structure */
void copy_result(N_array_2d * status, N_array_2d * phead_start, double *result,
		 struct Cell_head *region, N_array_2d * target);
N_les *create_solve_les(N_geom_data * geom, N_gwflow_data2d * data,
			N_les_callback_2d * call, int solver, int maxit,
			double error);

/* ************************************************************************* */
/* Set up the arguments we are expecting ********************************** */
/* ************************************************************************* */
void set_params()
{
    param.phead = G_define_option();
    param.phead->key = "phead";
    param.phead->type = TYPE_STRING;
    param.phead->required = YES;
    param.phead->gisprompt = "old,raster,raster";
    param.phead->description = _("The initial piezometric head in [m]");

    param.status = G_define_option();
    param.status->key = "status";
    param.status->type = TYPE_STRING;
    param.status->required = YES;
    param.status->gisprompt = "old,raster,raster";
    param.status->description =
	_
	("The status for each cell, = 0 - inactive, 1 - active, 2 - dirichlet");

    param.kf_x = G_define_option();
    param.kf_x->key = "kf_x";
    param.kf_x->type = TYPE_STRING;
    param.kf_x->required = YES;
    param.kf_x->gisprompt = "old,raster,raster";
    param.kf_x->description =
	_("The x-part of the permeability tensor in [m/s]");

    param.kf_y = G_define_option();
    param.kf_y->key = "kf_y";
    param.kf_y->type = TYPE_STRING;
    param.kf_y->required = YES;
    param.kf_y->gisprompt = "old,raster,raster";
    param.kf_y->description =
	_("The y-part of the permeability tensor in [m/s]");

    param.q = G_define_option();
    param.q->key = "q";
    param.q->type = TYPE_STRING;
    param.q->required = NO;
    param.q->gisprompt = "old,raster,raster";
    param.q->description = _("Sources and sinks in [m^3/s]");

    param.s = G_define_option();
    param.s->key = "s";
    param.s->type = TYPE_STRING;
    param.s->required = YES;
    param.s->gisprompt = "old,raster,raster";
    param.s->description = _("Specific yield in [1/m]");

    param.r = G_define_option();
    param.r->key = "r";
    param.r->type = TYPE_STRING;
    param.r->required = NO;
    param.r->gisprompt = "old,raster,raster";
    param.r->description = _("Reacharge raster map in [m^3/s]");

    param.top = G_define_option();
    param.top->key = "top";
    param.top->type = TYPE_STRING;
    param.top->required = YES;
    param.top->gisprompt = "old,raster,raster";
    param.top->description = _("Top surface of the aquifer in [m]");

    param.bottom = G_define_option();
    param.bottom->key = "bottom";
    param.bottom->type = TYPE_STRING;
    param.bottom->required = YES;
    param.bottom->gisprompt = "old,raster,raster";
    param.bottom->description = _("Bottom surface of the aquifer in [m]");

    param.output = G_define_option();
    param.output->key = "output";
    param.output->type = TYPE_STRING;
    param.output->required = YES;
    param.output->gisprompt = "new,raster,raster";
    param.output->description =
	_
	("The piezometric head result of the numerical calculation will be written to this map.");

    param.vector = G_define_option();
    param.vector->key = "velocity";
    param.vector->type = TYPE_STRING;
    param.vector->required = NO;
    param.vector->gisprompt = "new,raster,raster";
    param.vector->description =
	_
	("Calculate the groundwater distance velocity vector field and write the x, and y components to maps named name_[xy]. name is the basename for the new raster maps.");


    param.type = G_define_option();
    param.type->key = "type";
    param.type->type = TYPE_STRING;
    param.type->required = NO;
    param.type->answer = "confined";
    param.type->options = "confined,unconfined";
    param.type->description =
	_("Which type of groundwater flow? confined or unconfined.");

    param.dt = G_define_option();
    param.dt->key = "dt";
    param.dt->type = TYPE_DOUBLE;
    param.dt->required = YES;
    param.dt->answer = "3600";
    param.dt->description = _("Calculation time");

    param.maxit = G_define_option();
    param.maxit->key = "maxit";
    param.maxit->type = TYPE_INTEGER;
    param.maxit->required = NO;
    param.maxit->answer = "100000";
    param.maxit->description = _("Maximum number of iteration");

    param.error = G_define_option();
    param.error->key = "error";
    param.error->type = TYPE_DOUBLE;
    param.error->required = NO;
    param.error->answer = "0.0000000001";
    param.error->description =
	_("Break criteria for the cg and bicgstab solver");

    param.solver = G_define_option();
    param.solver->key = "solver";
    param.solver->type = TYPE_INTEGER;
    param.solver->required = NO;
    param.solver->answer = "0";
    param.solver->options = "0,1,2,3";
    param.solver->description =
	_
	("Which kind of solver should be used, 0 - cg, 1 - bicgstab, 2 - LU, 3 - Gauss");

    param.sparse = G_define_flag();
    param.sparse->key = 's';
    param.sparse->description =
	_
	("Use a sparse linear equation system, only available with cg and bicgstab solver");

}

/* ************************************************************************* */
/* Main function *********************************************************** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    struct GModule *module;
    N_gwflow_data2d *data;
    N_geom_data *geom;
    N_les *les = NULL;
    N_les_callback_2d *call;
    double *tmp_vect = NULL;
    struct Cell_head region;
    double error, max_norm = 0, tmp;
    int maxit, i, inner_count = 0;
    int solver, x, y, stat;
    N_gradient_field_2d *field = NULL;
    N_array_2d *xcomp = NULL;
    N_array_2d *ycomp = NULL;
    char *buff;


    /* Initialize GRASS */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster");
    module->description =
	_
	("Numerical calculation program for transient, confined and unconfined groundwater flow in two dimensions");

    /* Get parameters from user */
    set_params();

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /*Set the maximum iterations */
    sscanf(param.maxit->answer, "%i", &(maxit));
    /*Set the calculation error break criteria */
    sscanf(param.error->answer, "%lf", &(error));
    /*Set the solver */
    sscanf(param.solver->answer, "%i", &(solver));

    /*Some error catching */
    if (solver > 3)
	G_fatal_error
	    (_
	     ("The choosen solver <%i> does not exist. Valid solvers are  0 - cg, 1 - bicgstab, 2 - LU and 3 - Gauss"),
	     solver);

    if (solver > 1 && param.sparse->answer)
	G_fatal_error(_
		      ("The LU and Gauss solver do not work with sparse matrices"));


    /*get the current region */
    G_get_set_window(&region);

    /*allocate the geometry structure */
    geom = N_alloc_geom_data();

    /*Fill the geom structure with data */
    geom->rows = region.rows;
    geom->cols = region.cols;
    geom->dx = region.ew_res;
    geom->dy = region.ns_res;
    geom->Az = geom->dy * geom->dx;

    /*Set the function callback to the groundwater flow function */
    call = N_alloc_les_callback_2d();
    N_set_les_callback_2d_func(call, (*N_callback_gwflow_2d));	/*gwflow 2d */

    /*Allocate the groundwater flow data structure */
    data = N_alloc_gwflow_data2d(geom->cols, geom->rows);

    /* set the groundwater type */
    if (param.type->answer) {
	if (strncmp("unconfined", param.type->answer, 10) == 0) {
	    data->gwtype = N_GW_UNCONFINED;
	}
	else {
	    data->gwtype = N_GW_CONFINED;
	}
    }

    /*Set the calculation time */
    sscanf(param.dt->answer, "%lf", &(data->dt));

    /*read all input maps into the memory and take care of the
     * null values.*/
    N_read_rast_to_array_2d(param.phead->answer, data->phead);
    N_read_rast_to_array_2d(param.phead->answer, data->phead_start);
    N_read_rast_to_array_2d(param.status->answer, data->status);
    N_convert_array_2d_null_to_zero(data->status);
    N_read_rast_to_array_2d(param.kf_x->answer, data->kf_x);
    N_convert_array_2d_null_to_zero(data->kf_x);
    N_read_rast_to_array_2d(param.kf_y->answer, data->kf_y);
    N_convert_array_2d_null_to_zero(data->kf_y);
    N_read_rast_to_array_2d(param.q->answer, data->q);
    N_convert_array_2d_null_to_zero(data->q);
    N_read_rast_to_array_2d(param.s->answer, data->s);
    N_convert_array_2d_null_to_zero(data->s);
    N_read_rast_to_array_2d(param.top->answer, data->top);
    N_convert_array_2d_null_to_zero(data->top);
    N_read_rast_to_array_2d(param.bottom->answer, data->bottom);
    N_convert_array_2d_null_to_zero(data->bottom);

    /*Reacharge is optional */
    if (param.r->answer) {
	N_read_rast_to_array_2d(param.r->answer, data->r);
	N_convert_array_2d_null_to_zero(data->r);
    }

    /* Set the inactive values to zero, to assure a no flow boundary */
    for (y = 0; y < geom->rows; y++) {
	for (x = 0; x < geom->cols; x++) {
	    stat = (int)N_get_array_2d_d_value(data->status, x, y);
	    if (stat == N_CELL_INACTIVE) {	/*only inactive cells */
		N_put_array_2d_d_value(data->kf_x, x, y, 0);
		N_put_array_2d_d_value(data->kf_y, x, y, 0);
		N_put_array_2d_d_value(data->s, x, y, 0);
		N_put_array_2d_d_value(data->q, x, y, 0);
	    }
	}
    }


    /*assemble the linear equation system  and solve it */
    les = create_solve_les(geom, data, call, solver, maxit, error);

    /* copy the result into the phead array for output or unconfined calculation */
    copy_result(data->status, data->phead_start, les->x, &region, data->phead);

  /****************************************************/
    /*explicite calculation of free groundwater surface */

  /****************************************************/
    if (data->gwtype == N_GW_UNCONFINED) {
	/* allocate memory and copy the result into a new temporal vector */
	if (!(tmp_vect = (double *)calloc(les->rows, sizeof(double))))
	    G_fatal_error(_("Out of memory"));

	/*copy data */
	for (i = 0; i < les->rows; i++)
	    tmp_vect[i] = les->x[i];

	/*count the number of inner iterations */
	inner_count = 0;

	do {
	    G_message(_("Calculation of unconfined groundwater flow loop %i\n"),
		      inner_count + 1);

	    /* we will allocate a new les for each loop */
	    if (les)
		N_free_les(les);

	    /*assemble the linear equation system  and solve it */
	    les = create_solve_les(geom, data, call, solver, maxit, error);

	    /*calculate the maximum norm of the groundwater height difference */
	    tmp = 0;
	    max_norm = 0;
	    for (i = 0; i < les->rows; i++) {
		tmp = fabs(les->x[i] - tmp_vect[i]);
		if (max_norm < tmp)
		    max_norm = tmp;

		/*copy the result */
		tmp_vect[i] = les->x[i];
	    }

	    G_message(_
		      ("Maximum difference between this and last inkrement: %g"),
		      max_norm);

	    /* copy the result into the phead array */
	    copy_result(data->status, data->phead_start, les->x, &region,
			data->phead);
	     /**/ inner_count++;
	}
	while (max_norm > 0.01 && inner_count < 100);

	/*release memory */
	if (tmp_vect)
	    free(tmp_vect);
    }

    /*write the result to the output file */
    N_write_array_2d_to_rast(data->phead, param.output->answer);

    /*release the memory */
    if (les)
	N_free_les(les);


    /*Compute the the velocity field if required and write the result into three rast maps */
    if (param.vector->answer) {
	/* calculate the vector field and write the interpolated components to rast maps */
	field =
	    N_compute_gradient_field_2d(data->phead, data->kf_x, data->kf_y,
					geom);

	/*allocate the vector arrays */
	xcomp = N_alloc_array_2d(geom->cols, geom->rows, 1, DCELL_TYPE);
	ycomp = N_alloc_array_2d(geom->cols, geom->rows, 1, DCELL_TYPE);

	/*compute the vector components */
	N_compute_gradient_field_components_2d(field, xcomp, ycomp);

	/*write data to raster maps */
	G_asprintf(&buff, "%s_x", param.vector->answer);
	N_write_array_2d_to_rast(xcomp, buff);
	G_asprintf(&buff, "%s_y", param.vector->answer);
	N_write_array_2d_to_rast(ycomp, buff);
	if (buff)
	    G_free(buff);

	if (xcomp)
	    N_free_array_2d(xcomp);
	if (ycomp)
	    N_free_array_2d(ycomp);
	if (field)
	    N_free_gradient_field_2d(field);
    }


    if (data)
	N_free_gwflow_data2d(data);
    if (geom)
	G_free(geom);
    if (call)
	G_free(call);

    return (EXIT_SUCCESS);
}


/* ************************************************************************* */
/* this function copies the result from the x vector to a N_array_2d array * */
/* ************************************************************************* */
void
copy_result(N_array_2d * status, N_array_2d * phead_start, double *result,
	    struct Cell_head *region, N_array_2d * target)
{
    int y, x, rows, cols, count, stat;
    double d1 = 0;
    DCELL val;

    rows = region->rows;
    cols = region->cols;

    count = 0;
    for (y = 0; y < rows; y++) {
	G_percent(y, rows - 1, 10);
	for (x = 0; x < cols; x++) {
	    stat = (int)N_get_array_2d_d_value(status, x, y);
	    if (stat == N_CELL_ACTIVE) {	/*only active cells */
		d1 = result[count];
		val = (DCELL) d1;
		count++;
	    }
	    else if (stat == N_CELL_DIRICHLET) {	/*dirichlet cells */
		d1 = N_get_array_2d_d_value(phead_start, x, y);
		val = (DCELL) d1;
	    }
	    else {
		G_set_null_value(&val, 1, DCELL_TYPE);
	    }
	    N_put_array_2d_d_value(target, x, y, val);
	}
    }

    return;
}

/* *************************************************************** */
/* ***** create and solve the linear equation system ************* */
/* *************************************************************** */
N_les *create_solve_les(N_geom_data * geom, N_gwflow_data2d * data,
			N_les_callback_2d * call, int solver, int maxit,
			double error)
{

    N_les *les;

    /*assemble the linear equation system */
    if (param.sparse->answer)
	les =
	    N_assemble_les_2d(N_SPARSE_LES, geom, data->status, data->phead,
			      (void *)data, call);
    else
	les =
	    N_assemble_les_2d(N_NORMAL_LES, geom, data->status, data->phead,
			      (void *)data, call);

    /*solve the equation system */
    switch (solver) {
    case 0:
	N_solver_cg(les, maxit, error);
	break;
    case 1:
	N_solver_bicgstab(les, maxit, error);
	break;
    case 2:
	N_solver_lu(les);
	break;
    case 3:
	N_solver_gauss(les);
	break;
    }

    if (les == NULL)
	G_fatal_error(_
		      ("Could not create and solve the linear equation system"));

    return les;
}

