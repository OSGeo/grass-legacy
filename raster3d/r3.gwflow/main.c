
/****************************************************************************
*
* MODULE:       r3.gwflow 
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert <at> gmx <dot> de
* 		27 11 2006 Berlin
* PURPOSE:      Calculates three dimensional groundwater flow
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
#include <grass/gis.h>
#include <grass/G3d.h>
#include <grass/glocale.h>
#include <grass/N_pde.h>
#include <grass/N_gwflow.h>


/*- Parameters and global variables -----------------------------------------*/
typedef struct
{
    struct Option *output, *phead, *status, *kf_x, *kf_y, *kf_z, *q, *s, *r,
	*dt, *maxit, *error, *solver;
    struct Flag *mask;
    struct Flag *sparse;
} paramType;

paramType param;		/*Parameters */

/*- prototypes --------------------------------------------------------------*/
void set_params();		/*Fill the paramType structure */
void fill_array_3d(N_array_3d * data, G3D_Region * region, char *name);
void write_result(N_array_3d * status, N_array_3d * phead_start, double *result, G3D_Region * region,
		    char *name);

/* ************************************************************************* */
/* Set up the arguments we are expecting ********************************** */
/* ************************************************************************* */
void set_params()
{
    param.phead = G_define_option();
    param.phead->key = "phead";
    param.phead->type = TYPE_STRING;
    param.phead->required = YES;
    param.phead->gisprompt = "old,grid3,3d-raster";
    param.phead->description = _("The initial piezometric head in [m]");

    param.status = G_define_option();
    param.status->key = "status";
    param.status->type = TYPE_STRING;
    param.status->required = YES;
    param.status->gisprompt = "old,grid3,3d-raster";
    param.status->description =
	_("The status for each cell, = 0 - inactive, 1 - active, 2 - dirichlet");

    param.kf_x = G_define_option();
    param.kf_x->key = "kf_x";
    param.kf_x->type = TYPE_STRING;
    param.kf_x->required = YES;
    param.kf_x->gisprompt = "old,grid3,3d-raster";
    param.kf_x->description = _("The x-part of the transmissivity tensor in [m/s]");

    param.kf_y = G_define_option();
    param.kf_y->key = "kf_y";
    param.kf_y->type = TYPE_STRING;
    param.kf_y->required = YES;
    param.kf_y->gisprompt = "old,grid3,3d-raster";
    param.kf_y->description = _("The y-part of the transmissivity tensor in [m/s]");

    param.kf_z = G_define_option();
    param.kf_z->key = "kf_z";
    param.kf_z->type = TYPE_STRING;
    param.kf_z->required = YES;
    param.kf_z->gisprompt = "old,grid3,3d-raster";
    param.kf_z->description = _("The z-part of the transmissivity tensor in [m/s]");

    param.q = G_define_option();
    param.q->key = "q";
    param.q->type = TYPE_STRING;
    param.q->required = NO;
    param.q->gisprompt = "old,grid3,3d-raster";
    param.q->description = _("Sources and sinks in [m^3/s]");

    param.s = G_define_option();
    param.s->key = "s";
    param.s->type = TYPE_STRING;
    param.s->required = YES;
    param.s->gisprompt = "old,grid3,3d-raster";
    param.s->description = _("Specific yield in 1/m");

    param.r = G_define_option();
    param.r->key = "r";
    param.r->type = TYPE_STRING;
    param.r->required = NO;
    param.r->gisprompt = "old,raster,raster";
    param.r->description = _("Reacharge raster map in m^3/s");

    param.output = G_define_option();
    param.output->key = "output";
    param.output->type = TYPE_STRING;
    param.output->required = YES;
    param.output->gisprompt = "new,grid3,3d-raster";
    param.output->description = _("The output");

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
    param.error->description = _("Break criteria for the cg and bicgstab solver");

    param.solver = G_define_option();
    param.solver->key = "solver";
    param.solver->type = TYPE_INTEGER;
    param.solver->required = NO;
    param.solver->answer = "0";
    param.solver->description = _("Which kind of solver should be used, 0 - cg, 1 - bicgstab, 2 - LU, 3 - Gauss");

    param.mask = G_define_flag();
    param.mask->key = 'm';
    param.mask->description = _("Use G3D mask (if exists)");

    param.sparse = G_define_flag();
    param.sparse->key = 's';
    param.sparse->description = _("Use a sparse linear equation system, only available with cg and bicgstab solver");

}

/* ************************************************************************* */
/* Main function, open the G3D map and create the raster maps ************** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    struct GModule *module;
    N_gwflow_data3d *data;
    N_geom_data *geom;
    N_les *les;
    N_les_callback_3d *call;
    G3D_Region region;
    double error;
    int maxit;
    int solver;

    /* Initialize GRASS */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster3d, voxel");
    module->description = _("Groundwater calculation program");

    /* Get parameters from user */
    set_params();

    /* Have GRASS get pheads */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /*Set the maximum iterations */
    sscanf(param.maxit->answer, "%i", &(maxit));
    /*Set the calculation error break criteria */
    sscanf(param.error->answer, "%lf", &(error));
    /*Set the solver*/
    sscanf(param.solver->answer, "%i", &(solver));

    /*Some error catching*/
    if(solver > 3)
      G3d_fatalError("The choosen solver <%i> does not exist. Valid solvers are  0 - cg, 1 - bicgstab, 2 - LU and 3 - Gauss", solver);

    if(solver > 1 && param.sparse->answer)
    	G3d_fatalError("The LU and Gauss solver do not work with sparse matrices");


    /*Set the defaults */
    G3d_initDefaults();

    /*get the current region */
    G3d_getWindow(&region);

    /*allocate the geometry structure*/
    geom = N_alloc_geom_data();

    /*Fill the geom structure with data*/
    geom->rows =   region.rows;
    geom->cols =   region.cols;
    geom->depths = region.depths;
    geom->dx = region.ew_res;
    geom->dy = region.ns_res;
    geom->dz = region.tb_res;
    geom->Ax = geom->dy * geom->dz;
    geom->Ay = geom->dx * geom->dz;
    geom->Az = geom->dy * geom->dx;

    /*Set the function callback to the groundwater flow function*/
    call = N_alloc_les_callback_3d();
    N_set_les_callback_3d_func(call, (*N_callback_gwflow_3d)); /*gwflow 3d*/

    /*Allocate the groundwater flow data structure */
    data = N_alloc_gwflow_data3d(geom->depths, geom->rows, geom->cols);

    /*Set the calculation time */
    sscanf(param.dt->answer, "%lf", &(data->dt));

    /*read the g3d maps into the memory*/
    fill_array_3d(data->phead, &region, param.phead->answer);
    fill_array_3d(data->phead_start, &region, param.phead->answer);
    fill_array_3d(data->status, &region, param.status->answer);
    fill_array_3d(data->kf_x, &region, param.kf_x->answer);
    fill_array_3d(data->kf_y, &region, param.kf_y->answer);
    fill_array_3d(data->kf_z, &region, param.kf_z->answer);
    fill_array_3d(data->q, &region, param.q->answer);
    fill_array_3d(data->s, &region, param.s->answer);

    /*assemble the linear equation system*/
    if(param.sparse->answer)
    {
      les = N_assemble_les_3d(N_SPARSE_LES, geom, data->status, data->phead, (void *)data, call);
    } else {
      les = N_assemble_les_3d(N_NORMAL_LES, geom, data->status, data->phead, (void *)data, call);
    }
    
    /*solve the equation system*/
    if(solver == 0)
      N_solver_cg(les, maxit, error);
    else if(solver == 1)
      N_solver_bicgstab(les, maxit, error);
    else if(solver == 2)
      N_solver_lu(les);
    else if(solver == 3)
      N_solver_gauss(les);
    
    /*write the result to the output file*/
    write_result(data->status, data->phead_start, les->x, &region, param.output->answer);

    /*release the memory*/
    N_free_les(les);
    N_free_gwflow_data3d(data);
    G_free(geom);
    G_free(call);

    return (EXIT_SUCCESS);
}

/* ************************************************************************* */
/* Read the data from a g3d map into an N_array_3d ************************* */
/* ************************************************************************* */
void fill_array_3d(N_array_3d * data, G3D_Region * region, char *name)
{
    void *map = NULL;		/*The 3D Rastermap */
    int changemask = 0;
    int z, y, x, rows, cols, depths, type;
    double d1 = 0, f1 = 0;

    rows = region->rows;
    cols = region->cols;
    depths = region->depths;


    if (NULL == G_find_grid3(name, ""))
	G3d_fatalError(_("Requested g3d map <%s> not found"), name);

    /*Open all maps with default region */
    map =
	G3d_openCellOld(name, G_find_grid3(name, ""), G3D_DEFAULT_WINDOW,
			G3D_TILE_SAME_AS_FILE, G3D_USE_CACHE_DEFAULT);

    if (map == NULL)
	G3d_fatalError(_("Error opening g3d map <%s>"), name);

    type = G3d_tileTypeMap(map); 

    G_message(_("Copy g3d map <%s> into the memory"), name);

    /*if requested set the Mask on */
    if (param.mask->answer) {
	if (G3d_maskFileExists()) {
	    changemask = 0;
	    if (G3d_maskIsOff(map)) {
		G3d_maskOn(map);
		changemask = 1;
	    }
	}
    }

    for (z = 0; z < depths; z++) {	/*From the bottom to the top */
	G_percent(z, depths - 1, 10);
	for (y = 0; y < rows; y++) {
	    for (x = 0; x < cols; x++) {
		if (type == G3D_FLOAT) {
		    G3d_getValue(map, x, y, z, &f1, type);
		    N_put_array_3d_value_double(data, z, y, x, (double)f1);
		}
		else {
		    G3d_getValue(map, x, y, z, &d1, type);
		    N_put_array_3d_value_double(data, z, y, x, d1);
		}
	    }
	}
    }

    /*We set the Mask off, if it was off before */
    if (param.mask->answer) {
	if (G3d_maskFileExists())
	    if (G3d_maskIsOn(map) && changemask)
		G3d_maskOff(map);
    }

    /* Close files and exit */
    if (!G3d_closeCell(map))
	G3d_fatalError(map, NULL, 0, _("Error closing g3d file"));

    return;
}


/* ************************************************************************* */
/* this function writes the result from the x vector to a g3d map ********** */
/* ************************************************************************* */
void write_result(N_array_3d * status, N_array_3d * phead_start, double *result, G3D_Region * region,
		    char *name)
{
    void *map = NULL;		/*The 3D Rastermap */
    int changemask = 0;
    int z, y, x, rows, cols, depths, count, stat;
    double d1 = 0;

    rows = region->rows;
    cols = region->cols;
    depths = region->depths;

    /*Open the new map*/
    map = G3d_openCellNew(name, G3D_DOUBLE, G3D_USE_CACHE_DEFAULT, region);

    if (map == NULL)
	G3d_fatalError(_("Error opening g3d map <%s>"), name);

    G_message(_("Write the result to new g3d map <%s>"), name);

    /*if requested set the Mask on */
    if (param.mask->answer) {
	if (G3d_maskFileExists()) {
	    changemask = 0;
	    if (G3d_maskIsOff(map)) {
		G3d_maskOn(map);
		changemask = 1;
	    }
	}
    }

    count = 0;
    for (z = 0; z < depths; z++) {	/*From the bottom to the top */
	G_percent(z, depths - 1, 10);
	for (y = 0; y < rows; y++) {
	    for (x = 0; x < cols; x++) {
		stat = (int)N_get_array_3d_value_double(status, z, y, x);
		if (stat == N_CELL_ACTIVE) { /*only active cells*/
		    d1 = result[count];
		    count++;
		}
		else if (stat == N_CELL_DIRICHLET) { /*dirichlet cells*/
		    d1 = N_get_array_3d_value_double(phead_start, z, y, x);
		} else {
		    G3d_setNullValue(&d1, 1, G3D_DOUBLE);
		}
		G3d_putDouble(map, x, y, z, d1);
	    }
	}
    }

    /*We set the Mask off, if it was off before */
    if (param.mask->answer) {
	if (G3d_maskFileExists())
	    if (G3d_maskIsOn(map) && changemask)
		G3d_maskOff(map);
    }

    /* Close files and exit */
    if (!G3d_closeCell(map))
	G3d_fatalError(map, NULL, 0, _("Error closing g3d file"));

    return;
}

