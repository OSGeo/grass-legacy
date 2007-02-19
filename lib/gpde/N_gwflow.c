
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      groundwater flow in porous media 
* 		part of the gpde library
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include "grass/N_gwflow.h"

/* *************************************************************** */
/* ***************** N_gwflow_data3d ***************************** */
/* *************************************************************** */
/*!
 * \brief Alllocate memory for the groundwater calculation data structure in 3 dimensions
 *
 * The groundwater calculation data structure will be allocated and
 * all appendant 3d arrays. The offset for the 3d arrays  is 1 because 
 * a 7 point star scheme is used to create the mass balance.
 *
 * \param cols   int
 * \param rows   int
 * \param depths int
 * \return N_gwflow_data3d *
 * */
N_gwflow_data3d *N_alloc_gwflow_data3d(int cols, int rows, int depths)
{
    N_gwflow_data3d *data;

    data = (N_gwflow_data3d *) G_calloc(1, sizeof(N_gwflow_data3d));

    data->phead = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->phead_start = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->status = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->kf_x = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->kf_y = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->kf_z = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->q = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->s = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->nf = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->r = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);

    return data;
}

/* *************************************************************** */
/* ********************* N_free_gwflow_data3d ******************** */
/* *************************************************************** */
/*!
 * \brief Release the memory for the groundwater calculation data structure in 3 dimensions
 *
 * \param data N_gwflow_data3d *
 * \return void *
 * */

void N_free_gwflow_data3d(N_gwflow_data3d * data)
{
    N_free_array_3d(data->phead);
    N_free_array_3d(data->phead_start);
    N_free_array_3d(data->status);
    N_free_array_3d(data->kf_x);
    N_free_array_3d(data->kf_y);
    N_free_array_3d(data->kf_z);
    N_free_array_3d(data->q);
    N_free_array_3d(data->s);
    N_free_array_3d(data->nf);
    N_free_array_2d(data->r);

    G_free(data);

    data = NULL;;

    return;
}

/* *************************************************************** */
/* ******************** N_alloc_gwflow_data2d ******************** */
/* *************************************************************** */
/*!
 * \brief Alllocate memory for the groundwater calculation data structure in 2 dimensions
 *
 * The groundwater calculation data structure will be allocated and
 * all appendant 2d arrays. The offset for the 2d arrays  is 1 because 
 * a 5 point star scheme is used to create the mass balance.
 *
 * \param cols int
 * \param rows int
 * \return N_gwflow_data2d *
 * */
N_gwflow_data2d *N_alloc_gwflow_data2d(int cols, int rows)
{
    N_gwflow_data2d *data;

    data = (N_gwflow_data2d *) G_calloc(1, sizeof(N_gwflow_data2d));

    data->phead = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->phead_start = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->status = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->kf_x = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->kf_y = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->q = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->s = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->nf = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->r = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->top = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->bottom = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);

    return data;
}

/* *************************************************************** */
/* ****************** N_free_gwflow_data2d *********************** */
/* *************************************************************** */
/*!
 * \brief Release the memory for the groundwater calculation data structure in 2 dimensions
 *
 * \param data N_gwflow_data2d *
 * \return void
 * */
void N_free_gwflow_data2d(N_gwflow_data2d * data)
{
    N_free_array_2d(data->phead);
    N_free_array_2d(data->phead_start);
    N_free_array_2d(data->status);
    N_free_array_2d(data->kf_x);
    N_free_array_2d(data->kf_y);
    N_free_array_2d(data->q);
    N_free_array_2d(data->s);
    N_free_array_2d(data->nf);
    N_free_array_2d(data->r);
    N_free_array_2d(data->top);
    N_free_array_2d(data->bottom);

    G_free(data);

    data = NULL;;

    return;
}

/* *************************************************************** */
/* ***************** N_callback_gwflow_3d ************************ */
/* *************************************************************** */
/*!
 * \brief This callback function creates the mass bilanz of a 7 point star
 *
 * The mass balance is the the common groundwater flow equation based
 * on darcy. 
 * \f[Ss \frac{\partial h}{\partial t} = \nabla {\bf K} \nabla h - q \f]
 *
 * This equation is discretizised with the finite volume method in three dimensions.
 *
 *
 * \param gwdata N_gwflow_data3d *
 * \param geom N_geom_data *
 * \param col   int
 * \param row   int
 * \param depth int
 * \return N_data_star *
 *
 * */
N_data_star *N_callback_gwflow_3d(void *gwdata, N_geom_data * geom, int col,
				  int row, int depth)
{
    double kf_e = 0, kf_w = 0, kf_n = 0, kf_s = 0, kf_t = 0, kf_b = 0;
    double dx, dy, dz, Ax, Ay, Az;
    double kf_x, kf_y, kf_z;
    double kf_xw, kf_yn, kf_zt;
    double kf_xe, kf_ys, kf_zb;
    double hc_start;
    double Ss, r, nf, q;
    double C, W, E, N, S, T, B, V;
    N_data_star *mat_pos;
    N_gwflow_data3d *data;

    /*cast the void pointer to the right data structure */
    data = (N_gwflow_data3d *) gwdata;

    dx = geom->dx;
    dy = geom->dy;
    dz = geom->dz;
    Ax = geom->dy * geom->dz;
    Ay = geom->dx * geom->dz;
    Az = geom->dx * geom->dy;

    /*read the data from the arrays */
    hc_start = N_get_array_3d_d_value(data->phead_start, col, row, depth);

    kf_x = N_get_array_3d_d_value(data->kf_x, col, row, depth);
    kf_y = N_get_array_3d_d_value(data->kf_y, col, row, depth);
    kf_z = N_get_array_3d_d_value(data->kf_z, col, row, depth);

    kf_xw = N_get_array_3d_d_value(data->kf_x, col - 1, row, depth);
    kf_xe = N_get_array_3d_d_value(data->kf_x, col + 1, row, depth);
    kf_yn = N_get_array_3d_d_value(data->kf_y, col, row - 1, depth);
    kf_ys = N_get_array_3d_d_value(data->kf_y, col, row + 1, depth);
    kf_zt = N_get_array_3d_d_value(data->kf_z, col, row, depth + 1);
    kf_zb = N_get_array_3d_d_value(data->kf_z, col, row, depth - 1);

    if (kf_xw + kf_x != 0)
	kf_w = 2 * kf_xw * kf_x / (kf_xw + kf_x);
    if (kf_xe + kf_x != 0)
	kf_e = 2 * kf_xe * kf_x / (kf_xe + kf_x);
    if (kf_yn + kf_y != 0)
	kf_n = 2 * kf_yn * kf_y / (kf_yn + kf_y);
    if (kf_ys + kf_y != 0)
	kf_s = 2 * kf_ys * kf_y / (kf_ys + kf_y);
    if (kf_zt + kf_z != 0)
	kf_t = 2 * kf_zt * kf_z / (kf_zt + kf_z);
    if (kf_zb + kf_z != 0)
	kf_b = 2 * kf_zb * kf_z / (kf_zb + kf_z);

    /*inner sources */
    q = N_get_array_3d_d_value(data->q, col, row, depth);
    /*specific yield */
    Ss = N_get_array_3d_d_value(data->s, col, row, depth);
    /*porosity */
    nf = N_get_array_3d_d_value(data->nf, col, row, depth);

    /*mass balance center cell to western cell */
    W = -1 * Ax * kf_w / dx;
    /*mass balance center cell to eastern cell */
    E = -1 * Ax * kf_e / dx;
    /*mass balance center cell to northern cell */
    N = -1 * Ay * kf_n / dy;
    /*mass balance center cell to southern cell */
    S = -1 * Ay * kf_s / dy;
    /*mass balance center cell to top cell */
    T = -1 * Az * kf_t / dz;
    /*mass balance center cell to bottom cell */
    B = -1 * Az * kf_b / dz;

    /*specific yield */
    Ss = Az * dz * Ss;

    /*the diagonal entry of the matrix */
    C = -1 * (W + E + N + S + T + B - Ss / data->dt);

    /*the entry in the right side b of Ax = b */
    V = (q + hc_start * Ss / data->dt);

    /*only the top cells will have reacharge */
    if (depth == geom->depths - 2) {
	r = N_get_array_2d_d_value(data->r, col, row);
	V += r * Az;
    }

    G_debug(5, "N_callback_gwflow_3d: called [%i][%i][%i]", depth, col, row);

    /*create the 7 point star entries */
    mat_pos = N_create_7star(C, W, E, N, S, T, B, V);

    return mat_pos;
}

/* *************************************************************** */
/* ****************** N_callback_gwflow_2d *********************** */
/* *************************************************************** */
/*!
 * \brief This callback function creates the mass bilanz of a 5 point star
 *
 * The mass balance is the the common groundwater flow equation based
 * on darcy. 
 * \f[Ss \frac{\partial h}{\partial t} = \nabla {\bf K} \nabla h - q \f]
 *
 * This equation is discretizised with the finite volume method in two dimensions.
 *
 * \param gwdata N_gwflow_data2d *
 * \param geom N_geom_data *
 * \param col int
 * \param row int
 * \return N_data_star *
 *
 * */
N_data_star *N_callback_gwflow_2d(void *gwdata, N_geom_data * geom, int col,
				  int row)
{
    double T_e = 0, T_w = 0, T_n = 0, T_s = 0;
    double z_e = 0, z_w = 0, z_n = 0, z_s = 0;
    double dx, dy, Az;
    double kf_x, kf_y;
    double z, top;
    double kf_xw, kf_yn;
    double z_xw, z_yn;
    double kf_xe, kf_ys;
    double z_xe, z_ys;
    double hc, hc_start;
    double Ss, r, nf, q;
    double C, W, E, N, S, V;
    N_gwflow_data2d *data;
    N_data_star *mat_pos;

    /*cast the void pointer to the right data structure */
    data = (N_gwflow_data2d *) gwdata;

    dx = geom->dx;
    dy = geom->dy;
    Az = geom->dx * geom->dy;

    /*read the data from the arrays */
    hc_start = N_get_array_2d_d_value(data->phead_start, col, row);
    hc = N_get_array_2d_d_value(data->phead, col, row);
    top = N_get_array_2d_d_value(data->top, col, row);


    if (hc > top) {		/*If the aquifer is confined */
	z = N_get_array_2d_d_value(data->top, col,
				       row) -
	    N_get_array_2d_d_value(data->bottom, col, row);
	z_xw =
	    N_get_array_2d_d_value(data->top, col - 1,
				       row) -
	    N_get_array_2d_d_value(data->bottom, col - 1, row);
	z_xe =
	    N_get_array_2d_d_value(data->top, col + 1,
				       row) -
	    N_get_array_2d_d_value(data->bottom, col + 1, row);
	z_yn =
	    N_get_array_2d_d_value(data->top, col,
				       row - 1) -
	    N_get_array_2d_d_value(data->bottom, col, row - 1);
	z_ys =
	    N_get_array_2d_d_value(data->top, col,
				       row + 1) -
	    N_get_array_2d_d_value(data->bottom, col, row + 1);

	if (z_xw + z != 0)
	    z_w = (z_xw + z) / 2;
	if (z_xe + z != 0)
	    z_e = (z_xe + z) / 2;
	if (z_yn + z != 0)
	    z_n = (z_yn + z) / 2;
	if (z_ys + z != 0)
	    z_s = (z_ys + z) / 2;
    }
    else {			/* the aquifer is unconfined */

	/* If the aquifer is unconfied use an explicite scheme to solve
	 * the nonlinear equation. We use the phead from the first iteration */
	z = N_get_array_2d_d_value(data->phead, col, row);
	z_xw = N_get_array_2d_d_value(data->phead, col - 1, row);
	z_xe = N_get_array_2d_d_value(data->phead, col + 1, row);
	z_yn = N_get_array_2d_d_value(data->phead, col, row - 1);
	z_ys = N_get_array_2d_d_value(data->phead, col, row + 1);

	if (z_xw + z != 0)
	    z_w = (z_xw + z) / 2;
	if (z_xe + z != 0)
	    z_e = (z_xe + z) / 2;
	if (z_yn + z != 0)
	    z_n = (z_yn + z) / 2;
	if (z_ys + z != 0)
	    z_s = (z_ys + z) / 2;

    }

    /* Inner sources */
    q = N_get_array_2d_d_value(data->q, col, row);
    nf = N_get_array_2d_d_value(data->nf, col, row);

    /* specific yield */
    Ss = N_get_array_2d_d_value(data->s, col, row) * Az;
    /* reacharge */
    r = N_get_array_2d_d_value(data->r, col, row);

    /*get the surrounding permeabilities */
    kf_x = N_get_array_2d_d_value(data->kf_x, col, row);
    kf_y = N_get_array_2d_d_value(data->kf_y, col, row);
    kf_xw = N_get_array_2d_d_value(data->kf_x, col - 1, row);
    kf_xe = N_get_array_2d_d_value(data->kf_x, col + 1, row);
    kf_yn = N_get_array_2d_d_value(data->kf_y, col, row - 1);
    kf_ys = N_get_array_2d_d_value(data->kf_y, col, row + 1);

    /* calculate the transmissivities */
    if (kf_xw + kf_x != 0)
	T_w = 2 * kf_xw * kf_x / (kf_xw + kf_x) * z_w;
    if (kf_xe + kf_x != 0)
	T_e = 2 * kf_xe * kf_x / (kf_xe + kf_x) * z_e;
    if (kf_yn + kf_y != 0)
	T_n = 2 * kf_yn * kf_y / (kf_yn + kf_y) * z_n;
    if (kf_ys + kf_y != 0)
	T_s = 2 * kf_ys * kf_y / (kf_ys + kf_y) * z_s;

    /*mass balance center cell to western cell */
    W = -1 * T_w * dy / dx;
    /*mass balance center cell to eastern cell */
    E = -1 * T_e * dy / dx;
    /*mass balance center cell to northern cell */
    N = -1 * T_n * dx / dy;
    /*mass balance center cell to southern cell */
    S = -1 * T_s * dx / dy;

    /*the diagonal entry of the matrix */
    C = -1 * (W + E + N + S - Ss / data->dt);

    /*the entry in the right side b of Ax = b */
    V = (q + hc_start * Ss / data->dt) + r * Az;

    G_debug(5, "N_callback_gwflow_2d: called [%i][%i]", row, col);

    /*create the 5 point star entries */
    mat_pos = N_create_5star(C, W, E, N, S, V);

    return mat_pos;
}
