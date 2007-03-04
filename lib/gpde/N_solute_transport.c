
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      solute transport in porous media
* 		part of the gpde library
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include "grass/N_solute_transport.h"


/* local Prototypes */
int local_peclet(double z);

/* ************************************************************************* *
 * ************************************************************************* *
 * ************************************************************************* */
/*! \brief This is just a placeholder
 *
 * */
N_data_star *N_callback_solute_transport_3d(void *solutedata,
					    N_geom_data * geom, int col,
					    int row, int depth)
{
    N_data_star *mat_pos = NULL;
    double C = 4, W = -1, E = -1, N = -1, S = -1, T = -1, B = -1, V = 0;


    /*create the 5 point star entries */
    mat_pos = N_create_7star(C, W, E, N, S, T, B, V);

    return mat_pos;
}

/* ************************************************************************* *
 * ************************************************************************* *
 * ************************************************************************* */
/*!
 * \brief This callback function creates the mass balance of a 5 point star
 *
 * The mass balance is based on the common solute transport equation:
 *
 * \f[\frac{\partial c_g}{\partial t} R = \nabla \cdot ({\bf D} \nabla c_g - {\bf u} c_g) + \sigma + \frac{q}{n_f}(c_g - c_in) \f]
 *
 * This equation is discretizised with the finite volume method in two dimensions.
 *
 *
 * \param solutedata  * N_solute_transport_data2d - a void pointer to the data structure
 * \param geom N_geom_data *
 * \param col   int
 * \param row   int
 * \return N_data_star * - a five point data star
 *
 * */
N_data_star *N_callback_solute_transport_2d(void *solutedata,
					    N_geom_data * geom, int col,
					    int row)
{
    double Df_e = 0, Df_w = 0, Df_n = 0, Df_s = 0;
    double z_e = 0, z_w = 0, z_n = 0, z_s = 0;
    double dx, dy, Az;
    double diff_x, diff_y;
    double z;
    double diff_xw, diff_yn;
    double z_xw, z_yn;
    double diff_xe, diff_ys;
    double z_xe, z_ys;
    double cin = 0, cg, cg_start;
    double R, nf, cs, q;
    double C, W, E, N, S, V;
    double rw = 0.5, re = 0.5, rn = 0.5, rs = 0.5;
    double vw = 0, ve = 0, vn = 0, vs = 0;
    double Ds_w = 0, Ds_e = 0, Ds_n = 0, Ds_s = 0;
    double Dw = 0, De = 0, Dn = 0, Ds = 0;

    N_solute_transport_data2d *data = NULL;
    N_data_star *mat_pos;
    N_gradient_2d grad;

    /*cast the void pointer to the right data structure */
    data = (N_solute_transport_data2d *) solutedata;

    N_get_gradient_2d(data->grad, &grad, col, row);

    dx = geom->dx;
    dy = geom->dy;
    Az = geom->dx * geom->dy;

    /*read the data from the arrays */
    cg_start = N_get_array_2d_d_value(data->c_start, col, row);
    cg = N_get_array_2d_d_value(data->c, col, row);

    /* calculate the cell height */
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

    /*get the surrounding diffusion tensor entries */
    diff_x = N_get_array_2d_d_value(data->diff_x, col, row);
    diff_y = N_get_array_2d_d_value(data->diff_y, col, row);
    diff_xw = N_get_array_2d_d_value(data->diff_x, col - 1, row);
    diff_xe = N_get_array_2d_d_value(data->diff_x, col + 1, row);
    diff_yn = N_get_array_2d_d_value(data->diff_y, col, row - 1);
    diff_ys = N_get_array_2d_d_value(data->diff_y, col, row + 1);

    /* calculate the diffusion on the cell borders using the harmonical mean */
    if (diff_xw + diff_x != 0)
	Df_w = 2 * diff_xw * diff_x / (diff_xw + diff_x);
    if (diff_xe + diff_x != 0)
	Df_e = 2 * diff_xe * diff_x / (diff_xe + diff_x);
    if (diff_yn + diff_y != 0)
	Df_n = 2 * diff_yn * diff_y / (diff_yn + diff_y);
    if (diff_ys + diff_y != 0)
	Df_s = 2 * diff_ys * diff_y / (diff_ys + diff_y);


    /* calculate the dispersion */
    /*todo */

    /* calculate the velocity parts */
    vw = grad.WC;
    if ((Df_w + Ds_w) != 0)
	rw = local_peclet(dx * vw / (Df_w + Ds_w));

    ve = grad.EC;
    if ((Df_e + Ds_e) != 0)
	re = local_peclet(dx * ve / (Df_e + Ds_e));

    vn = grad.NC;
    if ((Df_n + Ds_n) != 0)
	rn = local_peclet(dy * vn / (Df_n + Ds_n));

    vs = grad.SC;
    if ((Df_s + Ds_s) != 0)
	rs = local_peclet(dy * vs / (Df_s + Ds_s));

    /* put the diffusion and dispersion together */
    Dw = ((Df_w + Ds_w));
    De = ((Df_e + Ds_e));
    Dn = ((Df_n + Ds_n));
    Ds = ((Df_s + Ds_s));


    /*mass balance center cell to western cell */
    W = -1 * (Dw - vw * (1 - rw)) * dy * z_w / dx;
    /*mass balance center cell to eastern cell */
    E = -1 * (De - ve * (1 - re)) * dy * z_e / dx;
    /*mass balance center cell to northern cell */
    N = -1 * (Dn - vn * (1 - rn)) * dx * z_n / dy;
    /*mass balance center cell to southern cell */
    S = -1 * (Ds - vs * (1 - rs)) * dx * z_s / dy;

    /* Retardation */
    R = N_get_array_2d_d_value(data->R, col, row);
    /* Inner sources */
    cs = N_get_array_2d_d_value(data->cs, col, row);
    /* effective porosity */
    nf = N_get_array_2d_d_value(data->nf, col, row);
    /* groundwater sources and sinks */
    q = N_get_array_2d_d_value(data->q, col, row);



    /*the diagonal entry of the matrix */
    C = ((Dw - vw * (rw)) * dy * z_w / dx +
	 (De - ve * (re)) * dy * z_e / dx +
	 (Dn - vn * (rn)) * dx * z_n / dy +
	 (Ds - vs * (rs)) * dx * z_s / dy + Az * z * R / data->dt);

    /*the entry in the right side b of Ax = b */
    V = (cs + cg_start * Az * z * R / data->dt);

    G_debug(5, "N_callback_solute_transport_2d: called [%i][%i]", row, col);

    /*create the 5 point star entries */
    mat_pos = N_create_5star(C, W, E, N, S, V);

    return mat_pos;
}

/* ************************************************************************* *
 * ************************************************************************* *
 * ************************************************************************* */
/*!
 * \brief Alllocate memory for the solute transport data structure in three dimensions
 *
 * The solute transport data structure will be allocated including
 * all appendant 3d arrays. The offset for the 3d arrays is one
 * to establish homogeneous Neumann boundary conditions at the calculation area border.
 * This data structure is used to create a linear equation system based on the computation of
 * solute transport in porous media with the finite volume method.
 *
 * \param cols   int
 * \param rows   int
 * \param depths int
 * \return N_solute_transport_data3d *
 * */

N_solute_transport_data3d *N_alloc_solute_transport_data3d(int cols, int rows,
							   int depths)
{
    N_solute_transport_data3d *data = NULL;

    data =
	(N_solute_transport_data3d *) G_calloc(1,
					       sizeof
					       (N_solute_transport_data3d));

    data->c = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->c_start = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->status = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->diff_x = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->diff_y = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->diff_z = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->q = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->cs = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->R = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->nf = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);
    data->cin = N_alloc_array_3d(cols, rows, depths, 1, DCELL_TYPE);

    data->grad = N_alloc_gradient_field_3d(cols, rows, depths);

    return data;
}

/* ************************************************************************* *
 * ************************************************************************* *
 * ************************************************************************* */
/*!
 * \brief Alllocate memory for the solute transport data structure in two dimensions
 *
 * The solute transport data structure will be allocated including
 * all appendant 2d arrays. The offset for the 2d arrays is one
 * to establish homogeneous Neumann boundary conditions at the calculation area border.
 * This data structure is used to create a linear equation system based on the computation of
 * solute transport in porous media with the finite volume method.
 *
 * \param cols   int
 * \param rows   int
 * \return N_solute_transport_data2d *
 * */


N_solute_transport_data2d *N_alloc_solute_transport_data2d(int cols, int rows)
{
    N_solute_transport_data2d *data = NULL;

    data =
	(N_solute_transport_data2d *) G_calloc(1,
					       sizeof
					       (N_solute_transport_data2d));

    data->c = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->c_start = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->status = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->diff_x = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->diff_y = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->q = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->cs = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->R = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->nf = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->cin = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->top = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    data->bottom = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);

    data->grad = N_alloc_gradient_field_2d(cols, rows);

    return data;
}

/* ************************************************************************* *
 * ************************************************************************* *
 * ************************************************************************* */
/*!
 * \brief Release the memory of the solute transport data structure in three dimensions
 *
 * \param data N_solute_transport_data2d *
 * \return void *
 * */
void N_free_solute_transport_data3d(N_solute_transport_data3d * data)
{
    N_free_array_3d(data->c);
    N_free_array_3d(data->c_start);
    N_free_array_3d(data->status);
    N_free_array_3d(data->diff_x);
    N_free_array_3d(data->diff_y);
    N_free_array_3d(data->diff_z);
    N_free_array_3d(data->q);
    N_free_array_3d(data->cs);
    N_free_array_3d(data->R);
    N_free_array_3d(data->nf);
    N_free_array_3d(data->cin);

    G_free(data);

    data = NULL;

    return;
}

/* ************************************************************************* *
 * ************************************************************************* *
 * ************************************************************************* */
/*!
 * \brief Release the memory of the solute transport data structure in two dimensions
 *
 * \param data N_solute_transport_data2d *
 * \return void *
 * */
void N_free_solute_transport_data2d(N_solute_transport_data2d * data)
{
    N_free_array_2d(data->c);
    N_free_array_2d(data->c_start);
    N_free_array_2d(data->status);
    N_free_array_2d(data->diff_x);
    N_free_array_2d(data->diff_y);
    N_free_array_2d(data->q);
    N_free_array_2d(data->cs);
    N_free_array_2d(data->R);
    N_free_array_2d(data->nf);
    N_free_array_2d(data->cin);
    N_free_array_2d(data->top);
    N_free_array_2d(data->bottom);

    G_free(data);

    data = NULL;

    return;
}

/* **************************************************************** *
 * **************************************************************** *
 * **************************************************************** */
int local_peclet(double z)
{
    if (z > 0)
	return 1;
    if (z == 0)
	return 0.5;
    if (z < 0)
	return 0;

    return 0;
}
