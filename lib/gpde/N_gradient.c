
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:     	gradient management functions 
* 		part of the gpde library
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <grass/N_pde.h>

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_2d structure
 *
 * return N_gradient_2d *
 *
 * */
N_gradient_2d *N_alloc_gradient_2d()
{
    N_gradient_2d *grad;

    grad = (N_gradient_2d *) G_calloc(1, sizeof(N_gradient_2d));

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_2d structure
 *
 * return void
 *
 * */
void N_free_gradient_2d(N_gradient_2d * grad)
{
    G_free(grad);
    grad = NULL;

    return;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief allocate and initialize a N_gradient_2d structure
 *
 * \param NC double - the gradient between northern and center cell
 * \param SC double - the gradient between southern and center cell
 * \param WC double - the gradient between western and center cell
 * \param EC double - the gradient between eastern and center cell
 * return N_gradient_2d *
 *
 * */
N_gradient_2d *N_create_gradient_2d(double NC, double SC, double WC, double EC)
{
    N_gradient_2d *grad;

    G_debug(5, "N_create_gradient_2d: create N_gradient_2d");

    grad = N_alloc_gradient_2d();

    grad->NC = NC;
    grad->SC = SC;
    grad->WC = WC;
    grad->EC = EC;

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief copy a N_gradient_2d structure
 *
 * \param source - the source N_gradient_2d struct
 * \param target - the target N_gradient_2d struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int N_copy_gradient_2d(N_gradient_2d * source, N_gradient_2d * target)
{
    G_debug(5, "N_copy_gradient_2d: copy N_gradient_2d");

    if (!source || !target)
	return 0;

    target->NC = source->NC;
    target->SC = source->SC;
    target->WC = source->WC;
    target->EC = source->EC;

    return 1;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Return a N_gradient_2d structure calculated from the input gradient field
 * at position [row][col]
 *
 *  This function returns the gradient of a cell at position [row][col] from the input gradient field.
 *  Returend is a new structure of type N_gradient_2d.
 *
 *  \param field N_gradient_field_2d * - A two dimensional gradient field
 *  \param gradient N_gradient_2d * - the gradient structure which should be filled with data, if a NULL pointer is given, a new structure will be created
 *  \param col int
 *  \param row int
 *  \return N_gradient_2d * - the new or filled gradient structure
 *  
 *
 * */
N_gradient_2d *N_get_gradient_2d(N_gradient_field_2d * field,
				 N_gradient_2d * gradient, int col, int row)
{
    double NC, SC, WC, EC;
    N_gradient_2d *grad = gradient;


    NC = -1 * N_get_array_2d_value_dcell(field->y_array, col, row);
    SC = N_get_array_2d_value_dcell(field->y_array, col, row + 1);
    WC = -1 * N_get_array_2d_value_dcell(field->x_array, col, row);
    EC = N_get_array_2d_value_dcell(field->x_array, col + 1, row);

    G_debug(5,
	    "N_get_gradient_2d: calculate N_gradient_2d NC %g SC %g WC %g EC %g",
	    NC, SC, WC, EC);

    /*if gradient is a NULL pointer, create a new one */
    if (!grad) {
	grad = N_create_gradient_2d(NC, SC, WC, EC);
    }
    else {
	grad->NC = NC;
	grad->SC = SC;
	grad->WC = WC;
	grad->EC = EC;
    }

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_3d structure
 *
 * return N_gradient_3d *
 *
 * */
N_gradient_3d *N_alloc_gradient_3d()
{
    N_gradient_3d *grad;

    grad = (N_gradient_3d *) G_calloc(1, sizeof(N_gradient_3d));

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_3d structure
 *
 * return void
 *
 * */
void N_free_gradient_3d(N_gradient_3d * grad)
{
    G_free(grad);
    grad = NULL;

    return;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief allocate and initialize a N_gradient_3d structure
 *
 * \param NC double - the gradient between northern and center cell
 * \param SC double - the gradient between southern and center cell
 * \param WC double - the gradient between western and center cell
 * \param EC double - the gradient between eastern and center cell
 * \param TC double - the gradient between top and center cell
 * \param BC double - the gradient between bottom and center cell
 * return N_gradient_3d *
 *
 * */
N_gradient_3d *N_create_gradient_3d(double NC, double SC, double WC, double EC,
				    double TC, double BC)
{
    N_gradient_3d *grad;

    G_debug(5, "N_create_gradient_3d: create N_gradient_3d");

    grad = N_alloc_gradient_3d();

    grad->NC = NC;
    grad->SC = SC;
    grad->WC = WC;
    grad->EC = EC;
    grad->TC = TC;
    grad->BC = BC;

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief copy a N_gradient_3d structure
 *
 * \param source - the source N_gradient_3d struct
 * \param target - the target N_gradient_3d struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int N_copy_gradient_3d(N_gradient_3d * source, N_gradient_3d * target)
{
    G_debug(5, "N_copy_gradient_3d: copy N_gradient_3d");

    if (!source || !target)
	return 0;

    target->NC = source->NC;
    target->SC = source->SC;
    target->WC = source->WC;
    target->EC = source->EC;
    target->TC = source->TC;
    target->BC = source->BC;

    return 1;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Return a N_gradient_3d structure calculated from the input gradient field
 * at position [depth][row][col]
 *
 *  This function returns the gradient of a 3d cell at position [depth][row][col] from the input gradient field.
 *  Returend is a new structure of type N_gradient_3d.
 *
 *  \param field N_gradient_field_3d * - A three dimensional gradient field
 *  \param N_gradient_3d * - an existing gradient structure or a NULL pointer, if a NULL pointer is providet a new structure will be returned
 *  \param col int
 *  \param row int
 *  \param depth int
 *  \return N_gradient_3d *
 *  
 *
 * */
N_gradient_3d *N_get_gradient_3d(N_gradient_field_3d * field,
				 N_gradient_3d * gradient, int col, int row,
				 int depth)
{
    double NC, SC, WC, EC, TC, BC;
    N_gradient_3d *grad = gradient;

    NC = -1 * N_get_array_3d_value_double(field->y_array, col, row, depth);
    SC = N_get_array_3d_value_double(field->y_array, col, row + 1, depth);
    WC = -1 * N_get_array_3d_value_double(field->x_array, col, row, depth);
    EC = N_get_array_3d_value_double(field->x_array, col + 1, row, depth);
    BC = -1 * N_get_array_3d_value_double(field->z_array, col, row, depth);
    TC = N_get_array_3d_value_double(field->z_array, col, row, depth + 1);

    G_debug(6,
	    "N_get_gradient_3d: calculate N_gradient_3d NC %g SC %g WC %g EC %g TC %g BC %g",
	    NC, SC, WC, EC, TC, BC);

    /*if gradient is a NULL pointer, create a new one */
    if (!grad) {
	grad = N_create_gradient_3d(NC, SC, WC, EC, TC, BC);
    }
    else {
	grad->NC = NC;
	grad->SC = SC;
	grad->WC = WC;
	grad->EC = EC;
	grad->BC = BC;
	grad->TC = TC;
    }

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_neighbours_x structure
 *
 * This structure contains all neighbour gradients in x direction of one cell  
 *
 * \return N_gradient_neighbours_x  *
 *
 * */
N_gradient_neighbours_x *N_alloc_gradient_neighbours_x()
{
    N_gradient_neighbours_x *grad;

    grad =
	(N_gradient_neighbours_x *) G_calloc(1,
					     sizeof(N_gradient_neighbours_x));

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_neighbours_x structure
 *
 * return void
 *
 * */
void N_free_gradient_neighbours_x(N_gradient_neighbours_x * grad)
{
    G_free(grad);
    grad = NULL;

    return;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate and initialize a N_gradient_neighbours_x structure
 *
 * \param NWN double - the gradient between north-west and northern cell
 * \param NEN double - the gradient between north-east and northern cell
 * \param WC double - the gradient between western and center cell
 * \param EC double - the gradient between eastern and center cell
 * \param SWS double - the gradient between south-west and southern cell
 * \param SES double - the gradient between south-east and southern cell
 * return N_gradient_neighbours_x *
 
 *
 * */
N_gradient_neighbours_x *N_create_gradient_neighbours_x(double NWN, double NEN,
							double WC, double EC,
							double SWS, double SES)
{
    N_gradient_neighbours_x *grad;

    G_debug(6,
	    "N_create_gradient_neighbours_x: create N_gradient_neighbours_x");

    grad = N_alloc_gradient_neighbours_x();

    grad->NWN = NWN;
    grad->NEN = NEN;
    grad->WC = WC;
    grad->EC = EC;
    grad->SWS = SWS;
    grad->SES = SES;

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief copy a N_gradient_neighbours_x structure
 *
 * \param source - the source N_gradient_neighbours_x struct
 * \param target - the target N_gradient_neighbours_x struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int
N_copy_gradient_neighbours_x(N_gradient_neighbours_x * source,
			     N_gradient_neighbours_x * target)
{
    G_debug(6, "N_copy_gradient_neighbours_x: copy N_gradient_neighbours_x");

    if (!source || !target)
	return 0;

    target->NWN = source->NWN;
    target->NEN = source->NEN;
    target->WC = source->WC;
    target->EC = source->EC;
    target->SWS = source->SWS;
    target->SES = source->SES;

    return 1;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_neighbours_y structure
 *
 * This structure contains all neighbour gradients in y direction of one cell  
 *
 * \return N_gradient_neighbours_y  *
 *
 * */
N_gradient_neighbours_y *N_alloc_gradient_neighbours_y()
{
    N_gradient_neighbours_y *grad;

    grad =
	(N_gradient_neighbours_y *) G_calloc(1,
					     sizeof(N_gradient_neighbours_y));

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_neighbours_y structure
 *
 * return void
 *
 * */
void N_free_gradient_neighbours_y(N_gradient_neighbours_y * grad)
{
    G_free(grad);
    grad = NULL;

    return;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate and initialize a N_gradient_neighbours_y structure
 *
 * \param NWW double - the gradient between north-west and western cell
 * \param NEW double - the gradient between north-east and eastern cell
 * \param NC double - the gradient between northern and center cell
 * \param SC double - the gradient between southern and center cell
 * \param SWW double - the gradient between south-west and western cell
 * \param SEE double - the gradient between south-east and eastern cell
 * return N_gradient_neighbours_y *
 
 *
 * */
N_gradient_neighbours_y *N_create_gradient_neighbours_y(double NWW, double NEE,
							double NC, double SC,
							double SWW, double SEE)
{
    N_gradient_neighbours_y *grad;

    G_debug(6,
	    "N_create_gradient_neighbours_y: create N_gradient_neighbours_y");

    grad = N_alloc_gradient_neighbours_y();

    grad->NWW = NWW;
    grad->NEE = NEE;
    grad->NC = NC;
    grad->SC = SC;
    grad->SWW = SWW;
    grad->SEE = SEE;

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief copy a N_gradient_neighbours_y structure
 *
 * \param source - the source N_gradient_neighbours_y struct
 * \param target - the target N_gradient_neighbours_y struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int
N_copy_gradient_neighbours_y(N_gradient_neighbours_y * source,
			     N_gradient_neighbours_y * target)
{
    G_debug(6, "N_copy_gradient_neighbours_y: copy N_gradient_neighbours_y");

    if (!source || !target)
	return 0;

    target->NWW = source->NWW;
    target->NEE = source->NEE;
    target->NC = source->NC;
    target->SC = source->SC;
    target->SWW = source->SWW;
    target->SEE = source->SEE;

    return 1;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_neighbours_z structure
 *
 * This structure contains all neighbour gradients in z direction of one cell  
 *
 * \return N_gradient_neighbours_z  *
 *
 * */
N_gradient_neighbours_z *N_alloc_gradient_neighbours_z()
{
    N_gradient_neighbours_z *grad;

    grad =
	(N_gradient_neighbours_z *) G_calloc(1,
					     sizeof(N_gradient_neighbours_z));

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_neighbours_z structure
 *
 * return void
 *
 * */
void N_free_gradient_neighbours_z(N_gradient_neighbours_z * grad)
{
    G_free(grad);
    grad = NULL;

    return;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate and initialize a N_gradient_neighbours_z structure
 *
 * \param NWZ double - the gradient between upper and lower north-western cells
 * \param NZ double - the gradient between upper and lower northern cells
 * \param NEZ double - the gradient between upper and lower north-eastern cells
 * \param WZ double - the gradient between upper and lower western cells
 * \param CZ double - the gradient between upper and lower center cells
 * \param EZ double - the gradient between upper and lower eastern cells
 * \param SWZ double - the gradient between upper and lower south-western cells
 * \param SZ double - the gradient between upper and lower southern cells
 * \param SEZ double - the gradient between upper and lower south-eastern cells
 * return N_gradient_neighbours_z *
 
 *
 * */
N_gradient_neighbours_z *N_create_gradient_neighbours_z(double NWZ, double NZ,
							double NEZ, double WZ,
							double CZ, double EZ,
							double SWZ, double SZ,
							double SEZ)
{
    N_gradient_neighbours_z *grad;

    G_debug(6,
	    "N_create_gradient_neighbours_z: create N_gradient_neighbours_z");

    grad = N_alloc_gradient_neighbours_z();

    grad->NWZ = NWZ;
    grad->NZ = NZ;
    grad->NEZ = NEZ;
    grad->WZ = WZ;
    grad->CZ = CZ;
    grad->EZ = EZ;
    grad->SWZ = SWZ;
    grad->SZ = SZ;
    grad->SEZ = SEZ;

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief copy a N_gradient_neighbours_z structure
 *
 * \param source - the source N_gradient_neighbours_z struct
 * \param target - the target N_gradient_neighbours_z struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int
N_copy_gradient_neighbours_z(N_gradient_neighbours_z * source,
			     N_gradient_neighbours_z * target)
{
    G_debug(6, "N_copy_gradient_neighbours_z: copy N_gradient_neighbours_z");

    if (!source || !target)
	return 0;

    target->NWZ = source->NWZ;
    target->NZ = source->NZ;
    target->NEZ = source->NEZ;
    target->WZ = source->WZ;
    target->CZ = source->CZ;
    target->EZ = source->EZ;
    target->SWZ = source->SWZ;
    target->SZ = source->SZ;
    target->SEZ = source->SEZ;

    return 1;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_neighbours_2d structure
 *
 * This structure contains all neighbour gradients in all directions of one cell 
 * in a 2d raster layer
 *
 * \return N_gradient_neighbours_2d *
 *
 * */
N_gradient_neighbours_2d *N_alloc_gradient_neighbours_2d()
{
    N_gradient_neighbours_2d *grad;

    grad =
	(N_gradient_neighbours_2d *) G_calloc(1,
					      sizeof(N_gradient_neighbours_2d));

    grad->x = N_alloc_gradient_neighbours_x();
    grad->y = N_alloc_gradient_neighbours_y();

    return grad;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_neighbours_2d structure
 *
 * return void
 *
 * */
void N_free_gradient_neighbours_2d(N_gradient_neighbours_2d * grad)
{

    N_free_gradient_neighbours_x(grad->x);
    N_free_gradient_neighbours_y(grad->y);

    G_free(grad);
    grad = NULL;

    return;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate and initialize a N_gradient_neighbours_2d structure
 *
 * The parameter N_gradient_neighbours x and y are copied into the new allocated structure 
 * and can be deleted after the initializing
 *
 * \return N_gradient_neighbours_2d * -- if failure NULL is returned
 *
 * */
N_gradient_neighbours_2d
    *N_create_gradient_neighbours_2d(N_gradient_neighbours_x * x,
				     N_gradient_neighbours_y * y)
{
    N_gradient_neighbours_2d *grad;
    int fail = 0;

    G_debug(5,
	    "N_create_gradient_neighbours_2d: create N_gradient_neighbours_2d");

    grad = N_alloc_gradient_neighbours_2d();

    if (!N_copy_gradient_neighbours_x(x, grad->x))
	fail++;
    if (!N_copy_gradient_neighbours_y(y, grad->y))
	fail++;

    if (fail > 0) {
	N_free_gradient_neighbours_2d(grad);
	grad = NULL;
    }

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief copy a N_gradient_neighbours_2d structure
 *
 * \param source - the source N_gradient_neighbours_2d struct
 * \param target - the target N_gradient_neighbours_2d struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int
N_copy_gradient_neighbours_2d(N_gradient_neighbours_2d * source,
			      N_gradient_neighbours_2d * target)
{
    int fail = 0;

    G_debug(5, "N_copy_gradient_neighbours_2d: copy N_gradient_neighbours_2d");

    if (!source || !target)
	return 0;

    if (!(N_copy_gradient_neighbours_x(source->x, target->x)))
	fail++;
    if (!(N_copy_gradient_neighbours_y(source->y, target->y)))
	fail++;

    if (fail > 0) {
	return 0;
    }

    return 1;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_neighbours_3d structure
 *
 * This structure contains all neighbour gradients in all directions of one cell 
 * in a 3d raster layer
 *
 * \return N_gradient_neighbours_3d *
 *
 * */
N_gradient_neighbours_3d *N_alloc_gradient_neighbours_3d()
{
    N_gradient_neighbours_3d *grad;

    grad =
	(N_gradient_neighbours_3d *) G_calloc(1,
					      sizeof(N_gradient_neighbours_3d));

    grad->xt = N_alloc_gradient_neighbours_x();
    grad->xc = N_alloc_gradient_neighbours_x();
    grad->xb = N_alloc_gradient_neighbours_x();
    grad->yt = N_alloc_gradient_neighbours_y();
    grad->yc = N_alloc_gradient_neighbours_y();
    grad->yb = N_alloc_gradient_neighbours_y();
    grad->zt = N_alloc_gradient_neighbours_z();
    grad->zb = N_alloc_gradient_neighbours_z();

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_neighbours_3d structure
 *
 * return void
 *
 * */
void N_free_gradient_neighbours_3d(N_gradient_neighbours_3d * grad)
{

    N_free_gradient_neighbours_x(grad->xt);
    N_free_gradient_neighbours_x(grad->xc);
    N_free_gradient_neighbours_x(grad->xb);
    N_free_gradient_neighbours_y(grad->yt);
    N_free_gradient_neighbours_y(grad->yc);
    N_free_gradient_neighbours_y(grad->yb);
    N_free_gradient_neighbours_z(grad->zt);
    N_free_gradient_neighbours_z(grad->zb);

    G_free(grad);
    grad = NULL;

    return;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate and initialize a N_gradient_neighbours_3d structure
 *
 * The parameter N_gradient_neighbours x[tcb] and y[tcb] and z[tb] are copied into the new allocated structure 
 * and can be deleted after the initializing
 *
 * \return N_gradient_neighbours_3d * -- if failure NULL is returned
 
 *
 * */
N_gradient_neighbours_3d
    *N_create_gradient_neighbours_3d(N_gradient_neighbours_x * xt,
				     N_gradient_neighbours_x * xc,
				     N_gradient_neighbours_x * xb,
				     N_gradient_neighbours_y * yt,
				     N_gradient_neighbours_y * yc,
				     N_gradient_neighbours_y * yb,
				     N_gradient_neighbours_z * zt,
				     N_gradient_neighbours_z * zb)
{
    N_gradient_neighbours_3d *grad;
    int fail = 0;

    G_debug(5,
	    "N_create_gradient_neighbours_3d: create N_gradient_neighbours_3d");

    grad = N_alloc_gradient_neighbours_3d();

    if (!(N_copy_gradient_neighbours_x(xt, grad->xt)))
	fail++;
    if (!(N_copy_gradient_neighbours_x(xc, grad->xc)))
	fail++;
    if (!(N_copy_gradient_neighbours_x(xb, grad->xb)))
	fail++;
    if (!(N_copy_gradient_neighbours_y(yt, grad->yt)))
	fail++;
    if (!(N_copy_gradient_neighbours_y(yc, grad->yc)))
	fail++;
    if (!(N_copy_gradient_neighbours_y(yb, grad->yb)))
	fail++;
    if (!(N_copy_gradient_neighbours_z(zt, grad->zt)))
	fail++;
    if (!(N_copy_gradient_neighbours_z(zb, grad->zb)))
	fail++;

    if (fail > 0) {
	return NULL;
    }

    return grad;
}

/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief copy a N_gradient_neighbours_3d structure
 *
 * \param source - the source N_gradient_neighbours_3d struct
 * \param target - the target N_gradient_neighbours_3d struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int
N_copy_gradient_neighbours_3d(N_gradient_neighbours_3d * source,
			      N_gradient_neighbours_3d * target)
{
    int fail = 0;

    G_debug(5, "N_copy_gradient_neighbours_3d: copy N_gradient_neighbours_3d");

    if (!source || !target)
	return 0;

    if (!(N_copy_gradient_neighbours_x(source->xt, target->xt)))
	fail++;
    if (!(N_copy_gradient_neighbours_x(source->xc, target->xc)))
	fail++;
    if (!(N_copy_gradient_neighbours_x(source->xb, target->xb)))
	fail++;
    if (!(N_copy_gradient_neighbours_y(source->yt, target->yt)))
	fail++;
    if (!(N_copy_gradient_neighbours_y(source->yc, target->yc)))
	fail++;
    if (!(N_copy_gradient_neighbours_y(source->yb, target->yb)))
	fail++;
    if (!(N_copy_gradient_neighbours_z(source->zt, target->zt)))
	fail++;
    if (!(N_copy_gradient_neighbours_z(source->zb, target->zb)))
	fail++;

    if (fail > 0) {
	return 0;
    }

    return 1;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_field_2d
 *
 * The field arrays are of type DCELL. The array in x direction
 * is one column larger, the array in y direction is one row larger.
 * This is needed to store the gradients between cells.
 *
 * a -> array in x direction
 * b -> array in y direction
 * c = a and b 
 *
 * c c c c c c a
 * c c c c c c a
 * c c c c c c a
 * c c c c c c a
 * c c c c c c a
 * b b b b b b 
 *
 * 
 * \param rows - number of rows of the 2d array from which the gradient should be calculated
 * \param cols - number of cols of the 2d array from which the gradient should be calculated
 * return N_gradient_field_2d *
 *
 * */
N_gradient_field_2d *N_alloc_gradient_field_2d(int cols, int rows)
{
    N_gradient_field_2d *field;

    G_debug(5,
	    "N_alloc_gradient_field_2d: allocate a N_gradient_field_2d struct");

    field = (N_gradient_field_2d *) G_calloc(1, sizeof(N_gradient_field_2d));

    field->x_array = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);
    field->y_array = N_alloc_array_2d(cols, rows, 1, DCELL_TYPE);

    return field;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_neighbours_2d structure
 *
 * return void
 *
 * */
void N_free_gradient_field_2d(N_gradient_field_2d * field)
{

    N_free_array_2d(field->x_array);
    N_free_array_2d(field->y_array);

    G_free(field);

    field = NULL;

    return;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Copy N_gradient_field_2d structure from source to target
 *
 * \param source - the source N_gradient_field_2d struct
 * \param target - the target N_gradient_field_2d struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int
N_copy_gradient_field_2d(N_gradient_field_2d * source,
			 N_gradient_field_2d * target)
{
    G_debug(3, "N_copy_gradient_field_2d: copy N_gradient_field_2d");

    if (!source || !target)
	return 0;

    N_copy_array_2d(source->x_array, target->x_array);
    N_copy_array_2d(source->y_array, target->y_array);

    return 1;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief This function computes the gradient based on the input N_array_2d pot
 * (that means potential), a relaxion N_array_2d named relax and the distance between two cells 
 * saved in the N_geom_data struct.
 *
 * The gradient is calculated between cells for each cell and direction.
 *
 *
 \verbatim
 ______________ 
 |    |    |    |
 |    |    |    |
 |----|-NC-|----|
 |    |    |    |
 |   WC    EC   |
 |    |    |    |
 |----|-SC-|----|
 |    |    |    |
 |____|____|____|
 
 
 x - direction:
 
 r = 2 * relax[row][col]*relax[row][col + 1] / (relax[row][col]*relax[row][col + 1])
 EC = r * (pot[row][col] - pot[row][col + 1])/dx
 
 y - direction:
 
 r = 2 * relax[row][col]*relax[row + 1][col] / (relax[row][col]*relax[row + 1][col])
 SC = r * (pot[row][col] - pot[row + 1][col])/dy
 
 the values SC and EC are the negative values of the next row/col
 
 
 \endverbatim
 * \param pot N_array_2d * - the potential N_array_2d 
 * \param relax_x N_array_2d * - the relaxion N_array_2d used to modify the gradient in x-direction
 * \param relax_y N_array_2d * - the relaxion N_array_2d used to modify the gradient in y-direction
 * \param geom N_geom_data * - geometry data structure
 * return N_gradient_field_2d * - this field is new allocated
 *
 * */
N_gradient_field_2d *N_compute_gradient_field_2d(N_array_2d * pot,
						 N_array_2d * relax_x,
						 N_array_2d * relax_y,
						 N_geom_data * geom)
{
    int i, j;
    int rows, cols;
    double dx, dy, p1, p2, r1, r2, mean, grad, res;
    N_gradient_field_2d *field;


    if (pot->cols != relax_x->cols || pot->cols != relax_y->cols)
	G_fatal_error
	    ("N_compute_gradient_field_2d: the arrays are not of equal size");

    if (pot->rows != relax_x->rows || pot->rows != relax_y->rows)
	G_fatal_error
	    ("N_compute_gradient_field_2d: the arrays are not of equal size");

    G_debug(3, "N_compute_gradient_field_2d: compute gradient field");

    rows = pot->rows;
    cols = pot->cols;
    dx = geom->dx;
    dy = geom->dy;

    field = N_alloc_gradient_field_2d(cols, rows);

    for (j = 0; j < rows; j++)
	for (i = 0; i < cols - 1; i++) {
	    grad = 0;
	    mean = 0;

	    //Only compute if the arrays are not null
	    if (!N_is_array_2d_value_null(pot, i, j) &&
		!N_is_array_2d_value_null(pot, i + 1, j)) {
		p1 = N_get_array_2d_value_dcell(pot, i, j);
		p2 = N_get_array_2d_value_dcell(pot, i + 1, j);
		grad = (p1 - p2) / dx;	/* gradient */
	    }
	    if (!N_is_array_2d_value_null(relax_x, i, j) &&
		!N_is_array_2d_value_null(relax_x, i + 1, j)) {
		r1 = N_get_array_2d_value_dcell(relax_x, i, j);
		r2 = N_get_array_2d_value_dcell(relax_x, i + 1, j);
		mean = 2 * (r1 * r2) / (r1 + r2);	/*harmonical mean */
	    }

	    res = mean * grad;

	    N_put_array_2d_value_dcell(field->x_array, i + 1, j, res);

	}

    for (j = 0; j < rows - 1; j++)
	for (i = 0; i < cols; i++) {
	    grad = 0;
	    mean = 0;

	    //Only compute if the arrays are not null
	    if (!N_is_array_2d_value_null(pot, i, j) &&
		!N_is_array_2d_value_null(pot, i, j + 1)) {
		p1 = N_get_array_2d_value_dcell(pot, i, j);
		p2 = N_get_array_2d_value_dcell(pot, i, j + 1);
		grad = (p1 - p2) / dy;	/* gradient */
	    }
	    if (!N_is_array_2d_value_null(relax_y, i, j) &&
		!N_is_array_2d_value_null(relax_y, i, j + 1)) {
		r1 = N_get_array_2d_value_dcell(relax_y, i, j);
		r2 = N_get_array_2d_value_dcell(relax_y, i, j + 1);
		mean = 2 * (r1 * r2) / (r1 + r2);	/*harmonical mean */
	    }

	    res = mean * grad;

	    N_put_array_2d_value_dcell(field->y_array, i, j + 1, res);

	}

    return field;
}

/*! 
 * \bief Calculate the x and y vector components from a gradient field for each cell and stores them in the provided N_array_2d structures
 *
 * The arrays must have the same size as the gradient field.
 *
 * \param field N_gradient_field_2d *
 * \param x_comp N_array_2d * - the array in which the x component will be written
 * \param y_comp N_array_2d * - the array in which the y component will be written
 *
 * \return void
 * */
void
N_compute_gradient_field_components_2d(N_gradient_field_2d * field,
				       N_array_2d * x_comp, N_array_2d * y_comp)
{
    int i, j;
    int rows, cols;
    double vx, vy;
    N_array_2d *x = x_comp;
    N_array_2d *y = y_comp;
    N_gradient_2d grad;


    if (!x)
	G_fatal_error("N_compute_gradient_components_2d: x array is empty");
    if (!y)
	G_fatal_error("N_compute_gradient_components_2d: y array is empty");

    cols = field->x_array->cols;
    rows = field->x_array->rows;

    /*Check the array sizes */
    if (x->cols != cols || x->rows != rows)
	G_fatal_error
	    ("N_compute_gradient_components_2d: the size of the x array dont fit the gradient field size");
    if (y->cols != cols || y->rows != rows)
	G_fatal_error
	    ("N_compute_gradient_components_2d: the size of the y array dont fit the gradient field size");

    for (j = 0; j < rows; j++)
	for (i = 0; i < cols; i++) {
	    N_get_gradient_2d(field, &grad, i, j);
	    vx = (-1 * grad.WC + grad.EC) / 2;
	    vy = -1 * (-1 * grad.NC + grad.SC) / 2;	/*the gradient must be inverted, because grass counts the rows from north to south */
	    N_put_array_2d_value_dcell(x, i, j, vx);
	    N_put_array_2d_value_dcell(y, i, j, vy);
	}

    return;
}

/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Allocate a N_gradient_field_3d
 *
 * The field arrays are always of type G3D_DOUBLE. The array in x direction
 * is one column larger, the array in y direction is one row larger
 * and the array in z direction is one depth larger.
 * This is needed to store the gradients between cells.
 *
 * 
 * \param cols - number of cols of the 3d array from which the gradient should be calculated
 * \param rows - number of rows of the 3d array from which the gradient should be calculated
 * \param depths - number of depths of the 3d array from which the gradient should be calculated
 * return N_gradient_field_3d *
 *
 * */
N_gradient_field_3d *N_alloc_gradient_field_3d(int cols, int rows, int depths)
{
    N_gradient_field_3d *field;

    G_debug(5,
	    "N_alloc_gradient_field_3d: allocate a N_gradient_field_3d struct");

    field = (N_gradient_field_3d *) G_calloc(1, sizeof(N_gradient_field_3d));

    field->x_array = N_alloc_array_3d(cols, rows, depths, 1, G3D_DOUBLE);
    field->y_array = N_alloc_array_3d(cols, rows, depths, 1, G3D_DOUBLE);
    field->z_array = N_alloc_array_3d(cols, rows, depths, 1, G3D_DOUBLE);

    return field;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Free's a N_gradient_neighbours_3d structure
 *
 * return void
 *
 * */
void N_free_gradient_field_3d(N_gradient_field_3d * field)
{

    N_free_array_3d(field->x_array);
    N_free_array_3d(field->y_array);
    N_free_array_3d(field->z_array);

    G_free(field);

    field = NULL;

    return;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief Copy N_gradient_field_3d structure from source to target
 *
 * \param source - the source N_gradient_field_3d struct
 * \param target - the target N_gradient_field_3d struct
 * return int - 1 success, 0 failure while copying
 *
 * */
int
N_copy_gradient_field_3d(N_gradient_field_3d * source,
			 N_gradient_field_3d * target)
{
    G_debug(3, "N_copy_gradient_field_3d: copy N_gradient_field_3d");

    if (!source || !target)
	return 0;

    N_copy_array_3d(source->x_array, target->x_array);
    N_copy_array_3d(source->y_array, target->y_array);
    N_copy_array_3d(source->z_array, target->z_array);

    return 1;
}


/* ************************************************************************ */
/* ************************************************************************ */
/* ************************************************************************ */
/*!
 * \brief This function computes the gradient based on the input N_array_3d pot
 * (that means potential), a relaxion N_array_3d named relax and the distance between two cells 
 * saved in the N_geom_data struct.
 *
 * The gradient is calculated between cells for each cell and direction.
 *
 *
 \verbatim
 
 x - direction:
 
 r = 2 * relax[depth][row][col]*relax[depth][row][col + 1] / (relax[depth][row][col]*relax[depth][row][col + 1])
 WC = r * (pot[depth][row][col] - pot[depth][row][col + 1])/dx
 
 NC - direction:
 
 r = 2 * relax[depth][row][col]*relax[depth][row + 1][col] / (relax[depth][row][col]*relax[depth][row + 1][col])
 NC = r * (pot[depth][row][col] - pot[depth][row + 1][col])/dy
 
 the values BC, SC and EC are the negative values of the next depth/row/col
 
 
 \endverbatim
 * \param pot N_array_3d * - the potential N_array_2d 
 * \param relax_x N_array_2d * - the relaxion N_array_2d used to modify the gradient in x-direction
 * \param relax_y N_array_2d * - the relaxion N_array_2d used to modify the gradient in y-direction
 * \param relax_z N_array_2d * - the relaxion N_array_2d used to modify the gradient in z-direction
 * \param geom N_geom_data * - geometry data structure
 * return N_gradient_field_3d * - this field is new allocated
 *
 * */
N_gradient_field_3d *N_compute_gradient_field_3d(N_array_3d * pot,
						 N_array_3d * relax_x,
						 N_array_3d * relax_y,
						 N_array_3d * relax_z,
						 N_geom_data * geom)
{
    int i, j, k;
    int cols, rows, depths;
    double dx, dy, dz, p1, p2, r1, r2, mean, grad, res;
    N_gradient_field_3d *field;


    if (pot->cols != relax_x->cols || pot->cols != relax_y->cols ||
	pot->cols != relax_z->cols)
	G_fatal_error
	    ("N_compute_gradient_field_3d: the arrays are not of equal size");

    if (pot->rows != relax_x->rows || pot->rows != relax_y->rows ||
	pot->rows != relax_z->rows)
	G_fatal_error
	    ("N_compute_gradient_field_3d: the arrays are not of equal size");

    if (pot->depths != relax_x->depths || pot->depths != relax_y->depths ||
	pot->depths != relax_z->depths)
	G_fatal_error
	    ("N_compute_gradient_field_3d: the arrays are not of equal size");

    G_debug(3, "N_compute_gradient_field_3d: compute gradient field");

    rows = pot->rows;
    cols = pot->cols;
    depths = pot->depths;
    dx = geom->dx;
    dy = geom->dy;
    dz = geom->dz;

    field = N_alloc_gradient_field_3d(cols, rows, depths);

    for (k = 0; k < depths; k++)
	for (j = 0; j < rows; j++)
	    for (i = 0; i < cols - 1; i++) {
		grad = 0;
		mean = 0;

		//Only compute if the arrays are not null
		if (!N_is_array_3d_value_null(pot, i, j, k) &&
		    !N_is_array_3d_value_null(pot, i + 1, j, k)) {
		    p1 = N_get_array_3d_value_double(pot, i, j, k);
		    p2 = N_get_array_3d_value_double(pot, i + 1, j, k);
		    grad = (p1 - p2) / dx;	/* gradient */
		}
		if (!N_is_array_3d_value_null(relax_x, i, j, k) &&
		    !N_is_array_3d_value_null(relax_x, i + 1, j, k)) {
		    r1 = N_get_array_3d_value_double(relax_x, i, j, k);
		    r2 = N_get_array_3d_value_double(relax_x, i + 1, j, k);
		    if ((r1 + r2) != 0)
			mean = 2 * (r1 * r2) / (r1 + r2);	/*harmonical mean */
		}

		res = mean * grad;

		G_debug(6,
			"N_compute_gradient_field_3d: X-direction insert value %6.5g at %i %i %i ",
			res, k, j, i + 1);

		N_put_array_3d_value_double(field->x_array, i + 1, j, k, res);

	    }

    for (k = 0; k < depths; k++)
	for (j = 0; j < rows - 1; j++)
	    for (i = 0; i < cols; i++) {
		grad = 0;
		mean = 0;

		//Only compute if the arrays are not null
		if (!N_is_array_3d_value_null(pot, i, j, k) &&
		    !N_is_array_3d_value_null(pot, i, j + 1, k)) {
		    p1 = N_get_array_3d_value_double(pot, i, j, k);
		    p2 = N_get_array_3d_value_double(pot, i, j + 1, k);
		    grad = (p1 - p2) / dy;	/* gradient */
		}
		if (!N_is_array_3d_value_null(relax_y, i, j, k) &&
		    !N_is_array_3d_value_null(relax_y, i, j + 1, k)) {
		    r1 = N_get_array_3d_value_double(relax_y, i, j, k);
		    r2 = N_get_array_3d_value_double(relax_y, i, j + 1, k);
		    if ((r1 + r2) != 0)
			mean = 2 * (r1 * r2) / (r1 + r2);	/*harmonical mean */
		}

		res = mean * grad;

		G_debug(6,
			"N_compute_gradient_field_3d: Y-direction insert value %6.5g at %i %i %i ",
			res, k, j + 1, i);

		N_put_array_3d_value_double(field->y_array, i, j + 1, k, res);

	    }

    for (k = 0; k < depths - 1; k++)
	for (j = 0; j < rows; j++)
	    for (i = 0; i < cols; i++) {
		grad = 0;
		mean = 0;

		//Only compute if the arrays are not null
		if (!N_is_array_3d_value_null(pot, i, j, k) &&
		    !N_is_array_3d_value_null(pot, i, j, k + 1)) {
		    p1 = N_get_array_3d_value_double(pot, i, j, k);
		    p2 = N_get_array_3d_value_double(pot, i, j, k + 1);
		    grad = (p1 - p2) / dz;	/* gradient */
		}
		if (!N_is_array_3d_value_null(relax_z, i, j, k) &&
		    !N_is_array_3d_value_null(relax_z, i, j, k + 1)) {
		    r1 = N_get_array_3d_value_double(relax_z, i, j, k);
		    r2 = N_get_array_3d_value_double(relax_z, i, j, k + 1);
		    if ((r1 + r2) != 0)
			mean = 2 * (r1 * r2) / (r1 + r2);	/*harmonical mean */
		}

		res = mean * grad;

		G_debug(6,
			"N_compute_gradient_field_3d: Z-direction insert value %6.5g at %i %i %i ",
			res, k + 1, j, i);

		N_put_array_3d_value_double(field->z_array, i, j, k + 1, res);

	    }

    return field;
}

/*! 
 * \bief Calculate the x, y and z vector components for each cell and store them in the provided N_array_3d structures
 *
 * The arrays must have the same size as the gradient field.
 *
 * \param field N_gradient_field_3d *
 * \param x_comp N_array_3d * - the array in which the x component will be written
 * \param y_comp N_array_3d * - the array in which the y component will be written
 * \param z_comp N_array_3d * - the array in which the z component will be written
 *
 * \return void
 * */
void
N_compute_gradient_field_components_3d(N_gradient_field_3d * field,
				       N_array_3d * x_comp, N_array_3d * y_comp,
				       N_array_3d * z_comp)
{
    int i, j, k;
    int rows, cols, depths;
    double vx, vy, vz;
    N_array_3d *x = x_comp;
    N_array_3d *y = y_comp;
    N_array_3d *z = z_comp;
    N_gradient_3d grad;


    if (!x)
	G_fatal_error("N_compute_gradient_components_3d: x array is empty");
    if (!y)
	G_fatal_error("N_compute_gradient_components_3d: y array is empty");
    if (!z)
	G_fatal_error("N_compute_gradient_components_3d: z array is empty");

    cols = field->x_array->cols;
    rows = field->x_array->rows;
    depths = field->x_array->depths;

    /*Check the array sizes */
    if (x->cols != cols || x->rows != rows || x->depths != depths)
	G_fatal_error
	    ("N_compute_gradient_components_3d: the size of the x array dont fit the gradient field size");
    if (y->cols != cols || y->rows != rows || y->depths != depths)
	G_fatal_error
	    ("N_compute_gradient_components_3d: the size of the y array dont fit the gradient field size");
    if (z->cols != cols || z->rows != rows || z->depths != depths)
	G_fatal_error
	    ("N_compute_gradient_components_3d: the size of the z array dont fit the gradient field size");

    for (k = 0; k < depths; k++)
	for (j = 0; j < rows; j++)
	    for (i = 0; i < cols; i++) {
		N_get_gradient_3d(field, &grad, i, j, k);
		vx = (-1 * grad.WC + grad.EC) / 2;
		vy = -1 * (-1 * grad.NC + grad.SC) / 2;	/*the gradient must be inverted, because grass counts the rows from north to south */
		vz = (grad.TC + -1 * grad.BC) / 2;
		N_put_array_3d_value_double(x, i, j, k, vx);
		N_put_array_3d_value_double(y, i, j, k, vy);
		N_put_array_3d_value_double(z, i, j, k, vz);
	    }


    return;
}
