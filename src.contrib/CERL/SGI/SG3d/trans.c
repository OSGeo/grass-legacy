
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include <math.h>

#define MAX_STACK 20
static float c_stack[MAX_STACK][4][4];
static int stack_ptr = -1;
static float d[4][4];
static float pnts[1][4];

#define PI  3.14159265358979323846

/*
**  Current transformation matrix
*/
static float trans_mat[4][4] =
{
    { 1., 0., 0., 0. },
    { 0., 1., 0., 0. },
    { 0., 0., 1., 0. },
    { 0., 0., 0., 1. }
};

static float ident[4][4] =
{
    { 1., 0., 0., 0. },
    { 0., 1., 0., 0. },
    { 0., 0., 1., 0. },
    { 0., 0., 0., 1. }
};

P_scale (x, y, z)
    float x, y, z;
{
    d[0][0] = x;  d[0][1] = 0.; d[0][2] = 0.; d[0][3] = 0.;
    d[1][0] = 0.; d[1][1] = y;  d[1][2] = 0.; d[1][3] = 0.;
    d[2][0] = 0.; d[2][1] = 0.; d[2][2] = z;  d[2][3] = 0.;
    d[3][0] = 0.; d[3][1] = 0.; d[3][2] = 0.; d[3][3] = 1.; 

    P_pushmatrix ();
    P__transform (4, trans_mat, c_stack[stack_ptr], d);
    P_popmatrix ();
}

P_translate (x, y, z)
    float x, y, z;
{
    d[0][0] = 1.; d[0][1] = 0.; d[0][2] = 0.; d[0][3] = 0.;
    d[1][0] = 0.; d[1][1] = 1.; d[1][2] = 0.; d[1][3] = 0.;
    d[2][0] = 0.; d[2][1] = 0.; d[2][2] = 1.; d[2][3] = 0.;
    d[3][0] = x;  d[3][1] = y;  d[3][2] = z;  d[3][3] = 1.;

    P_pushmatrix ();
    P__transform (4, trans_mat, c_stack[stack_ptr], d);
    P_popmatrix ();
}

/*
**   multiply 'in' matrix (homogenous coordinate generally) by
**   the current transformation matrix, placing the result in 'out'
**
**       [in][trans_mat] => [out]
*/
P_transform (num_vert, in, out)
    int num_vert;
    float in[][4];
    float out[][4];
{
    P__transform (num_vert, in, out, trans_mat);
}

static
P__transform (num_vert, in, out, c)
    int num_vert;
    float in[][4];
    float out[][4];
    float c[4][4];
{
    register int k, j, i;

    for (i = 0 ; i < num_vert ; i++)
	for (j = 0 ; j < 4 ; j++)
	{
	    out[i][j] = 0.;
	    for (k = 0 ; k < 4 ; k++)
		out[i][j] += c[k][j] * in[i][k];
	}
}

P_matrix_copy (from, to, size)
    float from[][4];
    float to[][4];
    int size;
{
    register int i, j;

    for (i = 0 ; i < size ; i++)
	for (j = 0 ; j < 4 ; j++)
	    to[i][j] = from[i][j];
}
    
P_display_matrix (in, size)
    float in[][4];
{
    register int j, i;
    for (i = 0 ; i < size ; i++)
    {
	for (j = 0 ; j < 4 ; j++)
	    printf (" %4.2f", in[i][j]);
	printf ( "\n");
    }
    printf ( "\n");
}

P_display_matrix_err (in, size)
    float in[][4];
{
    register int j, i;
    for (i = 0 ; i < size ; i++)
    {
	for (j = 0 ; j < 4 ; j++)
	    fprintf (stderr, " %4.2f", in[i][j]);
	fprintf (stderr,  "\n");
    }
    fprintf (stderr,  "\n");
}

/*
** push current transformation matrix onto matrix stack
*/
P_pushmatrix ()
{
    if (stack_ptr >= MAX_STACK)
    {
	fprintf (stderr, "Out of matrix stack space\n");
	return (-1);
    }
    stack_ptr++;
    P_matrix_copy (trans_mat, c_stack[stack_ptr], 4);
    return (0);
}

/*
** pop top of matrix stack, placing it into the current transformation matrix
*/
P_popmatrix ()
{
    if (stack_ptr < 0)
    {
	fprintf (stderr, "Tried to pop an empty stack\n");
	return (-1);
    }
    P_matrix_copy (c_stack[stack_ptr], trans_mat, 4);
    stack_ptr--;
    return (0);
}

/* plot point w/ current transformation matrix 
*/
P_pnt (x, y, z)
    float x, y, z;
{
    float out[1][4];
    pnts[0][0] = x;
    pnts[0][1] = y;
    pnts[0][2] = z;
    pnts[0][3] = 1.;

    P__transform (1, pnts, out, trans_mat);

    printf ( "%f %f %f %f\n", out[0][0], out[0][1], out[0][2], out[0][3]);
}

/* rotate 90 deg around Z */
/* I.E. transpose x/y */
P_rot_90z ()
{
    d[0][0] = 0.; d[0][1] = 1.; d[0][2] = 0.; d[0][3] = 0.;
    d[1][0] = 1.; d[1][1] = 0.; d[1][2] = 0.; d[1][3] = 0.;
    d[2][0] = 0.; d[2][1] = 0.; d[2][2] = 1.; d[2][3] = 0.;
    d[3][0] = 0.; d[3][1] = 0.; d[3][2] = 0.; d[3][3] = 1.;
    
    P_pushmatrix ();
    P__transform (4, trans_mat, c_stack[stack_ptr], d);
    P_popmatrix ();
}

/*  angle is expressed in tenths of degrees */
P_rotate (angle, axis)
    int angle;
    char axis;
{
    double theta;

    P_matrix_copy (ident, d, 4);

    theta = (PI / 1800.) * (float) angle; /* convert to radians */

    /* optimize to handle rotations of mutliples of 90 deg */
    switch (axis) {
	case 'x':
	    d[1][1] =  cos (theta);
	    d[1][2] =  sin (theta);
	    d[2][1] = -sin (theta);
	    d[2][2] =  cos (theta);
	    break;
	case 'y':
	    d[0][0] =  cos (theta);
	    d[0][2] = -sin (theta);
	    d[2][0] =  sin (theta);
	    d[2][2] =  cos (theta);
	    break;
	case 'z':
	    d[0][0] =  cos (theta);
	    d[0][1] =  sin (theta);
	    d[1][0] = -sin (theta);
	    d[1][1] =  cos (theta);
	    break;
    }

    P_pushmatrix ();
    P__transform (4, trans_mat, c_stack[stack_ptr], d);
    P_popmatrix ();
}
