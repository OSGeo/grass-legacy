/*
* $Id$
*/

/* NOTE: This file should be REMOVED and any calls to the functions in this
** file should be replaced with appropriate OpenGL calls.
*/

/*
**  Written by Dave Gerdes Jan 1990
**  Copyright  Dave Gerdes 1990    All rights reserved
**
**
**  Matrix Transformation library.
**  
**   P_pushmatrix ()
**   P_popmatrix ()
**   P_scale ()		
**   P_translate ()
**   P_rot ()
**   P_rotate ()
**   P_transform ()      transform array of vectors using current T matrix
**      		 This routine should be available in GL!
**
**  Arguments are same as GL counterparts
**
**  I threw this code together in January at the beginning of this
**  class.  I was still learning about GL at the time.
**  There are many places where the code could be improved.
**
*/
#include <stdio.h>
#include <math.h>

#include "gstypes.h"

#define MAX_STACK 20

static float c_stack[MAX_STACK][4][4];	/* matrix stack */
static int stack_ptr = -1;		/* index of curr matrix depth */
static float d[4][4];			/* tmp matrix */

#define NPI  3.14159265358979323846

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

/************************************************************************/
void P_scale (float x, float y, float z)
{
    d[0][0] = x;  d[0][1] = 0.; d[0][2] = 0.; d[0][3] = 0.;
    d[1][0] = 0.; d[1][1] = y;  d[1][2] = 0.; d[1][3] = 0.;
    d[2][0] = 0.; d[2][1] = 0.; d[2][2] = z;  d[2][3] = 0.;
    d[3][0] = 0.; d[3][1] = 0.; d[3][2] = 0.; d[3][3] = 1.; 

    /*
    **  will write into 1 down on matrix stack
    **  and then the popmatrix() will place it as the current T matrix
    */
    P_pushmatrix ();
    P__transform (4, d, c_stack[stack_ptr], trans_mat);
    P_popmatrix ();
    
    return;
}

/************************************************************************/
void P_translate (float x, float y, float z)
{
    d[0][0] = 1.; d[0][1] = 0.; d[0][2] = 0.; d[0][3] = 0.;
    d[1][0] = 0.; d[1][1] = 1.; d[1][2] = 0.; d[1][3] = 0.;
    d[2][0] = 0.; d[2][1] = 0.; d[2][2] = 1.; d[2][3] = 0.;
    d[3][0] = x;  d[3][1] = y;  d[3][2] = z;  d[3][3] = 1.;

    P_pushmatrix ();
    P__transform (4, d, c_stack[stack_ptr], trans_mat);
    P_popmatrix ();
    
    return;
}

/************************************************************************/
/*
**   multiply 'in' matrix (homogenous coordinate generally) by
**   the current transformation matrix, placing the result in 'out'
**
**       [in][trans_mat] => [out]
*/
void P_transform (int num_vert, float (*in)[4], float (*out)[4])
{
    P__transform (num_vert, in, out, trans_mat);
    
    return;
}

/************************************************************************/
void P__transform (int num_vert, float (*in)[4], float (*out)[4], float (*c)[4])
{
    register int k, j, i;

    for (i = 0 ; i < num_vert ; i++)
    {
	for (j = 0 ; j < 4 ; j++)
	{
	    out[i][j] = 0.;
	
	    for (k = 0 ; k < 4 ; k++)
	    {
		out[i][j] += in[i][k] * c[k][j] ;
	    }
	}
    }
    
    return;
}

/************************************************************************/
void P_matrix_copy (float (*from)[4], float (*to)[4], int size)
{
    register int i, j;

    for (i = 0 ; i < size ; i++)
    {
	for (j = 0 ; j < 4 ; j++)
	{
	    to[i][j] = from[i][j];
	}
    }
    
    return;
}
    
/************************************************************************/
/*
** push current transformation matrix onto matrix stack
*/
int P_pushmatrix (void)
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

/************************************************************************/
/*
** pop top of matrix stack, placing it into the current transformation matrix
*/
int P_popmatrix (void)
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

/************************************************************************/
/*  angle is expressed in tenths of degrees */
void P_rotate (int angle, char axis)
{
    P_rot (angle/10., axis);
    
    return;
}

/************************************************************************/
void P_rot (float angle, char axis)
{
    double theta;

    P_matrix_copy (ident, d, 4);

    theta = (NPI / 180.) * angle; /* convert to radians */

    /* optimize to handle rotations of mutliples of 90 deg */
    switch (axis)
    {
	case 'X':
	case 'x':
	
	    d[1][1] =  cos (theta);
	    d[1][2] =  sin (theta);
	    d[2][1] = -sin (theta);
	    d[2][2] =  cos (theta);
	
	    break;
	case 'Y':
	case 'y':
	
	    d[0][0] =  cos (theta);
	    d[0][2] = -sin (theta);
	    d[2][0] =  sin (theta);
	    d[2][2] =  cos (theta);
	    break;
	case 'Z':
	case 'z':
	
	    d[0][0] =  cos (theta);
	    d[0][1] =  sin (theta);
	    d[1][0] = -sin (theta);
	    d[1][1] =  cos (theta);
	
	    break;
    }

    P_pushmatrix ();
    P__transform (4, d, c_stack[stack_ptr], trans_mat);
    P_popmatrix ();

    return;
}

/************************************************************************/
/*  angle is expressed in radians  */
void P_rad_rotate (double theta, char axis)
{

    P_matrix_copy (ident, d, 4);

    /* optimize to handle rotations of mutliples of 90 deg */
    switch (axis)
    {
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
    P__transform (4, d, c_stack[stack_ptr], trans_mat);
    P_popmatrix ();
}
