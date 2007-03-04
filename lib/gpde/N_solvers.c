
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      linear equation system solvers
* 		part of the gpde library
*               
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <math.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "grass/N_pde.h"

/*local protos */
void gauss_elimination(double **A, double *b, int rows);
void lu_decomposition(double **A, int rows);
void backward_solving(double **A, double *x, double *b, int rows);
void forward_solving(double **A, double *x, double *b, int rows);

inline double *vectmem(int size);
inline void matrix_vector_product(N_les * les, double *source, double *result);
inline void sparse_matrix_vector_product(N_les * les, double *source,
					 double *result);
inline int check_symmetry(N_les * les);	/*not jet implemented */
inline void scalar_product(double *source_a, double *source_b, double *result,
			   int row);
inline void sub_vectors(double *source_a, double *source_b, double *result,
			int row);
inline void sub_vectors_scalar(double *source_a, double *source_b,
			       double *result, double scalar_b, int rows);
inline void add_vectors(double *source_a, double *source_b, double *result,
			int rows);
inline void add_vectors_scalar(double *source_a, double *source_b,
			       double *result, double scalar_b, int rows);
inline void add_vectors_scalar2(double *source_a, double *source_b,
				double *result, double scalar_a,
				double scalar_b, int rows);
inline void scalar_vector_product(double *source, double *result, double skalar,
				  int rows);
inline void sync_vectors(double *source, double *target, int rows);

inline int sparse_jacobi_gauss(N_les * L, int maxit, double sor, double error,
			       const char *type);
inline int jacobi(double **M, double *b, double *x, int rows, int maxit,
		  double sor, double error);
inline int gauss_seidel(double **M, double *b, double *x, int rows, int maxit,
			double sor, double error);

/* ******************************************************* *
 * ******** overrelaxed jacobian ************************* *
 * ******************************************************* */
/*!
 * \brief The iterative jacobian solver for regular matrices
 *
 * The result is written to the vector L->x of the les.
 * This iterative solver works with sparse matrices and regular quadratic matrices.
 *
 * The parameter <i>maxit</i> specifies the maximum number of iterations. If the maximum is reached, the
 * solver will abort the calculation and writes the current result into the vector L->x.
 * The parameter <i>err</i> defines the error break criteria for the solver.
 *
 * \param L N_les *  -- the linear equatuin system
 * \param maxit int -- the maximum number of iterations
 * \param sor double -- defines the successive overrelaxion parameter [0:1]
 * \param error double -- defines the error break criteria
 * 
 * */
int N_solver_jacobi(N_les * L, int maxit, double sor, double error)
{
    if (L->type == N_NORMAL_LES)
	return jacobi(L->A, L->b, L->x, L->rows, maxit, sor, error);
    else
	return sparse_jacobi_gauss(L, maxit, sor, error,
				   N_SOLVER_ITERATIVE_JACOBI);
}


/* ******************************************************* *
 * ********* overrelaxed gauss seidel ******************** *
 * ******************************************************* */
/*!
 * \brief The iterative overrelaxed gauss seidel solver for regular matrices
 *
 * The result is written to the vector L->x of the les.
 * This iterative solver works with sparse matrices and regular quadratic matrices.
 *
 * The parameter <i>maxit</i> specifies the maximum number of iterations. If the maximum is reached, the
 * solver will abort the calculation and writes the current result into the vector L->x.
 * The parameter <i>err</i> defines the error break criteria for the solver.
 *
 * \param L N_les *  -- the linear equatuin system
 * \param maxit int -- the maximum number of iterations
 * \param sor double -- defines the successive overrelaxion parameter [0:1]
 * \param error double -- defines the error break criteria
 * 
 * */

int N_solver_SOR(N_les * L, int maxit, double sor, double error)
{
    if (L->type == N_NORMAL_LES)
	return gauss_seidel(L->A, L->b, L->x, L->rows, maxit, sor, error);
    else
	return sparse_jacobi_gauss(L, maxit, sor, error,
				   N_SOLVER_ITERATIVE_SOR);
}

/* ******************************************************* *
 * ****************** conjugate gradients **************** *
 * ******************************************************* */
/*!
 * \brief The iterative conjugate gradients solver for symmetric positive definite matrices
 *
 * The result is written to the vector L->x of the les.
 * This iterative solver works with sparse matrices and regular quadratic matrices.
 *
 * The parameter <i>maxit</i> specifies the maximum number of iterations. If the maximum is reached, the
 * solver will abort the calculation and writes the current result into the vector L->x.
 * The parameter <i>err</i> defines the error break criteria for the solver.
 *
 * \param L N_les *  -- the linear equatuin system
 * \param maxit int -- the maximum number of iterations
 * \param err double -- defines the error break criteria
 * 
 * */
int N_solver_cg(N_les * L, int maxit, double err)
{
    double *r;
    double *p;
    double *v;
    double *x, *b;
    double s = 0.0;
    double a0 = 0, a1 = 0, mygamma, tmp = 0;
    int m, rows, i;
    int finished = 0;

    x = L->x;
    b = L->b;
    rows = L->rows;

    r = vectmem(rows);
    p = vectmem(rows);
    v = vectmem(rows);

    /*
     * residual calculation 
     */
#pragma omp parallel
    {
	/* matrix vector multiplication */
	if (L->type == N_SPARSE_LES)
	    sparse_matrix_vector_product(L, x, v);
	else
	    matrix_vector_product(L, x, v);

	sub_vectors(b, v, r, rows);
	sync_vectors(r, p, rows);

	/* scalar product */
#pragma omp for schedule (static) private(i) reduction(+:s)
	for (i = 0; i < rows; i++) {
	    s += r[i] * r[i];
	}
    }

    a0 = s;
    s = 0.0;

    /* ******************* */
    /* start the iteration */
    /* ******************* */
    for (m = 0; m < maxit; m++) {
#pragma omp parallel default(shared)
	{
	    /* matrix vector multiplication */
	    if (L->type == N_SPARSE_LES)
		sparse_matrix_vector_product(L, p, v);
	    else
		matrix_vector_product(L, p, v);


	    /* scalar product */
#pragma omp for schedule (static) private(i) reduction(+:s)
	    for (i = 0; i < rows; i++) {
		s += v[i] * p[i];
	    }

	    /* barrier */
#pragma omp single
	    {
		tmp = s;
		mygamma = a0 / tmp;
		s = 0.0;
	    }

	    add_vectors_scalar(x, p, x, mygamma, rows);
	    sub_vectors_scalar(r, v, r, mygamma, rows);

	    /* scalar product */
#pragma omp for schedule (static) private(i) reduction(+:s)
	    for (i = 0; i < rows; i++) {
		s += r[i] * r[i];
	    }

	    /* barrier */
#pragma omp single
	    {
		a1 = s;
		tmp = a1 / a0;
		a0 = a1;
		s = 0.0;
	    }
	    add_vectors_scalar(r, p, p, tmp, rows);
	}

	if (L->type == N_SPARSE_LES)
	    G_message(_("sparse CG -- iteration %i error  %g\n"), m, a0);
	else
	    G_message(_("CG -- iteration %i error  %g\n"), m, a0);

	if (a0 < err) {
	    finished = 1;
	    break;
	}
    }

    G_free(r);
    G_free(p);
    G_free(v);

    return finished;
}

/* ******************************************************* *
 * ************ biconjugate gradients ******************** *
 * ******************************************************* */
/*!
 * \brief The iterative biconjugate gradients solver with stabilization for unsymmetric nondefinite matrices
 *
 * The result is written to the vector L->x of the les.
 * This iterative solver works with sparse matrices and regular quadratic matrices.
 *
 * The parameter <i>maxit</i> specifies the maximum number of iterations. If the maximum is reached, the
 * solver will abort the calculation and writes the current result into the vector L->x.
 * The parameter <i>err</i> defines the error break criteria for the solver.
 *
 * \param L N_les *  -- the linear equatuin system
 * \param maxit int -- the maximum number of iterations
 * \param err double -- defines the error break criteria
 * 
 * 
 * */
int N_solver_bicgstab(N_les * L, int maxit, double err)
{
    double *r;
    double *r0;
    double *p;
    double *v;
    double *s;
    double *t;
    double *x, *b;
    double s1 = 0.0, s2 = 0.0, s3 = 0.0;
    double alpha = 0, beta = 0, omega, rr0 = 0, error;
    int m, rows, i;
    int finished = 0;

    x = L->x;
    b = L->b;
    rows = L->rows;
    r = vectmem(rows);
    r0 = vectmem(rows);
    p = vectmem(rows);
    v = vectmem(rows);
    s = vectmem(rows);
    t = vectmem(rows);

#pragma omp parallel
    {
	if (L->type == N_SPARSE_LES)
	    sparse_matrix_vector_product(L, x, v);
	else
	    matrix_vector_product(L, x, v);
	sub_vectors(b, v, r, rows);
	sync_vectors(r, r0, rows);
	sync_vectors(r, p, rows);
    }

    s1 = s2 = s3 = 0.0;

    /* ******************* */
    /* start the iteration */
    /* ******************* */
    for (m = 0; m < maxit; m++) {

#pragma omp parallel default(shared)
	{
	    if (L->type == N_SPARSE_LES)
		sparse_matrix_vector_product(L, p, v);
	    else
		matrix_vector_product(L, p, v);

	    /* scalar product */
#pragma omp for schedule (static) private(i) reduction(+:s1, s2, s3)
	    for (i = 0; i < rows; i++) {
		s1 += r[i] * r[i];
		s2 += r[i] * r0[i];
		s3 += v[i] * r0[i];
	    }

#pragma omp single
	    {
		error = s1;
		rr0 = s2;
		alpha = rr0 / s3;
		s1 = s2 = s3 = 0.0;
	    }

	    sub_vectors_scalar(r, v, s, alpha, rows);
	    if (L->type == N_SPARSE_LES)
		sparse_matrix_vector_product(L, s, t);
	    else
		matrix_vector_product(L, s, t);

	    /* scalar product */
#pragma omp for schedule (static) private(i) reduction(+:s1, s2)
	    for (i = 0; i < rows; i++) {
		s1 += t[i] * s[i];
		s2 += t[i] * t[i];
	    }

#pragma omp single
	    {
		omega = s1 / s2;
		s1 = s2 = 0.0;
	    }

	    add_vectors_scalar2(p, s, r, alpha, omega, rows);
	    add_vectors(x, r, x, rows);
	    sub_vectors_scalar(s, t, r, omega, rows);

#pragma omp for schedule (static) private(i) reduction(+:s1)
	    for (i = 0; i < rows; i++) {
		s1 += r[i] * r0[i];
	    }

#pragma omp single
	    {
		beta = alpha / omega * s1 / rr0;
		s1 = s2 = s3 = 0.0;
	    }

	    sub_vectors_scalar(p, v, p, omega, rows);
	    add_vectors_scalar(r, p, p, beta, rows);
	}


	if (L->type == N_SPARSE_LES)
	    G_message(_("sparse BiCGStab -- iteration %i error  %g\n"), m,
		      error);
	else
	    G_message(_("BiCGStab -- iteration %i error  %g\n"), m, error);

	if (error < err) {
	    finished = 1;
	    break;
	}
    }

    G_free(r);
    G_free(r0);
    G_free(p);
    G_free(v);
    G_free(s);
    G_free(t);

    return finished;
}


/***********************************************************
 * GAUSS elimination solver for Ax = b *********************
 * ********************************************************/
/*!
 * \brief The gauss elimination solver for quardatic matrices
 *
 * This solver does not support sparse matrices
 * The result is written to the vector x in the N_les structure
 *
 * \param les N_les *
 * \return int
 * */
int N_solver_gauss(N_les * les)
{

    if (les->type != N_NORMAL_LES)
	G_fatal_error
	    (_
	     ("The gauss elimination solver works only with normal quadratic matrices"));

    G_message(_("Starting gauss elimination solver"));

    gauss_elimination(les->A, les->b, les->rows);
    backward_solving(les->A, les->x, les->b, les->rows);

    return 0;
}

/***********************************************************
 * LU solver for Ax = b ************************************
 * ********************************************************/
/*!
 * \brief The LU solver for quardatic matrices
 *
 * This solver does not support sparse matrices
 * The result is written to the vector x in the N_les structure
 *
 * \param les N_les *
 * \return int
 * */
int N_solver_lu(N_les * les)
{
    int i;
    double *c, *tmpv;

    if (les->type != N_NORMAL_LES)
	G_fatal_error
	    (_("The lu solver works only with normal quadratic matrices"));

    G_message(_("Starting lu solver method"));

    tmpv = vectmem(les->rows);
    c = vectmem(les->rows);

    lu_decomposition(les->A, les->rows);

#pragma omp parallel
    {

#pragma omp for  schedule (static) private(i)
	for (i = 0; i < les->rows; i++) {
	    tmpv[i] = les->A[i][i];
	    les->A[i][i] = 1;
	}

#pragma omp single
	{
	    forward_solving(les->A, les->b, les->b, les->rows);
	}

#pragma omp for  schedule (static) private(i)
	for (i = 0; i < les->rows; i++) {
	    les->A[i][i] = tmpv[i];
	}

#pragma omp single
	{
	    backward_solving(les->A, les->x, les->b, les->rows);
	}
    }
    G_free(c);
    G_free(tmpv);


    return 0;
}


/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Calculates the scalar product of vector a and b 
 *
 * The result is written to variable scalar
 *
 * \param a       double *
 * \param b       double *
 * \param scalar  double *
 * \param rows int
 * \return void
 *
 * */
void scalar_product(double *a, double *b, double *scalar, int rows)
{
    int i;
    double s = 0.0;

#pragma omp parallel for schedule (static) reduction(+:s)
    for (i = 0; i < rows; i++) {
	s += a[i] * b[i];
    }

    *scalar = s;
    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Calculates the matrix - vector product of matrix L->A and vector x 
 *
 * The result is written to vector named result. This function only works with
 * regular quadratic matrices.
 *
 * \param L N_les *
 * \param x double *
 * \param result double *
 * \return void
 *
 * */
void matrix_vector_product(N_les * L, double *x, double *result)
{
    int i, j;
    double tmp;

#pragma omp for schedule (static) private(i, j, tmp)
    for (i = 0; i < L->rows; i++) {
	tmp = 0;
	for (j = 0; j < L->rows; j++) {
	    tmp += L->A[i][j] * x[j];
	}
	result[i] = tmp;
    }
    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Calculates the matrix - vector product of sparse matrix L->Asp and vector x 
 *
 * The result is written to vector named result. This function only works with
 * sparse matrices matrices.
 *
 * \param L N_les * 
 * \param x double * 
 * \param result double *
 * \return void
 *
 * */
void sparse_matrix_vector_product(N_les * L, double *x, double *result)
{
    int i, j;
    double tmp;

#pragma omp for schedule (static) private(i, j, tmp)
    for (i = 0; i < L->rows; i++) {
	tmp = 0;
	for (j = 0; j < L->Asp[i]->cols; j++) {
	    tmp += L->Asp[i]->values[j] * x[L->Asp[i]->index[j]];
	}
	result[i] = tmp;
    }
    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Multipiles the vector a and b with the scalars scalar_a and scalar_b and adds them
 *
 *
 * The result is written to the vector named result.
 *
 * \param a      double *
 * \param b      double *
 * \param result double *
 * \param scalar_a double
 * \param scalar_b double
 * \param rows int
 * 
 * */
void
add_vectors_scalar2(double *a, double *b, double *result, double scalar_a,
		    double scalar_b, int rows)
{
    int i;

#pragma omp for schedule (static)
    for (i = 0; i < rows; i++) {
	result[i] = scalar_a * a[i] + scalar_b * b[i];
    }

    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Multipiles the vector b with the scalar scalar_b and adds  a to b
 *
 *
 * The result is written to the vector named result.
 *
 * \param a      double *
 * \param b      double *
 * \param result double *
 * \param scalar_b double
 * \param rows int
 * 
 * */
void
add_vectors_scalar(double *a, double *b, double *result, double scalar_b,
		   int rows)
{
    int i;

#pragma omp for schedule (static)
    for (i = 0; i < rows; i++) {
	result[i] = a[i] + scalar_b * b[i];
    }

    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Multipiles the vector b with the scalar scalar_b and substracts b from a
 *
 *
 * The result is written to the vector named result.
 *
 * \param a       double *
 * \param b       double *
 * \param result  double *
 * \param scalar_b double
 * \param rows int
 * 
 * */
void
sub_vectors_scalar(double *a, double *b, double *result, double scalar_b,
		   int rows)
{
    int i;

#pragma omp for schedule (static)
    for (i = 0; i < rows; i++) {
	result[i] = a[i] - scalar_b * b[i];
    }

    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Adds a to b
 *
 *
 * The result is written to the vector named result.
 *
 * \param a       double *
 * \param b       double *
 * \param result  double *
 * \param rows int
 * 
 * */
void add_vectors(double *a, double *b, double *result, int rows)
{
    int i;

#pragma omp for schedule (static)
    for (i = 0; i < rows; i++) {
	result[i] = a[i] + b[i];
    }

    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Substracts b from a 
 *
 *
 * The result is written to the vector named result.
 *
 * \param a      double *
 * \param b      double *
 * \param result double *
 * \param rows int
 * 
 * */
void sub_vectors(double *a, double *b, double *result, int rows)
{
    int i;

#pragma omp for schedule (static)
    for (i = 0; i < rows; i++) {
	result[i] = a[i] - b[i];
    }

    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Multipiles the vector a with the scalar named scalar
 *
 *
 * The result is written to the vector named result.
 *
 * \param a      double *
 * \param result double *
 * \param scalar double *
 * \param rows int
 * 
 * */
void scalar_vector_product(double *a, double *result, double scalar, int rows)
{
    int i;

#pragma omp for schedule (static)
    for (i = 0; i < rows; i++) {
	result[i] = scalar * a[i];
    }

    return;
}

/* ******************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Copies the source vector to the target vector
 *
 * \param source double *
 * \param target double *
 * \param rows int
 * 
 * */
void sync_vectors(double *source, double *target, int rows)
{
    int i;

#pragma omp for schedule (static)
    for (i = 0; i < rows; i++) {
	target[i] = source[i];
    }

    return;
}

/***********************************************************
 * vectmem *************************************************
 * ********************************************************/
/*!
 * \brief Allocate vector memory 
 *
 * \param rows int
 * \return double *
 *
 * */
double *vectmem(int rows)
{
    double *vector;

    vector = (double *)(G_calloc(rows, (sizeof(double))));
    return vector;
}

/***********************************************************
 * gauss elimination ***************************************
 * ********************************************************/
/*!
 * \brief Gauss elimination
 *
 * \param A double **
 * \param b double * 
 * \param rows int
 * \return void
 *
 * */
void gauss_elimination(double **A, double *b, int rows)
{
    int i, j, k;
    double tmpval = 0.0;

    for (k = 0; k < rows - 1; k++) {
#pragma omp parallel for schedule (static) private(i, j, tmpval) shared(k, A, b, rows)
	for (i = k + 1; i < rows; i++) {
	    tmpval = A[i][k] / A[k][k];
	    b[i] = b[i] - tmpval * b[k];
	    for (j = k + 1; j < rows; j++) {
		A[i][j] = A[i][j] - tmpval * A[k][j];
	    }
	}
    }

    return;
}

/***********************************************************
 * lu decomposition ****************************************
 * ********************************************************/
/*!
 * \brief lu decomposition
 *
 * \param A double **
 * \param rows int
 * \return void
 *
 * */
void lu_decomposition(double **A, int rows)
{

    int i, j, k;

    for (k = 0; k < rows - 1; k++) {

#pragma omp parallel for schedule (static) private(i, j) shared(k, A, rows)
	for (i = k + 1; i < rows; i++) {

	    A[i][k] = A[i][k] / A[k][k];

	    for (j = k + 1; j < rows; j++) {

		A[i][j] = A[i][j] - A[i][k] * A[k][j];
	    }
	}
    }

    return;
}


/***********************************************************
 * backward solving ****************************************
 * ********************************************************/
/*!
 * \brief backward solving
 *
 * \param A double **
 * \param x double *
 * \param b double *
 * \param rows int
 * \return void
 *
 * */
void backward_solving(double **A, double *x, double *b, int rows)
{
    int i, j;
    double tmpval = 0.0;

    for (i = rows - 1; i >= 0; i--) {
	tmpval = 0;
	for (j = i + 1; j < rows; j++) {
	    tmpval += A[i][j] * x[j];
	}
	x[i] = (b[i] - tmpval) / A[i][i];
    }

    return;
}


/***********************************************************
 * forward solving *****************************************
 * ********************************************************/
/*!
 * \brief forward solving
 *
 * \param A double **
 * \param x double *
 * \param b double *
 * \param rows int
 * \return void
 *
 * */
void forward_solving(double **A, double *x, double *b, int rows)
{
    int i, j;
    double tmpval = 0.0;

    for (i = 0; i < rows; i++) {
	tmpval = 0;
	for (j = 0; j < i; j++) {
	    tmpval += A[i][j] * x[j];
	}
	x[i] = (b[i] - tmpval) / A[i][i];
    }

    return;
}


/* ******************************************************* *
 * ***** solving a tridiagonal eq system ***************** *
 * ******************************************************* */
void thomalg(double **M, double *V, int rows)
{
    double *Vtmp;
    double *g;
    double b;
    int i;

    Vtmp = vectmem(rows);
    g = vectmem(rows);

    for (i = 0; i < rows; i++) {
	if (i == 0) {
	    b = M[i][i];
	    Vtmp[i] = V[i] / b;
	}
	else {
	    b = M[i][i] - M[i][i - 1] * g[i - 1];
	    Vtmp[i] = (V[i] - Vtmp[i - 1] * M[i][i - 1]) / b;
	}
	if (i < rows - 1) {
	    g[i] = M[i][i + 1] / b;
	}
    }

    V[rows - 1] = Vtmp[rows - 1];
    for (i = rows - 2; i >= 0; i--) {
	V[i] = Vtmp[i] - g[i] * V[i + 1];
    }

    G_free(Vtmp);
    G_free(g);
}


/* ******************************************************* *
 * ****** sparse jacobi and SOR, SSOR algorithm ********** *
 * ******************************************************* */
int
sparse_jacobi_gauss(N_les * L, int maxit, double sor, double error,
		    const char *type)
{
    int i, j, k, rows, finished = 0;
    double *Enew, *x, *b;
    double E, err = 0;

    x = L->x;
    b = L->b;
    rows = L->rows;

    Enew = vectmem(rows);

    for (k = 0; k < maxit; k++) {
	err = 0;
	{
	    if (k == 0) {
		for (j = 0; j < rows; j++) {
		    Enew[j] = x[j];
		}
	    }
	    for (i = 0; i < rows; i++) {
		E = 0;
		if (strcmp(type, N_SOLVER_ITERATIVE_JACOBI) == 0) {
		    for (j = 0; j < L->Asp[i]->cols; j++) {
			E += L->Asp[i]->values[j] * x[L->Asp[i]->index[j]];
		    }
		}
		else {
		    for (j = 0; j < L->Asp[i]->cols; j++) {
			E += L->Asp[i]->values[j] * Enew[L->Asp[i]->index[j]];
		    }
		}
		Enew[i] = x[i] - sor * (E - b[i]) / L->Asp[i]->values[0];
	    }
	    for (j = 0; j < rows; j++) {
		err += (x[j] - Enew[j]) * (x[j] - Enew[j]);

		x[j] = Enew[j];
	    }
	}

	if (strcmp(type, N_SOLVER_ITERATIVE_JACOBI) == 0)
	    G_message(_("sparse Jacobi -- iteration %5i error %g\n"), k, err);
	else if (strcmp(type, N_SOLVER_ITERATIVE_SOR) == 0)
	    G_message(_("sparse SOR -- iteration %5i error %g\n"), k, err);

	if (err < error) {
	    finished = 1;
	    break;
	}
    }

    G_free(Enew);

    return finished;
}

/* ******************************************************* *
 * ******* direct jacobi ********************************* *
 * ******************************************************* */
int jacobi(double **M, double *b, double *x, int rows, int maxit, double sor,
	   double error)
{
    int i, j, k;
    double *Enew;
    double E, err = 0;

    Enew = vectmem(rows);

    for (j = 0; j < rows; j++) {
	Enew[j] = x[j];
    }

    for (k = 0; k < maxit; k++) {
	for (i = 0; i < rows; i++) {
	    E = 0;
	    for (j = 0; j < rows; j++) {
		E += M[i][j] * x[j];
	    }
	    Enew[i] = x[i] - sor * (E - b[i]) / M[i][i];
	}
	err = 0;
	for (j = 0; j < rows; j++) {
	    err += (x[j] - Enew[j]) * (x[j] - Enew[j]);

	    x[j] = Enew[j];
	}
	G_message(_("Jacobi -- iteration %5i error %g\n"), k, err);
	if (err < error)
	    break;
    }

    return 1;
}

/* ******************************************************* *
 * ******* direct gauss seidel *************************** *
 * ******************************************************* */
int gauss_seidel(double **M, double *b, double *x, int rows, int maxit,
		 double sor, double error)
{
    int i, j, k;
    double *Enew;
    double E, err = 0;

    Enew = vectmem(rows);

    for (j = 0; j < rows; j++) {
	Enew[j] = x[j];
    }

    for (k = 0; k < maxit; k++) {
	for (i = 0; i < rows; i++) {
	    E = 0;
	    for (j = 0; j < rows; j++) {
		E += M[i][j] * Enew[j];
	    }
	    Enew[i] = x[i] - sor * (E - b[i]) / M[i][i];
	}
	err = 0;
	for (j = 0; j < rows; j++) {
	    err += (x[j] - Enew[j]) * (x[j] - Enew[j]);

	    x[j] = Enew[j];
	}
	G_message(_("Gauss-Seidel -- iteration %5i error %g\n"), k, err);
	if (err < error)
	    break;
    }

    return 1;
}
