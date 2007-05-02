
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      local prototypes for linear equation system solvers
* 		part of the gpde library
*               
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

inline double *vectmem(int size);
void gauss_elimination(double **A, double *b, int rows);
void lu_decomposition(double **A, int rows);
void backward_solving(double **A, double *x, double *b, int rows);
void forward_solving(double **A, double *x, double *b, int rows);

inline int sparse_jacobi_gauss(N_les * L, int maxit, double sor, double error,
			       const char *type);
inline int jacobi(double **M, double *b, double *x, int rows, int maxit,
		  double sor, double error);
inline int gauss_seidel(double **M, double *b, double *x, int rows, int maxit,
			double sor, double error);

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



