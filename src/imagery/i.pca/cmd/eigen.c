int G_tqli();
void G_tred2();
double **G_alloc_matrix();
double *G_alloc_vector();

int eigen(M, Vectors, lambda, n) 
/* Computes eigenvalues (and eigen vectors if desired) for	*
*  symmetric matices. 						*/
double 	**M;	    /* Input matrix */
double  **Vectors;  /* eigen vector matrix -output */
double  *lambda;    /* Output eigenvalues */
int	n;	    /* Input matrix dimension */
{
	int   i,j;
	double **a,*e;

	a = G_alloc_matrix(n,n);
	e = G_alloc_vector(n);

	for(i=0; i<n; i++)
	for(j=0; j<n; j++) 
	  a[i][j] = M[i][j];

	G_tred2(a,n,lambda,e);
	G_tqli(lambda,e,n,a);
	
	/* Returns eigenvectors	*/
	for(i=0; i<n; i++) 
	for(j=0; j<n; j++) 
	  Vectors[i][j] = a[i][j]; 

	G_free_matrix(a);
	G_free_vector(e);
}


/***************************************************************************/
/* The "egvorder" (eigsrt) function is  */
/* Based on "Numerical Recipies in C; The Art of Scientific Computing"
   (Cambridge University Press, 1988).  Copyright (C) 1986, 1988 by
   Numerical Recipes Software.  Permission is granted for unlimited
   use within GRASS only.  */

egvorder(d,z,bands)
double *d,**z;
long   bands;
{
double  p;
long    i, j, k;

for (i = 0 ; i<(bands-1) ; i++) {
    p = d[k=i];
    for (j = i+1; j <bands; j++)
      if(d[j] >= p) p=d[k=j];
/*
   interchange eigen values i and k and corresponding eigen vectors
*/
  if (k !=  i)
    {
      d[k] = d[i];
      d[i] = p;
      for (j = 0; j <bands; j++) {
         p = z[j][i];
         z[j][i] = z[j][k];
         z[j][k] = p;
         }
      }
   }
#ifdef DEBUG
  for(i=1;i<=bands;i++) printf("e[%d] = %lf\n",i,d[i]);
#endif
}

/***************************************************************************/

transpose(eigmat,bands)
double **eigmat;
long  bands;
{
  double **trans;
  int i,j;
  trans = (double **) G_malloc((bands+1)*sizeof(double *));
  for(i=0 ; i<bands ; i++)
    trans[i] = (double *)G_malloc ((bands+1)*sizeof(double));
  for(i=0 ; i<bands ; i++)
    for(j=0 ; j<bands ; j++)
      trans[j][i]=eigmat[i][j];
  for (i=0;i<bands;i++)
    for (j=0;j<bands;j++)
      eigmat[i][j]=trans[i][j];
}
/***************************************************************************/




