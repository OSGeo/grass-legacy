
/*---------------------------------------------------------*
 *               AGNPS/GRASS Interface Project             *
 *  Developed in the Agriculture Engineering Department    *
 *                at Purdue University                     *
 *                        by                               *
 *         Raghavan Srinivasan and Bernard Engel           *
 *                                                         *
 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
 *   permission is granted, this material shall not be     *
 *   copied, reproduced or coded for reproduction by any   *
 *   electrical, mechanical or chemical processes,  or     *
 *   combinations thereof, now known or later developed.   *
 *---------------------------------------------------------*/

/*
 * Modified on 4 October 1991 cr_stats()
 * 
 * write the stats (cumulative distribution curve data) on a file with
 * extension file_name_grid_size_stats. It reads Xcdf.dat as an input file
 * to get the comman X axis intervals
 */
#include "map_gen.h"

int cr_stats ()
{

  int i;
  char buf[512];
  FILE *fs, *fd;
  extern void working_sngl ();
  float A[10], B[10], E[10], F[10], G[10];
  float H[10], I[10], J[10];
  float K[10], L[10], O[10], P[10], Q[10];
  float R[10], S[10], T[10];
  int cdfy ();

  sprintf (buf, "fcdf.dat");
  if ((fd = fopen (buf, "r")) == NULL)
  {
    printf ("ASCII cdf stats file %s can't be open\n", buf);
    return 1;
  }

  sprintf (buf, "%s_%d_stats", file_name, grid_res[cur]);
  if ((fs = fopen (buf, "w")) == NULL)
  {
    printf ("ASCII AGNPS stats file %s can't be open\n", buf);
    exit (0);
  }

  /*
   * fprintf(fs,"Stats for grid size %d and total cells
   * %d\n",grid_res[cur],no_cells[cur]);
   */

  no_X = 10;
  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (Slope, cur);

  for (i = 0; i < 10; i++)
  {
    A[i] = Y[i];
    K[i] = X[i];
  }

  /*
   * fprintf(fs,"Slope stats\n"); fprintf(fs,"Slope in %%    %% of
   * Cumulative area	area frequency\n"); fprintf(fs,"%f	%f
   * %f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],Y[i]-Y[i-1]);
   */

  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (Slope_ln, cur);

  for (i = 0; i < 10; i++)
  {
    B[i] = Y[i];
    L[i] = X[i];
  }

  /*
   * fprintf(fs,"CN stats\n"); fprintf(fs,"CN    %% of Cumulative area
   * area frequency\n"); fprintf(fs,"%f	%f	%f\n",X[0],Y[0],Y[0]);
   * for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],Y[i]-Y[i-1]);
   */

  /*
   * for(i=0; i < 10; i++){ fgets(buf,sizeof buf,fd); X[i] = atof(buf); }
   * cdfy(K_val,cur); for(i=0; i < 10; i++){ C[i] = Y[i]; M[i] = X[i]; }
   */

  /*
   * fprintf(fs,"K factor stats\n"); fprintf(fs,"K factor    %% of
   * Cumulative area	area frequency\n"); fprintf(fs,"%f	%f
   * %f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],Y[i]-Y[i-1]);
   */

  /*
   * for(i=0; i < 10; i++){ fgets(buf,sizeof buf,fd); X[i] = atof(buf); }
   * cdfy(C_fac,cur); for(i=0; i < 10; i++) { D[i] = Y[i]; N[i] = X[i]; }
   */

  /*
   * fprintf(fs,"C factor stats\n"); fprintf(fs,"C factor     %% of
   * Cumulative area	area frequency\n"); fprintf(fs,"%f	%f
   * %f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],Y[i]-Y[i-1]);
   */

  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (1, cur);
  for (i = 0; i < 10; i++)
  {
    E[i] = Y[i];
    O[i] = X[i];
  }

  /*
   * fprintf(fs,"Sediment erosion stats\n"); fprintf(fs,"Sediment erosion
   * in tons    %% of Cumulative area	area frequency\n"); fprintf(fs,"%f
   * %f	%f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],((Y[i]-Y[i-1])/cell_area[cur]));
   */

  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (2, cur);
  for (i = 0; i < 10; i++)
  {
    F[i] = Y[i];
    P[i] = X[i];
  }

  /*
   * fprintf(fs,"Sediment Deposition stats\n"); fprintf(fs,"Sediment
   * Deposition in tons    %% of Cumulative area	area
   * frequency\n"); fprintf(fs,"%f	%f	%f\n",X[0],Y[0],Y[0]);
   * for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],((Y[i]-Y[i-1])/cell_area[cur]));
   */

  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (3, cur);
  for (i = 0; i < 10; i++)
  {
    G[i] = Y[i];
    Q[i] = X[i];
  }

  /*
   * fprintf(fs,"Sediment Yield stats\n"); fprintf(fs,"Sediment Yield in
   * tons    %% of Cumulative area	area frequency\n"); fprintf(fs,"%f
   * %f	%f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],((Y[i]-Y[i-1])/cell_area[cur]));
   */

  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (4, cur);
  for (i = 0; i < 10; i++)
  {
    H[i] = Y[i];
    R[i] = X[i];
  }

  /*
   * fprintf(fs,"Upstream Runoff stats\n"); fprintf(fs,"Up_ro in     %% of
   * Cumulative area	area frequency\n"); fprintf(fs,"%f	%f
   * %f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],((Y[i]-Y[i-1])/cell_area[cur]));
   */

  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (5, cur);
  for (i = 0; i < 10; i++)
  {
    I[i] = Y[i];
    S[i] = X[i];
  }

  /*
   * fprintf(fs,"Ro gen stats\n"); fprintf(fs,"Ro gen in    %% of
   * Cumulative area	area frequency\n"); fprintf(fs,"%f	%f
   * %f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],((Y[i]-Y[i-1])/cell_area[cur]));
   */

  for (i = 0; i < 10; i++)
  {
    fgets (buf, sizeof buf, fd);
    X[i] = atof (buf);
  }
  cdfy (6, cur);
  for (i = 0; i < 10; i++)
  {
    J[i] = Y[i];
    T[i] = X[i];
  }

  /*
   * fprintf(fs,"Down Stream Runoff stats\n"); fprintf(fs,"ds_ro in   %%
   * of Cumulative area	area frequency\n"); fprintf(fs,"%f	%f
   * %f\n",X[0],Y[0],Y[0]); for(i = 1; i <10; i++) fprintf(fs,"%f	%f
   * %f\n",X[i],Y[i],((Y[i]-Y[i-1])/cell_area[cur]));
   */

  /*
   * fprintf(fs,"%5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f
   * %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f
   * %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f
   * %5.2f\n",K[0],A[0],A[0],L[0],B[0],B[0],M[0],C[0],C[0],N[0],D[0],D[0],O
   * [0],E[0],E[0],P[0],F[0],F[0],Q[0],G[0],G[0],R[0],H[0],H[0],S[0],I[0],I
   * [0],T[0],J[0],J[0]);
   * 
   * for(i = 1; i<10 ; i++)fprintf(fs,"%5.2f %5.2f %5.2f %5.2f %5.2f %5.2f
   * %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f
   * %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f
   * %5.2f
   * %5.2f\n",K[i],A[i],((A[i]-A[i-1])/cell_area[cur]),L[i],B[i],((B[i]-B[i
   * -1])/cell_area[cur]),M[i],C[i],((C[i]-C[i-1])/cell_area[cur]),N[i],D[i
   * ],((D[i]-D[i-1])/cell_area[cur]),O[i],E[i],((E[i]-E[i-1])/cell_area[cu
   * r]),P[i],F[i],((F[i]-F[i-1])/cell_area[cur]),Q[i],G[i],((G[i]-G[i-1])/
   * cell_area[cur]),R[i],H[i],((H[i]-H[i-1])/cell_area[cur]),S[i],I[i],((I
   * [i]-I[i-1])/cell_area[cur]),T[i],J[i],((J[i]-J[i-1])/cell_area[cur]));
   * 
   */
  fprintf (fs, "%5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f \n", K[0], A[0], L[0], B[0], O[0], E[0], P[0], F[0], Q[0], G[0], R[0], H[0], S[0], I[0], T[0], J[0]);

  for (i = 1; i < 10; i++)
    fprintf (fs, "%5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f \n", K[i], A[i], L[i], B[i], O[i], E[i], P[i], F[i], Q[i], G[i], R[i], H[i], S[i], I[i], T[i], J[i]);

  fclose (fs);
  fclose (fd);
  return 0;
}
