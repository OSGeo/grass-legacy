
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
 * Modified on 2 June 1991 The gen_above and with_in for sediment movement
 * were modified to erosion and deposition. The variables
 * cell_sediment[i].gen_above and cell_sediment[i].within are actually
 * represent erosion and deposition at that cell respectively.
 * erosion(variable cell_sediment[i].gen_above) = cell_sediment[i].within
 * deposition(variable cell_sediment[i].within) = (cell_sediment[i].within
 * + cell_sediment[i].gen_above)*cell_sediment[i].deposition/100
 * 
 * In this routine the output ASCII file is read and all the output maps are
 * created using reclass program
 */
#include "map_gen.h"

int read_output (fl_name, j)
  char *fl_name;
  int j;			/* reading current or previous run data
				 * sets */
{
  FILE *fd;
  char buf[512];
  int G_remove (), G_warning ();
  int read_summary (), read_sed_anlys (), read_soil_loss (), read_nutrients ();


  sprintf (buf, "%s.nps", fl_name);

  if ((fd = fopen (buf, "r")) == NULL && j != prev)
  {
    printf ("ASCII AGNPS Output file %s can't be open\n", buf);
    G_remove ("cell", cell_num_map->p);
    exit (0);
  }
  else if ((fd = fopen (buf, "r")) == NULL && j == prev)
  {
    ANALYSIS = NO;
    G_warning ("AGNPS ASCII outfile is not found");
    return 1;
  }

  read_summary (fd, j);
  read_sed_anlys (fd, j);
  read_soil_loss (fd, j);
  read_nutrients (fd, j);

  fclose (fd);
  return 0;
}


int read_summary (fd, j)
  FILE *fd;
  int j;
{
  int i;
  char buf[1024];

  printf ("Reading watershed summary output\n");

  for (i = 0; i < 4; i++)
  {
    fgets (buf, 1024, fd);
  }
  G_strcpy (wshd_des, buf);

  fscanf (fd, "%d", &total_area[j]);

  fscanf (fd, "%f %f %f %d %s %f %f", &cell_area[j], &rainfall[j], &ei[j], &outlet_cell[j], buf, &ro_vol[j], &peak_ro_rate[j]);

  fscanf (fd, "%f %f %f %f %f %f %f %f", &tot_N_sed[j], &tot_N_ro[j], &sol_N_conc_ro[j], &tot_P_sed[j], &tot_P_ro[j], &sol_P_conc_ro[j], &tot_COD_ro[j], &sol_COD_conc_ro[j]);

  /*
   * printf("%d \n", total_area); printf("%f %f %f %f\n",
   * cell_area,rainfall,ei,sol_COD_conc_ro);
   */
  return 0;
}


int read_sed_anlys (fd, j)
  FILE *fd;
  int j;
{
  int i;
  char buf[512];


  for (i = 0; i < 2; i++)
    fscanf (fd, "%s", buf);
  /*
   * printf(" sediment %s\n",buf);
   */

  printf ("Reading Sediment Movement summary output\n");

  for (i = 0; i < 6; i++)
  {
    /*
     * printf(" sediment %s\n",buf);
     */
    fscanf (fd, "%f %f %d %d %f %f %f", &wshd_sed[j].upland[i], &wshd_sed[j].channel[i], &wshd_sed[j].del_ratio[i], &wshd_sed[j].enrich_ratio[i], &wshd_sed[j].mean_conc[i], &wshd_sed[j].weighted_yield[i], &wshd_sed[j].yield[i]);
  }
  /*
   * printf("yield %f %f\n",wshd_sed[j].upland[5], wshd_sed[j].yield[5]);
   */

  return 0;
}


int read_soil_loss (fd, k)
  FILE *fd;
  int k;
{
  int i, j;
  char buf[512], cell_div[5];
  char *tempfile1, *tempfile2, *tempfile3, *tempfile4, *tempfile5, *tempfile6;
  FILE *fs, *ft, *fu, *fv, *fw, *fx;
  float temp;
  int G_system ();

  printf ("Reading Sediment and Runoff Movement on Cell basis \n");

  gen_above_min[k] = 1000.0;
  gen_above_max[k] = 0.0;
  within_min[k] = 1000.0;
  within_max[k] = 0.0;
  yield_min[k] = 1000.0;
  yield_max[k] = 0.0;
  max_sed[k] = 0.0;
  min_sed[k] = 1000.0;

  ro_us_min[k] = 1000.0;
  ro_us_max[k] = 0.0;
  ro_gen_min[k] = 1000.0;
  ro_gen_max[k] = 0.0;
  ro_ds_min[k] = 1000.0;
  ro_ds_max[k] = 0.0;
  max_ro[k] = 0.0;
  min_ro[k] = 1000.0;

  /* generate a temporary files name and create it */
  tempfile1 = G_tempfile ();
  tempfile2 = G_tempfile ();
  tempfile3 = G_tempfile ();
  tempfile4 = G_tempfile ();
  tempfile5 = G_tempfile ();
  tempfile6 = G_tempfile ();

  fs = fopen (tempfile1, "w");
  ft = fopen (tempfile2, "w");
  fu = fopen (tempfile3, "w");

  fv = fopen (tempfile4, "w");
  fw = fopen (tempfile5, "w");
  fx = fopen (tempfile6, "w");

  for (i = 0; i < 2; i++)
    fscanf (fd, "%s", buf);
  /*
   * printf("soil loss %s\n",buf); printf("soil loss %d\n",no_cells[k]);
   */


  for (i = 0; i < no_cells[k]; i++)
  {
    fscanf (fd, "%s %s %d %f %f %d %f %d %s", buf, cell_div, &cell_runoff[k][i].dr_area, &cell_runoff[k][i].overland_ro, &cell_runoff[k][i].us_ro, &cell_runoff[k][i].peak_us, &cell_runoff[k][i].ds_ro, &cell_runoff[k][i].peak_ds, buf);

    /*
     * write to temp files for later to reclass and to generate runoff
     * maps
     */
    fprintf (fv, "%d = %d \n", i + 1, (int) (cell_runoff[k][i].us_ro * sig_fac));
    fprintf (fw, "%d = %d \n", i + 1, (int) (cell_runoff[k][i].overland_ro * sig_fac));
    fprintf (fx, "%d = %d \n", i + 1, (int) (cell_runoff[k][i].ds_ro * sig_fac));

    if (ro_us_min[k] >= cell_runoff[k][i].us_ro)
      ro_us_min[k] = cell_runoff[k][i].us_ro;
    if (ro_us_max[k] <= cell_runoff[k][i].us_ro)
      ro_us_max[k] = cell_runoff[k][i].us_ro;
    if (ro_gen_min[k] >= cell_runoff[k][i].overland_ro)
      ro_gen_min[k] = cell_runoff[k][i].overland_ro;
    if (ro_gen_max[k] <= cell_runoff[k][i].overland_ro)
      ro_gen_max[k] = cell_runoff[k][i].overland_ro;
    if (ro_ds_min[k] >= cell_runoff[k][i].ds_ro)
      ro_ds_min[k] = cell_runoff[k][i].ds_ro;
    if (ro_ds_max[k] <= cell_runoff[k][i].ds_ro)
      ro_ds_max[k] = cell_runoff[k][i].ds_ro;

    for (j = 0; j < 6; j++)
      fscanf (fd, "%f %f %f %f %d", &cell_sediment[k][i].cell_erosion[j], &cell_sediment[k][i].gen_above[j], &cell_sediment[k][i].within[j], &cell_sediment[k][i].yield[j], &cell_sediment[k][i].deposition[j]);

    temp = cell_sediment[k][i].gen_above[5];
    cell_sediment[k][i].gen_above[5] = cell_sediment[k][i].within[5];
    cell_sediment[k][i].within[5] = (temp + cell_sediment[k][i].gen_above[5]) * cell_sediment[k][i].deposition[5] / 100;

    /*
     * write to temp files for later to reclass and to generate soil loss
     * maps
     */
    fprintf (fs, "%d = %d \n", i + 1, (int) (cell_sediment[k][i].gen_above[5] * sig_fac));
    fprintf (ft, "%d = %d \n", i + 1, (int) (cell_sediment[k][i].within[5] * sig_fac));
    fprintf (fu, "%d = %d \n", i + 1, (int) (cell_sediment[k][i].yield[5] * sig_fac));

    if (gen_above_min[k] >= cell_sediment[k][i].gen_above[5])
      gen_above_min[k] = cell_sediment[k][i].gen_above[5];
    if (gen_above_max[k] <= cell_sediment[k][i].gen_above[5])
      gen_above_max[k] = cell_sediment[k][i].gen_above[5];
    if (within_min[k] >= cell_sediment[k][i].within[5])
      within_min[k] = cell_sediment[k][i].within[5];
    if (within_max[k] <= cell_sediment[k][i].within[5])
      within_max[k] = cell_sediment[k][i].within[5];
    if (yield_min[k] >= cell_sediment[k][i].yield[5])
      yield_min[k] = cell_sediment[k][i].yield[5];
    if (yield_max[k] <= cell_sediment[k][i].yield[5])
      yield_max[k] = cell_sediment[k][i].yield[5];
  }

  if (min_sed[k] >= gen_above_min[k])
    min_sed[k] = gen_above_min[k];
  if (min_sed[k] >= within_min[k])
    min_sed[k] = within_min[k];
  if (min_sed[k] >= yield_min[k])
    min_sed[k] = yield_min[k];

  if (max_sed[k] <= gen_above_max[k])
    max_sed[k] = gen_above_max[k];
  if (max_sed[k] <= within_max[k])
    max_sed[k] = within_max[k];
  if (max_sed[k] <= yield_max[k])
    max_sed[k] = yield_max[k];

  if (min_ro[k] >= ro_us_min[k])
    min_ro[k] = ro_us_min[k];
  if (min_ro[k] >= ro_gen_min[k])
    min_ro[k] = ro_gen_min[k];
  if (min_ro[k] >= ro_ds_min[k])
    min_ro[k] = ro_ds_min[k];

  if (max_ro[k] <= ro_us_max[k])
    max_ro[k] = ro_us_max[k];
  if (max_ro[k] <= ro_gen_max[k])
    max_ro[k] = ro_gen_max[k];
  if (max_ro[k] <= ro_ds_max[k])
    max_ro[k] = ro_ds_max[k];

  fclose (fs);
  fclose (ft);
  fclose (fu);
  fclose (fv);
  fclose (fw);
  fclose (fx);

  printf ("Creating Sediment movement maps\n");

  if (k == cur)
  {

    sprintf (buf, "r.reclass input=%s output=Sed_in < %s", cell_num_map->p, tempfile1);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=Sed_gen < %s", cell_num_map->p, tempfile2);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=Sed_out < %s", cell_num_map->p, tempfile3);
    G_system (buf);

    /* create runoff output maps */
    printf ("Creating Runoff movement maps\n");

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, ro_us, tempfile4);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, ro_gen, tempfile6);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, ro_ds, tempfile6);
    G_system (buf);
  }
  if (k == prev)
  {

    sprintf (buf, "r.reclass input=%s output=Sed_in_prev < %s", cell_num_map->p, tempfile1);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=Sed_gen_prev < %s", cell_num_map->p, tempfile2);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=Sed_out_prev < %s", cell_num_map->p, tempfile3);
    G_system (buf);

    /* create runoff output maps */
    printf ("Creating Runoff movement maps\n");

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, ro_us, tempfile4);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, ro_gen, tempfile6);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, ro_ds, tempfile6);
    G_system (buf);
  }

  sprintf (buf, "/bin/rm -f  %s %s %s %s %s %s", tempfile1, tempfile2, tempfile3, tempfile4, tempfile5, tempfile6);
  G_system (buf);
  return 0;
}


int read_nutrients (fd, j)
  FILE *fd;
  int j;
{

  int i, k;
  char buf[512], cell_div[5];
  char *t1, *t2, *t3, *t4, *t5, *t6, *t7, *t8, *t9, *t10;
  FILE *fs, *ft, *fu, *fv, *fw, *fx, *fy, *fz, *fa, *fb;
  int G_system();

  printf ("Reading N, P and COD Movement on Cell basis \n");

  N_sed_in_min[j] = 1000.0;
  N_sed_in_max[j] = 0.0;
  N_sed_out_min[j] = 1000.0;
  N_sed_out_max[j] = 0.0;
  N_ro_in_min[j] = 1000.0;
  N_ro_in_max[j] = 0.0;
  N_ro_out_min[j] = 1000.0;
  N_ro_out_max[j] = 0.0;

  P_sed_in_min[j] = 1000.0;
  P_sed_in_max[j] = 0.0;
  P_sed_out_min[j] = 1000.0;
  P_sed_out_max[j] = 0.0;
  P_ro_in_min[j] = 1000.0;
  P_ro_in_max[j] = 0.0;
  P_ro_out_min[j] = 1000.0;
  P_ro_out_max[j] = 0.0;

  COD_ro_in_min[j] = 1000.0;
  COD_ro_in_max[j] = 0.0;
  COD_ro_out_min[j] = 1000.0;
  COD_ro_out_max[j] = 0.0;

  max_N_sed[j] = 0.0;
  min_N_sed[j] = 1000.0;

  max_P_sed[j] = 0.0;
  min_P_sed[j] = 1000.0;

  max_N_ro[j] = 0.0;
  min_N_ro[j] = 1000.0;

  max_P_ro[j] = 0.0;
  min_P_ro[j] = 1000.0;

  max_COD_ro[j] = 0.0;
  min_COD_ro[j] = 1000.0;

  /* input file to reclassify for N Nutrient maps */
  t1 = G_tempfile ();
  t2 = G_tempfile ();
  t3 = G_tempfile ();
  t4 = G_tempfile ();

  fs = fopen (t1, "w");
  ft = fopen (t2, "w");
  fu = fopen (t3, "w");
  fv = fopen (t4, "w");

  /* input file to reclassify for P Nutrient maps */
  t5 = G_tempfile ();
  t6 = G_tempfile ();
  t7 = G_tempfile ();
  t8 = G_tempfile ();

  fw = fopen (t5, "w");
  fx = fopen (t6, "w");
  fy = fopen (t7, "w");
  fz = fopen (t8, "w");

  /* input file to reclassify for COD Nutrient maps */
  t9 = G_tempfile ();
  t10 = G_tempfile ();

  fa = fopen (t9, "w");
  fb = fopen (t10, "w");

  for (i = 0; i < 2; i++)
    fscanf (fd, "%s", buf);


  for (i = 0; i < no_cells[cur]; i++)
  {
    fscanf (fd, "%s %s %d %f %f %f %f %d", buf, cell_div, &k, &nut_anlys[j][i].N_sed_within, &nut_anlys[j][i].N_sed_outlet, &nut_anlys[j][i].N_sol_within, &nut_anlys[j][i].N_sol_outlet, &nut_anlys[j][i].N_conc);

    fscanf (fd, "%f %f %f %f %d %f %f %d", &nut_anlys[j][i].P_sed_within, &nut_anlys[j][i].P_sed_outlet, &nut_anlys[j][i].P_sol_within, &nut_anlys[j][i].P_sol_outlet, &nut_anlys[j][i].P_conc, &nut_anlys[j][i].COD_sol_within, &nut_anlys[j][i].COD_sol_outlet, &nut_anlys[j][i].COD_conc);

    /* write input file for creating N nutrient maps */
    fprintf (fs, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].N_sed_within * sig_fac));
    fprintf (ft, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].N_sed_outlet * sig_fac));
    fprintf (fu, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].N_sol_within * sig_fac));
    fprintf (fv, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].N_sol_outlet * sig_fac));

    /* write input file for creating P nutrient maps */
    fprintf (fw, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].P_sed_within * sig_fac));
    fprintf (fx, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].P_sed_outlet * sig_fac));
    fprintf (fy, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].P_sol_within * sig_fac));
    fprintf (fz, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].P_sol_outlet * sig_fac));

    /* write input file for creating P nutrient maps */
    fprintf (fa, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].COD_sol_within * sig_fac));
    fprintf (fb, "%d = %d \n", i + 1, (int) (nut_anlys[j][i].COD_sol_outlet * sig_fac));

    /* find max and min of each of the output in N map */
    if (N_sed_in_min[j] >= nut_anlys[j][i].N_sed_within)
      N_sed_in_min[j] = nut_anlys[j][i].N_sed_within;
    if (N_sed_in_max[j] <= nut_anlys[j][i].N_sed_within)
      N_sed_in_max[j] = nut_anlys[j][i].N_sed_within;
    if (N_sed_out_min[j] >= nut_anlys[j][i].N_sed_outlet)
      N_sed_out_min[j] = nut_anlys[j][i].N_sed_outlet;
    if (N_sed_out_max[j] <= nut_anlys[j][i].N_sed_outlet)
      N_sed_out_max[j] = nut_anlys[j][i].N_sed_outlet;
    if (N_ro_in_min[j] >= nut_anlys[j][i].N_sol_within)
      N_ro_in_min[j] = nut_anlys[j][i].N_sol_within;
    if (N_ro_in_max[j] <= nut_anlys[j][i].N_sol_within)
      N_ro_in_max[j] = nut_anlys[j][i].N_sol_within;

    if (N_ro_out_min[j] >= nut_anlys[j][i].N_sol_outlet)
      N_ro_out_min[j] = nut_anlys[j][i].N_sol_outlet;
    if (N_ro_out_max[j] <= nut_anlys[j][i].N_sol_outlet)
      N_ro_out_max[j] = nut_anlys[j][i].N_sol_outlet;

    /* find max and min of each of the output in P map */
    if (P_sed_in_min[j] >= nut_anlys[j][i].P_sed_within)
      P_sed_in_min[j] = nut_anlys[j][i].P_sed_within;
    if (P_sed_in_max[j] <= nut_anlys[j][i].P_sed_within)
      P_sed_in_max[j] = nut_anlys[j][i].P_sed_within;
    if (P_sed_out_min[j] >= nut_anlys[j][i].P_sed_outlet)
      P_sed_out_min[j] = nut_anlys[j][i].P_sed_outlet;
    if (P_sed_out_max[j] <= nut_anlys[j][i].P_sed_outlet)
      P_sed_out_max[j] = nut_anlys[j][i].P_sed_outlet;
    if (P_ro_in_min[j] >= nut_anlys[j][i].P_sol_within)
      P_ro_in_min[j] = nut_anlys[j][i].P_sol_within;
    if (P_ro_in_max[j] <= nut_anlys[j][i].P_sol_within)
      P_ro_in_max[j] = nut_anlys[j][i].P_sol_within;

    if (P_ro_out_min[j] >= nut_anlys[j][i].P_sol_outlet)
      P_ro_out_min[j] = nut_anlys[j][i].P_sol_outlet;
    if (P_ro_out_max[j] <= nut_anlys[j][i].P_sol_outlet)
      P_ro_out_max[j] = nut_anlys[j][i].P_sol_outlet;

    /* find max and min of each of the output in COD map */
    if (COD_ro_in_min[j] >= nut_anlys[j][i].COD_sol_within)
      COD_ro_in_min[j] = nut_anlys[j][i].COD_sol_within;
    if (COD_ro_in_max[j] <= nut_anlys[j][i].COD_sol_within)
      COD_ro_in_max[j] = nut_anlys[j][i].COD_sol_within;

    if (COD_ro_out_min[j] >= nut_anlys[j][i].COD_sol_outlet)
      COD_ro_out_min[j] = nut_anlys[j][i].COD_sol_outlet;
    if (COD_ro_out_max[j] <= nut_anlys[j][i].COD_sol_outlet)
      COD_ro_out_max[j] = nut_anlys[j][i].COD_sol_outlet;

    /* find max and min in sed and ro of N output maps */
    if (min_N_sed[j] >= N_sed_in_min[j])
      min_N_sed[j] = N_sed_in_min[j];
    if (min_N_sed[j] >= N_sed_out_min[j])
      min_N_sed[j] = N_sed_out_min[j];

    if (min_N_ro[j] >= N_ro_in_min[j])
      min_N_ro[j] = N_ro_in_min[j];
    if (min_N_ro[j] >= N_ro_out_min[j])
      min_N_ro[j] = N_ro_out_min[j];

    if (max_N_sed[j] <= N_sed_in_max[j])
      max_N_sed[j] = N_sed_in_max[j];
    if (max_N_sed[j] <= N_sed_out_max[j])
      max_N_sed[j] = N_sed_out_max[j];

    if (max_N_ro[j] <= N_ro_in_max[j])
      max_N_ro[j] = N_ro_in_max[j];
    if (max_N_ro[j] <= N_ro_out_max[j])
      max_N_ro[j] = N_ro_out_max[j];

    /* find max and min in sed and ro of P output maps */
    if (min_P_sed[j] >= P_sed_in_min[j])
      min_P_sed[j] = P_sed_in_min[j];
    if (min_P_sed[j] >= P_sed_out_min[j])
      min_P_sed[j] = P_sed_out_min[j];

    if (min_P_ro[j] >= P_ro_in_min[j])
      min_P_ro[j] = P_ro_in_min[j];
    if (min_P_ro[j] >= P_ro_out_min[j])
      min_P_ro[j] = P_ro_out_min[j];

    if (max_P_sed[j] <= P_sed_in_max[j])
      max_P_sed[j] = P_sed_in_max[j];
    if (max_P_sed[j] <= P_sed_out_max[j])
      max_P_sed[j] = P_sed_out_max[j];

    if (max_P_ro[j] <= P_ro_in_max[j])
      max_P_ro[j] = P_ro_in_max[j];
    if (max_P_ro[j] <= P_ro_out_max[j])
      max_P_ro[j] = P_ro_out_max[j];

    /* find max and min in sed and ro of COD output maps */
    if (min_COD_ro[j] >= COD_ro_in_min[j])
      min_COD_ro[j] = COD_ro_in_min[j];
    if (min_COD_ro[j] >= COD_ro_out_min[j])
      min_COD_ro[j] = COD_ro_out_min[j];

    if (max_COD_ro[j] <= COD_ro_in_max[j])
      max_COD_ro[j] = COD_ro_in_max[j];
    if (max_COD_ro[j] <= COD_ro_out_max[j])
      max_COD_ro[j] = COD_ro_out_max[j];
  }

  fclose (fs);
  fclose (ft);
  fclose (fu);
  fclose (fv);

  fclose (fw);
  fclose (fx);
  fclose (fy);
  fclose (fz);

  fclose (fa);
  fclose (fb);


  printf ("Creating N associated Nutrient maps\n");

  if (j == cur)
  {
    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, N_sed_in, t1);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, N_sed_out, t2);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, N_ro_in, t3);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, N_ro_out, t4);
    G_system (buf);

    printf ("Creating P associated Nutrient maps\n");

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, P_sed_in, t5);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, P_sed_out, t6);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, P_ro_in, t7);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, P_ro_out, t8);
    G_system (buf);

    printf ("Creating COD associated Nutrient maps\n");

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, COD_ro_in, t9);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, COD_ro_out, t10);
    G_system (buf);
  }

  if (j == prev)
  {
    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, N_sed_in, t1);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, N_sed_out, t2);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, N_ro_in, t3);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, N_ro_out, t4);
    G_system (buf);

    printf ("Creating P associated Nutrient maps\n");

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, P_sed_in, t5);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, P_sed_out, t6);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, P_ro_in, t7);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, P_ro_out, t8);
    G_system (buf);

    printf ("Creating COD associated Nutrient maps\n");

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, COD_ro_in, t9);
    G_system (buf);

    sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, COD_ro_out, t10);
    G_system (buf);
  }

  sprintf (buf, "/bin/rm -f %s %s %s %s %s ", t1, t2, t3, t4, t5);
  G_system (buf);
  sprintf (buf, "/bin/rm -f %s %s %s %s %s ", t6, t7, t8, t9, t10);
  G_system (buf);
  return 0;
}
