
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
 * Modified on 2 June 1991 Raghavan Srinivasan (srin@ecn.purdue.edu)
 * 
 * cdfx(vaiable, option,j)
 * 
 * variable: one of the input/output variable for which the stats are derived
 * option: either user selected or 10 equal intervals of X axies j:
 * current or selected simulation
 * 
 * This routine fixes the X axies array coordinates system.
 */

#include "map_gen.h"

int cdfx (variable, option, j)
  int variable;			/* name of the variable */
  int option;			/* 1-user specified interval 2-10 equal
				 * intervals */
  int j;			/* read current or previous run data sets */
{
  int i;
  int get_interval ();

  /* initialize the X and Y array */
  for (i = 0; i < 10; i++)
  {
    X[i] = 0.0;
  }

  /* the variable X[] is set */
  switch (variable)
  {
  case Slope:
    if (option == 2)
      no_X = get_interval (slope_pct[j].mn, slope_pct[j].mx, 10);
    else
      no_X = get_interval (slope_pct[j].mn, slope_pct[j].mx, 0);
    break;
  case CN:
    if (option == 2)
      no_X = get_interval (cn[j].mn, cn[j].mx, 10);
    else
      no_X = get_interval (cn[j].mn, cn[j].mx, 0);
    break;
  case Slope_ln:
    if (option == 2)
      no_X = get_interval (slope_ln[j].mn, slope_ln[j].mx, 10);
    else
      no_X = get_interval (slope_ln[j].mn, slope_ln[j].mx, 0);
    break;
  case K_val:
    if (option == 2)
      no_X = get_interval (k_val[j].mn, k_val[j].mx, 10);
    else
      no_X = get_interval (k_val[j].mn, k_val[j].mx, 0);
    break;
  case C_fac:
    if (option == 2)
      no_X = get_interval (c_fac[j].mn, c_fac[j].mx, 10);
    else
      no_X = get_interval (c_fac[j].mn, c_fac[j].mx, 0);
    break;
  case Fert_fac:
    if (option == 2)
      no_X = get_interval (fert_fac[j].mn, fert_fac[j].mx, 10);
    else
      no_X = get_interval (fert_fac[j].mn, fert_fac[j].mx, 0);
    break;
  case 1:
    if (option == 2)
      no_X = get_interval (gen_above_min[j], gen_above_max[j], 10);
    else
      no_X = get_interval (gen_above_min[j], gen_above_min[j], 0);
    break;
  case 2:
    if (option == 2)
      no_X = get_interval (within_min[j], within_max[j], 10);
    else
      no_X = get_interval (within_min[j], within_max[j], 0);
    break;
  case 3:
    if (option == 2)
      no_X = get_interval (yield_min[j], yield_max[j], 10);
    else
      no_X = get_interval (yield_min[j], yield_max[j], 0);
    break;
  case 4:
    if (option == 2)
      no_X = get_interval (ro_us_min[j], ro_us_max[j], 10);
    else
      no_X = get_interval (ro_us_min[j], ro_us_max[j], 0);
    break;
  case 5:
    if (option == 2)
      no_X = get_interval (ro_gen_min[j], ro_gen_max[j], 10);
    else
      no_X = get_interval (ro_gen_min[j], ro_gen_max[j], 0);
    break;
  case 6:
    if (option == 2)
      no_X = get_interval (ro_ds_min[j], ro_ds_max[j], 10);
    else
      no_X = get_interval (ro_ds_min[j], ro_ds_max[j], 0);
    break;
  case 7:
    if (option == 2)
      no_X = get_interval (N_sed_in_min[j], N_sed_in_max[j], 10);
    else
      no_X = get_interval (N_sed_in_min[j], N_sed_in_max[j], 0);
    break;
  case 8:
    if (option == 2)
      no_X = get_interval (N_sed_out_min[j], N_sed_out_max[j], 10);
    else
      no_X = get_interval (N_sed_out_min[j], N_sed_out_max[j], 0);
    break;
  case 9:
    if (option == 2)
      no_X = get_interval (N_ro_in_min[j], N_ro_in_max[j], 10);
    else
      no_X = get_interval (N_ro_in_min[j], N_ro_in_max[j], 0);
    break;
  case 10:
    if (option == 2)
      no_X = get_interval (N_ro_out_min[j], N_ro_out_max[j], 10);
    else
      no_X = get_interval (N_ro_out_min[j], N_ro_out_max[j], 0);
    break;
  case 11:
    if (option == 2)
      no_X = get_interval (P_sed_in_min[j], P_sed_in_max[j], 10);
    else
      no_X = get_interval (P_sed_in_min[j], P_sed_in_max[j], 0);
    break;
  case 12:
    if (option == 2)
      no_X = get_interval (P_sed_out_min[j], P_sed_out_max[j], 10);
    else
      no_X = get_interval (P_sed_out_min[j], P_sed_out_max[j], 0);
    break;
  case 13:
    if (option == 2)
      no_X = get_interval (P_ro_in_min[j], P_ro_in_max[j], 10);
    else
      no_X = get_interval (P_ro_in_min[j], P_ro_in_max[j], 0);
    break;
  case 14:
    if (option == 2)
      no_X = get_interval (P_ro_out_min[j], P_ro_out_max[j], 10);
    else
      no_X = get_interval (P_ro_out_min[j], P_ro_out_max[j], 0);
    break;
  case 15:
    if (option == 2)
      no_X = get_interval (COD_ro_in_min[j], COD_ro_in_max[j], 10);
    else
      no_X = get_interval (COD_ro_in_min[j], COD_ro_in_max[j], 0);
    break;
  case 16:
    if (option == 2)
      no_X = get_interval (COD_ro_out_min[j], COD_ro_out_max[j], 10);
    else
      no_X = get_interval (COD_ro_out_min[j], COD_ro_out_max[j], 0);
    break;
  }

  return 0;
}

int cdfy (variable, j)
  int variable;			/* name of the variable */
  int j;			/* read current or previous run data sets */
{
  int i;
  int k;


  /* initialize  Y array */
  for (i = 0; i < 10; i++)
  {
    Y[i] = 0.0;
  }

  printf ("Computing pdf x,y co-ordinates \n");

  for (i = 0; i < no_cells[j]; i++)
  {
    k = 0;
    switch (variable)
    {
    case Slope:
      while (k < no_X)
      {
	if (ag_inp[j][i].slope_pct <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case CN:
      while (k < no_X)
      {
	if (ag_inp[j][i].cn <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case Slope_ln:
      while (k < no_X)
      {
	if (ag_inp[j][i].slope_ln <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case K_val:
      while (k < no_X)
      {
	if (ag_inp[j][i].k_val <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case C_fac:
      while (k < no_X)
      {
	if (ag_inp[j][i].c_fac <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case Fert_fac:
      while (k < no_X)
      {
	if (ag_inp[j][i].fert_fac <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 1:
      while (k < no_X)
      {
	if (cell_sediment[j][i].gen_above[5] <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 2:
      while (k < no_X)
      {
	if (cell_sediment[j][i].within[5] <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 3:
      while (k < no_X)
      {
	if (cell_sediment[j][i].yield[5] <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 4:
      while (k < no_X)
      {
	if (cell_runoff[j][i].us_ro <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 5:
      while (k < no_X)
      {
	if (cell_runoff[j][i].overland_ro <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 6:
      while (k < no_X)
      {
	if (cell_runoff[j][i].ds_ro <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 7:
      while (k < no_X)
      {
	if (nut_anlys[j][i].N_sed_within <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 8:
      while (k < no_X)
      {
	if (nut_anlys[j][i].N_sed_outlet <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 9:
      while (k < no_X)
      {
	if (nut_anlys[j][i].N_sol_within <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 10:
      while (k < no_X)
      {
	if (nut_anlys[j][i].N_sol_outlet <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 11:
      while (k < no_X)
      {
	if (nut_anlys[j][i].P_sed_within <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 12:
      while (k < no_X)
      {
	if (nut_anlys[j][i].P_sed_outlet <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 13:
      while (k < no_X)
      {
	if (nut_anlys[j][i].P_sol_within <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 14:
      while (k < no_X)
      {
	if (nut_anlys[j][i].P_sol_outlet <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 15:
      while (k < no_X)
      {
	if (nut_anlys[j][i].COD_sol_within <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    case 16:
      while (k < no_X)
      {
	if (nut_anlys[j][i].COD_sol_outlet <= X[k])
	{
	  Y[k] = Y[k] + 1;
	}
	k++;
      }
      break;
    }
  }

  /* compute cumulative percentage area */
  for (i = 0; i < no_X; i++)
    Y[i] = Y[i] / (double) no_cells[j];
  return 0;
}

int get_interval (min, max, no_x)
  double min, max;
  int no_x;
{

  double incr;
  float value1;
  int i;
  char buf[512];
  int G_clear_screen ();
  int G_gets ();


  if (no_x == 10)
  {
    incr = ((max - min) / (double) (no_x - 1));
    X[0] = min;
    for (i = 1; i < 9; i++)
      X[i] = min + incr * i;
    X[9] = max;
  }

  else
  {
    G_clear_screen ();
    i = 1;
    X[0] = min;
    while (i < 10)
    {
      printf ("Value should be between %6.2f  and %6.2f\n", X[i - 1], max);
      printf ("Enter the next value (enter -1 to terminate) ---> ");
      while (!G_gets (buf));
      sscanf (buf, "%f", &value1);
      if (value1 == -1.0)
      {
	X[i] = max;
	no_x = i + 1;
	break;
      }
      else if (value1 <= max && value1 > X[i - 1])
      {
	X[i] = value1;
	i++;
      }
      else
	printf ("Entered a wrong value, please enter once again\n");
    }
  }

  if (no_x == 10)
    X[no_x - 1] = max;

  return (no_x);
}
