
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

#include "map_gen.h"
#include "window_management.h"
#define MAX(x,y)             (( x>y ) ? x : y)
#define MIN(x,y)             (( x<y ) ? x : y)

int user_view (choice, j)
  int choice;
  int j;
{

  char buf[512], buf1[512];
  float max = 0.0, min = 0.0;
  int V_clear (), V_line (), V_ques (), V_call (), G_warning ();

  orig_max = 0.0;
  orig_min = 0.0;

  switch (choice)
  {
  case soil_loss:
    orig_max = max_sed[j];
    orig_min = min_sed[j];
    break;
  case runoff:
    orig_max = max_ro[j];
    orig_min = min_ro[j];
    break;
  case nutrients:
    if (NUTRIENT == N && NUT_ATTCH == sed)
    {
      orig_max = max_N_sed[j];
      orig_min = min_N_sed[j];
    }
    else if (NUTRIENT == N && NUT_ATTCH == ro)
    {
      orig_max = max_N_ro[j];
      orig_min = min_N_ro[j];
    }
    else if (NUTRIENT == P && NUT_ATTCH == sed)
    {
      orig_max = max_P_sed[j];
      orig_min = min_P_sed[j];
    }
    else if (NUTRIENT == P && NUT_ATTCH == ro)
    {
      orig_max = max_P_ro[j];
      orig_min = min_P_ro[j];
    }
    else if (NUTRIENT == COD)
    {
      orig_max = max_COD_ro[j];
      orig_min = min_COD_ro[j];
    }
    break;
  case analysis:
    if (SED_ANALYSIS)
    {
      orig_max = MAX (max_sed[cur], max_sed[prev]);
      orig_min = MIN (min_sed[cur], min_sed[prev]);
    }
    else if (RO_ANALYSIS)
    {
      orig_max = MAX (max_ro[cur], max_ro[prev]);
      orig_min = MIN (min_ro[cur], min_ro[prev]);
    }
    else if (N_SED_ANALYSIS)
    {
      orig_max = MAX (max_N_sed[cur], max_N_sed[prev]);
      orig_min = MIN (min_N_sed[cur], min_N_sed[prev]);
    }
    else if (P_SED_ANALYSIS)
    {
      orig_max = MAX (max_P_sed[cur], max_P_sed[prev]);
      orig_min = MIN (min_P_sed[cur], min_P_sed[prev]);
    }
    else if (N_RO_ANALYSIS)
    {
      orig_max = MAX (max_N_ro[cur], max_N_ro[prev]);
      orig_min = MIN (min_N_ro[cur], min_N_ro[prev]);
    }
    else if (P_RO_ANALYSIS)
    {
      orig_max = MAX (max_P_ro[cur], max_P_ro[prev]);
      orig_min = MIN (min_P_ro[cur], min_P_ro[prev]);
    }
    else if (COD_ANALYSIS)
    {
      orig_max = MAX (max_COD_ro[cur], max_COD_ro[prev]);
      orig_min = MIN (min_COD_ro[cur], min_COD_ro[prev]);
    }
    break;
  }

  for (;;)
  {
    V_clear ();
    V_line (3, "Please give the maximum and minimum value range to display");
    sprintf (buf1, "Enter the minimum value (%5.2f-%5.2f)", orig_min, orig_max);
    V_line (5, buf1);
    sprintf (buf, "Enter the maximum value (%5.2f-%5.2f)", orig_min, orig_max);
    V_line (7, buf);

    V_ques (&min, 'f', 5, 48, 5);
    V_ques (&max, 'f', 7, 48, 5);

    if (!V_call ())
      exit (1);

    if (max <= orig_max && max >= min && min >= orig_min)
      break;
    else
      G_warning ("Please enter correct values between max and min");
  }

  orig_max = max;
  orig_min = min;

  return 0;
}
