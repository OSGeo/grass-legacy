/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://www.usol.com/~mccauley/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include<stdio.h>
#include<math.h>
#include"zufall.h"

int 
normalsv (double *svbox)
{
  int i, k;
  extern struct klotz1 klotz1_1;

  /*
   * saves common block klotz0 containing buffers and pointers. IMPORTANT:
   * svbox must be dimensioned at least 1634 in driver. The entire
   * contents of blocks klotz0 (via zufallsv) and klotz1 must be saved.
   */

  /* Parameter adjustments */

  if (klotz1_1.first == 0)
    fprintf (stdout, " ERROR in normalsv, save of unitialized block\n");

  /* save zufall block klotz0 */
  zufallsv (svbox);

  svbox[608] = (double) klotz1_1.first;
  svbox[609] = (double) klotz1_1.xptr;
  k = 609;
  for (i = 0; i < 1024; ++i)
    svbox[i + k] = klotz1_1.xbuff[i];

  return 0;
}				/* normalsv */
