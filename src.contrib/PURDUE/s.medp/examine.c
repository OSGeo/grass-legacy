#include <stdio.h>
#include <math.h>
#include "Vect.h"
#include "polish.h"

#define FALSE 0
#define TRUE 1
#define RING 1
#define SILENT 0
#define SMALL_NUM 1e-10

/* determine p and q, error check for non-rectangular grid, and determine
   where to place row, column, and all effects in output */
examine_grid (Map, p, q, tmpx, tmpy, verbose,row_g,col_g,all_g)
  struct Map_info *Map;
  int *p, *q, verbose;
  double *tmpx, *tmpy;
  double **row_g, **col_g, *all_g; 
{
  int bell, i, j, rotated, dblcompare();
  P_NODE *Node;
  double angle, r, t, seglen, xmin[2], ymin[2], atan(), cos(), sin();

  /*
   * note that nodes, areas, and lines are indexed beginning with 1, not 0
   * like a conventional C array
   */

  for (i = 1; i <= Map->n_nodes; ++i)	/* for each node */
  {
    Node = &(Map->Node[i]);	/* just set pointer */
    tmpx[i - 1] = Node->x;	/* copy into my arrays */
    tmpy[i - 1] = Node->y;
  }

  /*
   * now that I have the points of all nodes in my arrays, how do I check to
   * see that the points form a grid? I need to save the dimensions (in no.
   * of points) of the (possibly rotated) square thing in {p} & {q}.
   */
  if (verbose)
    fprintf (stderr, "Checking grid rotation ...      ");

  /* qsort() sorts in ascending order (i.e., tmp[0] will be smallest) */
  qsort (tmpx, Map->n_nodes, sizeof (double), dblcompare);
  qsort (tmpy, Map->n_nodes, sizeof (double), dblcompare);

  if (tmpx[0] == tmpx[1] && tmpy[0] == tmpy[1])
    rotated = FALSE;
  else
    rotated = TRUE;
  if (rotated)
  {
    /* Find the angle of rotation and hope that (ymin[0], ymin[1]) is
       the origin for rotation
     */
    ymin[1]=tmpy[0];
    xmin[0]=tmpx[0];
   
    for (i = 1; i <= Map->n_nodes; ++i)	
    {
      Node = &(Map->Node[i]);
      if (Node->x == xmin[0])
        xmin[1] = Node->y;
      if (Node->y == ymin[1])
        ymin[0] = Node->x;
    }

    angle = -atan ((xmin[0] - ymin[0]) / (xmin[1] - ymin[1]));

    /* Now rotate and translate each point */
    for (i = 1; i <= Map->n_nodes; ++i)	/* rotate each node by {angle} */
    {
      Node = &(Map->Node[i]);
      tmpx[i - 1] = (Node->x-ymin[0]) * cos (angle) 
	+ (Node->y-ymin[1]) * sin(angle);
      tmpy[i - 1] = -(Node->x-ymin[0]) * sin (angle) 
	+ (Node->y-ymin[1]) * cos(angle);
    }
  }
  qsort (tmpx, Map->n_nodes, sizeof (double), dblcompare);
  qsort (tmpy, Map->n_nodes, sizeof (double), dblcompare);
  /*
    At this point, if the nodes represent a grid, we should have segments of
    each array ({tmpx} and {tmpy}) which are the same.  So, we've got a
    2D array in a 1D array. First, make an initial estimate of the segment 
    lengths. 
   */
  if(verbose)
  {
    if (rotated) 
      fprintf (stderr, "% 2.2f deg\n",angle*180/3.1415927);
    else
      fprintf (stderr, "\b\bnot rotated \n");
  }

  for (r=0,t=0,i=1,*p=1,*q=1; i< Map->n_nodes; ++i)
  {
    if (fabs(tmpx[i]-tmpx[i-1]) < SMALL_NUM && !r)
      (*p)++;
    else
      r=1;
    if (fabs(tmpy[i]-tmpy[i-1]) < SMALL_NUM && !t)
      (*q)++;
    else
      t=1;
    if (r && t) 
      i=Map->n_nodes;
  }

  /* Now we have initial estimates of the segments lengths.
     For {tmpx}, segments may be {p} long and
     for {tmpy}, segments may be {q} long.  We know that
     {p*q} should be equal {Map->n_nodes}. Let's check.
   */

  if ((*p)*(*q)==Map->n_nodes) 
  {
    /* Okay, makes sense so far, now lets check every segment */
    seglen=(int) Map->n_nodes/(*p);
    bell=SILENT;
    for (i=0; i< Map->n_nodes-seglen; i+=seglen)
    {
/*-
#ifdef DIAG
  fprintf(stderr, "x:");
  for(j=i; j< i+seglen; ++j)
     fprintf(stderr, "%g ", tmpx[j]);
  fprintf(stderr, "\n");
#endif
*/
      j=i;
      while(fabs(tmpx[j]-tmpx[i])  < SMALL_NUM)
	j++;
      if (j!=(i+seglen))
        bell=RING;
    }
    seglen=(int) Map->n_nodes/(*q);
    for (i=0; i< Map->n_nodes-seglen; i+=seglen)
    {
/*-
#ifdef DIAG
  fprintf(stderr, "y:");
  for(j=i; j< i+seglen; ++j)
     fprintf(stderr, "%g ", tmpx[j]);
  fprintf(stderr, "\n");
#endif
*/
      j=i;
      while(fabs(tmpy[j]-tmpy[i]) < SMALL_NUM)
	j++;
      if (j!=(i+seglen))
        bell=RING;
    }
  }
  else
    return 1;

  if (bell== RING)
    return 1;
  else
  {
    /* determine where to plot row, column, and all effects */
    if(!rotated)      
      angle=0;

    seglen=(int) Map->n_nodes / (*p);
    for(i=0; i< *q; ++i)
    {
      col_g[i][0]= (tmpx[i* (int) seglen]+ymin[0]) * cos(angle) 
	- (tmpy[Map->n_nodes] +ymin[1])*sin(angle);
      col_g[i][1]= (tmpx[i* (int) seglen]+ymin[0])*sin(angle) 
	+ (tmpy[Map->n_nodes]+ymin[1])*cos(angle);
    }
    seglen=(int) Map->n_nodes / (*q);
    for(i=0; i< *p; ++i)
    {
      row_g[i][0]=(tmpx[Map->n_nodes]+ymin[0])*cos(angle) 
	- (tmpy[i* (int) seglen]+ymin[1])*sin(angle);
      row_g[i][1]=-(tmpx[Map->n_nodes]+ymin[0])*sin(angle) 
	+ (tmpy[i* (int) seglen]+ymin[1])*cos(angle);
    }
    all_g[0]=(tmpx[Map->n_nodes]+ymin[0])*cos(angle) - (tmpy[Map->n_nodes]+ymin[1])*sin(angle);
    all_g[1]=-(tmpx[Map->n_nodes]-ymin[0])*sin(angle) + (tmpy[Map->n_nodes]-ymin[1])*cos(angle);

    /* reset nodes to original coordinates*/
    for (i = 1; i <= Map->n_nodes; ++i)   /* for each node */
    {
      Node = &(Map->Node[i]);     /* just set pointer */
      tmpx[i - 1] = Node->x;      /* copy into my arrays */
      tmpy[i - 1] = Node->y;
    }
  }
  return 0;
}
