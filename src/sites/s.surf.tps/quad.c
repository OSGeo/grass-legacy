
/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#include <stdio.h>
extern  char *calloc(); /* removed <malloc.h> alpha930100 parghi 1993-03-30 */
extern  int  free();
extern  char *malloc();
extern  char *realloc();
#include <math.h>

#include "quad.h"
#include "externs.h"
#include "data.h"


 
struct quadfunc * QT_functions_new(compare,divide_data,add_data,
                          intersect,division_check,get_points)  
    int (*compare) ();
    VOID_T **(*divide_data) ();
    int (*add_data) ();
    int (*intersect) ();
    int (*division_check) ();
    int (*get_points) ();
/* Initializes FUNCTIONS structure with given arguments*/
{
    struct quadfunc *functions;
    if (!(functions = (struct quadfunc *) malloc (sizeof (struct quadfunc))))
    {
	return NULL;
    }
    functions->compare = compare;
    functions->divide_data = divide_data;
    functions->add_data = add_data;
    functions->intersect = intersect;
    functions->division_check = division_check;
    functions->get_points = get_points;
    return functions;
}




struct quadtree * QT_tree_new (data, ne_leaf,nw_leaf,se_leaf,sw_leaf,parent,functions,quadrant)
    VOID_T *data;
    struct quadtree *nw_leaf;
    struct quadtree *ne_leaf;
    struct quadtree *sw_leaf;
    struct quadtree *se_leaf;
    struct quadtree *parent;
    struct quadfunc *functions; 
    int             quadrant;
/*Initializes TREE using given arguments*/
{
    struct quadtree *tree;
    if (!(tree = (struct quadtree *) malloc (sizeof (struct quadtree))))
    {
	return NULL;
    }
    tree->data = data;
    tree->nw_leaf = nw_leaf;
    tree->ne_leaf = ne_leaf;
    tree->sw_leaf = sw_leaf;
    tree->se_leaf = se_leaf;
    tree->parent = parent;
    tree->functions = functions;
    tree->quadrant = quadrant;
    return tree;
}








int 
QT_insert_quad (point, tree)
    struct triple  *point;
    struct quadtree *tree;

{
    int     j=0, i, n,k;
    if (tree == NULL)  {
        fprintf(stderr,"insert: tree is NULL\n");
	return -5;
    }
    if (tree->data == NULL)  {
        fprintf(stderr,"insert: tree->data is NULL\n");
	return -5;
    }
    i = tree->functions->division_check (((struct quaddata *) (tree->data)));

    if (i<=0) {
      if (i==-1)
      {
	switch (tree->functions->compare (point, tree->data))
	{
	  case SW:
	    {
		j=QT_insert_quad (point, tree->sw_leaf);
		break;
	    }
	  case SE:
	    {
		j=QT_insert_quad (point, tree->se_leaf);
		break;
	    }
	  case NW:
	    {
		j=QT_insert_quad (point, tree->nw_leaf);
		break;
	    }
	  case NE:
	    {
		j=QT_insert_quad (point, tree->ne_leaf);
		break;
	    }
	  default:
	    return -3;
	}
      }
      else {
        if (i==0) { 
	    j=tree->functions->add_data (point, ((struct quaddata *) (tree->data)));
	}
      }
    }
    else
    {
        k=QT_divide_quad(tree);
        if(k==1) j=QT_insert_quad(point,tree);
	/* DPG hack */
	if (k == -3)
	{
	    static int      once = 0;
	    if (!once)
	    {
		fprintf (stderr, "Point out of range!\n");
		once = 1;
	    }
	    /*
	     * exit(0);
	     */
	}
        if(k<0) return k;

    }
    return j;
}



int 
QT_divide_quad (tree)
    struct quadtree *tree;
{
/*   VOID_T *sedata, *swdata, *nedata, *nwdata; */
   VOID_T *d,**datas;
   struct quadtree *par;

   datas=tree->functions->divide_data(tree->data);
   if (datas==NULL) 
   {
      fprintf(stderr,"datas is NULL\n");
      return -7;
   }
   if (datas[NE]==NULL) fprintf(stderr,"nedata is NULL\n");
   if (datas[SW]==NULL) fprintf(stderr,"swdata is NULL\n");
   if (datas[SE]==NULL) fprintf(stderr,"sedata is NULL\n");
   if (datas[NW]==NULL) fprintf(stderr,"nwdata is NULL\n");
   par = tree;
   d=datas[SW];
   tree->sw_leaf = QT_tree_new (d,NULL,NULL,NULL,NULL,par,tree->functions,SW);
   d=datas[SE];
   tree->se_leaf = QT_tree_new (d,NULL,NULL,NULL,NULL,par,tree->functions,SE);
   d=datas[NE];
   tree->ne_leaf = QT_tree_new (d,NULL,NULL,NULL,NULL,par,tree->functions,NE);
   d=datas[NW];
   tree->nw_leaf = QT_tree_new (d,NULL,NULL,NULL,NULL,par,tree->functions,NW);
    return 1;
}





int 
QT_region_data (tree, xmin, xmax, ymin, ymax, points, MAX)
    struct quadtree *tree;
    double          xmin;
    double          xmax;
    double          ymin;
    double          ymax;
    struct triple *points;
    int             MAX;	/* max number of points we can add (KMAX2) */
 /* note: this KMAX2 can be larger then KMAX */

{
    int             n = 0, i;
    if (tree == NULL) {
        fprintf(stderr,"QT_region_data: tree is NULL\n");
	return n;
    }
    if (tree->data == NULL) {
        fprintf(stderr,"QT_region_data: tree is NULL\n");
	return n;
    }
    if (tree->functions->intersect (xmin, xmax, ymin, ymax,
                             ((struct quaddata *) ( tree->data))))
    {
	if (tree->sw_leaf != NULL)
	{
	    if ((n = n + QT_region_data (tree->sw_leaf, xmin, xmax, ymin, ymax, points+n, MAX - n)) > MAX)
		return n;
	    if ((n = n + QT_region_data (tree->se_leaf, xmin, xmax, ymin, ymax, points+n, MAX - n)) > MAX)
		return n;
	    if ((n = n + QT_region_data (tree->nw_leaf, xmin, xmax, ymin, ymax, points+n, MAX - n)) > MAX)
		return n;
	    if ((n = n + QT_region_data (tree->ne_leaf, xmin, xmax, ymin, ymax, points+n, MAX - n)) > MAX)
		return n;
	}
	else
	{
	  n=tree->functions->get_points(points,((struct quaddata *) 
                       (tree->data)),xmin,xmax,ymin,ymax,MAX);
	}
	return n;
    }
    return 0;
}



int QT_print_tree(tree,nedge,sedge,eedge,wedge)
 struct quadtree *tree;
 double  nedge,sedge,eedge,wedge;
{
 double x_se,x_sw,x_ne,x_nw,y_se,y_sw,y_ne,y_nw;
    static int k=0;
    static int i=1;

    if (i)
    {
	i = 0;
	printf ("ORGANIZATION: USGS-NMD  DLG DATA - CHARACTE\n");
	printf ("DIGIT DATE:   1977,\n");
	printf ("DIGIT NAME:   grass\n");
	printf ("MAP NAME:     RAPID CITY, SD\n");
	printf ("MAP DATE:     1977,\n");
	printf ("MAP SCALE:    100000\n");
	printf ("OTHER INFO:   RC3.RDS03\n");
	printf ("ZONE:         13\n");
	printf ("WEST EDGE:    %lf\n",wedge);
	printf ("EAST EDGE:    %lf\n",eedge);
	printf ("SOUTH EDGE:   %lf\n",sedge);
	printf ("NORTH EDGE:   %lf\n",nedge);
	printf ("MAP THRESH:   2\n");
	printf ("VERTI:\n");

    }

 if (tree==NULL) return 0;
 if (tree->data==NULL) return 0;
 if (((struct quaddata *) (tree->data))->points == NULL) {
  QT_print_tree(tree->se_leaf,nedge,sedge,eedge,wedge);
  QT_print_tree(tree->sw_leaf,nedge,sedge,eedge,wedge);
  QT_print_tree(tree->ne_leaf,nedge,sedge,eedge,wedge);
  QT_print_tree(tree->nw_leaf,nedge,sedge,eedge,wedge);
 }
 else {
  x_sw=((struct quaddata *) (tree->data))->x_orig;
  y_sw=((struct quaddata *) (tree->data))->y_orig;
  x_nw=x_sw;
  y_se=y_sw;
  x_se=x_sw+((struct quaddata *) (tree->data))->n_cols*ew_res;
  y_nw=y_sw+((struct quaddata *) (tree->data))->n_rows*ns_res;
  y_ne=y_nw;
  x_ne=x_se;
  printf("L   5\n");
  printf(" %lf  %lf\n",y_sw,x_sw);
  printf(" %lf  %lf\n",y_nw,x_nw);
  printf(" %lf  %lf\n",y_ne,x_ne);
  printf(" %lf  %lf\n",y_se,x_se);
  printf(" %lf  %lf\n",y_sw,x_sw);
 }
return 1;
}


