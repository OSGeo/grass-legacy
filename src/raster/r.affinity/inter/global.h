#include "imagery.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define MAX_GREYLEVAL   400  /* appended by Tian */
#define MAX_BANDNUMBER   7   /* Tian */
#define MAX_CLUSTERS    40   /* Tian */

GLOBAL CELL *vals;
GLOBAL struct Ref Ref;
GLOBAL struct Signature S;
GLOBAL CELL **cell;
GLOBAL int *cellfd;
GLOBAL CELL *class_cell, *reject_cell,*t_cell;
GLOBAL int class_fd, reject_fd,t_fd;
GLOBAL double *B;
GLOBAL double *P;
GLOBAL double mean;   
GLOBAL char group[50];
GLOBAL char subgroup[50];
GLOBAL char t_map[50]; 
GLOBAL char *t_mapset;    
GLOBAL int  types[MAX_BANDNUMBER]; 
GLOBAL int layernum;
GLOBAL char class_name[50],reject_name[50];
GLOBAL int  val_num[MAX_BANDNUMBER];   /* the number of the different values in each layer */
GLOBAL int clusternum,nozero;
GLOBAL int cluster_val[MAX_CLUSTERS];
GLOBAL struct Categories cats[MAX_BANDNUMBER]; 
GLOBAL double means[MAX_BANDNUMBER][MAX_CLUSTERS];
GLOBAL double  pixelnum[MAX_BANDNUMBER];


/* appended by Tian 12/05/1997 */
struct Node_stats
{
	CELL  greyv;      /* grey leval value */
        long count_c;    /* the number of the pixels with this grey level in one catergory */
	long count_all;  /* the number of the pixels with this grey leval of the whole image*/

};


struct Node_stats *statsb0;   /* MAX_GREYLEVAL, MAXGREYLEVAL..... MAXGREYLEVAL */
struct Node_stats *statsb1;
struct Node_stats *statsb2;
struct Node_stats *statsb3;
struct Node_stats *statsb4;
struct Node_stats *statsb5;
struct Node_stats *statsb6;
struct Node_stats *stats;

