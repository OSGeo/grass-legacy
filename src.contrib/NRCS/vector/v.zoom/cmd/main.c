
/*  @(#)main.c     1.0  10/03/89   
 *  created by:         M.L.HOLKO, NRCS
 *
 * Program will accept a vector map and list of category numbers (from standard in)
 * it then sets the current region to be the maximum box surrounding the
 * select area categories. The margin option allows the user to provide a buffer
 * arround the areas selected as a fraction of the x and y distances.
 */
#include <stdio.h>
#include  "gis.h"
#include "Vect.h"
#define ERR -1




struct Map_info Map;

main (argc,argv)
int argc;
char *argv[];

{
        int vect_read, start=1; 
        register int area_num, i;
        char  *input, *mapset, buffr[100] ;
        int n_cats=0, catlst[1000], DO=0;
		struct Option *inopt, *maropt;
		struct Cell_head region;
		double n,s,e,w,N=0,S=0,E=0,W=0,mar;


	G_gisinit (argv[0]);


	inopt = G_define_option();
	inopt->key             = "input";
	inopt->type            = TYPE_STRING;
	inopt->required        = YES;
	inopt->gisprompt       = "old,dig,vect";
	inopt->description     = "vector input map name ";
						 
	maropt = G_define_option();
	maropt->key             = "margin";
	maropt->type            = TYPE_STRING;
	maropt->required        = NO;
	maropt->description     = "Margin as a fraction of window";
						 
       /* heeeerrrrrre's the   PARSER */
    if (G_parser (argc, argv))
        exit (-1);

if (maropt->answer != NULL)  sscanf(maropt->answer,"%lf",&mar);
else mar = 0.0;


/* --------------- Read category list --------------------*/
if (isatty(0)) 
    printf("Enter the category numbers for area(s) desired, use <CTRL-D> to quit\n");
    
  while (scanf("%d",&catlst[n_cats]) != EOF) {
	n_cats++;
	}
/* -------------   input file name --------------------- */    
	input = inopt->answer;
	mapset = G_find_vector (input, "") ;
	if (mapset == NULL)
		{
		sprintf(buffr,"Vector file [%s] not available in search list", input);
		G_fatal_error(buffr) ;
		}


/* ---------------- Executable code --------------------------- */
 
         /* Do initial read of input DIGIT file */
if ((vect_read = Vect_open_old(&Map,input, mapset)) < 0 )
        {
        G_fatal_error("Reading input file.") ;
        return(ERR) ;
        } 
if (vect_read < 2)
	{
	G_fatal_error("You must run v.support on this file.") ;
	return(ERR) ;
	}

	G_get_window(&region);
	N = region.north;
	S = region.south;
	E = region.east;
	W = region.west;

         /* Cycle through all areas */
for (area_num = 1 ; area_num <= Map.n_areas ; area_num++) { 
	for (i=0;i<n_cats;i++) {
       	if (Map.Att[Map.Area[area_num].att].cat == catlst[i]) {
/*		fprintf(stderr,"%d %d %d %d\n",i,area_num,Map.Att[Map.Area[area_num].att].cat,catlst[i]); */
			if (V2_get_area_bbox(&Map,area_num,&n,&s,&e,&w) != 0) {
				fprintf(stderr,"No Bounding box for area %d\n",area_num);
				}
			else {
				DO = 1;
/*			fprintf(stderr,"%f %f %f %f\n%f %f %f %f\n",N,S,E,W,n,s,e,w); */
				if (start) { N=n; S=s; E=e; W=w; start=0;}
				else {
					if (n > N) N = n;
					if (s < S) S = s;
					if (e > E) E = e;
					if (w < W) W = w;
					}
				}
			}
        }

	}
if (DO) {
	region.north = N + (mar * (N-S));
	region.south = S - (mar * (N-S));
	region.east = E + (mar * (E-W));
	region.west = W - (mar * (E-W));
	if ( G_put_window(&region) != 1)
		fprintf(stderr,"Error in setting window\n");
        printf("do \"d.erase; d.vect %s\" to see results\n",input);
	}
}
