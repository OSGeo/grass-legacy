/*  @(#)psu_alabel.c     1.0  3/27/90
 *  created by:         M.L.HOLKO, SCS
 *
 * Program will read a vector map and find the northern most point 
 * centered on each labeled area.
 */
#include <stdio.h>
#include  "gis.h"
#include "Vect.h"
#define ERR 1




struct Map_info Map;
/*struct head Head;*/

main (argc,argv)
int argc;
char *argv[];

{
char input[100], mapset[100];
int i, vect_read;
double x, y;

if (argc <= 2 ) {
        fprintf (stderr,"\n psu_alabel mapset dig_file\n");
        exit(ERR);
	}

/*-------------- INIT --------------------------------*/

G_gisinit (argv[0]);



/* which arguement */

/* -------------   input file name --------------------- */    

if (sscanf (argv[1], "%s",mapset) != 1) 
	{
	fprintf (stderr,"\n Error in mapset name or options ");
	fprintf (stderr,"\n psu_alabel mapset dig_file\n");
                exit(ERR);
	}
if (sscanf (argv[2], "%s",input) != 1) 
	{
	fprintf (stderr,"\n Error in input file name or options ");
	fprintf (stderr,"\n psu_alabel mapset dig_file\n");
	exit(ERR);
	}

/* ---------------- Executable code --------------------------- */
 
fprintf(stderr,"\nLoading vector information.\n");

         /* Do initial read of input DIGIT file */

/*if (dig_P_init(input, mapset, &Map ) == -1 )
        {
        G_fatal_error("Reading input file.") ;
        return(-1) ;
        } 
*/

if ((vect_read = Vect_open_old(&Map,input, mapset)) < 0 )
	{
	G_fatal_error("Reading input file.") ;
	}
																				if (vect_read < 2)
	{
	G_fatal_error("You must run v.support on this file.") ;
	}

fprintf(stderr,"Processing\n");


         /* Cycle through all areas */

for (i = 1 ; i <= Map.n_areas ; i++)
	{ 
	x = Map.Area[i].W + ((Map.Area[i].E - Map.Area[i].W) / 2.0);
	y = Map.Area[i].N;
	printf("A %lf\t%lf\t%d\n",x,y,Map.Att[Map.Area[i].att].cat);
	}
}
