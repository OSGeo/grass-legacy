
/*  @(#)dig.to.geo.c     1.0  10/17/89   
 *  created by:         M.L. Holko, SCS
 *  revised for GRASS 4.0 5/7/91
 *
 * Program will read vector maps and print to standard out the
 * UTM coordinates of  the line work. This output is designed to
 * be piped to the MAPGEN program proj and ultimately to lines
 * to greate the line work overlay for the vector file.
 */

#include  "gis.h"
#include "Vect.h"
#define ERR 1



main (argc,argv)
int argc;
char *argv[];

{
	int cnt, n, n_lines, vect_read;
	register int i;
	double *x, *y;
	char  input[50], mapset[50] ;
	struct line_pnts *p;
	struct Map_info Map;


G_gisinit (argv[0]);
                                    /*  check for arg. should be some */
        if (argc <= 2 ) {
		fprintf(stderr,"dig_to_geo mapset digit\n");
		exit(ERR);
		}
	else
/* which arguement */
             {
	        if (sscanf (argv[1], "%s",mapset) != 1) 
	           {
	           fprintf (stderr,"\n Error in mapset name ");
	           exit(ERR);
	           }
	        if (sscanf (argv[2], "%s",input) != 1) 
	           {
	           fprintf (stderr,"\n Error in input file name ");
	           exit(ERR);
	           }
             }    /* end arg. check */

/* ---------------- Executable code --------------------------- */
 
        fprintf(stderr,"\nLoading vector information.\n");
                     /* Do initial read of input DIGIT file */
        if ((vect_read = Vect_open_old(&Map,input, mapset)) < 0 )
                {
		G_fatal_error("Reading input file.") ;
		return(-1) ;
                }
        if (vect_read < 2)
                {
                G_fatal_error("You must run v.support on this file.") ;
                return(-1) ;
                }
        p = Vect_new_line_struct();


             fprintf(stderr,"\nProcessing .....");

                     /* Cycle through all lines */

	n_lines = Map.n_lines;
/*DEBUG fprintf(stderr,"lines=%d\n",n_lines);*/
			
	for (i=1;i<=n_lines;i++)
		{
		fprintf(stderr,".");
		printf("#-b\n");
		V1_read_next_line(&Map,p);
		x = p->x; y = p->y; n = p->n_points;
		while (n-- > 0)
			{
/*DEBUG fprintf(stderr,"%lf	%lf\n", *x++, *y++);*/
			printf("%lf	%lf\n", *x++, *y++);
			}
		}
fprintf(stderr,"\n");
}
