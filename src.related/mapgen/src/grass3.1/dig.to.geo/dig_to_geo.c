
/*  @(#)dig.to.geo.c     1.0  10/17/89   
 *  created by:         M.L. Holko, SCS
 *
 * Program will read vector maps and print to standard out the
 * UTM coordinates of  the line work. This output is designed to
 * be piped to the MAPGEN program proj and ultimately to lines
 * to greate the line work overlay for the vector file.
 */

#include  "gis.h"
#include "dig_head.h"
#include "digit.h"
#define ERR 1



main (argc,argv)
int argc;
char *argv[];

{
	int cnt, n, n_lines;
	register int i;
	double *x, *y;
	char  input[50], mapset[50] ;
	struct line_pnts *p;
	struct head Head;
	struct Map_info Map;



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
	     if (dig_P_init(input, mapset, &Map ) == -1 )
                {
		G_fatal_error("Reading input file.") ;
		return(-1) ;
                }

                     /* Read and write header info */
	     dig_read_head_binary (Map.digit, &Head);


             fprintf(stderr,"\nProcessing .....");

                     /* Cycle through all lines */

	n_lines = dig_P_num_lines(&Map);
			
	for (i=1;i<=n_lines;i++)
		{
		fprintf(stderr,".");
		printf("#-b\n");
		dig_P_read_next_line(&Map,&p);
		x = p->x; y = p->y; n = p->n_points;
		while (n-- > 0)
			{
			printf("%lf	%lf\n", *x++, *y++);
			}
		}
fprintf(stderr,"\n");
}
