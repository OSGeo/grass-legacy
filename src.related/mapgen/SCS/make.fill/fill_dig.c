/*  @(#)fill.dig.c     1.0  10/03/89   
 *  created by:         M.L.HOLKO, SCS
 *
 * Program will read a vector map and create an ascii coordinate file
 * suitable for input to the mapgen programs proj and lines for
 * generating area fill paterns. This program was adapted from
 * snoop written by R. Glenn, SCS and plotmap written by P. Carlson, SCS.
 */
#include <stdio.h>
#include  "gis.h"
#include "Vect.h"
#include "pattern.h"
#define ERR -1




struct Map_info Map;

main (argc,argv)
int argc;
char *argv[];

{
        int linea,cat,cnt,vect_read; 
        register int area_num, i;
        char  input[50], mapset[50] ;
        double *IX, *IY, scale = 1.0;
        int n_points, *i_points;
        struct pattern pattern;
        int SELECT=0, n_cat, *catlst, *pcatlst;
	struct line_pnts *BP;

if (argc <= 2 ) {
        fprintf (stderr,"\n fill_dig [-sp space -c pen -a angle -sc scale -S num_cats] mapset dig_file\n");
        exit(ERR);
}
else

{          
/*-------------- INIT --------------------------------*/
pattern.spacing = 1000.0;
pattern.color = 1;
pattern.angle = 0.0;
pcatlst = (int *) G_malloc(sizeof(int));
if (pcatlst == 0)
        {fprintf (stderr, "Out of Memory\n"); exit (ERR);}
n_cat = 1;


/* which arguement */
for (cnt=1;cnt < argc; cnt++) {

/* --------------- scale --------------------*/
if (strncmp( argv[cnt],"-sc",3) == 0)
        sscanf(argv[++cnt],"%lf",&scale);


/* --------------- pattern spacing --------------------*/
else if (strncmp( argv[cnt],"-sp",3) == 0)
        sscanf(argv[++cnt],"%lf",&pattern.spacing);


/* --------------- pattern angle --------------------*/
else if (strncmp( argv[cnt],"-a",2) == 0)
        sscanf(argv[++cnt],"%lf",&pattern.angle);

/* --------------- pattern color --------------------*/
else if (strncmp( argv[cnt],"-c",2) == 0)
        sscanf(argv[++cnt],"%d",&pattern.color);

/* --------------- Select mode --------------------*/
else if (strncmp( argv[cnt],"-S",2) == 0)
        {
        sscanf(argv[++cnt],"%d",&n_cat);
        SELECT = 1;
        free(pcatlst);
        pcatlst = catlst = (int *) G_malloc(n_cat * sizeof(int));
        if (pcatlst == 0)
                {fprintf (stderr, "Out of Memory\n"); exit (ERR);}
        for (i=0;i<n_cat;i++)
                scanf("%d",catlst++);
        }

/* -------------   input file name --------------------- */    
else {
        if (sscanf (argv[cnt++], "%s",mapset) != 1) 
                {
                fprintf (stderr,"\n Error in mapset name or options ");
                fprintf (stderr,"\n fill_dig [-sp space -c pen -a angle -sc scale -S num_cats] mapset dig_file\n");
                exit(ERR);
                }
        if (sscanf (argv[cnt], "%s",input) != 1) 
                {
                fprintf (stderr,"\n Error in input file name or options ");
                fprintf (stderr,"\n fill_dig [-sp space -c pen -a angle -sc scale -S num_cats] mapset dig_file\n");
                exit(ERR);
                }
        }
}

/* ---------------- Executable code --------------------------- */
 
fprintf(stderr,"\nLoading vector information.\n");

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

BP = Vect_new_line_struct();


         /* Read and write header info */

fprintf(stderr,"\nProcessing ");

        /* Get cat number of areas to fill   */
if (!SELECT)
*pcatlst = cat = 1;
SELECT = 0;

         /* Cycle through all areas */

for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
{ 
fprintf(stderr,".");

/*DEBUG fprintf(stderr,"\narea= %d, n_lines = %ld, n_isles= %ld, active= %ld\n",area_num,Map.Area[area_num].n_lines,Map.Area[area_num].n_isles,Map.Area[area_num].alive);*/

/* See if category for area "area_num" is selected */
catlst = pcatlst;
for (i=0;i<n_cat;i++) {
/*DEBUG fprintf(stderr,"%d__%d ",*catlst,Map.Att[Map.Area[area_num].att].cat);*/
        if (Map.Att[Map.Area[area_num].att].cat == *(catlst++))
                {SELECT = 1; i = n_cat;}
        else SELECT = 0;
        }

/* ---------------- Do work -----------------------------*/

if (SELECT) {
if (Map.Area[area_num].n_lines)
        {
/* -------------- Get area points -------------------------*/
/* if (0 > dig_P_get_area_xy(&Map, area_num, &n_points, &X, &Y))*/
if (0 > Vect_get_area_points(&Map, area_num, BP))
        {fprintf (stderr, "Out of Memory\n"); exit (ERR);}

/* -------------- Get island points -----------------------*/
if (Map.Area[area_num].n_isles > 0) 
        {
        i_points = (int *) G_malloc(Map.Area[area_num].n_isles * sizeof(int));
        if (i_points == 0)
                {fprintf (stderr, "Out of Memory\n"); exit (ERR);}

        if (0 > dig_P_get_isle(&Map, Map.Area[area_num].isles, Map.Area[area_num].n_isles, i_points, &IX, &IY))
                {fprintf (stderr, "Out of Memory\n"); exit (ERR);}

        /* DEBUG  fprintf(stderr,"isles=%d npts=%d\n",Map.Area[area_num].n_isles,i_points);*/
        }

/* ----------------- Do area fill ---------------------------*/
/* DEBUG  fprintf(stderr,"isle%d x%d y%d ix%d iy%d ip%d",Map.Area[area_num].n_isles,X,Y,IX,IY,i_points);*/

dig_fill(BP->n_points,BP->x,BP->y,&pattern,Map.Area[area_num].n_isles,IX,IY,i_points,scale);
        if (i_points != 0) {free(i_points); i_points = 0;}
        }
}
}
}
fprintf(stderr,"\n");
}
