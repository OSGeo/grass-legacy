/*
*  THIS FUNCTION CONVERTS GRASS AREA DATA INTO MOSS IMPORT FORMAT
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include  "Vect.h"
#include  "gis.h"

#define LATLONG 3          /* LAT/LONG DATA TYPE */
#define MAXISLE 2000       /* MAX # OF ISLANDS PER MOSS FEATURE */

area_to_moss (map,pcats,moss,proj)

struct Map_info *map;      /* SPATIAL INFO FOR THE DIGIT FILE */
struct Categories *pcats;  /* CATEGORY INFO FOR THE DIGIT FILE */
FILE *moss;                /* POINTER TO MOSS IMPORT FILE */
int proj;                  /* PROJECTION TYPE */

{
    char *text;                             /* CATEGORY TEXT */

    double *x,*y;                           /* COORDINATE VARIABLES */
    double tol;             /* TOLERANCE FOR ELIMINATING DUPLICATE POINTS */

    int nareas,area,ncoord;                 /* AREA INDEX AND COUNTER VARIABLES */
    int category;                           /* CATEGORY NUMBER OF AN AREA */
    int nisle,island,isle,isle_flag,npts;   /* ISLAND INDEX, FLAG, AND COUNTER VARIABLES */
    int count=0;                            /* # OF AREAS ACTUALLY CONVERTED */

    P_AREA *poly;                           /* AREA STRUCTURE INFO FOR CURRENT AREA */
    struct line_pnts polypts[MAXISLE+1];    /* ARRAY OF STRUCTURES CONTAINING 
                                            * COORDINATE INFO FOR A POLYGON AND 
                                             * ALL OF ITS ISLANDS */

    struct line_pnts *Tpoints;  

	Tpoints = Vect_new_line_struct();

    printf ("\nConverting GRASS areas into MOSS import polygon features\n");

    /* INITIALIZE MEMORY ALLOCATION FIELD FOR THE POLYPTS ARRAY OF STRUCTURES */
    for (island = 0; island <= MAXISLE; island++)
         polypts[island].alloc_points = 0;
    
    /* INITIALIZE TOLERANCE FOR ELIMINATING DUPLICATE MOSS POINTS */
    if (proj == LATLONG)
       tol = .00001;
    else
       tol = .01;

    /* GET NUMBER OF AREAS, AND PROCESS THEM SEQUENTIALLY */
    nareas = V2_num_areas (map);

    for (area = 1; area <= nareas; area++)
    {
        /* GET INFORMATION FOR THE CURRENT PRIMARY AREA */
		/* following is replaced in grass4.0:
        if (dig_P_get_area (map,area,&poly) < 0 ||
            dig_P_get_area_xy (map,area,&ncoord,&x,&y) < 0)
        */
        if (V2_get_area (map,area,&poly) < 0 ||
            Vect_get_area_points (map,area, Tpoints) < 0)
        {
            printf ("\nNot able to read area %d\n",area);
            exit (-1);
        }
      
        /* ELIMINATE DUPLICATE MOSS POINTS */
        prune_points (Tpoints->x,Tpoints->y,&(Tpoints->n_points),tol);
        if (Tpoints->n_points < 4)   /* NO LONGER HAVE A POLYGON */
        {
           printf ("\nArea %d is too small to be moved into MOSS\n",area);
           continue;
        }

        /* STORE COORDINATE INFO FOR AREA IN THE 1ST ELEMENT OF POLYPTS ARRAY */
        store_points (Tpoints->x,Tpoints->y,Tpoints->n_points,&polypts[0]);
 
        /* GET THE ATTRIBUTE AND CATEGORY TEXT OF THIS AREA */
        category = V2_area_att (map,area);
        text = G_get_cat (category,pcats);
        
        /* PROCESS THE ISLANDS OF THIS AREA */
        nisle = poly->n_isles;
        if (nisle > MAXISLE)
        {
           printf ("\nArea %d has more than the MOSS ",area);
           printf ("maximum of %d islands.\n",MAXISLE);
           exit (-1);
        }

        for (island = 1; island <= nisle; island++)
        {
            /* EXTRACT ISLAND COORDINATES */
            isle = poly->isles[island-1];
            if (get_isle_xy (map,isle,&npts,&(Tpoints->x),&(Tpoints->y)) < 0)
            {
                printf ("\nNot able to read isle %d\n",isle);
                exit (-1);
            }

            /* ELIMINATE DUPLICATE MOSS POINTS */
            prune_points (Tpoints->x,Tpoints->y,&npts,tol);
            if (npts < 4)   /* NO LONGER HAVE AN ISLAND */
            {
               printf
               ("\nIsland %d of area %d is too small to be moved into MOSS\n",
                   island,area);
               continue;
            }

            /* STORE COORDINATE DATA FOR THIS ISLAND */
            store_points (Tpoints->x,Tpoints->y,npts,&polypts[island]);
            Tpoints->n_points += npts;
        }

        /* WRITE A HEADER RECORD FOR THE CURRENT AREA */
        if (write_moss_header (moss,area,Tpoints->n_points,text,proj) < 0)
        {
            printf ("\nNot able to write MOSS import file\n");
            exit (-1);
        }

        /* LOOP THROUGH THE AREA COORDINATE STRUCTURE ARRAY, WRITING A SERIES OF
           COORDINATE RECORDS FOR THE PARENT AND EACH ISLAND */
        isle_flag = 0;
        for (island = 0; island <= nisle; island++)
        {
            if (island == 1) isle_flag = 1;
            if (write_moss_coordinates (moss, polypts[island].n_points,
                polypts[island].x, polypts[island].y, isle_flag, proj) < 0)
            {
                printf ("\nNot able to write MOSS import file\n");
                exit (-1);
            }
        }
        count++;
    }   /* END OUTER LOOP */

    Vect_destroy_line_struct (Tpoints);
    printf ("\n%d Areas Converted\n",count);
    return (0);
}
