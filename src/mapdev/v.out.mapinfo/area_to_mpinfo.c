/*
*  THIS FUNCTION CONVERTS GRASS AREA DATA INTO MAPINFO IMPORT FORMAT
*
* $Id$
*
*  Written BY:
*  R.L. Glenn, USDA, NRCS, 9/19/95
*/

#include  "Vect.h"
#include  "gis.h"

#define MAXISLE 2000       /* MAX # OF ISLANDS PER MAPINFO FEATURE */
#define ABS(x) ((x) < 0 ? -(x) : (x))
struct line_pnts *get_isle_xy();

area_to_mpinfo (map,pcats,mpmif,mpmid,proj,nad,zone)

struct Map_info *map;      /* SPATIAL INFO FOR THE DIGIT FILE */
struct Categories *pcats;  /* CATEGORY INFO FOR THE DIGIT FILE */
FILE *mpmid, *mpmif;       /* POINTER$ TO MAPINFO EXPORT FILES */
int proj;                  /* PROJECTION TYPE */
int nad;                   /* NAD TYPE */
int zone;                  /* ZONE */

{

    double *x,*y;           /* COORDINATE VARIABLES */
    double tol;             /* TOLERANCE FOR ELIMINATING DUPLICATE POINTS */

    int nareas,area; /* AREA INDEX AND COUNTER VARIABLES */
    int category;           /* CATEGORY NUMBER OF AN AREA */
    int nisle,island,isle;  /* ISLAND INDEX, FLAG, AND COUNTER VARIABLES */
    int count=0;            /* # OF AREAS ACTUALLY CONVERTED */
    int ii;
    char label[80],ans[10], *pntr;
    P_AREA *poly;           /* AREA STRUCTURE INFO FOR CURRENT AREA */

    struct line_pnts *Tpoints;  

    Tpoints = Vect_new_line_struct();

    fprintf (stderr, "\nConverting GRASS areas into MAPINFO import Region features\n");

    tol = .00001;

    /* WRITE HEADER RECORDS FOR THE MIF */
    if (write_mpinfo_header (mpmif,3,proj,nad,zone) < 0)
        {
            fprintf (stderr, "\nNot able to write MAPINFO import header\n");
            exit (-1);
        }

    /* GET NUMBER OF AREAS, AND PROCESS THEM SEQUENTIALLY */
    nareas = V2_num_areas (map);

    for (area = 1; area <= nareas; area++)
    {
        category = map->Att[map->Area[area].att].cat;
	if(category == 0) {
	  fprintf (stderr, " Area %d has no category value, quitting\n",area);
	  exit(-1);
	  }
	for(ii=0;ii < pcats->ncats; ii++) {
          if (pcats->num == category) {
            sprintf(label,"%s",pcats->labels[ii]);
	   /* now eliminate everything after the first "~" in the label */
	    for (pntr=label;*pntr;pntr++)
		if (*pntr == '\176') break;
	    *pntr = NULL;
	  }
        }

        /* GET INFORMATION FOR THE CURRENT PRIMARY AREA */
        if (V2_get_area (map,area,&poly) < 0 ||
            Vect_get_area_points (map,area, Tpoints) < 0)
        {
            fprintf (stderr, "\nNot able to read area %d\n",area);
            exit (-1);
        }

        nisle = poly->n_isles;

        /* ELIMINATE DUPLICATE MAPINFO POINTS */
        prune_points (Tpoints->x,Tpoints->y,&(Tpoints->n_points),tol);
        if (Tpoints->n_points < 4)   /* NO LONGER HAVE A POLYGON */
        {
           fprintf (stderr, "\nArea %d is too small to be moved into MAPINFO\n",area);
           continue;
        }
/*
fprintf (stderr, "Area %d\n",area);
fprintf (stderr, "number of islands %d\n",nisle);
fprintf (stderr, "category:%d  label <%s>\n", category,label);
fprintf (stderr, "continue ?"); gets(ans);
*/
            /* WRITE REGION TYPE HEADER */
        if (!nisle)    /* WITHOUT ISLANDS */    
           fprintf (mpmif,"Region 1\015\n %d\015\n",Tpoints->n_points);
	else           /* WITHISLANDS */    
           fprintf (mpmif,"Region %d\015\n %d\015\n",nisle+1,Tpoints->n_points);

            /* WRITE AREA COORD. RECORDS IN THE MIF */
        if (write_mpinfo_coordinates (mpmif, Tpoints->n_points,
                Tpoints->x, Tpoints->y, proj, 3) < 0)
            {
                fprintf (stderr, "\nNot able to write MAPINFO import file\n");
                exit (-1);
            }
        fflush(mpmif);
        
            /* WRITE AREA RECORD IN THE MID */
        fprintf(mpmid,"%d,\"%s\"\015\n",category,label);
        fflush(mpmid);

        /* PROCESS THE ISLANDS FOR THIS AREA */
        if (nisle > MAXISLE)
        {
           fprintf (stderr, "\nArea %d has more than the MAPINFO ",area);
           fprintf (stderr, "maximum of %d islands.\n",MAXISLE);
           exit (-1);
        }

        for (island = 1; island <= nisle; island++)
        {
/*
fprintf (stderr, "	ISLAND # = %d\n",island); 
*/
            /* EXTRACT ISLAND COORDINATES */
            isle = poly->isles[island-1];
            
            Tpoints = get_isle_xy (map,isle);

	    if (Tpoints == NULL)
            {
                fprintf (stderr, "\nNot able to read isle %d\n",isle);
                exit (-1);
            }

            /* ELIMINATE DUPLICATE MAPINFO POINTS */
            prune_points (Tpoints->x,Tpoints->y,&(Tpoints->n_points),tol);
            if (Tpoints->n_points < 4)   /* NO LONGER HAVE AN ISLAND */
	    {
               fprintf (stderr, "\nIsland %d of area %d is too small to be moved into MAPINFO\n",
                   island,area);
               continue;
            }
/*
fprintf (stderr, "after prune %d\n",island);
for (ii = 0; ii<Tpoints->n_points; ii++) 
fprintf (stderr, "%d  x: %10.5f  y: %10.5f\n",ii,Tpoints->x[ii], Tpoints->y[ii]);
fprintf (stderr, "\ncontinue?"); gets(ans);
*/
            fprintf (mpmif," %d\015\n",Tpoints->n_points);

            /* WRITE ISLAND COORD. RECORDS IN THE MIF */
            if (write_mpinfo_coordinates (mpmif, Tpoints->n_points,
                Tpoints->x, Tpoints->y, proj, 3) < 0)
            {
                fprintf (stderr, "\nNot able to write MAPINFO import file\n");
                exit (-1);
            }
            fflush(mpmif);
        
        }   /*END ISLAND LOOP */
        
	  /* ADD PEN TRAILER RECORD */
	fprintf (mpmif,"    Pen (1,2,0) \015\n");
        fflush(mpmif);

/******************
        +++ PROCESS THE ISLANDS AS AREAS +++
        for (island = 1; island <= nisle; island++)
        {

fprintf (stderr, "	ISLAND Area # = %d\n",island); 

            +++ EXTRACT ISLAND COORDINATES +++
            isle = poly->isles[island-1];
            
            +++ FIND THE ISLAND's CAT and LABEL, BY LOOKING
	       AT THE LINES THAT MAKE UP THE ISLAND +++
	    {
            register int jj, kk;
            int line, linea, larea, rarea;

            for(kk=0;kk<map->Isle[isle].n_lines;kk++) {
                linea = ABS (map->Isle[isle].lines[kk]);
                line = map->Isle[isle].lines[kk];
                larea = map->Line[linea].left;
                rarea = map->Line[linea].right;
                if (larea > 0) jj = map->Area[larea].att;
                if (rarea > 0) jj = map->Area[rarea].att;
                }
            category= map->Att[jj].cat;
	    if(category == 0) {
	      fprintf (stderr, " Island %d in Area %d has no category value, quitting\n",isle,area);
	      exit(-1);
	      }
            for(ii=0;ii < pcats->count; ii++) {
                if (pcats->list[ii].num == category)
                    sprintf(label,"%s",pcats->list[ii].label);
                }
            }
+++
fprintf (stderr, "island category= %d, label <%s>\n",category,label);
fprintf (stderr, "continue ?"); gets(ans);
+++

            Tpoints = get_isle_xy (map,isle);

	    if (Tpoints == NULL)
            {
                fprintf (stderr, "\nNot able to read isle %d\n",isle);
                exit (-1);
            }

            +++ ELIMINATE DUPLICATE MAPINFO POINTS +++
            prune_points (Tpoints->x,Tpoints->y,&(Tpoints->n_points),tol);
            if (Tpoints->n_points < 4)   +++ NO LONGER HAVE AN ISLAND +++
	    {
               fprintf (stderr, "\nIsland %d of area %d is too small to be moved into MAPINFO\n",
                   island,area);
               continue;
            }
+++
fprintf (stderr, "after prune %d\n",island);
for (ii = 0; ii<Tpoints->n_points; ii++) 
fprintf (stderr, "%d  x: %10.5f  y: %10.5f\n",ii,Tpoints->x[ii], Tpoints->y[ii]);
fprintf (stderr, "\ncontinue?"); gets(ans);
+++
+++                 WRITE REGION TYPE HEADER 
            fprintf (mpmif,"I-Region 1\015\n %d\015\n",Tpoints->n_points);
+++
            +++ WRITE ISLAND COORD. RECORDS IN THE MIF +++
            if (write_mpinfo_coordinates (mpmif, Tpoints->n_points,
                Tpoints->x, Tpoints->y, proj, 3) < 0)
            {
                fprintf (stderr, "\nNot able to write MAPINFO import file\n");
                exit (-1);
            }
            fflush(mpmif);
        
+++             WRITE ISLAND RECORD IN THE MID
            fprintf(mpmid,"A %d,\"%s\"\015\n",category,label);
            fflush(mpmid);
+++
        }   +++END ISLAND LOOP +++
**********************************************/

        count++;
    }   /* END AREA LOOP */

    Vect_destroy_line_struct (Tpoints);
    fprintf (stderr, "\n%d Areas Converted\n",count);
    return (0);
}
