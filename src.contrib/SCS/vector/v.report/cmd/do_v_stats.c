/*  @(#)do_v_stats.c     1.0  04/02/91   */
/*  created by:         R.L.Glenn, SCS
 *
 * Program will read vector maps: areas, lines, or sites
 *         then will produce 'r.stats' type report
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "Vect.h"
#include "gis.h"
#include "global.h"

#define		DIG_DIR		"dig"

struct Map_info Map;
struct line_pnts *Points;
struct Categories cats;
struct Poly_List
{
	CELL num          ;   /* category number */
	double area       ;   /* area in square meters */
	int poly_cnt      ;   /* number of areas of this category number */
} *poly               ;

struct Line_List
{
	CELL num          ;   /* category number */
	double length     ;   /* length */
	int line_cnt      ;   /* number of lines of this category number */
} *vect               ;
static int cmp (const void *,const void *);
static int ABS (int);

int do_v_stats(int verbose, int lay_no, char *fd)

{
	int nArea, nLine, map_type, v_r;
	int cnt=0, line, linea, island;
	CELL cat_no;
	int first, tot_cats, n_points;
	register int area_num, line_num, i, ii, jj, kk, p;
	char *label_name, null[1];
	char aline[80];
	char map_name[100], *mapset;
	long len, nalloc=1024;
	double xcent, ycent, *X, *Y;
	double length;
	double f_area;
	double ot_area;
	double tot_area;
	P_AREA *Areas;
	P_LINE *Lines;
	FILE *dumpfile;

        G_begin_distance_calculations();
        G_begin_polygon_area_calculations();
	null[0] = '\0';
        sprintf(map_name,"%s",layers[lay_no].name);
	mapset = layers[lay_no].mapset;
	map_type = layers[lay_no].type;
	cats = layers[lay_no].labels;

	/* ---------------- Executable code --------------------------- */

	nArea = nLine = 0;
	tot_area = 0.0;
verbose=1;
	if(verbose)
          fprintf(stderr,"Loading vector information...\n");

/*fprintf(stderr,"cat.count = %d,  cat.num= %d\n",cats.count,cats.num);*/

	        /* Do initial read of map_name DIGIT file */
	if ((v_r = Vect_open_old(&Map,map_name,mapset)) < 0)
	{
		fprintf(stderr,"Reading map_name file.") ;
		sleep(2);
		return(-1) ;
	}
	if (v_r < 2)
	{
		fprintf(stderr,"v.support must be run on this map\n");
		sleep(2);
		return(-1) ;
	}

		  /* Initialize the Point struct */
        Points = Vect_new_line_struct();

	if (verbose)
	   {
  	   if (map_type == 1) fprintf(stderr,"\nProcessing area      ");
	   if (map_type == 2) fprintf(stderr,"\nProcessing line      ");
	   if (map_type == 3) fprintf(stderr,"\nProcessing site      "); 
	   }

	if (map_type == 1)
	{
		/* Allocate memory for Polygon List */
		poly = (struct Poly_List *) G_calloc ( nalloc , sizeof(struct Poly_List));

		first = 1;

                if (Map.n_areas <= 0)
	           {
	           fprintf (stderr, "\n\n NO data of that type for %s\n",
		                                           map_name);
		   sleep(2);
		   return(-1);
	           }

		/* Cycle through all areas */
		for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
		{
			if (verbose)
  			   fprintf(stderr,"\b\b\b\b\b%5d",area_num); 
			/* get the category number for area "area_num" */
			cat_no = Map.Att[Map.Area[area_num].att].cat;
/*fprintf(stderr,"\ncat#= %d\n",cat_no);*/
			label_name = G_get_cat(cat_no, &cats);

			/* Calculate polygon area */
			V2_get_area(&Map,area_num,&Areas);
			Vect_get_area_points(&Map, area_num, Points);
		        f_area = G_area_of_polygon(Points->x, 
					       Points->y, Points->n_points);
						      
			/* dig_find_area2(&Map,Areas,&f_area);*/

			  /* if this area contains islands remove the
			    island areas */
                        if (Map.Area[area_num].n_isles)
			   {
			   for (ii=0; ii < Map.Area[area_num].n_isles; ii++)
			     {
			     island = Map.Area[area_num].isles[ii];
			     Vect_get_isle_points(&Map, Map.Area[area_num].isles[ii], Points);
			     f_area = f_area - G_area_of_polygon(Points->x,
					      Points->y, Points->n_points);
			     }
			   }
			   tot_area = f_area;

			if (first)
			{
				poly[0].num = cat_no;
				poly[0].poly_cnt = 1;
				poly[0].area = tot_area;
				i = 0;
				cnt = 1;
				first = 0;
			}
			else
			{
				for (i=0; i<cnt; i++)
				{
					if (poly[i].num == cat_no)
					{
						poly[i].poly_cnt = poly[i].poly_cnt + 1;
						poly[i].area = poly[i].area + tot_area;
						break;
					}
				}
				if (i >= cnt)
				{
					cnt++;
					poly[i].num = cat_no;
					poly[i].poly_cnt = 1;
					poly[i].area = tot_area;
					qsort (poly, cnt, sizeof (struct Poly_List), cmp);
	                                if (cnt == nalloc)
	                                 {
	                                 nalloc = nalloc + 256;
	                                 len = (long) nalloc * sizeof(struct Line_List) ;
					/* make sure len doesn't overflow int */
	                                 if (len != (int) len) 
		                                 return -1;
	                                 poly = (struct Poly_List *) G_realloc(poly, (int)len);
	                                 }
				}
			}
			tot_area = 0.0;
		} /* end for cycle through Map.Areas */

		if (cnt == 0)
	           {
	           fprintf (stderr, "\n\n NO area type data for %s\n",
		                                           map_name);
		   sleep(2);
		   return(-1);
	           }
		fprintf(stderr,"\n");
		/************ REPORT FOR AREAS **********************************/
		/* write the stats to the temp file */

		dumpfile = fopen(fd,"w");
		if (!dumpfile)
		{
			perror (fd);
			sleep(2);
			return(-1);
		}

                fprintf(dumpfile,"area %ld\n",nalloc);

		for (i=0; i<cnt; i++)
		fprintf (dumpfile, "%5d:%4d:%14.2f\n",
				    poly[i].num,poly[i].poly_cnt,
				    poly[i].area);
		fclose(dumpfile);
		G_free(poly);

	}  /* end for areas */
	/************ END REPORT FOR AREAS ******************************/

	if (map_type > 1)
	{
		/* Allocate memory for Line/Site List */
		vect = (struct Line_List *) G_calloc ( nalloc , sizeof(struct Line_List));
		first = 1;
		/* Cycle through all lines */
		for (line_num = 1 ; line_num <= Map.n_lines ; line_num++)
		{
			Lines = &(Map.Line[line_num]);
			    /* skip area lines, deads */
			if (Lines->type != 4 && Lines->type != 1) continue;
			    /* skip site lines if this is a line report */
			if (Lines->type == 4 && map_type == 2) continue;
			    /* skip line lines if this is a site report */
			if (Lines->type == 1 && map_type == 3) continue;
			if (verbose)
  			  fprintf(stderr,"\b\b\b\b\b%5d",line_num);
			/* get the category number for line_num */
			cat_no = Map.Att[Lines->att].cat;
/*fprintf(stderr,"\ncat#= %d\n",cat_no);*/
			label_name = G_get_cat(cat_no, &cats);
/*fprintf(stderr,"\ncat#= %d, label= <%s>\n",cat_no,label_name);*/
			if (Lines->type == 1)
			{
			   if (0 > V1_read_line (&Map, Points,Lines->offset))
					{
					fprintf (stderr, "Out of Memory\n");
					sleep(2);
					return (-1);
					}
			   /* Calculate line length */
                           n_points = Points->n_points;
			   X = Points->x;
			   Y = Points->y;

			   length = 0.0;
			   for ( p = 0; p < Points->n_points - 1; p++ ) {
                               length += G_distance( Points->x[p], Points->y[p], Points->x[p+1], Points->y[p+1]); 
			   }
			}
			else 
			{
				length = 0.0;
			}

			if (first)
			{
				vect[0].num = cat_no;
				vect[0].line_cnt = 1;
				vect[0].length = length;
				i = 0;
				cnt = 1;
				first = 0;
			}
			else
			{
				for (i=0; i<cnt; i++)
				{
					if (vect[i].num == cat_no)
					{
					vect[i].line_cnt = vect[i].line_cnt + 1;
					vect[i].length = vect[i].length + length;
					break;
					}
				}
				if (i >= cnt)
				{
					cnt++;
					vect[i].num = cat_no;
					vect[i].line_cnt = 1;
					vect[i].length = length;
					qsort (vect, cnt, sizeof (struct Line_List), cmp);
	                                if (cnt == nalloc)
	                                 {
	                                 nalloc = nalloc + 256;
	                                 len = (long) nalloc * sizeof(struct Line_List) ;
					/* make sure len doesn't overflow int */
	                                 if (len != (int) len) 
		                                 return -1;
	                                 vect = (struct Line_List *) G_realloc(vect, (int)len);
	                                 }
				}
			}
		} /* end for cycle thru Map.n_lines */

		if (cnt == 0)
	           {
	           if (map_type == 2) 
		       fprintf (stderr, "\n\n NO line type data for %s\n",
		                                           map_name);
                   else
		       fprintf (stderr, "\n\n NO site type data for %s\n",
		                                           map_name);
		   sleep(2);
		   return(-1);
	           }
		fprintf(stderr,"\n");

		/************ REPORT FOR LINES OR SITES *************************/
		/* write the stats to the temp file */

		dumpfile = fopen(fd,"w");
		if (!dumpfile)
		{
			perror (fd);
			sleep(2);
			return(-1);
		}

                if (map_type == 2) fprintf(dumpfile,"line %d\n",cnt);
                else fprintf(dumpfile,"site %d\n",cnt);

		for (i=0; i<cnt; i++)
		    fprintf (dumpfile, "%5d:%4d:%14.2f\n",
			vect[i].num,vect[i].line_cnt,vect[i].length);

		fclose(dumpfile);
		G_free(vect);

	}  /* end for Line/Site */
	/************ END REPORT FOR LINES OR SITES *********************/

	Vect_close (&Map);
/*
#ifdef SYSV
	system("tput clear");
#else
	system("clear");
#endif
*/
	return(0);
}

int codes ( char ctype)
{
	switch (ctype) {
	case LINE:
		return ('L');
		break;
	case AREA:
		return ('A');
		break;
	case DOT:
		return ('P');
		break;
	case DEAD_LINE:
		return ('l');
		break;
	case DEAD_AREA:
		return ('a');
		break;
	case DEAD_DOT:
		return ('p');
		break;
	default:
		return ('X');
	}
}

static int cmp (const void *a,const void *b)
{
	if(a < b)
		return -1;
	if(a > b)
		return 1;
	return 0;
}

static int ABS (int x)
{
	if (x < 0) x = -x;
	return x;
}
