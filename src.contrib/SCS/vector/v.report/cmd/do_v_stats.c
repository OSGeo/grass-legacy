/*  @(#)do_v_stats.c     1.0  04/02/91   */
/*  created by:         R.L.Glenn, SCS
 *
 * Program will read vector maps: areas, lines, or sites
 *         then will produce 'r.stats' type report
 */

#include <stdio.h>
#include "global.h"
#include "Vect.h"

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

do_v_stats(verbose, lay_no, fd)
int lay_no;
char *fd;

{
	int nArea, nLine, map_type, v_r;
	int cnt=0, cat_no, line, linea, island;
	int first, tot_cats, no_cats=0, n_points, cmp();
	register int area_num, line_num, i, ii, jj, kk;
	char *calloc(), *label_name, null[1];
	char aline[80], blank_name[5], unknown_name[20], err_name[20];
	char map_name[100], *mapset;
	long len, nalloc=1024;
	double xcent, ycent, *X, *Y;
	double perim;
	double f_area;
	double tot_area;
	P_AREA *Areas;
	P_LINE *Lines;
	P_NODE *Nodes;
	P_ATT *atts;
	FILE *dumpfile;
	extern double perimeter();

	null[0] = '\0';
        sprintf(map_name,"%s\0",layers[lay_no].name);
	mapset = layers[lay_no].mapset;
	map_type = layers[lay_no].type;
	cats = layers[lay_no].labels;

	/* ---------------- Executable code --------------------------- */

	nArea = nLine = 0;
	tot_area = 0.0;
	strcpy(blank_name,"  ");
	strcpy(unknown_name,"label unknown ");
	strcpy(err_name, "cat file error");
verbose=1;
	if(verbose)
          fprintf(stderr,"\nLoading vector information.\n\n");

/*fprintf(stderr,"cat.count = %d,  cat.num= %d\n",cats.count,cats.num);*/
        if (cats.num == 0) no_cats = 1;

	/* Check cats for errors */
	if (!no_cats)
	{  /* count should always be one more than the num of categories */
		if(cats.count != cats.num+1)
		{
			fprintf(stderr,"\007");
			fprintf(stderr,"\nERROR: Error in category support file\n");
			fprintf(stderr,"\t Expected %d category codes read in only %d\n",cats.num,cats.count-1);
			sleep(2);
			return(-1);
		}
/*              for (cat_no=0;cat_no<=cats.num;cat_no++)
		{ ** a null value means less categories than count **
			if(strcmp(cats.list[cat_no].label,null) == 0)
			{
				fprintf(stderr,"\007");
				fprintf(stderr,"\nERROR: Error in category support file\n");
				fprintf(stderr,"\t Missing category code(s)\n");
				sleep(2);
				return(-1);
			}
		}
*/
	}

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
			         /* check if category is in valid range */
			if (cat_no > cats.num || cat_no < 0)
				label_name = unknown_name;
			         /* if cat file exists */
			else if (no_cats) label_name = blank_name;
			         /* if cat number NOT in cats file */
			else if(cats.list[cat_no].label == NULL)
				label_name = unknown_name;
				/* if cat number in cats file, but is empty */
			else if(strlen(cats.list[cat_no].label) == 0)
				label_name = blank_name;
				/* get the category label for "cat_no" */
			else label_name = cats.list[cat_no].label;

			/* Calculate polygon area */
			V2_get_area(&Map,area_num,&Areas);
			dig_find_area2(&Map,Areas,&f_area);
			tot_area = f_area;

			  /* if this area contains islands remove the
			    island areas */
                        if (Map.Area[area_num].n_isles)
			   {
			   for (ii=0; ii < Map.Area[area_num].n_isles; ii++)
			     {
			     island = Map.Area[area_num].isles[ii];
			     isle_area(&Map,island,&f_area);
			     if (f_area > 0) tot_area = tot_area - f_area;
			     else tot_area = tot_area + f_area;
			     }
			   }

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

                fprintf(dumpfile,"area\n");

		for (i=0; i<cnt; i++)
		fprintf (dumpfile, "%5d:%4d:%14.2lf%\n",
				    poly[i].num,poly[i].poly_cnt,
				    poly[i].area);
		fclose(dumpfile);
		free(poly);

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
			/* check if category is in valid range */
			if (cat_no > cats.num || cat_no < 0)
				label_name = unknown_name;
			/* if cat file exists */
			else if (no_cats) label_name = blank_name;
			/* if cat number NOT in cats file */
			else if(strcmp(cats.list[cat_no].label,null) == 0)
				label_name = unknown_name;
				/* if cat number in cats file, but is empty */
			else if(strlen(cats.list[cat_no].label) == 0)
				label_name = blank_name;
				/* get the category label for "cat_no" */
			else label_name = cats.list[cat_no].label;
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

			   perim = perimeter(n_points,X,Y);
			}
			else 
			{
				perim = 0.0;
			}

			if (first)
			{
				vect[0].num = cat_no;
				vect[0].line_cnt = 1;
				vect[0].length = perim;
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
					vect[i].length = vect[i].length + perim;
					break;
					}
				}
				if (i >= cnt)
				{
					cnt++;
					vect[i].num = cat_no;
					vect[i].line_cnt = 1;
					vect[i].length = perim;
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
		    fprintf (dumpfile, "%5d:%4d:%14.2lf\n",
			vect[i].num,vect[i].line_cnt,vect[i].length);

		fclose(dumpfile);
		free(vect);

	}  /* end for Line/Site */
	/************ END REPORT FOR LINES OR SITES *********************/

	Vect_close (&Map);
#ifdef SYSV
	system("tput clear");
#else
	system("clear");
#endif
	return(0);
}

codes (ctype)
char ctype;
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

static
cmp (a, b)
struct Cat_List *a, *b;
{
	if(a->num < b->num)
		return -1;
	if(a->num > b->num)
		return 1;
	return 0;
}

isle_area (map, isle, totalarea)
    struct Map_info *map;
    int  isle;
    double *totalarea;
{
    int cur_line;
    int ab_line;
    int i;
    double *x, *y;
    static struct line_pnts points;
    static int first_time;	/* 0 on startup */
    double tot_area, sum_area;


    *totalarea = 0.;

    if (! map->Isle[isle].n_lines)
	return(-1);

    tot_area = 0.0;

    if (first_time == 0)
    {
	points.alloc_points = 0;
	first_time = -1;
    }

    tot_area = 0.0;
    for (cur_line = 0; cur_line < map->Isle[isle].n_lines ; cur_line++)
    {
	ab_line = ABS(map->Isle[isle].lines[cur_line]);

	V2_read_line (map, &points, map->Line[ab_line].offset);
	if (points.n_points < 2)
	    continue;
	
	x= points.x;
	y= points.y;

	sum_area = 0.0;
	for (i=1; i < points.n_points; i++) 
	{
	    sum_area += (x[i]-x[i-1]) * (y[i] + y[i-1]);
	}
	if (map->Isle[isle].lines[cur_line] > 0)
	    tot_area += sum_area;
	else
	    tot_area -= sum_area;
    }
    *totalarea =  0.5 * tot_area;
    
    return(0);
}
