/*
**  - Written by Dave Gerdes  6/89
**    US Army Construction Engineering Research Lab
**  - New parser installed, 12/90 David Stigberg
**  - corrected line 115 (and rewritten ../diglib/prune.c) 2/98 Michel Wurtz
**  - added v.support check line 233 Markus Neteler 7/98
*/
#include    <stdio.h>
#include <stdlib.h>
#include    <math.h>
#include    "gis.h"
#include    "Vect.h"
#include "dig_atts.h"


#define MAIN
#define  USAGE  "v.prune [-i] input=name output=name thresh=value\n"

double threshold;
int *pruned;
int inches;


/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *out_name = NULL ;

#ifdef OLDPARSE
struct Command_keys vars[] = {
	{ "dig_in", 1 },
	{ "dig_out", 2 },
	{ "threshold", 3 },
	{ "input", 1 },
	{ "output", 2 },
	{ "thresh", 3 },
	{ NULL,     0 }
};

#endif /*OLDPARSE*/

double dig_unit_conversion ();
int debugf(char *, ...);
int export(char *, char *, char *);
int doit(struct Map_info *, struct Map_info *);
int get_line_center(double *, double *, struct line_pnts *);
int get_pruned_area_points(struct Map_info *, int, struct line_pnts *);
int get_pruned_isle_points(struct Map_info *, int, struct line_pnts *);
static	int   snapped = 0 ;

int main (int argc, char **argv)
{
	char *mapset;
	char errmsg[100];

	struct Option *old, *new, *thresh;
	struct Flag *inch_flag;

	G_gisinit(argv[0]);

	inch_flag = G_define_flag();
	inch_flag->key			= 'i';
	inch_flag->description	= "set threshold value in inches";

	old = G_define_option();
	old->key 			= "input";
	old->type 			= TYPE_STRING;
	old->required 		= YES;
	old->multiple 		= NO;
	old->description 	= "vector file to be pruned";

	new = G_define_option();
	new->key 			= "output";
	new->type 			= TYPE_STRING;
	new->required 		= YES;
	new->multiple 		= NO;
	new->description 	= "new pruned vector file";

	thresh = G_define_option();
	thresh->key 		= "thresh";
	thresh->key_desc 	= "value";
	thresh->type 		= TYPE_DOUBLE;
	thresh->required 	= YES;
	thresh->multiple 	= NO;
	thresh->description = "threshold value";

	threshold = 0.0;

	if (G_parser (argc, argv))
		exit(-1);

	/*unnecessary, but done to preserve old names*/
	dig_name = G_store(old->answer) ;
	out_name = G_store(new->answer) ;
	threshold = atof (thresh->answer) ;


	/*  check args and set flags  */

	if (dig_name == NULL  ||   out_name == NULL)
	{
		fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
		G_usage();
		exit (-1);
	}
	if (!*dig_name  || !*out_name)
	{
		fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
		G_usage();
		exit (-1);
	}

/*	if (threshold == 0.0)  Changed 25.2.98*/
	if (threshold < 0.0)
	{
		fprintf (stderr, "%s: Command line error: missing or improper threshold value.\n\n", argv[0]);
		G_usage();
		exit (-1);
	}

	inches = inch_flag->answer;

	/*DEBUG*/
	/*
	fprintf (stderr, "in = '%s' out = '%s'\n", dig_name, out_name);
	fprintf (stderr, "thresh = %f  inches = %d\n", threshold, inches);
	exit(0);
	*/
#ifdef OLDPARSE
	ret = G_parse_command (argc, argv, vars, load_args) ;
	if (ret > 0)	/* Help was requested */
		exit (1);

	if (ret < 0  ||  dig_name == NULL  ||   out_name == NULL)
	{
		fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		    argv[0], USAGE);
		exit (-1);
	}

	if (threshold == 0.0)
	{
		fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		    argv[0], USAGE);
		exit (-1);
	}

#endif /*OLDPARSE*/

	/* Show advertising */
	fprintf (stdout,"\n\n   Prune:   threshold = %f\n\n", threshold) ;

	if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
	{
		sprintf (errmsg, "Could not find DIG file %s\n", dig_name);
		G_fatal_error (errmsg);
		/*
		G_fatal_error ("Could not find DIG file %s\n", dig_name);
		*/
	}
	export (dig_name, mapset, out_name);
	exit (0);
}

#ifdef OLDPARSE
static int load_args (int position, char *str)
{
	switch(position)
	{
	case 1:
		dig_name = G_store(str) ;
		break ;
	case 2:
		out_name = G_store(str) ;
		break ;
	case 3:
		threshold = atof (str) ;
		if (str[strlen(str)-1] == 'i')
			inches = 1;
		else 
			inches = 0;
		break ;
	default:
		break;
	}	/*  switch  */

	return (0);
}
#endif /*OLDPARSE*/

#ifdef DEBUG
int debugf (char *format, a)
{
	va_list a;
	va_start(format,a);
	fprintf (stderr, format, a);
	va_end(format,a);

	return 0;
}
#endif

FILE *Out;

struct Map_info Map;
/*ORIGINAL struct head Head; */
struct dig_head d_head;

int export (char *dig_name, char *mapset, char *out_name)
{
	FILE *In;
	double x,y;
	int cat;
	char type;
	long offset;
	char errmsg[200];
	struct Map_info InMap, OutMap;

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	if (0 > Vect_open_old (&InMap, dig_name, mapset))
	{
		sprintf(errmsg, "Not able to open vector file <%s>\n", dig_name) ;
		G_fatal_error (errmsg);
	}
        /* added v.support check 6.7.98 */
        if (2 > Vect_open_old (&InMap, dig_name, mapset))
	{
		sprintf(errmsg, "Must first run v.support on vector file <%s>\n", dig_name) ;
		G_fatal_error (errmsg);
	}

	if (0 > Vect_open_new (&OutMap, out_name))
	{
		sprintf(errmsg, "Not able to open vector file <%s>\n", out_name) ;
		G_fatal_error (errmsg);
	}

	Vect_copy_head_data (&InMap.head, &OutMap.head);

	/* fix up threshold value to correspond to digit */
	if (inches)
	{
		threshold = threshold * dig_unit_conversion() * InMap.head.orig_scale;
	}
	/* in either case, take this resultant map value and div by 2 */
	threshold = threshold / 2.;

	/* first process points attributes */
	if (NULL == (In = G_fopen_old ("dig_att", dig_name, mapset)))
	{
		fprintf (stderr, "Cannot find attribute file.\n");
		exit (1);
	}
	if(NULL == (Out = G_fopen_new ("dig_att", out_name)))
	{
		fprintf (stderr, "Cannot write into new attribute file.\n");
		exit (1);
	}


	while (read_att(In, &type, &x, &y, &cat, &offset)!=1)
	{
	   if (type == FILE_DOT) /* point attribute */
	        write_att (Out, FILE_DOT, x, y, cat);
        }

	doit (&InMap, &OutMap);

	Vect_close (&OutMap);
	Vect_close (&InMap);

	fclose (Out);
	fclose (In);

	fprintf (stderr, "\nDone.\n");


	return(0) ;
}

int doit (struct Map_info *InMap, struct Map_info *OutMap)
{
	struct line_pnts *Points, *AreaPoints, **IslesPoints;
	register int old_n_points, area, line, type;
	int att_ind, i;
	double att_x, att_y, x1;
	int point_found;
	int diff, max_n_isles=0;
	int left;

	/*Points.alloc_points = 0; */

	/* Must always use this to create an initialized  line_pnts structure */
	Points = Vect_new_line_struct ();
	AreaPoints = Vect_new_line_struct ();
	IslesPoints = NULL;

	/*DEBUG*/ fprintf (stderr, "Resultant threshold = %f\n", threshold);
	left = diff = 0;
	pruned = (int *) G_malloc(sizeof( int) * (InMap->n_lines + 1));
	/* flags for each line indicating if the line was pruned or not */
	for(line=1;line<= InMap->n_lines; line++)
	    pruned[line] = 0;
	for(line=1;line<= InMap->n_lines; line++)
	{
		if (line % 10 == 0)
			fprintf (stderr, "Pruning line %5d  pruned: %d  left: %d\r", line, diff, left);
		/*offset = ftell (in);*/

		if (0 > (type = V2_read_line (InMap, Points, line)))
		{
			if (type == -1)
			{
				fprintf (stderr, "Out of memory on line %d\n", line);
				return (-1);
			}
			else /* EOF */
			{
				Vect_destroy_line_struct (Points);
				break;
			}
		}
		old_n_points = Points->n_points;
		Points->n_points = dig_prune (Points, threshold);
		if(old_n_points > Points->n_points) pruned[line] = 1;
		diff += old_n_points - Points->n_points;
		att_ind = InMap->Line[line].att;

		if(Points->n_points != old_n_points)
		/* line has been pruned, need to reattach attribute */
		{
		   if(att_ind != 0)
		   {
		       get_line_center(&att_x, &att_y, Points);
		       write_att(Out, FILE_LINE, att_x, att_y, InMap->Att[att_ind].cat);
		   }
		}
		else
		{
		   if(att_ind != 0)
		       write_att(Out, FILE_LINE, 
			       InMap->Att[att_ind].x, InMap->Att[att_ind].y, 
			       InMap->Att[att_ind].cat);
		}
		left += Points->n_points;
		Vect_write_line (OutMap, type, Points);
	}

	/* now reattach the area attributes when needed */
	if (InMap->n_areas > 0)
	      fprintf(stdout, "\nReattaching area attributes...\n");
	for(area = 1; area<= InMap->n_areas; area++)
	{
	   fprintf(stderr, "Processing area %5d of: %5d\r", area, InMap->n_areas);
	   att_ind = InMap->Area[area].att;
           if(0>(AreaPoints->n_points=get_pruned_area_points (InMap, area, AreaPoints)))
	   { 
	      fprintf (stdout,"Warning ! Can't read area %d",area);
	      continue;
           }
	   if((x1=dig_point_in_poly(InMap->Att[att_ind].x, InMap->Att[att_ind].y, AreaPoints)) == 0.0)
	   /* the attribute is outside the area */
	   {
	      /* find some point inside the area */
	      /* allocate enought space for island points */
              if(InMap->Area[area].n_isles > max_n_isles)
	      {
	         IslesPoints = (struct line_pnts **)
		   G_realloc(IslesPoints, (1+InMap->Area[area].n_isles) * sizeof(struct line_pnts *));
                 for(i=max_n_isles; i< InMap->Area[area].n_isles; i++)
		    IslesPoints[i] = Vect_new_line_struct();
                 max_n_isles = InMap->Area[area].n_isles;
              }
	      for(i=0;i<InMap->Area[area].n_isles;i++)
	      {
	         IslesPoints[i]->alloc_points = 0; 
		 IslesPoints[i]->n_points = get_pruned_isle_points(InMap, InMap->Area[area].isles[i], IslesPoints[i]);
		 if(IslesPoints[i]->n_points==0) fprintf (stdout,"WARNING ");
              } /* isles loop */

              if(InMap->Area[area].n_isles)
	           point_found = Vect_get_point_in_poly_isl(
				     AreaPoints, IslesPoints,
			 	     InMap->Area[area].n_isles, &att_x, &att_y);
              else
	           point_found = Vect_get_point_in_poly_isl(AreaPoints, NULL, 0, 
					        &att_x, &att_y);
          		   
              /* end of looking for new point inside area */
	      if(point_found>=0)
	      {
		       write_att(Out, FILE_AREA, att_x, att_y, InMap->Att[att_ind].cat);
	      }
	      else  fprintf (stdout,"WARNING: the area is empty!\n");
	   } /* if not inside area */
	   else
	   {
	       if(x1<0.0) fprintf (stdout,"couldn't read the line!\n");
		   write_att(Out, FILE_AREA, 
		       InMap->Att[att_ind].x, InMap->Att[att_ind].y, 
		       InMap->Att[att_ind].cat);

           }

	} /* done processing areas */
	Vect_destroy_line_struct(AreaPoints);
	for (i=0;i<max_n_isles;i++)
	   Vect_destroy_line_struct(IslesPoints[i]);

	return 0;
}

/* 
** find a fast approximate point on a chain to place a label
**  uses a city block distance approximation to choose the point
**  In other words use distance x+y to approximate len of hypot
*/

/*
**  return found point in *x and *y
** return 0 on success ,-1 on error
*/

int get_line_center (
    double *x,double *y,
    struct line_pnts *Points)
{
    register int i;
    register int n_points;
    register double *ux, *uy;
    double dist;		/* running total of line length */
    double half_dist;		/* half total line length */
    double len;			/* tmp length of current line seg */
    double frac;		/* overshoot / line length */

    n_points = Points->n_points;
    ux = Points->x;
    uy = Points->y;

    if (n_points <= 0)
	return -1;
    if (n_points == 1)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }
	
    dist = 0.0;
    /* get total dist */
    for (i = 1 ; i < n_points ; i++)
	dist += (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
    if (dist == 0.0)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }

    half_dist = dist / 2.0;

    dist = 0.0;
    for (i = 1 ; i < n_points ; i++)
    {
	len = (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
	dist += len;
	if (dist >= half_dist)  /* we're there */
	{
	    frac = 1 - (dist - half_dist) / len;
	    *x = frac * (ux[i]-ux[i-1]) + ux[i-1];
	    *y = frac * (uy[i]-uy[i-1]) + uy[i-1];
	    return (0);
	}
    }

    fprintf (stderr, "Get_line_center failed.\n");
    *x = Points->x[0];
    *y = Points->y[0];
    return (-1);
}


/*
**  Written by:  Mike Higgins 5 1988
** 		 Dave Gerdes
**  US Army Construction Engineering Research Lab

**
**  Modified for Vectlib 3/1991  dpg
**  Added Vect_get_isle_points ()  5/1992 dpg
*/

/*
**  returns the polygon array of points  in BPoints
**   returns  number of points or -1 on error
*/

static int first_time = 1;	/* zero at startup */
static struct line_pnts TPoints;

int 
get_pruned_area_points (struct Map_info *Map, int area, struct line_pnts *BPoints)
{
	register int i, line;
	int start, end, to, from, inc;
	P_AREA *Area;
	int done_yet;


	BPoints->n_points = 0;
	Area =  &(Map->Area[area]) ;

	if (first_time == 1)
	{
		TPoints.alloc_points = 0;	/* executed only once */
		first_time = 0; 
	}


	for (i = 0 ; i < Area->n_lines ; i++)
	{
		line = abs(Area->lines[i]);

		if (0 > V2_read_line (Map, &TPoints, line))
			return (-1);
                if(pruned[line]) 
		     TPoints.n_points = dig_prune(&TPoints, threshold);

		if (0 > dig_alloc_points (BPoints, TPoints.n_points + BPoints->n_points + 1))
			return(-1) ;

		if (Area->lines[i] < 0)
		{
			start = TPoints.n_points - 1;
			inc = -1 ;
			end = 0;
		}
		else
		{
			end = TPoints.n_points - 1;
			inc = 1 ;
			start = 0;
		}

		done_yet = 0;
		for(from = start, to = BPoints->n_points ; !done_yet ; from+=inc, to++)
		{
			if (from == end)
				done_yet = 1;
			BPoints->x[to] = TPoints.x[from];
			BPoints->y[to] = TPoints.y[from];
		}
		BPoints->n_points = TPoints.n_points + BPoints->n_points ;

	}

	return (BPoints->n_points);
}

int 
get_pruned_isle_points (struct Map_info *Map, int isle, struct line_pnts *BPoints)
{
	register int i, line;
	int start, end, to, from, inc;
	P_ISLE *Isle;
	int done_yet;



	BPoints->n_points = 0;
	Isle =  &(Map->Isle[isle]) ;

	if (first_time == 1)
	{
		TPoints.alloc_points = 0;	/* executed only once */
		first_time = 0; 
	}


	for (i = 0 ; i < Isle->n_lines ; i++)
	{
		line = abs(Isle->lines[i]);

		if (0 > V2_read_line (Map, &TPoints, line))
			return (-1);
                if(pruned[line]) TPoints.n_points = dig_prune(&TPoints, threshold);

		if (0 > dig_alloc_points (BPoints, TPoints.n_points + BPoints->n_points + 1))
			return(-1) ;

		if (Isle->lines[i] < 0)
		{
			start = TPoints.n_points - 1;
			inc = -1 ;
			end = 0;
		}
		else
		{
			end = TPoints.n_points - 1;
			inc = 1 ;
			start = 0;
		}

		done_yet = 0;
		for(from = start, to = BPoints->n_points ; !done_yet ; from+=inc, to++)
		{
			if (from == end)
				done_yet = 1;
			BPoints->x[to] = TPoints.x[from];
			BPoints->y[to] = TPoints.y[from];
		}
		BPoints->n_points = TPoints.n_points + BPoints->n_points ;

	}

	return (BPoints->n_points);
}

