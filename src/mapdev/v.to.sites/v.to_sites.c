/*
**  v.make_sites.c:  source for command line version
**
**  Written by Dave Gerdes  6/89
**
**  GRASS4.0 - converted for new parser - DKS 12/90
**
**  US Army Construction Engineering Research Lab
**
*/
#include    <stdio.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"
#include    "Vect.h"
#include    <math.h> 

#define MAIN
#define  USAGE  "v.to.sites [-a][-c][-i] input=dig file input output=site file output [dmax=value]\n"

/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *site_name = NULL ;


double dig_unit_conversion ();
static	int   snapped = 0 ;

main(argc, argv)
    int argc;
    char **argv;
{
    int   ret ;
    char *mapset;
    char errmsg[100];
    char dmaxchar[100];
    struct Map_info Map;
    struct Categories cats;
    double dist;
    FILE *out;

    struct Cell_head cellhd;
    struct Option *old, *new, *dmax;
    struct Flag   *cat, *Cat, *all, *interp;

    G_gisinit(argv[0]);
/* get default for min distance */
    if (G_get_window (&cellhd) == -1) exit (0); 
    if  (cellhd.ew_res < cellhd.ns_res) dist = cellhd.ew_res; 
    else dist = cellhd.ns_res; 
    sprintf (dmaxchar,"%lf",dist);

/*  check args and set flags  */
	
    old = G_define_option();
    old->key	= "input";	
    old->type	= TYPE_STRING;	
    old->required	= YES;	
    old->multiple	= NO;	
    old->gisprompt	= "old,dig,vector";
    old->description	= "vector file to be converted to sites";	

    new = G_define_option();
    new->key	= "output";	
    new->type	= TYPE_STRING;	
    new->required	= YES;	
    new->multiple	= NO;	
    new->gisprompt	= "new,site_lists,sites";
    new->description	= "sites file to be made from vector file";	

    all = G_define_flag ();
    all->key		= 'a';
    all->description	= "Output all verticies to site file";

    cat = G_define_flag ();
    cat->key		= 'c';
    cat->description	= "Use Category NUMERIC data instead of attribute";

    Cat = G_define_flag ();
    Cat->key		= 'C';
    Cat->description	= "Use Category TEXT data instead of attribute";

    interp = G_define_flag ();
    interp->key		= 'i';
    interp->description	= "Interpolate points along lies (for -a only)";

    dmax = G_define_option();
    dmax->key	= "dmax";	
    dmax->type	= TYPE_DOUBLE;	
    dmax->required	= NO;	
    dmax->answer	= dmaxchar;	
    dmax->description	= "Maximum distance between points (for -a -i only) ";	
 


    if (G_parser (argc, argv))
	exit(-1);

    dig_name = old->answer;
    site_name = new->answer;

    sscanf (dmax->answer, "%lf", &dist);

    if (dig_name == NULL  ||   site_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }

    if (!*dig_name  ||  !*site_name )
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }


    /* Show advertising */ 
    printf("\n\n   %s:\n\n", G_program_name()) ;
    if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
    {
	sprintf (errmsg, "Could not find vector file %s\n", dig_name);
	G_fatal_error (errmsg);
    } 

    if (cat->answer || Cat->answer)
    {
      if (G_read_vector_cats (dig_name, mapset, &cats) < 0) {
	sprintf (errmsg, "Could not find category file for %s\n", dig_name);
	G_fatal_error (errmsg);
      }
    }

    if (all->answer) {
      if ( (Vect_open_old (&Map, old->answer, mapset) ) < 2)
      {
	sprintf(errmsg, "Could not open vector file <%s>\n", dig_name);
	G_fatal_error (errmsg);
      }
      out = G_fopen_sites_new (site_name);
      bin_to_asc (&Map, out, (cat->answer || Cat->answer) ? &cats : NULL, interp->answer ? dist : 0., (cat->answer != NULL));
      Vect_close (&Map);
    }
    else {
      export (dig_name, mapset, site_name,  (cat->answer || Cat->answer) ? &cats : NULL, (cat->answer != NULL)); 
    }
    exit (0);
}

#ifdef DEBUG
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif



export(dig_name, mapset, site_name, cats, num)
    char *dig_name, *mapset, *site_name;
    struct Categories *cats;
    int num;
{
    FILE *out;
    int level;
    struct Map_info Map;

    if ( ! mapset)
    {
	G_fatal_error ("No mapset specified.\n");
    }

    /*
    dig_P_init (dig_name, mapset, &Map);
    */
    level = Vect_open_old (&Map, dig_name, mapset);
    if (level < 0)
	G_fatal_error ("Could not open vector file");
	
    if (level < 2)
	G_fatal_error ("Could not open file at Level 2 access, run v.support");

    out = G_fopen_sites_new (site_name);

    doit (&Map, out, cats, num);

    fclose (out);

    Vect_close (&Map);

    return(0) ;
}

doit (map, out, cats, num)
    struct Map_info *map;
    FILE *out;
    struct Categories *cats;
    int num;
{
    P_LINE *Line;
    P_ATT *Att;
    struct line_pnts *Points;
    char desc[128];
    register int line, ret;
    int hits = 0;
    int labels = 0;
    int binary;
    char *p, *descp;
    
    /* make a quick pass through checking for labelled sites */
    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Line =  &(map->Line[line]);
	if (Line->type != DOT)
	    continue;
	hits++;
	if (Line->att)
	    labels++;
    }
    if (hits)
    {
	binary = 0;
	if (!labels)
	{
	    binary = 1;
	    printf ( "Creating a BINARY (0/1) site file\n");
	}
	else
	{
	    printf ( "Creating site file with category information\n");
	    if (labels < hits)
	    {
		printf ("Note: %d sites were not labeled\n", hits - labels);
	    }
	}
    }
    else
    { printf ( "No SITES found in vector file\n");
	return (0);
    }

    Points = Vect_new_line_struct ();

    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Line =  &(map->Line[line]);
	if (Line->type != DOT)
	    continue;


	if (Line->att > 0)
	{
	    Att = &(map->Att[Line->att]);

	    if (binary) 
		descp = "";
	    else
	    {
		if (cats)
		{
		    p = G_get_cat (Att->cat, cats);
		    if (num)
		    {
		      int att;
		      att = atoi (p);
		      sprintf(desc,"#%d",att);
		      descp = desc;
		    } else descp = p;
		} else 
		{
		    sprintf (desc, "#%d", Att->cat);
		    descp = desc;
		}
	    }

	    G_put_site (out, Att->x, Att->y, descp);
	}
	else
	{
	    if (0 > (ret = Vect__Read_line (map, Points, Line->offset)))
	    {
		if (ret == -1)
		{
		    fprintf (stderr, "Out of memory on line %d\n", line);
		    return (-1);
		}
		else 	/* EOF */
		{
		    fprintf (stderr, "Premature EOF. Dig_Plus file probably bad\n");
		    return (0);
		}
	    }
	    /* mark it as label 0 (unlabelled) */
	    if (binary || !num)
		G_put_site (out, Points->x[0], Points->y[0], "");
	    else
		G_put_site (out, Points->x[0], Points->y[0], "#0");
	}
    }

    Vect_destroy_line_struct (Points);
    
    printf ( "\nFound %d sites\n", hits);

    return 0;
}

bin_to_asc(Map,ascii,cats,dmax,num)
    FILE *ascii;
    struct Map_info *Map;
    struct Categories *cats;
    double dmax;
    int num;
{
    int type;
    register int i, tmp;
    double *xptr, *yptr,xprev,yprev,x,y,d,xt,yt;
    static struct line_pnts *Points;
    char buf1[100], buf2[100];
    char *p;
    int att,n_points,times,j,k,ind1,ind2;
    int prev = 0;
    int isnode =0;
    char desc[128], *descp;

    Points = Vect_new_line_struct ();	/* init line_pnts struct */
    Vect_set_constraint_type (Map, LINE|DOT);


    for (i = 1 ; i <= Map->n_lines ; i++)
    {
	if (Map->Line[i].att > 0)
	{
	    if (0 > V2_read_line (Map, Points, i))
		G_fatal_error ("Read error");

	   xptr = Points->x;
	   yptr = Points->y;
           prev = 0;
           n_points = Points->n_points;

	   while (n_points--)
	    {
                if ((n_points == Points->n_points-1)||(n_points==0))
                  isnode = 1;
                else isnode = 0;
		att = Map->Att[Map->Line[i].att].cat;
		sprintf(desc,"#%d",att);
		descp = desc;

		if (cats)
		{
		    p = G_get_cat (att, cats);
		    if (num)
		    {
		      att = atoi (p);
                      sprintf(desc,"#%d",att);
		      descp = desc;
		    } else descp = p;
		}

                if (prev == 0) {
                    xprev = *xptr;
                    yprev = *yptr;
                    prev = 1;
                    if (!isnode) {
		      G_put_site (ascii, *xptr++, *yptr++, descp);
                    }
                }
                else {
                  /* compare the distance between current and previous */
                  x = *xptr;
                  y = *yptr;
                  xt = fabs(x - xprev);
                  yt = fabs(y - yprev);
                  d = sqrt(xt*xt + yt*yt);
                  if ((d > dmax) && (dmax != 0.)) {
                    times = (int)(d/dmax + 0.5);
                    for (j = 0; j < times; j++) {
                      xt = x - j*((x - xprev)/times);
                      yt = y - j*((y - yprev)/times);
                      if ((!isnode)||(j!=0)) {
		        G_put_site (ascii, xt, yt, descp);
                      }
                      isnode = 0;

                    }
                    xptr++;
                    yptr++;
                  }
                  else {
                    if (!isnode) {
		      G_put_site (ascii, *xptr++, *yptr++, descp);
                    }
                  }
                  xprev = x; 
                  yprev = y;
                }

	    }
	}
    }

fprintf(stderr,"\nNumber of nodes = %d\n",Map->n_nodes);
    for (k = 1; k <= Map->n_nodes; k++) {
      x = Map->Node[k].x; 
      y = Map->Node[k].y; 

      if(Map->Node[k].lines != NULL) {
        ind1 = abs(Map->Node[k].lines[0]);
        ind2 = Map->Line[ind1].att;
        att = Map->Att[ind2].cat;

	sprintf(desc,"#%d",att);
	descp = desc;

        if (cats)
        {
           p = G_get_cat (att, cats);
           att = atoi (p);
	   if (num)
	     sprintf(desc,"#%d",att);
	   else
	     descp = p;
        }

        G_put_site (ascii, x, y, descp);
      }
    }

done:
    fclose(ascii);
    return (0);
}
