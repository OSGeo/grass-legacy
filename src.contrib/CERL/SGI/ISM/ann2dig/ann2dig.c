/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    <ctype.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


#define MAIN
#define  USAGE  "ISMann2dig input=annotation output=vector_file\n"

long ftell ();
double atof ();
int inches;

char **Labels;
int N_Labels;
int Alloc_Labels;
#define LABEL_MALLOC 3000

/*
#define DEBUG
*/
/*  command line args */
static	char  *in_name = NULL ;
static	char  *out_name = NULL ;


double dig_unit_conversion ();
static	int   snapped = 0 ;


struct data {
    char color;
    char style;
    char thick;
    int  label;
};

struct Option *old, *new;

main (argc, argv)
    int argc;
    char **argv;
{
    int   ret ;

    G_gisinit (argv[0]);

/*  check args and set flags  */
	
/************************** Command Parser ************************************/
        old = G_define_option();
        old->key                = "input";
        old->type               = TYPE_STRING;
        old->required           = YES;
        old->multiple           = NO;
        old->description        = "ISM Annotation input file";

        new = G_define_option();
        new->key                = "output";
        new->type               = TYPE_STRING;
        new->required           = YES;
        new->multiple           = NO;
        new->gisprompt          = "new,dig,vector";
        new->description        = "name of resulting vector file";

        if (G_parser (argc, argv))
                exit(-1);

    in_name  = old->answer;
    out_name = new->answer;

    if (in_name == NULL  ||   out_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

    export (in_name, out_name); 

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


struct Map_info Map;
struct dig_head Head;

static char read_buf[BUFSIZ];
static int  line_ready = 0;

export(in_name, out_name)
    char *in_name, *out_name;
{
    FILE *Out;
    FILE *In;
    FILE *Att;
    char *buf;
    char label[BUFSIZ];
    struct line_pnts *Points;
    double x, y;
    int point_cnt, i;
    int done, tmp_att;
    int obj_type;
    int ret;
    struct Categories Cats;
    static int first2 = 1;


    Points = Vect_new_line_struct ();

    /* open output files */
    Att = G_fopen_new ("dig_att", out_name);
    /*
    Out = G_fopen_new ("dig", out_name);
    */
    if (0 > Vect_open_new (&Map, out_name))
	G_fatal_error ("Can't open vector file for write");

    /* open input contour file */
    if (NULL == (In = fopen (in_name, "r")))
    {
	fprintf (stderr, "Cannot open contour file '%s'.\n", in_name);
	exit (1);
    }

    {	/* make head struct */
	strcpy (Head.organization, "From ISM Annotation Data");
	Head.date[0] = 0;
	Head.your_name[0] = 0;
	Head.map_name[0] = 0;
	Head.source_date[0] = 0;
	Head.orig_scale = 0;
	Head.line_3[0] = 0;
	Head.plani_zone = 0;
	Head.digit_thresh = 0;
	Head.map_thresh = 0;

	Head.W = 0;
	Head.E = 0;
	Head.S = 0;
	Head.N = 0;
    }

    Vect_copy_head_data (&Head, &(Map.head));


    /* init labels */
    Labels = (char **) G_malloc (LABEL_MALLOC * sizeof (char *));
    Alloc_Labels = LABEL_MALLOC; 
    N_Labels = 0;


    /* Now process the data, creating dig and dig_att output */

    done = 0;			/* after eof */

    while (!done)
    {
	static int cnt = 0;

	cnt++;
	ret = read_next_line (In, &buf);
	if (ret < 0)  /* EOF */
	{
	    done = 1;
	    break;
	}

	if (ret == 0)	/* something else */
	{
/*DEBUG*/  fprintf (stderr, "Unexpected text: %d '%s'\n", cnt, buf);
	    continue;
	}

	/* else have something of interest */

	if (!strncmp      ("SRFLNE", buf, 6))
	{
	    obj_type = LINE;
	}
	else if (!strncmp ("SRFPLY", buf, 6))
	    obj_type = AREA;
	else if (!strncmp ("SRFSYM", buf, 6))
	    obj_type = DOT;
	else
	    obj_type = 0;
	
	if (!obj_type)
	    continue;


	/* read in poly[line/gon] */

	Points->n_points = 0;
	point_cnt = 0;		/* number of points in curr line */
	first2 = 1;	/* restart for each line */
	while (1)
	{
	    static int first = 1;

	    ret = read_next_coord (In, &x, &y, label);
	    if (ret < 0) /* EOF */
	    {
		done = 1;
		break;
	    }
	    if (ret == 0)  /* something else (end of coords) */
	    {
		break;
	    }

	    if (first2)
	    {
		first2 = 0;
		if (*label)
		{
		    if (!(tmp_att = label_exists (label)))
		    {
			tmp_att = N_Labels+1;
			if (N_Labels >= Alloc_Labels-1)
			    Labels = (char **)G_realloc (Labels, 
			        (Alloc_Labels *= 2) * sizeof (char *));
			Labels[++N_Labels] = G_store (label);
		    }
		}
		else
		    tmp_att = 0;
	    }


	    if (first)
	    {
		first = 0;
		Head.N = y;
		Head.S = y;
		Head.E = x;
		Head.W = x;
	    }
	    else
	    {
		if (y > Head.N) Head.N = y;
		if (y < Head.S) Head.S = y;

		if (x > Head.E) Head.E = x;
		if (x < Head.W) Head.W = x;
	    }
	    if (0 > dig_alloc_points (Points, Points->n_points+1))
	    {
		fprintf (stderr,"Points: Out of memory. requested %d\n",Points->n_points+1);
		return (-1);
	    }

	    Points->x[point_cnt] = x;
	    Points->y[point_cnt++] = y;
	    Points->n_points++;
	}
	if (obj_type && obj_type != DOT)
	{
	    Vect_write_line (&Map, obj_type, Points);
	    if (tmp_att)
	    {
		get_line_center (&x, &y, Points);
		fprintf (Att, "L  %lf %lf %d\n", x, y, tmp_att);
	    }
	    point_cnt = 0;
	}
	else {
/*DEBUG*/  printf ("Warning: DOT line  %d points  Not currently implemented.\n", Points->n_points);
	}  /* TODO */
    }


    Vect_close (&Map);
    fclose (In);
    fclose (Att);

    /* 
    **  Write out Category strings 
    */
    G_init_cats (N_Labels, "", &Cats);
    for (i = 1 ; i <= N_Labels ; i++)
	if (0 > G_set_cat (i, Labels[i], &Cats))
	    break;
    G_write_vector_cats (out_name, &Cats);
    G_free_cats (&Cats);

    fprintf (stderr, "Done.\n");

    return(0) ;
}



/*
** read next coordinate pair and optional id
**   
**  returns -1 on EOF
**   	     0 on found something else
**           1 on successfull read of coordinates
**
**   if there is no id on the line, it will place a 0 in id[0];
**
**   if it finds something else, then it leaves that line in the buffer
**   and sets the line_ready flag, so read_next_line () can get it.
*/
read_next_coord (fp, x, y, id)
    FILE *fp;
    double *x, *y;
    char *id;
{
    register int i, ret;

    if (!line_ready)
    {
	if (NULL == fgets (read_buf, BUFSIZ, fp))
	    return (-1);
	G_squeeze (read_buf);
    }

    if (!isdigit (*read_buf))
    {
	line_ready = 1;
	return (0);
    }

    ret = sscanf (read_buf, " %lf %lf %[^{]", x, y, id);
    if (ret < 2)
	return (0);

    if (ret == 2)
	*id = 0;

    /* remove spaces and quotes */
    G_squeeze (id);
    for (i = 0 ; i < strlen (id) ; i++)
	if (id[i] == '"')
	    id[i] = ' ';
    G_squeeze (id);

    line_ready = 0;
    return (1);
}

/*
** read line expecting textual ISM ann data
**
**  returns -1 on EOF
**   	     0 on found something else
**           1 on successfull read of coordinates
*/
read_next_line (fp, str)
    FILE *fp;
    char **str;
{
    static int cnt = 0;

    if (cnt > 10)
	return (-1);

    if (!line_ready)
    {
	if (NULL == fgets (read_buf, BUFSIZ, fp))	/* EOF */
	    return (-1);
	G_squeeze (read_buf);
    }

    if (!isalpha (*read_buf))
    {
	line_ready = 1;
	cnt++;
	return (0);
    }

    cnt = 0;

    line_ready = 0;
    *str = read_buf;
    return (1);
}

label_exists (label)
    char *label;
{
    register int i;

    for (i = 1 ; i <= N_Labels ; i++)
	if (!strcmp (label, Labels[i]))
	    return (i);
    return (0);
}
