/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


#define MAIN
#define  USAGE  "ISMcont2dig contour=input dig_out=output\n"

long ftell ();
double atof ();
int inches;

/*
#define DEBUG
*/
/*  command line args */
static	char  *in_name = NULL ;
static	char  *out_name = NULL ;

double dig_unit_conversion ();
static	int   snapped = 0 ;



/*
** Structures for ISM Binary Contour file format
**  File is fortran variable length records. 
**  each record begins and ends with the length in bytes
*/
struct REC1 {
    float xgdmin;
    float xgdmax;
    float ygdmin;
    float ygdmax;
};

struct REC2 {
    float zlevel;
    int ipen;
    int n_points;

    float xy[5000];
};

struct Option *old, *new;

#define WORD_SIZE  4
#define ISM_RECORD_LEN 512*WORD_SIZE
#define NEW_LINE 3
#define CONT_LINE 2

char * getrecord ();


main (argc, argv)
    int argc;
    char **argv;
{
    G_gisinit(argv[0]);

/*  check args and set flags  */
	
/************************** Command Parser ************************************/
        old = G_define_option();
        old->key                        = "input";
        old->type                       =  TYPE_STRING;
        old->required           =  YES;
        old->multiple           =  NO;
        old->description        = "ISM Binary Contour file to import";

        new = G_define_option();
        new->key                        = "output";
        new->type                       =  TYPE_STRING;
        new->required           =  YES;
        new->multiple           =  NO;
        new->gisprompt          = "new,dig,vector";
        new->description        = "name of resulting vector file";

        if (G_parser (argc, argv))
                exit(-1);

    in_name = old->answer;
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

export(in_name, out_name)
    char *in_name, *out_name;
{
    FILE *In;
    FILE *Att;
    char buf[1024];
    struct line_pnts *Points;
    double x, y;
    int point_cnt, i;
    int first, done, tmp_att;

    struct REC1 *rec1p;
    struct REC2 *rec2p;


    Points = Vect_new_line_struct ();

    /* open output files */
    Att = G_fopen_new ("dig_att", out_name);

    if (0 > Vect_open_new (&Map, out_name))
        G_fatal_error ("Can't open vector file for write");

    /* open input contour file */
    if (NULL == (In = fopen (in_name, "r")))
    {
	fprintf (stderr, "Cannot open contour file '%s'.\n", in_name);
	exit (1);
    }

    /* read 1st line from contour file and fill header */
    rec1p =  (struct REC1 *) getrecord (In);

    {	/* make head struct */
	strcpy (Head.organization, "From ISM Contour Data");
	Head.date[0] = 0;
	Head.your_name[0] = 0;
	Head.map_name[0] = 0;
	Head.source_date[0] = 0;
	Head.orig_scale = 0;
	Head.line_3[0] = 0;
	Head.plani_zone = 0;
	Head.digit_thresh = 0;
	Head.map_thresh = 0;

	Head.W = rec1p->xgdmin;
	Head.E = rec1p->xgdmax;
	Head.S = rec1p->ygdmin;
	Head.N = rec1p->ygdmax;
    }

    Vect_copy_head_data (&Head, &(Map.head));


    /* Now process the data, creating dig and dig_att output */


    done = 0;			/* after eof */
    point_cnt = 0;		/* number of points in curr line */
    first = 1;			/* on first pass thru */

    while (!done)
    {
	rec2p =  (struct REC2 *) getrecord (In);

	if (!first)
	{
	    /* if (rec2p == NULL || rec2p->ipen == NEW_LINE) */
	    if (rec2p == NULL ? 1 : rec2p->ipen == NEW_LINE)
	    {
		int type = LINE;


#ifdef FOO
    /* turns out that the 1 point lines that contour gives tend to 
    **  exist on other lines, thus the label process gets screwd up
    **  so I am just blowing off 1 points lines
    */
		if (Points->n_points == 1)
		{
		    /* should already be alloced from below */
		    Points->n_points = 2;
		    Points->x[1] = Points->x[0];
		    Points->y[1] = Points->y[0];
		    type = DOT;
		}
		else
		    type = LINE;
#endif

		if (Points->n_points > 1)
		{
		    Vect_write_line (&Map, type, Points);
		    get_line_center (&x, &y, Points);

		    /* write attribute */
		    fprintf (Att, "L  %lf %lf %d\n", x, y, tmp_att);
		}
		
		point_cnt = 0;
		Points->n_points = 0;
	    }
	    else
		if (rec2p->ipen != CONT_LINE)
		    fprintf (stderr, "Unknown IPEN code: %d\n", rec2p->ipen);

	    if (rec2p == NULL)
	    {
		done = 1;
		continue;
	    }
	}
	else first = 0;

	if (0 > dig_alloc_points (Points, rec2p->n_points+point_cnt))
	{
	    fprintf (stderr,"Points: Out of memory. requested %d\n",rec2p->n_points+point_cnt);
	    return (-1);
	}

	tmp_att = rec2p->zlevel + .5;
	for (i = 0 ; i < rec2p->n_points ; i++)
	{
	    Points->x[point_cnt+i] = rec2p->xy[i*2];
	    Points->y[point_cnt+i] = rec2p->xy[i*2+1];
	}
	Points->n_points += rec2p->n_points;
	point_cnt += rec2p->n_points;
    }

    fclose (In);
    fclose (Att);

    Vect_close (&Map);

    fprintf (stdout, "Cont2dig complete.   You must now run v.support (option 1)\n");
    fprintf (stdout, " to build the dig_plus file.\n\n\n");


    return(0) ;
}

char *
getrecord (fp)
    FILE *fp;
{
    static char buf[ISM_RECORD_LEN];
    int head, tail;
    int ret;

    /* read in number of words to follow */
    if (0 >= fread (&head, WORD_SIZE, 1, fp))
	return NULL;

    if (0 >= fread (buf, 1, head, fp))
	return NULL;
    fread (&tail, WORD_SIZE, 1, fp);	/* read in end of record */

/*DEBUG*/      if (tail != head)
/*DEBUG*/  	fprintf (stderr, "Out of sync: %d %d\n", head, tail);

    return buf;
}
