/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include <gl.h>
#include    <stdio.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


#define MAIN
#define  USAGE  "Vexport.ism dig=input ism=output user=\n"

/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *ism_name = NULL ;
int User_Entry = 1;


struct Categories Cats;

static  int  load_args() ;

struct Command_keys vars[] = {
 { "dig", 1 },
 { "ism", 2 },
 { "user", 3 },
 { "input", 1 },
 { "output", 2 },
 { "in", 1 },
 { "out", 2 },
 { NULL,     0 }
};

char *prompt_for_file ();
double dig_unit_conversion ();
static	int   snapped = 0 ;

struct line_pnts Points;

main(argc, argv)
    int argc;
    char **argv;
{
    int   ret ;
    char *mapset;
    char msgbuf[50];
    char namebuf1[100];
    char namebuf2[100];

/*  check args and set flags  */
	
    Points.alloc_points = 0;

    if (argc != 1) /* override parse_command rules */
    {
	ret = G_parse_command (argc, argv, vars, load_args) ;
	if (ret > 0)	/* Help was requested */
	     exit (1);

	/*if (ret < 0  ||  dig_name == NULL  ||   ism_name == NULL)*/
	if (ret < 0)
	{
	    fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		    argv[0], USAGE);
	    exit (-1);
	}
    }

/* Show advertising */
    G_gisinit(argv[0]);
    printf("\n\n   Export.ism:\n\n") ;


    init_graphics ();

    *msgbuf = 0;
#ifdef GRAPH_FILE_INPUT
    while (dig_name == NULL  ||   ism_name == NULL)
    {
	panel_enter_filenames (&dig_name, &ism_name, msgbuf);
	if (G_find_file ("dig", dig_name, "") == NULL)
	{
	    sprintf (msgbuf, "file '%s' not found", dig_name);
	    free (dig_name);
	    dig_name = NULL;
	}
    }
    if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
	G_fatal_error ("Can't find input file\n");
#else



    if (!(mapset = G_ask_old (" Existing Digit file", namebuf1, "dig", "dig")))
	exit (1);
    dig_name = namebuf1;

    if (!(ism_name = prompt_for_file ("Name of new ISM Annotation file")))
	exit (1);

    /*
    if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
	G_fatal_error ("Can't find input file\n");
    */


#endif

    
    export (dig_name, mapset, ism_name); 
    exit (0);
}

static
load_args (position, str)
    int position;
    char *str;
{
    switch(position)
    {
	case 1:
		dig_name = G_store(str) ;
		break ;
	case 2:
		ism_name = G_store(str) ;
		break ;
	case 3:
		User_Entry = 0 ;
		break ;
	default:
		break;
    }	/*  switch  */

    return (0);
}

#ifdef DEBUG
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif


init_graphics ()
{
    /* IRIS */
    foreground ();
    prefposition (0, XMAXSCREEN/2, 0, YMAXSCREEN/2);
    noport ();
    winopen ("ISM2DIG");

    init_user_colors ();
}


struct Map_info Map;
struct head Head;

export (dig_name, mapset, ism_name)
    char *dig_name, *mapset, *ism_name;
{
    FILE *out;

    if (!mapset)
	G_fatal_error ("No mapset specified.\n");

    dig_P_init (dig_name, mapset, &Map);

    if (NULL == (out = fopen (ism_name, "w")))
	fprintf (stderr, "Cannot open file '%s'\n", ism_name), exit (1);

    dig_read_head_binary (Map.digit, &Head);

    

    /* turn off NO CATEGORY message */
    G_suppress_warnings (1);
    if (0 > G_read_vector_cats (dig_name, mapset, &Cats))
	G_init_cats (0, "", &Cats);	/* if not there, then init */
    G_suppress_warnings (0);



    if (Map.n_lines)
    {
	line_types (&Map, out);
	site_types (&Map, out);
    }

    if (Map.n_areas)
	area_types (&Map, out);




    fclose (out);
    dig_P_fini (&Map);

    return(0);
}

dump_line (fp, map, line, text)
    FILE *fp;
    struct Map_info *map;
    int line;
    char *text;
{
    register int i;
    P_LINE *Line;

    Line = &(map->Line[line]);

    dig__Read_line (&Points, map->digit, Line->offset);

    for (i = 0 ; i < Points.n_points ; i++)
	if (!i)
	    fprintf (fp, " %12f %12f %s\n", Points.x[i], Points.y[i], text);
	else
	    fprintf (fp, " %12f %12f\n", Points.x[i], Points.y[i]);
}


dump_site (fp, map, line, text)
    FILE *fp;
    struct Map_info *map;
    int line;
    char *text;
{
    P_LINE *Line;

    Line = &(map->Line[line]);

    dig__Read_line (&Points, map->digit, Line->offset);

    fprintf (fp, " %12f %12f %s\n", Points.x[0], Points.y[0], text);
}

dump_area (fp, map, area, text)
    FILE *fp;
    struct Map_info *map;
    int area;
    char *text;
{
    register int i;
    P_LINE *Line;
    struct line_pnts *Points;
    double *x, *y;

    Points = dig__P_get_area_xy (map, area);

    for (i = 0 ; i < Points->n_points ; i++)
	if (!i)
	    fprintf (fp, " %12f %12f %s\n", Points->x[i], Points->y[i], text);
	else
	    fprintf (fp, " %12f %12f\n", Points->x[i], Points->y[i]);
}


char *
prompt_for_file (msg)
    char *msg;
{
    static char input[200];

    while (1)
    {
	fprintf (stderr, "\n%s\n", msg);
	fprintf (stderr, "Hit RETURN %s\n", G_get_ask_return_msg ());
	fprintf (stderr, "> ");

	while (!G_gets (input)) ;

	G_squeeze (input);
	fprintf (stderr, "<%s>\n", input);

	if (*input == 0)
	    return NULL;

	if (!G_legal_filename (input))
	{
	    fprintf (stderr, "\n**<%s> illegal name **\n");
	    continue;
	}

	if (!strcmp (input, "list"))
	{
	    printf ("\n");
	    fflush (stdout);
	    system ("ls -C");

	    fflush (stdout);
	    printf ("\n");
	    printf ("hit RETURN to continue -->");
	    while (getchar() != '\n')
		;
	    printf ("\n");

	    continue;
	}

	return input;
    }
}
