/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


#define MAIN
#define  USAGE  "Vprune dig=input\n"

long ftell ();
double atof ();

/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;

static  int  load_args() ;

struct Command_keys vars[] = {
 { "dig", 1 },
 { "vect", 1 },
 { "input", 1 },
 { NULL,     0 }
};

double dig_unit_conversion ();
static	int   snapped = 0 ;

main (argc, argv)
    int argc;
    char **argv;
{
    int   ret ;
    char *mapset;


/*  check args and set flags  */
	
    ret = G_parse_command (argc, argv, vars, load_args) ;
    if (ret > 0)	/* Help was requested */
         exit (1);

    if (ret < 0  ||  dig_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

/* Show advertising */
    G_gisinit(argv[0]);
    printf("\n\n   Vclean:\n\n");

    if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
	G_fatal_error ("Could not find DIG file %s\n", dig_name);
    
    if (strcmp (mapset, G_mapset()))
	G_fatal_error ("Can only clean files in your mapset");
	

    
    export (dig_name, mapset); 
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


struct Map_info Map;
struct head Head;


/*  Can't copy in place, cuz I'm not sure if I can depend on the
**   ftruncate() call being available.  So will copy dig file
**   to a temp file, then clean from it into original, truncating
**   it as I do so.
**`
**  TODO:  this needs to be improved to support Plus files so do not
**         have to re-run support.vect
*/

/* coming in, mapset is guaranteed to be the users own mapset */
export(dig_name, mapset)
    char *dig_name, *mapset;
{
	FILE *Out;
	FILE *In;
	char buf[1024];
	char *tmpfile;

	if ( ! mapset)
	{
	    G_fatal_error ("No mapset specified.");
	}

/***************************************************************************/
	/* Temp use In/Out to Copy file to tempfile */
	if (NULL == (In = G_fopen_old ("dig", dig_name, mapset)))
	{
	    fprintf (stderr, "Cannot open input file.\n");
	    exit (-1);
	}

	tmpfile = G_tempfile ();
	Out = fopen (tmpfile, "w");

	if (0 > cp_filep (In, Out))
	    G_fatal_error ("File copy failed");

	fclose (In);
	fclose (Out);

/***************************************************************************/

	if (NULL == (In  = fopen (tmpfile, "r")))
	{
	    cleanup (tmpfile);
	    G_fatal_error ("Failed openning temp file");
	}
	if (NULL == (Out = G_fopen_new ("dig", dig_name)))
	{
	    cleanup (tmpfile);
	    G_fatal_error ("Failed openning dig file, may be corrupted.");
	}

	dig_read_head_binary (In, &Head);
	dig_write_head_binary (Out, &Head);

	doit (In, Out);

	fclose (Out);
	fclose (In);

	cleanup (tmpfile);

	fprintf (stderr, "Done.\n");

	return(0) ;
}

cleanup (file)
    char *file;
{
    unlink (file);
}

doit (in, out)
    FILE *in;
    FILE *out;
{
    struct line_pnts Points;
    register int line, type;
    int binary;
    long offset;
    int diff;
    int left;
    int old, new; 
    
    Points.alloc_points = 0;

    left = diff = 0;
    line = 0;
    while (1)
    {
	line++;

	offset = ftell (in);
	if (0 > (type = dig__Read_line (&Points, in, offset)))
	{
	    if (type == -1)
	    {
		fprintf (stderr, "Out of memory on line %d\n", line);
		return (-1);
	    }
	    else 	/* EOF */
		return (0);
	}
	if (type < 16)	/* if line ALIVE */
	    dig__Write_line (out, type, &Points);
    }
}

cp_filep  (in, out)
    FILE *in, *out;
{
    char buf[BUFSIZ];
    int red;
    int no_file = 0;

    {
        while (red = fread (buf, 1, BUFSIZ, in))
            fwrite (buf, 1, red, out);
        fclose (in);
    }
    fclose (out);

    return (0);
}
