/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    <math.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


#define MAIN
#define  USAGE  "dxf2dig dxf=file [lines=n,n,..] [labels=n,n...]"

/*
#define DEBUG
*/
/*  command line args */
static	char  *dxf_file = NULL ;
static  int   FILL, LABEL;

int all_lines = 1;	/* dump ALL lines unless user override */
int all_atts = 1;	/* dump ALL atts  unless user override */

static  int  load_args() ;

struct Command_keys vars[] = {
 { "dxf", 1 },
 { "lines", 2 },
 { "labels", 3 },
 { NULL,     0 }
};

main(argc, argv)
    int argc;
    char *argv[];
{
    int   ret ;
    char *line_mapset, *label_mapset;


/*  check args and set flags  */
	
    ret = G_parse_command (argc, argv, vars, load_args) ;
    if (ret > 0)	/* Help was requested */
         exit (1);

    if (ret < 0  ||  dxf_file == NULL )
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

    if (dxf_file != NULL)
	LABEL = 1;

    G_gisinit(argv[0]);


    exit (0);
}

static
load_args (position, str)
    int position;
    char *str;
{
    switch (position)
    {
	case 1:
		dxf_file = G_store(str) ;
		break ;
	case 2:
		add_line_layer (str);
		all_lines = 0;
		break ;
	case 3:
		add_att_layer (str);
		all_atts = 0;
		break;
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
