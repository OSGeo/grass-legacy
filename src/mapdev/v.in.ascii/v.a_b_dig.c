
/*  @(#)a_b_dig.c	2.1 6/26/87 */
/* v.a_b_dig.c renamed 12/90 */
#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

#define	A_DIR	"dig_ascii"


/*
#define DEBUG
*/

int 
main (int argc, char *argv[])
{
	FILE *ascii;
	struct dig_head d_head;
	struct GModule *module;
	struct Option *old, *new;
	char *mapset;
	char errmsg[200];

	struct Map_info Map;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts ASCII vector map layers "
		"into binary vector map layers.";

/************************** Command Parser ************************************/
	old = G_define_option();
	old->key			= "input";
	old->type			=  TYPE_STRING;
	old->required		=  YES;
	old->multiple		=  NO;
	old->gisprompt  	= "old,dig_ascii,ascii vector";
	old->description	= "ascii file to be converted to binary vector file";

	new = G_define_option();
	new->key			= "output";
	new->type			=  TYPE_STRING;
	new->required		=  YES;
	new->multiple		=  NO;
	new->gisprompt  	= "new,dig,vector";
	new->description	= "name of resulting vector file";

	if (G_parser (argc, argv))
		exit(-1);

    if (!*(old->answer)  || !*(new->answer))
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
		G_usage();
        exit (-1);
    }
/*****************************************************************************/

	if ((mapset = G_find_file2 (A_DIR, old->answer, "")) == NULL)
	{
		sprintf (errmsg, "Could not find ascii file <%s>\n", old->answer);
		G_fatal_error (errmsg);
	}
	if ( (ascii = G_fopen_old (A_DIR, old->answer, mapset) ) == NULL )
	{
		sprintf(errmsg, "Could not open ascii file <%s>\n", old->answer);
		G_fatal_error (errmsg);
	}

	/* if ( (binary = G_fopen_vector_new(new->answer) ) == NULL )*/

	if (0 > Vect_open_new (&Map, new->answer))
	{
		sprintf(errmsg, "Not able to open vector file <%s>\n", new->answer) ;
		G_fatal_error (errmsg);
	}


	if ( dig_read_head_ascii(ascii, &d_head) < 0)
	    G_fatal_error (" Can't read ascii file header");

	Vect_copy_head_data (&d_head, &Map.head);

	asc_to_bin(ascii, &Map) ;

	fclose(ascii) ;
	Vect_close (&Map);

	if (strcmp (new->answer, old->answer))	/* ! same ? */
	{
	    char file1[300], file2[300];

	    G__make_mapset_element( "dig_att") ;

	    if (0 ==access (file1, 0))
	    {
		G__file_name (file1, "dig_att", old->answer, mapset);
		G__file_name (file2, "dig_att", new->answer, G_mapset());
		cp_file (file1, file2);
	    }

	    /*
	    sprintf (command, "cp %s %s 2>&1 > /dev/null", file1, file2);
	    system (command);
	    */
	}

	exit(0) ;
}



#ifdef DEBUG
#include <stdarg.h>
int debugf (char *format, ...)
{
    va_list a;

    va_start(format,a);
    fprintf (stderr, format, ...);
    va_end(a);

    return 0;
}
#endif

int cp_file (char *from, char *to)
{
    FILE *in, *out;
    char buf[BUFSIZ];
    int red;
    int no_file = 0;

    if (NULL == (in = fopen (from, "r")))
    {
        no_file = 1;
    }
    if (NULL == (out = fopen (to, "w")))
    {
        if (!no_file)
            fclose (in);
        return (-1);
    }

    if (!no_file)
    {
        while (red = fread (buf, 1, BUFSIZ, in))
            fwrite (buf, 1, red, out);
        fclose (in);
    }
    fclose (out);

    return (0);
}

