/*  @(#)b_a_dig.c	2.1  6/26/87  */

#include "stdio.h"
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	FILE *ascii;
	struct GModule *module;
	struct Option *old, *new;
	char *mapset;
	char errmsg[200];
	struct Map_info Map;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts a binary GRASS vector map layer into "
		"an ASCII GRASS vector map layer.";

	old = G_define_option();
	old->key		= "input";
	old->type		=  TYPE_STRING;
	old->required		=  YES;
	old->multiple		=  NO;
	old->gisprompt  	= "old,dig,vector" ;
	old->description	= "binary vector file to be converted to ascii";

	new = G_define_option();
	new->key		= "output";
	new->type		=  TYPE_STRING;
	new->required		=  YES;
	new->multiple		=  NO;
	new->gisprompt  	= "new,dig_ascii,ascii vector" ;
	new->description	= "name of resulting ascii file";

	if (G_parser (argc, argv))
		exit(-1);

    if (!*(old->answer)  || !*(new->answer))
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
		G_usage();
        exit (-1);
    }

	if ((mapset = G_find_vector2 (old->answer, "")) == NULL)
	{
		sprintf (errmsg, "Could not find vector file <%s>\n", old->answer);
		G_fatal_error (errmsg);
	}

	Vect_set_open_level (1);	/* only need level I */
	if ( (Vect_open_old (&Map, old->answer, mapset) ) < 1)
	{
		sprintf(errmsg, "Could not open vector file <%s>\n", old->answer);
		G_fatal_error (errmsg);
	}

	if ( (ascii = G_fopen_new("dig_ascii", new->answer) ) == NULL )
	{
		sprintf(errmsg, "Not able to open ascii file <%s>\n", new->answer) ;
		G_fatal_error (errmsg);
	}

	dig_write_head_ascii(ascii, &(Map.head)) ;


	bin_to_asc (ascii, &Map) ;

	fclose(ascii) ;
	Vect_close (&Map);

	if (strcmp (mapset, G_mapset()) || strcmp (new->answer, old->answer))
	{				 /* not same name and mapset ? */
	    char file1[300], file2[300];

	    G__make_mapset_element( "dig_att") ;

	    G__file_name (file1, "dig_att", old->answer, mapset);
	    if (0 == access (file1, 0))
	    {
		G__file_name (file2, "dig_att", new->answer, G_mapset());
		if (0 > cp_file (file1, file2))
		    fprintf (stderr, "Error, dig_att file not created\n");
	    }


	}


	exit(0) ;
}


/*  1 successs
**  0 no in file to read
** -1 cant open output
*/
int cp_file (char *from, char *to)
{
    FILE *in, *out;
    char buf[BUFSIZ];
    int red;
    int no_file = 0;

    if (NULL == (in = fopen (from, "r")))
    {
        no_file = 1;
	return 0;
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

    return (1);
}

