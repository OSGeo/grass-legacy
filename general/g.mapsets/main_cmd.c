/*  g.mapsets   set current mapset path
 *
 */
#define MAIN
#include "gis.h"
#include "local_proto.h"
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include "externs.h"

static char Path[4096];

int 
main (int argc, char *argv[])
{
    int n;
    int i;
    int skip;
    char *cur_mapset;
    char **ptr;
    char **tokens;
    int no_tokens;
    FILE *fp;
	struct GModule *module;
    struct Option *opt1, *opt2;
    struct Flag *print;
    struct Flag *list;

    if (argc < 2)
	execl("/bin/sh", "sh", "-c", "$GISBASE/etc/g.mapsets.tcl", NULL);

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Modifies the user's current mapset "
		"search path, affecting the user's access to data existing "
		"under the other GRASS mapsets in the current location.";

    opt1 = G_define_option() ;
    opt1->key        = "mapset" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = NO ;
    opt1->multiple   = YES ;
    opt1->description= "Name(s) of existing mapset(s)" ;

    opt2 = G_define_option() ;
    opt2->key        = "addmapset" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = NO ;
    opt2->multiple   = YES ;
    opt2->description= "Name(s) of existing mapset(s) to add to search list" ;

    list = G_define_flag();
    list->key = 'l';
    list->description = "list all available mapsets";

    print = G_define_flag();
    print->key = 'p';
    print->description = "print current mapset search path";

    Path[0] = '\0';
    nchoices = 0;

    if (G_parser(argc, argv))
        exit(1);

    if (list->answer)
    {
	get_available_mapsets();
	display_available_mapsets(0);
    }

    if (opt1->answer)
    {
	for (ptr=opt1->answers; *ptr != NULL; ptr++)
	{
	    char *mapset;

	    mapset = *ptr;
	    if (G__mapset_permissions (mapset) < 0)
	    {
		char command[1024];

		fprintf (stderr, "\7ERROR: [%s] - no such mapset\n", mapset);
		fprintf (stderr, "\nAvailable mapsets:\n\n");
		sprintf (command, "ls -C %s/%s 1>&2", G_gisdbase(), G_location());
		system (command);
		sleep (3);
		exit(1);
	    }
	    nchoices++;
	    strcat (Path, mapset);
	    strcat (Path, " ");
	}
    }

    /* add to existing search path */
    if (opt2->answer)
    {
	char *oldname;
	Path[0] = '\0';
	
	/* read existing mapsets from SEARCH_PATH */
        for (n = 0; (oldname = G__mapset_name(n)) ; n++)
	{
	  strcat (Path, oldname);
	  strcat (Path, " "); 
	}

	/* fetch and add new mapsets from param list */
	for (ptr=opt2->answers; *ptr != NULL; ptr++)
	{
	    char *mapset;

	    mapset = *ptr;
	    if (G__mapset_permissions (mapset) < 0)
	    {
		char command[1024];

		fprintf (stderr, "\7ERROR: [%s] - no such mapset\n", mapset);
		fprintf (stderr, "\nAvailable mapsets:\n\n");
		sprintf (command, "ls -C %s/%s 1>&2", G_gisdbase(), G_location());
		system (command);
		sleep (3);
		exit(1);
	    }

	    nchoices++;
	    strcat (Path, mapset);
	    strcat (Path, " ");
	}
    }


    /* stuffem sets nchoices*/

    if (nchoices == 0)
    {
        goto DISPLAY;
    }

    /* note I'm assuming that mapsets cannot have ' 's in them */
    tokens = G_tokenize (Path, " ");

    fp = G_fopen_new ("", "SEARCH_PATH");
    if (!fp)
    {
        G_fatal_error ("Cannot open SEARCH_PATH for write");
        return (-1);
    }

    cur_mapset = G_mapset();

/*
* make sure current mapset is specified in the list
* if not add it to the head of the list
*/

    skip = 0;
    for (n = 0; n < nchoices; n++)
        if (strcmp (cur_mapset, tokens[n]) == 0)
        {
            skip = 1;
            break;
        }
    if (!skip)
    {
        fprintf (fp, "%s\n", cur_mapset);
    }

/*
* output the list, removing duplicates
*/

    no_tokens=G_number_of_tokens(tokens);

    for (n = 0; n < no_tokens ; n++)
    {
	skip = 0;
        for (i = n; i < no_tokens; i++)
	{	
	    if (i != n)	
	    {
            	if (strcmp (tokens[i], tokens[n]) == 0)
                     skip = 1;
	    } 
        }

	if (!skip)
        	fprintf (fp,"%s\n", tokens[n]);
    }

    fclose (fp);
    G_free_tokens (tokens);

DISPLAY:
    if (print->answer)
	display_mapset_path (0);
    return (0);
}
