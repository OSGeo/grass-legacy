#include "gis.h"
#include <unistd.h>
#include <stdlib.h>
#include "raster.h"
int main(int argc,char *argv[])
{
	struct GModule *module;
    struct Option *element, *prompt;
    struct Flag *quiet;
    char *tempfile;
    char command[1024];
    FILE *fd;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Prompts the user to select a GRASS data base file from among "
		"files displayed in a menu on the graphics monitor.";

    element = G_define_option();
    element->key = "element";
    element->key_desc = "name,description";
    element->type = TYPE_STRING;
    element->required = YES;
    element->description = "Database element , one word description";

    prompt = G_define_option();
    prompt->key = "prompt";
    prompt->key_desc = "\"message\"";
    prompt->type = TYPE_STRING;
    prompt->description = "Short user prompt message";

/*
    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "quiet";
*/

    G_disable_interactive();
    if (G_parser(argc,argv))
	exit(1);

/* make sure we can do graphics */
    R_open_driver();
    R_close_driver();

    tempfile = G_tempfile();
    unlink (tempfile);
    sprintf (command, "%s/etc/i.find %s %s %s %s",
	G_gisbase(), G_location(), G_mapset(), element->answers[0], tempfile);
    system(command);

    if (access(tempfile,0)==0)
    {
	if (prompt->answer)
	{
	    sprintf (command, "%s/etc/i.ask %s '%s'",
		G_gisbase(), tempfile, prompt->answer);
	}
	else
	{
	    sprintf (command, "%s/etc/i.ask %s",
		G_gisbase(), tempfile);
	}
	exit(system(command));
    }
    else
    {
	fd = popen ("d.menu tcolor=red > /dev/null", "w");
	if (fd)
	{
	    fprintf (fd, "** no %s files found **\n", element->answers[1]);
	    fprintf (fd, "Click here to CONTINUE\n");
	    pclose (fd);
	}
	exit(0);
    }
}
