#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"

int main (int argc, char *argv[])
{
	struct GModule *module;
    struct Option *pid;
    char *tempfile, *G__tempfile();
    int p;


    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Creates a temporary file and prints the file name.";

    pid = G_define_option();
    pid->key = "pid";
    pid->type = TYPE_INTEGER;
    pid->required = YES;
    pid->description = "Process id to use when naming the tempfile";

    G_disable_interactive();
    if (G_parser(argc,argv)) exit(1);

    if (sscanf (pid->answer, "%d", &p) != 1)
    {
	G_usage();
	exit(1);
    }
    tempfile = G__tempfile(p);
    umask(0);
/* create tempfile so next run of this program will create a unique name */
    close(creat(tempfile,0666));
    fprintf (stdout,"%s\n", tempfile);
    exit(0);
}
