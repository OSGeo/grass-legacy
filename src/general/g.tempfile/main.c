#include "gis.h"

main(argc,argv) char *argv[];
{
    struct Option *pid;
    char *tempfile, *G__tempfile();
    int p;


    G_gisinit(argv[0]);

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
    printf ("%s\n", tempfile);
    exit(0);
}
