#include "gis.h"
main(argc,argv) char *argv[];
{
    int error;
    int oops;

    struct Option *delete, *save, *show;
    struct Flag *list;

    G_gisinit(argv[0]);

    delete = G_define_option();
    delete->key="delete";
    delete->type=TYPE_STRING;
    delete->required=NO;
    delete->description="Name of DOS graphic file";

    save = G_define_option();
    save->key="save";
    save->type=TYPE_STRING;
    save->required=NO;
    save->description="Name of DOS graphic file";

    show = G_define_option();
    show->key="show";
    show->type=TYPE_STRING;
    show->required=NO;
    show->description="Name of DOS graphic file";

    list = G_define_flag();
    list->key='l';
    list->description="List all DOS graphic files";

    if (G_parser(argc,argv))
	exit(1);

    if (show->answer)
	run("show",show->answer);

    if (save->answer)
	run("save",save->answer);

    if (delete->answer)
	run("delete",delete->answer);

    error = 0;
    if (list->answer)
	error += run("list","");
    exit(error);
}

run(pgm,name)
    char *pgm,*name;
{
    char command[1024];
    char *getenv();

    sprintf (command, "%s/etc/DOS.%s %s", G_gisbase(), pgm, name);
    return system(command);
}
