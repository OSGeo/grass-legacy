#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	struct GModule *module;
    int stat;
    struct Option *painter;
    struct Flag *print, *list, *quiet;

    G_no_gisinit ();

	module = G_define_module();
	module->description =
		"Selects a device (printer) for GRASS hardcopy output.";

    painter = G_define_option();
    painter->key = "painter";
    painter->type = TYPE_STRING;
    painter->description="name of painter to select";
    painter->options=ls_painters();

    list = G_define_flag();
    list->key = 'l';
    list->description = "list all available painters";

    print = G_define_flag();
    print->key = 'p';
    print->description = "print name of currently selected painter";

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "quietly select painter";


    if (G_parser(argc,argv))
	exit(1);

    stat = 0;
    if (painter->answer)
	stat = select_painter(painter->answer, quiet->answer);

    if (list->answer)
	list_painters();
    if (print->answer)
	show_current_painter();
    exit(stat);
}
