#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    int stat;
    struct Option *painter;
    struct Flag *print, *list, *quiet;
    struct GModule *module;

    G_gisinit(argv[0]);
    
    /* Set description */
    module              = G_define_module();
    module->description = ""\
    "Selects a PostScript device for GRASS hardcopy output.";

    painter = G_define_option();
    painter->key = "painter";
    painter->type = TYPE_STRING;
    painter->description="name of PostScript painter to select";
    painter->options=ls_painters();

    list = G_define_flag();
    list->key = 'l';
    list->description = "list all available PostScript painters";

    print = G_define_flag();
    print->key = 'p';
    print->description = "print name of currently selected PostScript painter";

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "quietly select PostScript painter";

    if (G_parser(argc, argv)) exit(1);

    stat = 0;
    if (painter->answer) stat = select_painter(painter->answer, quiet->answer);

    if (list->answer) list_painters();
    if (print->answer) show_current_painter();
    exit(stat);
}
