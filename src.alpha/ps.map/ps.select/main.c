#include "gis.h"

main(argc, argv) 
int argc;
char *argv[];
{
    int stat;
    struct Option *painter;
    struct Flag *print, *list, *quiet;
    char *ls_painters();

    G_gisinit(argv[0]);

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
