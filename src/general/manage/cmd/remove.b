#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    int i;
    struct Option *item;

    init (argv[0]);

    item = G_define_option();
    item->key = "item";
    item->type = TYPE_STRING;
    item->required = YES;
    item->multiple = YES;
    item->description = "item(s) to be removed";

    n = parse(argc, argv);

    for (i = 0; item->answers[i]; i++)
	do_remove (n, item->answers[i]);
    exit(0);
}
