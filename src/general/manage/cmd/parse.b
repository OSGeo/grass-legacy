#include "list.h"
static struct Option *element;
init(pgm)
    char *pgm;
{
    G_gisinit(pgm);

    read_list(0);

    element = G_define_option();
    element->key =      "type";
    element->key_desc = "datatype";
    element->type     = TYPE_STRING;
    element->required = YES;
    element->multiple = NO;
    element->description = "database type";
}

parse(argc, argv)
    char *argv[];
{
    int n;

    if (G_parser(argc, argv))
    {
	fprintf (stderr, "\nWhere %s is one of:\n", element->key);
	show_elements();
	exit(1);
    }
    for (n = 0 ; n < nlist; n++)
    {
	if (strcmp (list[n].element[0], element->answer) == 0)
	    break;
	if (strcmp (list[n].alias, element->answer) == 0)
	    break;
    }

    if (n >= nlist)
    {
	fprintf (stderr, "%s: - no such database %s\n",
		element->answer, element->key);
	G_usage();
	fprintf (stderr, "\nWhere %s is one of:\n", element->key);
	show_elements();
	exit(1);
    }
    return n;
}
