#include "list.h"
static struct Option *element;
init(pgm)
    char *pgm;
{
    G_gisinit(pgm) ;

    read_list(0);
}
