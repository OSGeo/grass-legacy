#define GLOBAL
#include "tape.h"

main (argc,argv) char *argv[];
{
    G_gisinit(argv[0]);

    I_must_be_imagery_projection();

    ask_options();

    tm_extract();

}
