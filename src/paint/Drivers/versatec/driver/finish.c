#include "P.h"
Pfinish ()
{
    print_rasterfile();
    unlink (rasterfile);
}
