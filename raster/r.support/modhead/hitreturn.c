#include "config.h"

#include <stdlib.h>
#include "gis.h"
#include "glocale.h"


int hitreturn(void)
{
    char buf[100];

    G_message(_("\nhit RETURN to continue -->"));
    G_gets(buf);

    return EXIT_SUCCESS;
}
