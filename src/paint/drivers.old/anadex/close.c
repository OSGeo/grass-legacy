#include "P.h"

Pclose()
{
    Palpha() ;	/* flush any remaining graphics */
    Poutc('\f');
    Pflush();
    close (printer.fd);
}
