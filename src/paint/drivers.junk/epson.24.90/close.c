#include "P.h"

Pclose()
{
    Palpha() ;	/* flush any remaining graphics */
    formfeed();
    Pflush();
    close (printer.fd);
}
