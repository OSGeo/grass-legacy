#include "P.h"

Pfinish()
{
    Palpha() ;	/* flush any remaining graphics */
    formfeed();
    Pflush();
    sleep(30);  /* to allow timing problem with parallel ports to resolve */
}
