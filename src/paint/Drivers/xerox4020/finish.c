#include "P.h"

Pfinish()
{
    sleep(2);
    Palpha() ;	/* flush any remaining graphics */
    formfeed();
    Pflush();
}
