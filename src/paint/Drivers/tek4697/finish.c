#include "P.h"

Pfinish()
{
    Palpha() ;	/* flush any remaining graphics */
    formfeed();
    Pflush();
}
