#include "P.h"

Pfinish()
{
    flush_raster() ;	/* flush any remaining graphics */
    Pflush();
    close (magentafd);
    close (cyanfd);
    close (yellowfd);
    unlink (magentafile);
    unlink (cyanfile);
    unlink (yellowfile);
}
