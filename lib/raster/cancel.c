#include "gis.h"
#include "raster.h"
#include "graph.h"

static int cancel = 0;

/*!
 * \brief cancel 
 *
 *  \param v
 *  \return void
 */

void R_set_cancel ( int v )
{
    cancel = v;
}

/*!
 * \brief 
 *
 *  \return int
 */

int R_get_cancel ( void )
{
    return cancel;
}
