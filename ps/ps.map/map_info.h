/* Header file: map_info.h
 **
 ** Author: Paul W. Carlson     April 1992
 */

#include "clr.h"

struct map_info
{
    double x, y;
    char *font;
    int fontsize;
    PSCOLOR color, bgcolor, border;
};

#ifdef MAIN
struct map_info m_info;
#else
extern struct map_info m_info;
#endif
