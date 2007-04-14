#include <grass/config.h>
#ifndef __MINGW32__
#include <unistd.h>
#endif
#include <grass/gis.h>

/* Sleep */
unsigned int G_sleep (unsigned int seconds)
{
#ifdef __MINGW32__
    /* TODO: no sleep for now */
    return 0;    
#else
    return sleep(seconds);
#endif
}

