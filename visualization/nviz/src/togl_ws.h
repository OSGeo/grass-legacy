#ifndef TOGL_WS_H
# define TOGL_WS_H

/* define windowing system togl is compiled with */
#if USE_X11
# define TOGL_X11
#else
# define TOGL_AGL
#endif

#endif
