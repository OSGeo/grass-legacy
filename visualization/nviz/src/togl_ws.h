#ifndef TOGL_WS_H
#define TOGL_WS_H

/* define windowing system togl is compiled with */
#if USE_X11
# define TOGL_X11
#elif OPENGL_AQUA
# define TOGL_AGL
#else
# define TOGL_WGL
#endif

#endif
