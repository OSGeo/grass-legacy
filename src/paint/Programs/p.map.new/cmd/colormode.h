#define COLORMODE_APPROX     0
#define COLORMODE_DIFFUSION  1
#define COLORMODE_DITHER     2

#ifdef MAIN
    int colormode = COLORMODE_DITHER;
#else
    extern int colormode;
#endif
