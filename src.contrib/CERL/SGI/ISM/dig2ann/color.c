#include <gl.h>
#include <panel.h>

#define BLUE_START 0x77
#define GREEN_START 0x0
#define RED_START 0x0

#define BLUE_STOP 0x33
#define GREEN_STOP 0x0
#define RED_STOP 0x44

#define PAL_START 1024
#define NUM_STEPS 256

init_user_colors ()
{
    int from, to;
    float blue_step;
    float red_step ;
    float green_step;
    short blue;
    short red;
    short green;
    int i;

#if RED_STOP == RED_START
    red_step   = 0;
#else
    red_step   = (float) (RED_STOP   - RED_START) / NUM_STEPS;
#endif

#if GREEN_STOP == GREEN_START
    green_step = 0;
#else
    green_step = (float) (GREEN_STOP - GREEN_START) / NUM_STEPS;
#endif

#if BLUE_STOP == BLUE_START
    blue_step  = 0;
#else
    blue_step  = (float) (BLUE_STOP  - BLUE_START) / NUM_STEPS;
#endif

    from = PAL_START;
    to = PAL_START + NUM_STEPS - 1;

    for (i = from ; i <= to ; i++)
    {
	red   = RED_START + (i-from) * red_step;
	green = GREEN_START + (i-from) * green_step;
	blue  = BLUE_START + (i-from) * blue_step;
	mapcolor (i, red, green, blue);
    }
}

pnl_user_color (p)
    Panel *p;
{
/*
    if (!p->gid) return ;
    winset (p->gid);
    color (PAL_START+NUM_STEPS-1);

    pmv2(p->minx, p->miny);
    pdr2(p->maxx, p->miny);
    color (PAL_START);
    pdr2(p->maxx, p->maxy);
    pdr2(p->minx,  p->maxy);
    pclos();
*/
}
