/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */
#include <gl.h>
#include <panel.h>

main()
{
int i;
char *s;

    foreground();
    winopen();
    doublebuffer();
    gconfig();
    drawit();
    for (;;) {
	cmov2i(100,100);
	s=g_getstring(WHITE, BLUE, RED, "Edit this string", 30);
	(void) printf("s=%x  ",s);
	for (i=0;i<10;i++) (void) printf("[%3d] ",s[i]); 
	(void) printf("\n");
        if (s) (void) printf("%s\n",s);
	drawit();
    }
}

drawit()
{
    color(BLACK);
    clear();
    cmov2i(100,100);
    color(RED);
    charstr("Edit this string");
    swapbuffers();
}
