/* Function: Graph_close	P.W. Carlson		1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Graph_Close()
{
    register i;
    int fg, bg;
    static int colors[] = { 0, 4, 2, 6, 1, 5, 3, 7 };
    char buf[40];

    /* free up graphics memory */
    ioctl(vidfd, KDUNMAPDISP, 0);

    /* set text mode */
    textmode();

    /* get pointer to text memory */
    text_base = (char *)ioctl(vidfd, MAPEGA, 0);

    /* restore the screen */
    for (i = 0; i < 4000; i++) *(text_base + i) = screen[i];

    /* restore the cursor */
    set_cursor();

    /* restore the display atrributes */
    sprintf(buf, "\033[0;m");
    write(vidfd, buf, strlen(buf));
    if (attrib_byte & 8) 
    {	sprintf(buf, "\033[1;m");
    	write(vidfd, buf, strlen(buf));
    }
    if (attrib_byte & 128) 
    {	sprintf(buf, "\033[5;m");
    	write(vidfd, buf, strlen(buf));
    }
    fg = 30 + colors[attrib_byte & 7];
    bg = 40 + colors[(attrib_byte & 112) >> 4];
    sprintf(buf, "\033[%d;%d;m", fg, bg);
    write(vidfd, buf, strlen(buf));

    vid_is_open = 0;
    close(dacfd);
    dac_is_open = 0;
    close(mousfd);
}
