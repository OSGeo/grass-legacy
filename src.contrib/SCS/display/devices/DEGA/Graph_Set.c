/* Function: Graph_Set		P.W. Carlson	5/89		*/

#define GLOBAL
#include "ega_io.h"

Graph_Set() 
{
    unsigned char *ptr;
    register int n;

    static char version[8];
    static char * b1 = "\n\n*******************************************\n";
    static char * b2 = "*      EGA Device Driver Version ";
    static char * b3 = "     *\n*  Author: Paul W. Carlson  FTS 832-6795  *\n";
    static char * b4 = "*******************************************\n\n";
    char banner[300];

    /* open the EGA device driver */
    ega_is_open = 0;
    if ((egafd = open("/dev/ega", 0)) == -1)
    {	perror("Open /dev/ega");
	exit(1);
    }
    ega_is_open = 1;

    /* open the Logitech Bus Mouse device driver */
    mouse_is_open = 0;
    if((mousfd = open("/dev/mouse", 0)) == -1)
    {	perror("Open /dev/mouse");
	if (ega_is_open) Graph_Close();
	exit(-1);
    }
    mouse_is_open = 1;

    /* initialize some global variables */
    SCREEN_LEFT = 0;
    SCREEN_TOP = 0;
    SCREEN_RIGHT = H_RES - 1;
    SCREEN_BOTTOM = V_RES - 1;
    NCOLORS = 125;

    /* get the version of the EGA device driver */
    args.ptr1 = (unsigned char *)version;
    ioctl(egafd, EGA_VERSION, &args);

    /* make and display a banner */
    strcpy(banner, b1);
    strcat(banner, b2);
    strcat(banner, version);
    strcat(banner, b3);
    strcat(banner, b4);
    write(1, banner, strlen(banner));

    /* have the device drive save the text cursor */
    ioctl(egafd, EGA_GETCURS, &args);

    /* put the display in graphics mode (640x350x16 colors) */
    ptr = raster_buff;
    for (n = 0; n < 1024; n++) *ptr++ = 255;
    ioctl(egafd, EGA_GRAPHMODE, &args);
    args.arg1 = 80;
    ioctl(egafd, EGA_VIDWIDTH, &args);
    args.arg1 = args.arg2 = 0;
    ioctl(egafd, EGA_SETORIG, &args);
    args.arg1 = 6;
    args.arg2 = 6;
    ioctl(egafd, EGA_PUTPAL, &args);

    /* display the SCS logo */
    logo();
}
