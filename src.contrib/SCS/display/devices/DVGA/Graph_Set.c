/* Function: Graph_Set		P.W. Carlson	12/88		*/

#define GRAPH_SET
#include "vio_driver.h"

Graph_Set() 
{
    static char version[8];
    static char * b1 = "\n\n*******************************************\n";
    static char * b2 = "*  Orchid VGA Device Driver Version ";
    static char * b3 = "  *\n*  Author: Paul W. Carlson  FTS 832-6795  *\n";
    static char * b4 = "*******************************************\n\n";
    char banner[300];

    /* open the Orchid VGA device driver */
    if ((viofd = open("/dev/vio", 0)) == -1)
    {	perror("Open /dev/vio");
	exit(1);
    }

    /* open the Logitech Bus Mouse device driver */
    if ((mousfd = open("/dev/mouse", 0)) == -1) perror("Open /dev/mouse");

    /* initialize some global variables */
    SCREEN_LEFT = 0;
    SCREEN_TOP = 0;
    SCREEN_RIGHT = H_RES - 1;
    SCREEN_BOTTOM = V_RES - 1;
    NCOLORS = 250;

    /* get the version of the Orchid VGA device driver */
    args.ptr = (unsigned char *)version;
    ioctl(viofd, VIO_VERSION, &args);

    /* make and display a banner */
    strcpy(banner, b1);
    strcat(banner, b2);
    strcat(banner, version);
    strcat(banner, b3);
    strcat(banner, b4);
    write(1, banner, strlen(banner));

    /* have the device drive save the text cursor */
    ioctl(viofd, VIO_GETCURS, &args);

    /* put the display in graphics mode (800x600x256 colors) */
    ioctl(viofd, VIO_GRAPHMODE, &args);

    /* display the SCS logo */
    logo();
}
