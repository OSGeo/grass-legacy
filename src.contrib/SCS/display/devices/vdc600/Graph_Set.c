/* Function: Graph_Set		P.W. Carlson	1/90		*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#define GRAPH_SET
#include "vdc600.h"

Graph_Set() 
{
    int i; 
    unsigned char high_byte, low_byte;
    unsigned int cursor_address;
    struct port_io_arg curs;

    /* open the console device driver */
    if ((vidfd = open("/dev/console", 2)) == -1)
    {   printf("Can't open /dev/console.\n");
	exit(1);
    }
    vid_is_open = 1;

    /* save the cursor location */
    get_cursor();

    /* open the DAC device driver */
    if ((dacfd = open("/dev/dac", 2)) == -1)
    {   if (vid_is_open) Graph_Close();
	printf("Can't open /dev/dac.\n");
	exit(1);
    }
    dac_is_open = 1;

    /* open the Logitech Bus Mouse device driver */
    if ((mousfd = open("/dev/mouse", 0)) == -1) perror("Open /dev/mouse");

    /* initialize some global variables */
    SCREEN_LEFT = 0;
    SCREEN_TOP = 0;
    SCREEN_RIGHT = H_RES - 1;
    SCREEN_BOTTOM = V_RES - 1;
    NCOLORS = 250;

    /* get pointer to text memory */
    text_base = (char *)ioctl(vidfd, MAPEGA, 0);

    /* save the text screen */
    for (i = 0; i < 4000; i++) screen[i] = *(text_base + i);

    /* free up text memory */
    ioctl(vidfd, KDUNMAPDISP, 0);

    /* initialize the port structures */
    init_port_structs();

    /* get the attribute byte */
    attrib_byte = ioctl(vidfd, GIO_ATTR, 0);

    /* set 640x400, 256 color graphics mode */
    ioctl(vidfd, SW_VDC640x400V, 0);

    /* allocate graphics memory */
    alloc_graphics_mem();
    save_gbase = graphics_base;

    /* initialize video segment number */
    old_seg = 1;
    video.total_offset = 0;
    CHECK_SEG();

    /* display the SCS logo */
    scslogo();
}
