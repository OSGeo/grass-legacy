/* Function: Graph_close	P.W. Carlson		5/89	*/

#include "ega_io.h"

Graph_Close()
{
    /* erase the screen */
    args.ptr1 = raster_buff;
    ioctl(egafd, EGA_ERASE, &args);

    /* restore text screen */
    ioctl(egafd, EGA_TEXTMODE, &args);
    ioctl(egafd, EGA_PUTCURS, &args);

    /* close the EGA driver */
    close(egafd);
    ega_is_open = 0;

    /* if the mouse driver is open, close it */
    if (mouse_is_open) 
    {	close(mousfd);
	mouse_is_open = 0;
    }
}
