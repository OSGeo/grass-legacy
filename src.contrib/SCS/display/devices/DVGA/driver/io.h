/* File: io.h		Author: Paul W. Carlson		Nov. 1988 
 */
#define VIOMAP	('V' << 8)

/***************************************************************************
 *      The following are the command identifiers for the Orchid Designer VGA
 * 	device driver ioctl calls.  They are converted by the driver to indices
 *	into an array of pointers to functions.  
 ****************************************************************************/
#define VIO_DRAW	(VIOMAP | 0)
#define VIO_MOVE	(VIOMAP | 1)
#define VIO_PUTRAST	(VIOMAP | 2)
#define VIO_SETCOLOR	(VIOMAP | 3)
#define VIO_SETPIX	(VIOMAP | 4)
#define VIO_GETPIX	(VIOMAP | 5)
#define VIO_HORLINE	(VIOMAP | 6)
#define VIO_ERASE	(VIOMAP | 7)
#define VIO_PUTPAL	(VIOMAP | 8)
#define VIO_GETPAL	(VIOMAP | 9)
#define VIO_XHAIR	(VIOMAP | 10)
#define VIO_RUBBOX	(VIOMAP | 11)
#define VIO_RUBLINE	(VIOMAP | 12)
#define VIO_SETXHAIR	(VIOMAP | 13)
#define VIO_SETRBOX	(VIOMAP | 14)
#define VIO_SETRLINE	(VIOMAP | 15)
#define VIO_TEXTMODE	(VIOMAP | 16)
#define VIO_GRAPHMODE	(VIOMAP | 17)
#define VIO_VERTLINE	(VIOMAP | 18)
#define VIO_CIRCLE	(VIOMAP | 19)
#define VIO_FILLCIRC	(VIOMAP | 20)
#define VIO_BITTEXT	(VIOMAP | 21)
#define VIO_HILITE	(VIOMAP | 22)
#define VIO_UNHILITE	(VIOMAP | 23)
#define VIO_GETRAST	(VIOMAP | 24)
#define VIO_GETCURS	(VIOMAP | 25)
#define VIO_PUTCURS	(VIOMAP | 26)
#define VIO_VERSION	(VIOMAP | 27)

/***************************************************************************
 *      The following are the command identifiers for the Logitech Bus Mouse
 * 	device driver ioctl calls.  
 ****************************************************************************/
#define MOUSE_INIT	(('M' << 8) | 81)
#define GET_MOUSE	(('M' << 8) | 82)

/******** Define the horizontal and vertical dispaly resolution ******/
#define H_RES	800
#define V_RES	600

/******** Global variables *****************************/
#ifdef GLOBAL
#define GTYPE
#else 
#define GTYPE extern
#endif

GTYPE int viofd;			/* /dev/vio file descriptor */
GTYPE int mousfd;			/* /dev/mouse file descriptor */
GTYPE int cur_x, cur_y;			/* current coordinates */
GTYPE unsigned char cur_color; 		/* current color */
GTYPE unsigned char raster_buff[1024];	/* array of pixel colors */
GTYPE struct io_args {			/* ioctl structure */
    int arg1;
    int arg2;
    int arg3;
    int arg4;
    int arg5;
    unsigned char *ptr;
} args;
