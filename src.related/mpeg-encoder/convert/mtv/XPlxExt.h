/*
 * (c) Copyright 1991 Parallax Graphics Inc, All Rights Reserved
 *
 * Parallax Graphics, Inc.
 * 2500 Condensa Street
 * Santa Clara, California 95051
 *
 * $Log:	XPlxExt.h,v $
 * Revision 1.4  91/10/24  12:41:16  eric
 * use char * instead of caddr_t so we don't need sys/types.h
 * 
 * Revision 1.3  91/10/17  11:22:33  dave
 * added shared memory stuff to Image structure
 * 
 * Revision 1.2  91/08/19  12:40:40  eric
 * added rcs header
 * 
 *
 */

#ifdef H_ID
#ifndef lint
static char *rcsid_XPlxExt_h = "@(#)$Id: XPlxExt.h,v 1.4 91/10/24 12:41:16 eric Exp $ Parallax Graphics Inc";
#endif
#endif

/***************************************************************************
 The following are the immediate defines for the Parallax video extension. 
****************************************************************************/

/*
The following are valid values of the plx_hardware variable updated as
a result of a XPlxQueryConfig call.
*/

#define	PLX_UNKNOWN	0	/* Unknown configuration */
#define	PLX_1280	2	/* 1280 Series	*/	
#define	PLX_VIPER	3	/* Viper Series	*/
#define	PLX_VIDEOVIEW	4	/* VideoView	*/
#define	PLX_XVIDEO_8	8	/* XVideo, eight bitplane version */
#define	PLX_XVIDEO_24	9	/* XVideo, twenty four bitplane version	*/

/*
The following are valid nvselect Parameters for the XPlxVideoValuesSave and
XPlxVideoValuesRecall functions.
*/

#define USER_0		0	/* user 0 */
#define USER_1		1	/* user 1 */
#define FACTORY		2	/* factory. XPlxVideoValuesRecall only */

/*
The following are valid values of the standard parameter of XPlxInputSelect and
XPlxOutputSelect and are vaild standard values returned by XPlxQueryVideo.
*/
#define	PLX_NTSC	2		/* Video standards...	*/
#define	PLX_PAL		4
#define	PLX_SECAM	5

/*
The following are valid values of the channel parameter of
XPlxVideoInputSelect and XPlxVideoOutputSelect and are valid channel values
returned by XPlxQueryConfig.
*/
#define	PLX_IO_OFF	0		/* Various input/output defs... */
#define	PLX_INPUT_0	1
#define	PLX_INPUT_1	2
#define PLX_OUTPUT_0	32	

/*
The following are valid values of the format parameter of
XPlxVideoInputSelect and XPlxVideoOutputSelect and are vaild
format values returned by XPlxQueryConfig.
*/
#define	PLX_COMP	2		/* Composite	*/
#define	PLX_YC		3		/* SuperVHS	*/
#define	PLX_YUV		4		/* YUV Component */
#define	PLX_RGB		5		/* RGB Compontent */

/*
The following are valid values of the type parameter of XPlxVideoInputSelect
and XPlxVideoOutputSelect and are vaild  type values returned by
XPlxQueryConfig.
*/
#define	PLX_YUV7	1	/* Seven bit YUV format described in */
				/* 1280 or VIPER Series hardware manual */
#define	PLX_RGB24	4	/* Twenty four bit RGB (True Color)	*/
#define	PLX_RGB8	5	/* Eight bit Pseudo color		*/
#define PLX_GRAY5	7	/* Five bit Grayscale (bit <0>=0)	*/
				/* see Viper Manual			*/
#define	PLX_GRAY7	8	/* Seven bit grayscale (bit <0>=0)	*/
#define	PLX_GRAY8	9	/* Eight bit grayscale			*/

/*
The following are valid values of the type parameter of XPlxVideoTag.
*/	
#define	PLX_VIDEO	2	/* Video pixels	*/
#define	PLX_VIDEO_OVR	3	/* Graphics overlay	*/
#define	PLX_GRAPHICS_1	4	/* One bit graphics	*/
#define	PLX_GRAPHICS_8	5	/* Eight bit graphics	*/
#define	PLX_GRAPHICS_24	6	/* Twenty four bit graphics */
#define PLX_GRAPHICS    7	/* just graphics, superseeds 1,8,24 above */


/***************************************************************************
The following is the data structure maintained by the Parallax video
extension and made accessible to the programmer.
***************************************************************************/

/*
This data structure contains information about a particular video signal.
*/

typedef struct {
	unsigned	standard;	/* PLX_NTSC, PLX_PAL, or PLX_SECAM */
	unsigned	w;		/* Visible width of signal in pixels */
	unsigned 	h;		/* Visible height of signal in lines */
	unsigned 	b;		/* Digitizable vertical int. in lines */
	unsigned 	r;		/* Frame rate in Hz	*/
	unsigned	interlaced;	/* TRUE if signal is interlaced; */
	unsigned 	sync_ok;	/* TRUE if good sync at input. */
					/* Valid only as a	*/
					/* result of a XPlxQueryVideo call. */
} plx_signal;

/*
The following is a list of signal definitions available for a particular channel
*/
typedef struct _plx_signal_list {
	plx_signal	*signal;	/* Pointer to a plx_signal structure */ 
	struct _plx_signal_list *next_signal; /* Next plx_signal_list entry */
} plx_signal_list;

/* 
The following is a list of formats available for a particular channel
*/
typedef struct _plx_format_list {
	unsigned	format;		/* PLX_(COMP, YC, YUV, RGB)	*/
	struct _plx_format_list *next_format; /* Next plx_format_list entry */
} plx_format_list;

/*
The following is a list of types available for a particular channel
*/
typedef struct _plx_type_list {
	unsigned type;		/* PLX_(YUV7, RGB24, RGB8, GRAY7, GRAY8) */
	struct _plx_type_list *next_type; /* Next plx_type_list entry */
} plx_type_list;

/*
The following is a list of video input and output channels available for
this hardware configuration and the signal definitions and formats
available for each channel.
*/
typedef struct _plx_IO_list {
	unsigned	channel;	/* INPUT_0, INPUT_1, OUPUT_0...	*/
	plx_signal_list	*signal_list;	/* List of signal definitions */
					/* supported by */
					/* this channel	*/
	plx_format_list	*format_list;	/* List of formats supported by */
					/* this channel	*/
	plx_type_list *type_list;	/* List of types supported by */
					/* this channel */
	struct _plx_IO_list *next_IO;	/* Next plx_IO_list entry */
} plx_IO_list;

/*
The following is a structure that holds the hardware type and io lists.
A pointer to one of these is returned by XPlxQueryConfig.
*/

typedef struct _plx_IO {
	unsigned hardware;
	plx_IO_list *inputs;
	plx_IO_list *outputs;
} plx_IO;

/*
 * Data structure for "compressed image" data, used by
 * compressed image manipulation routines.
 */

typedef struct _XPlxCImage {
    int width, height;		/* size of image */
    int size;			/* size in bytes of image */
    char *data;			/* pointer to image data */
    char *info;			/* shared memory segment info */
} XPlxCImage;
