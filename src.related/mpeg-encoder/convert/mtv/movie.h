/***************************************************************************
 *	movie.h - Private j_movie include file
 *
 *	(C) Copyright 1991 by Paradise Software, Inc.
 *
 *	Written by John Malleo-Roach
 *
 *	Version %G% - %I%
 *
 ***************************************************************************/

/* CONSTANTS */

#ifndef NULL
#define NULL 				0
#endif
#ifndef TRUE
#define TRUE 				1
#endif
#ifndef FALSE
#define FALSE 				0
#endif
#ifndef BOOLEAN
#define BOOLEAN				int
#endif

/* Supported j_movie versions													*/

/* Initial released version														*/

#define JPEG_VERSION_1		  1
#define JPEG_VERSION_2		  2

/* Bytes 200-255 are reserved as code words by the compression/playback engine	*/

#define LOAD_CMAP			249

#define LOAD_UNIFLIX		239
#define LOAD_JPEG			236

#define CLEAR_FRAME			238
#define END_FRAME			237

#define LOAD_AUDIO_0		229
#define LOAD_AUDIO_1		228
#define LOAD_AUDIO_2		227
#define LOAD_AUDIO_3		226

/* JPEG header information														*/

#define FILLER				 48

/* TYPEDEFS */

typedef struct {
	char magic[8];			/* defined as "j_movie"						*/
	int version;			/* Version number							*/
	int fps;				/* frames per second						*/
	int frames;				/* Number of frames in the sequence			*/
	int width;				/* Width of each frame 						*/
	int height;				/* Height of each frame 					*/
	int bandwidth;			/* Bandwidth required for playback			*/
	int qfactor;			/* Quanization factor used in compression	*/
	int mapsize;			/* Number of colors used in colormap		*/
	int indexbuf;			/* Offset in file of frame indexes			*/
	int tracks;				/* Number of audio tracks used				*/
	int volbase;			/* Baseline for volume control				*/
	int audioslice;			/* Number of audio bytes per frame			*/
	Audio_hdr audio;		/* Audio header from input file				*/
	char filler[FILLER];	/* Pad out this record to 128 bytes			*/
} jpheader;

/* end of movie.h */
