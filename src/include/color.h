/* $Header: /xgrass/src/lib/RCS/color.h,v 0.0 1992/05/05 14:56:55 sink Exp sink $ */

/* 
 * color.h - color definitions
 * 
 * Author:	Christopher A. Kent
 * 		Western Research Laboratory
 * 		Digital Equipment Corporation
 * Date:	Sun Dec 13 1987
 * Copyright (c) 1987 Christopher A. Kent
 */

/*
 * $Log: color.h,v $
 * Revision 0.0  1992/05/05  14:56:55  sink
 * auto checkin: Tue May  5 09:56:55 CDT 1992
 *
 * Revision 0.0.0.4  1992/03/08  17:56:38  kurt
 * auto checkin: Sun Mar  8 11:56:38 CST 1992
 *
 * Revision 0.0.0.3  1992/02/17  22:45:59  kurt
 * auto checkin: Mon Feb 17 16:45:59 CST 1992
 *
 * Revision 0.0.0.2  1992/02/17  22:33:58  kurt
 * auto checkin: Mon Feb 17 16:33:58 CST 1992
 *
 * Revision 0.0.0.1  1992/01/13  14:32:58  kurt
 * auto checkin: Mon Jan 13 08:32:58 CST 1992
 *
 * Revision 0.0  1992/01/09  03:38:21  kurt
 * auto checkin: Wed Jan  8 21:38:21 CST 1992
 *
 * Revision 0.0.0.1  1992/01/04  02:28:58  kurt
 * auto checkin on Fri Jan  3 20:28:58 CST 1992
 *
 * Revision 0.0  1991/12/31  05:11:25  kurt
 * auto checkin on Mon Dec 30 23:11:25 CST 1991
 *
 * Revision 0.0  1991/12/31  04:49:20  kurt
 * auto checkin on Mon Dec 30 22:49:20 CST 1991
 *
 * Revision 1.2  88/06/30  09:58:56  mikey
 * Handles CMY also.
 * 
 * Revision 1.1  88/06/30  09:10:53  mikey
 * Initial revision
 * 
 */

typedef	struct _RGB {
	unsigned short r, g, b;
} RGB;

typedef	struct _HSV {
	float	h, s, v;	/* [0, 1] */
} HSV;

typedef struct _CMY {
	unsigned short c, m, y;
} CMY;

extern RGB	RGBWhite, RGBBlack;

RGB	MixRGB();
RGB	MixHSV();
RGB	HSVToRGB();
HSV	RGBToHSV();
float	RGBDist();
RGB	PctToRGB();
HSV	PctToHSV();
RGB	CMYToRGB();
CMY	RGBToCMY();
