/*----------------------------------------------------------------------*/
/* Copyright (c) 1987							*/
/* by CompuServe Inc., Columbus, Ohio.  All Rights Reserved		*/
/* GIFBASE.H can be copied and distributed freely for any		*/
/* non-commercial purposes. GIFBASE.H can only be incorporated		*/
/* into commercial software with the permission of CompuServe Inc.	*/
/*----------------------------------------------------------------------*/

#define MaxColors	256		/* rev 87a */

struct ColorEntry
    {
    unsigned char red, green, blue;
    };

extern char GIFsignature[];

extern short BaseLine[5], LineOffset[5];
