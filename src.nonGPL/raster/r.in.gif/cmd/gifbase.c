/*----------------------------------------------------------------------*/
/* Copyright (c) 1987							*/
/* by CompuServe Inc., Columbus, Ohio.  All Rights Reserved		*/
/* GIFBASE.C can be copied and distributed freely for any		*/
/* non-commercial purposes. GIFBASE.C can only be incorporated		*/
/* into commercial software with the permission of CompuServe Inc.	*/
/*----------------------------------------------------------------------*/

#include "gifbase.h"

char GIFsignature[] = "GIF87a";

short
    BaseLine[5] = {0, 4, 2, 1, 0},
    LineOffset[5] = {8, 8, 4, 2, 0};
