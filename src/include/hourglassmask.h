/**********************************************************************
 hourglassmask.h - the hour glass cursor mask bitmap
 **********************************************************************/
#ifndef HOURGLASSMASK_H
#define HOURGLASSMASK_H
#define hourglassmask_width 16
#define hourglassmask_height 16
#define hourglassmask_x_hot 7
#define hourglassmask_y_hot 7
#ifdef __STDC__
typedef unsigned char HOURGLASSMASK_H_TYPE;
#else
typedef char HOURGLASSMASK_H_TYPE;
#endif
static HOURGLASSMASK_H_TYPE hourglassmask_bits[] = {
   0xff, 0xff, 0xff, 0xff, 0xfe, 0x7f, 0xfc, 0x3f, 0xf8, 0x1f, 0xf0, 0x0f,
   0xe0, 0x07, 0xc0, 0x03, 0xc0, 0x03, 0xe0, 0x07, 0xf0, 0x0f, 0xf8, 0x1f,
   0xfc, 0x3f, 0xfe, 0x7f, 0xff, 0xff, 0xff, 0xff};
#endif
