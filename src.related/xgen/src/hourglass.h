/**********************************************************************
 hourglass.h - the hour glass cursor bitmap
 **********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#define hourglass_width 16
#define hourglass_height 16
#define hourglass_x_hot 7
#define hourglass_y_hot 7
#ifdef DOEXEC
static unsigned char            hourglass_bits[] = {
    0x00, 0x00, 0xfe, 0x7f, 0x04, 0x20, 0x08, 0x10, 0x30, 0x0c, 0xe0, 0x07,
    0xc0, 0x03, 0x80, 0x01, 0x80, 0x01, 0x40, 0x02, 0x20, 0x04, 0x90, 0x09,
0xc8, 0x13, 0xf4, 0x2f, 0xfe, 0x7f, 0x00, 0x00};
#endif
