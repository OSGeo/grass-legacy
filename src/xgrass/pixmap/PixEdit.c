/* * Last edited: Sep 28 13:31 1991 (mallet) */
/*
 * $Id: PixEdit.c,v 1.5.1.4.2.3 1991/09/28 12:06:16 mallet Exp $
 * 
 * Copyright 1991 Lionel Mallet
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Lionel MALLET not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  Lionel MALLET makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Lionel MALLET DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL Lionel MALLET BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION 
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  This software is opened and free. Furthermore, everybody is kindly
 * invited to participate to improve it for the benefit of all.
 * Improvements can be new features, bugs fixes and porting issues
 * resolution.
 *
 * Author:  Lionel Mallet, SIMULOG
 */

/*
 * $XConsortium: BitEdit.c,v 1.7 90/06/09 20:19:19 dmatic Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Davor Matic, MIT X Consortium
 */

static char rcsid[] = "$Id: PixEdit.c,v 1.5.1.4.2.3 1991/09/28 12:06:16 mallet Exp $";


#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>
#include <X11/extensions/shape.h>
#ifndef USE_ATHENA
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/ToggleBG.h>
#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#define CHARSET XmSTRING_DEFAULT_CHARSET
#else USE_ATHENA
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/bitmaps/xlogo16>
#endif USE_ATHENA
#define Down_width 30
#define Down_height 30
static char Down_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02, 0x00, 0x00, 0x18, 0x03, 0x00,
   0x00, 0xb8, 0x03, 0x00, 0x00, 0xf8, 0x03, 0x00, 0x00, 0x58, 0x03, 0x00,
   0x00, 0xb8, 0x03, 0x00, 0x00, 0x58, 0x03, 0x00, 0x00, 0xb8, 0x03, 0x00,
   0x00, 0x58, 0x03, 0x00, 0x00, 0xb8, 0x03, 0x00, 0x00, 0x58, 0x03, 0x00,
   0x00, 0xb8, 0x03, 0x00, 0x00, 0x58, 0x03, 0x00, 0x00, 0xb8, 0x03, 0x00,
   0x00, 0x58, 0x03, 0x00, 0xf0, 0xb8, 0xe3, 0x01, 0xf0, 0x59, 0xf3, 0x01,
   0xb0, 0xbb, 0xbb, 0x01, 0x70, 0x5f, 0xdf, 0x01, 0xe0, 0xbe, 0xef, 0x00,
   0xc0, 0x5d, 0x77, 0x00, 0x80, 0xab, 0x3a, 0x00, 0x00, 0x57, 0x1d, 0x00,
   0x00, 0xae, 0x0e, 0x00, 0x00, 0x5c, 0x07, 0x00, 0x00, 0xb8, 0x03, 0x00,
   0x00, 0xf0, 0x01, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00};
#define DownM_width 15
#define DownM_height 15
static char DownM_bits[] = {
   0x00, 0x00, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xc0, 0x01, 0xfe, 0x3f, 0xac, 0x1a, 0x58, 0x0d, 0xb0, 0x06, 0x60, 0x03,
   0xc0, 0x01, 0x80, 0x00, 0x00, 0x00};
#define FlipHoriz_width 30
#define FlipHoriz_height 30
static char FlipHoriz_bits[] = {
   0x00, 0x80, 0x07, 0x00, 0x00, 0x80, 0x0f, 0x00, 0x00, 0x80, 0x1d, 0x00,
   0x00, 0x80, 0x3b, 0x00, 0x00, 0x00, 0x77, 0x00, 0x00, 0x00, 0xee, 0x00,
   0x00, 0x00, 0xdc, 0x01, 0xc0, 0xff, 0xbf, 0x03, 0xe0, 0xff, 0x5f, 0x07,
   0xb0, 0xaa, 0xaa, 0x0e, 0x58, 0x55, 0x55, 0x1d, 0xac, 0xaa, 0xaa, 0x0e,
   0xfe, 0xff, 0x5f, 0x07, 0xff, 0xff, 0xbf, 0x03, 0x6b, 0x00, 0xdc, 0x01,
   0x77, 0x00, 0xee, 0x00, 0x6b, 0x00, 0x77, 0x00, 0x77, 0x80, 0x3b, 0x00,
   0x6b, 0x80, 0x1d, 0x00, 0x77, 0x80, 0x0f, 0x00, 0x6b, 0x80, 0x07, 0x00,
   0xff, 0x1f, 0x00, 0x00, 0xfe, 0x0f, 0x00, 0x00, 0xac, 0x06, 0x00, 0x00,
   0x58, 0x03, 0x00, 0x00, 0xb0, 0x06, 0x00, 0x00, 0xe0, 0x0f, 0x00, 0x00,
   0xc0, 0x1f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define FlipHorizM_width 15
#define FlipHorizM_height 15
static char FlipHorizM_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0xff, 0x07, 0x56, 0x0d, 0xac, 0x1a, 0x58, 0x35,
   0xb0, 0x6a, 0xe0, 0x7f, 0xb0, 0x6a, 0x58, 0x35, 0xac, 0x1a, 0x56, 0x0d,
   0xff, 0x07, 0x00, 0x00, 0x00, 0x00};
#define FlipVert_width 30
#define FlipVert_height 30
static char FlipVert_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00,
   0x00, 0x1f, 0x00, 0x00, 0x80, 0x3b, 0x00, 0x00, 0xc0, 0x75, 0x00, 0x00,
   0xe0, 0xea, 0x00, 0x00, 0x70, 0xd5, 0x01, 0x00, 0xb8, 0xaa, 0x03, 0x00,
   0xdc, 0x75, 0x07, 0x00, 0xee, 0xfb, 0x0e, 0x00, 0xf7, 0xf5, 0x1d, 0x00,
   0xbb, 0xbb, 0x1b, 0x00, 0x9f, 0x35, 0x1f, 0x00, 0x8f, 0x3b, 0x1e, 0x00,
   0x80, 0x35, 0x00, 0x00, 0x80, 0x3b, 0x00, 0x00, 0x80, 0x35, 0x20, 0x08,
   0x80, 0x3b, 0x60, 0x0c, 0x80, 0x35, 0xe0, 0x0e, 0x80, 0x3b, 0xe0, 0x0f,
   0x80, 0x35, 0x60, 0x0d, 0x80, 0x3b, 0xe0, 0x0e, 0x80, 0xf5, 0x7f, 0x0d,
   0x00, 0xfb, 0xff, 0x06, 0x00, 0xb6, 0x6a, 0x03, 0x00, 0x7c, 0xf5, 0x01,
   0x00, 0xb8, 0xea, 0x00, 0x00, 0xf0, 0x7f, 0x00, 0x00, 0xe0, 0x3f, 0x00};
#define FlipVertM_width 15
#define FlipVertM_height 15
static char FlipVertM_bits[] = {
   0x04, 0x10, 0x0c, 0x18, 0x1c, 0x1c, 0x34, 0x16, 0x6c, 0x1b, 0xd4, 0x15,
   0xac, 0x1a, 0xd4, 0x15, 0xac, 0x1a, 0xd4, 0x15, 0xac, 0x1a, 0xd8, 0x0d,
   0xb0, 0x06, 0xe0, 0x03, 0xc0, 0x01};
#define Fold_width 30
#define Fold_height 30
static char Fold_bits[] = {
   0xff, 0x3f, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0x57, 0xf5, 0xab, 0x3a,
   0xab, 0xff, 0x7f, 0x35, 0xd7, 0x3f, 0xff, 0x3a, 0xab, 0x03, 0x70, 0x35,
   0x57, 0x07, 0xb8, 0x3a, 0xbb, 0x0e, 0x5c, 0x37, 0x7f, 0x1d, 0xae, 0x3f,
   0xfb, 0x3a, 0xd7, 0x37, 0xdf, 0xf5, 0xeb, 0x3e, 0x9b, 0xeb, 0x75, 0x36,
   0x1f, 0xd7, 0x3a, 0x3e, 0x1f, 0xae, 0x1d, 0x3e, 0x0e, 0xfc, 0x0e, 0x1c,
   0x0e, 0xdc, 0x0f, 0x1c, 0x1f, 0x6e, 0x1d, 0x3e, 0x1f, 0xd7, 0x3a, 0x3e,
   0x9b, 0xeb, 0x75, 0x36, 0xdf, 0xf5, 0xeb, 0x3e, 0xfb, 0x3a, 0xd7, 0x37,
   0x7f, 0x1d, 0xae, 0x3f, 0xbb, 0x0e, 0x5c, 0x37, 0x57, 0x07, 0xb8, 0x3a,
   0xab, 0x03, 0x70, 0x35, 0xd7, 0x3f, 0xff, 0x3a, 0xab, 0xff, 0x7f, 0x35,
   0x57, 0xf5, 0xab, 0x3a, 0xff, 0xff, 0xff, 0x3f, 0xff, 0x3f, 0xff, 0x3f};
#define FoldM_width 15
#define FoldM_height 15
static char FoldM_bits[] = {
   0x3f, 0x3f, 0x1f, 0x3e, 0x0f, 0x3c, 0x1f, 0x3e, 0x3b, 0x37, 0xf1, 0x23,
   0xe0, 0x01, 0xe0, 0x01, 0xf1, 0x23, 0x3b, 0x37, 0x1f, 0x3e, 0x0f, 0x3c,
   0x1f, 0x3e, 0x3f, 0x3f, 0x00, 0x00};
#define Left_width 30
#define Left_height 30
static char Left_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,
   0x00, 0x37, 0x00, 0x00, 0x80, 0x3b, 0x00, 0x00, 0xc0, 0x1d, 0x00, 0x00,
   0xe0, 0x0e, 0x00, 0x00, 0x70, 0x07, 0x00, 0x00, 0xb8, 0xff, 0xff, 0x1f,
   0x5c, 0xff, 0xff, 0x0f, 0xae, 0xaa, 0xaa, 0x06, 0x57, 0x55, 0x55, 0x03,
   0xae, 0xaa, 0xaa, 0x06, 0x5c, 0xff, 0xff, 0x0f, 0xb8, 0xff, 0xff, 0x1f,
   0x70, 0x07, 0x00, 0x00, 0xe0, 0x0e, 0x00, 0x00, 0xc0, 0x1d, 0x00, 0x00,
   0x80, 0x3b, 0x00, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,
   0x00, 0x3c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define LeftM_width 15
#define LeftM_height 15
static char LeftM_bits[] = {
   0x00, 0x00, 0x80, 0x00, 0xc0, 0x00, 0xe0, 0x00, 0xb0, 0x00, 0xd8, 0x00,
   0xac, 0x3f, 0xd6, 0x3f, 0xac, 0x3f, 0xd8, 0x00, 0xb0, 0x00, 0xe0, 0x00,
   0xc0, 0x00, 0x80, 0x00, 0x00, 0x00};
#define Right_width 30
#define Right_height 30
static char Right_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00,
   0x00, 0x80, 0x0f, 0x00, 0x00, 0x80, 0x1d, 0x00, 0x00, 0x80, 0x3b, 0x00,
   0x00, 0x00, 0x77, 0x00, 0x00, 0x00, 0xee, 0x00, 0x00, 0x00, 0xdc, 0x01,
   0xff, 0xff, 0xbf, 0x03, 0xfe, 0xff, 0x5f, 0x07, 0xac, 0xaa, 0xaa, 0x0e,
   0x58, 0x55, 0x55, 0x1d, 0xac, 0xaa, 0xaa, 0x0e, 0xfe, 0xff, 0x5f, 0x07,
   0xff, 0xff, 0xbf, 0x03, 0x00, 0x00, 0xdc, 0x01, 0x00, 0x00, 0xee, 0x00,
   0x00, 0x00, 0x77, 0x00, 0x00, 0x80, 0x3b, 0x00, 0x00, 0x80, 0x1d, 0x00,
   0x00, 0x80, 0x0f, 0x00, 0x00, 0x80, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define RightM_width 15
#define RightM_height 15
static char RightM_bits[] = {
   0x00, 0x00, 0x80, 0x00, 0x80, 0x01, 0x80, 0x03, 0x80, 0x06, 0x80, 0x0d,
   0xfe, 0x1a, 0xfe, 0x35, 0xfe, 0x1a, 0x80, 0x0d, 0x80, 0x06, 0x80, 0x03,
   0x80, 0x01, 0x80, 0x00, 0x00, 0x00};
#define RotateLeft_width 30
#define RotateLeft_height 30
static char RotateLeft_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x7c, 0x00, 0x00,
   0x00, 0x6e, 0x00, 0x00, 0x00, 0x77, 0x00, 0x00, 0x80, 0x3b, 0x00, 0x00,
   0xc0, 0x1d, 0x00, 0x00, 0xe0, 0x0e, 0x00, 0x00, 0x70, 0xff, 0x0f, 0x00,
   0xb8, 0xfe, 0x1f, 0x00, 0x5c, 0x55, 0x35, 0x00, 0xae, 0xaa, 0x6a, 0x00,
   0x5c, 0x55, 0xd5, 0x00, 0xb8, 0xfe, 0xff, 0x01, 0x70, 0xff, 0xff, 0x03,
   0xe0, 0x0e, 0x58, 0x03, 0xc0, 0x1d, 0xb8, 0x03, 0x80, 0x3b, 0x58, 0x03,
   0x00, 0x77, 0xb8, 0x03, 0x00, 0x6e, 0x58, 0x03, 0x00, 0x7c, 0xb8, 0x03,
   0x00, 0x78, 0x58, 0x03, 0x00, 0x00, 0xb8, 0x03, 0x00, 0x00, 0x58, 0x03,
   0x00, 0x00, 0xf8, 0x03, 0x00, 0x00, 0xb8, 0x03, 0x00, 0x00, 0x18, 0x03,
   0x00, 0x00, 0x08, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define RotateLeftM_width 15
#define RotateLeftM_height 15
static char RotateLeftM_bits[] = {
   0x00, 0x00, 0x80, 0x00, 0xc0, 0x00, 0xe0, 0x00, 0xb0, 0x00, 0xd8, 0x00,
   0xac, 0x0f, 0xd6, 0x18, 0xac, 0x3f, 0xd8, 0x38, 0xb0, 0x38, 0xe0, 0x38,
   0xc0, 0x38, 0x80, 0x38, 0x00, 0x00};
#define RotateRight_width 30
#define RotateRight_height 30
static char RotateRight_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00, 0x00, 0x80, 0x0f, 0x00,
   0x00, 0x80, 0x1d, 0x00, 0x00, 0x80, 0x3b, 0x00, 0x00, 0x00, 0x77, 0x00,
   0x00, 0x00, 0xee, 0x00, 0x00, 0x00, 0xdc, 0x01, 0x00, 0xfc, 0xbf, 0x03,
   0x00, 0xfe, 0x5f, 0x07, 0x00, 0xab, 0xaa, 0x0e, 0x80, 0x55, 0x55, 0x1d,
   0xc0, 0xaa, 0xaa, 0x0e, 0xe0, 0xff, 0x5f, 0x07, 0xf0, 0xff, 0xbf, 0x03,
   0xb0, 0x06, 0xdc, 0x01, 0x70, 0x07, 0xee, 0x00, 0xb0, 0x06, 0x77, 0x00,
   0x70, 0x87, 0x3b, 0x00, 0xb0, 0x86, 0x1d, 0x00, 0x70, 0x87, 0x0f, 0x00,
   0xb0, 0x86, 0x07, 0x00, 0x70, 0x07, 0x00, 0x00, 0xb0, 0x06, 0x00, 0x00,
   0xf0, 0x07, 0x00, 0x00, 0x70, 0x07, 0x00, 0x00, 0x30, 0x06, 0x00, 0x00,
   0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define RotateRightM_width 15
#define RotateRightM_height 15
static char RotateRightM_bits[] = {
   0x00, 0x00, 0x80, 0x00, 0x80, 0x01, 0x80, 0x03, 0x80, 0x06, 0x80, 0x0d,
   0xf8, 0x1a, 0x8c, 0x35, 0xfe, 0x1a, 0x8e, 0x0d, 0x8e, 0x06, 0x8e, 0x03,
   0x8e, 0x01, 0x8e, 0x00, 0x00, 0x00};
#define Stipple_width 4
#define Stipple_height 4
static char Stipple_bits[] = {
   0x00, 0x00, 0x03, 0x03};
#define Up_width 30
#define Up_height 30
static char Up_bits[] = {
   0x00, 0x80, 0x00, 0x00, 0x00, 0xc0, 0x01, 0x00, 0x00, 0xe0, 0x03, 0x00,
   0x00, 0x70, 0x07, 0x00, 0x00, 0xb8, 0x0e, 0x00, 0x00, 0x5c, 0x1d, 0x00,
   0x00, 0xae, 0x3a, 0x00, 0x00, 0x57, 0x75, 0x00, 0x80, 0xbb, 0xee, 0x00,
   0xc0, 0x7d, 0xdf, 0x01, 0xe0, 0xbe, 0xbe, 0x03, 0x60, 0x77, 0x77, 0x03,
   0xe0, 0xb3, 0xe6, 0x03, 0xe0, 0x71, 0xc7, 0x03, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0x70, 0x07, 0x00, 0x00, 0xb0, 0x06, 0x00, 0x00, 0x70, 0x07, 0x00,
   0x00, 0xb0, 0x06, 0x00, 0x00, 0x70, 0x07, 0x00, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0x70, 0x07, 0x00, 0x00, 0xb0, 0x06, 0x00, 0x00, 0x70, 0x07, 0x00,
   0x00, 0xb0, 0x06, 0x00, 0x00, 0xf0, 0x07, 0x00, 0x00, 0x70, 0x07, 0x00,
   0x00, 0x30, 0x06, 0x00, 0x00, 0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00};
#define UpM_width 15
#define UpM_height 15
static char UpM_bits[] = {
   0x00, 0x00, 0x80, 0x00, 0xc0, 0x01, 0x60, 0x03, 0xb0, 0x06, 0x58, 0x0d,
   0xac, 0x1a, 0xfe, 0x3f, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xc0, 0x01, 0xc0, 0x01, 0x00, 0x00};
#include "Pixmap.h"
#include "Version.h"
#define TOGGLE True
#define BUTTON False

static char *usage = "[-options ...]\n\
\n\
where options include:\n\
     -size WIDTHxHEIGHT\n\
     -squares dimension\n\
     -resize, +resize\n\
     -grid, +grid\n\
     -stippled\n\
     -stipple pixmap\n\
     -axes, +axes\n\
     -proportional, +proportional\n\
     -hl color\n\
     -fr color\n\
     -tr color\n\
     -fn/-font fontname\n\
     -filename filename\n\
     -f filename\n\
     -in filename\n\
\n\
The default WIDTHxHEIGHT is 32x32.\n";


static XrmOptionDescRec options[] = {
  { "-squares",      "*pixmap.squareSize",     XrmoptionSepArg, NULL},
  { "-resize",       "*pixmap.resize",         XrmoptionNoArg,  "False"},
  { "+resize",       "*pixmap.resize",         XrmoptionNoArg,  "True"},
  { "-grid",         "*pixmap.grid",           XrmoptionNoArg,  "False"},
  { "+grid",         "*pixmap.grid",           XrmoptionNoArg,  "True"},
  { "-stippled",     "*pixmap.stippled",       XrmoptionNoArg,  "False"},
  { "-stipple",      "*pixmap.pixmap*stipple", XrmoptionSepArg, NULL},
  { "-axes",         "*pixmap.axes",           XrmoptionNoArg,  "False"},
  { "+axes",         "*pixmap.axes",           XrmoptionNoArg,  "True"},
  { "-proportional", "*pixmap.proportional",   XrmoptionNoArg,  "False"},
  { "+proportional", "*pixmap.proportional",   XrmoptionNoArg,  "True"},
  { "-hl",           "*pixmap.highlight",      XrmoptionSepArg, NULL},
  { "-fr",           "*pixmap.framing",        XrmoptionSepArg, NULL},
  { "-tr",           "*pixmap.transparent",    XrmoptionSepArg, NULL},
  { "-fn",           "*pixmap.font",           XrmoptionSepArg, "fixed"},
  { "-font",         "*pixmap.font",           XrmoptionSepArg, "fixed"},
  { "-filename",     "*pixmap.filename",       XrmoptionSepArg, NULL},
  { "-f",            "*pixmap.filename",       XrmoptionSepArg, NULL},
  { "-in",           "*pixmap.filename",       XrmoptionSepArg, NULL},
};

/* Color is the maximal id usable for menu entries different from color */
#define Color 200
#define Color_offset Color

typedef struct {
  int             id;
  String          name;
  Boolean         trap;
  char            *bits;
  int             width; 
  int             height;
  Widget          widget;
  } ButtonRec;

static ButtonRec file_menu[] = {
#define Load 26
  {Load, "load", BUTTON, NULL, 0, 0},
#define Insert 101
  {Insert, "insert", BUTTON, NULL, 0, 0},
#define Save 27
  {Save, "save", BUTTON, NULL, 0, 0},
#define SaveAs 28
  {SaveAs, "saveAs", BUTTON, NULL, 0, 0},
#define Dummy -1
  {Dummy, "line", False, NULL, 0, 0},
#define Resize 24
  {Resize, "resize", BUTTON, NULL, 0, 0},
#define Rescale 79
  {Rescale, "rescale", BUTTON, NULL, 0, 0},
#define Filename 74
  {Filename, "filename", BUTTON, NULL, 0, 0},
#define Hints 106
  {Hints, "hintsCmt", BUTTON, NULL, 0, 0},
#define Colors 107
  {Colors, "colorsCmt", BUTTON, NULL, 0, 0},
#define Pixels 108
  {Pixels, "pixelsCmt", BUTTON, NULL, 0, 0},    
  {Dummy, "line", False, NULL, 0, 0},
#define Quit 75
  {Quit, "quit", BUTTON, NULL, 0, 0},
};

static ButtonRec edit_menu[] = {
#define Image 77
  {Image, "image", TOGGLE, NULL, 0, 0},
  {Dummy, "line", False, NULL, 0, 0},
#define ColorPixel 109
  {ColorPixel, "addColor", BUTTON, NULL, 0, 0},
#define ColorSname 110
  {ColorSname, "symbolicName", BUTTON, NULL, 0, 0},
#define ColorMname 111
  {ColorMname, "monochromeName", BUTTON, NULL, 0, 0},
#define ColorG4name 112
  {ColorG4name, "g4Name", BUTTON, NULL, 0, 0},
#define ColorGname 113
  {ColorGname, "gName", BUTTON, NULL, 0, 0},
#define ColorCname 114
  {ColorCname, "cName", BUTTON, NULL, 0, 0},
  {Dummy, "line", False, NULL, 0, 0},
#define Grid 23
  {Grid, "grid", TOGGLE, NULL, 0, 0},
#define Axes 34
  {Axes, "axes", TOGGLE, NULL, 0, 0},
#define Proportional 97
  {Proportional, "proportional", TOGGLE, NULL, 0, 0},
#define Zoom 100
  {Zoom, "zoom", TOGGLE, NULL, 0, 0},
/* Dummy */
  {Dummy, "line", False, NULL, 0, 0},
#define Cut 30
  {Cut, "cut", BUTTON, NULL, 0, 0},
#define Copy 31
  {Copy, "copy", BUTTON, NULL, 0, 0},
#define Paste 4
  {Paste, "paste", BUTTON, NULL, 0, 0},
};

static ButtonRec buttons[] = {
/*#define Clear 1*/
  {Clear, "clear", BUTTON, NULL, 0, 0},
/*#define Set 2*/
  {Set, "set", BUTTON, NULL, 0, 0},
#define Redraw 5
  {Redraw, "redraw", BUTTON, NULL, 0, 0},
#define CopyImm 102
  {CopyImm, "copy", TOGGLE, NULL, 0, 0},
#define MoveImm 103
  {MoveImm, "move", TOGGLE, NULL, 0, 0},
#define MarkImm 104
  {MarkImm, "mark", TOGGLE, NULL, 0, 0},
#define UnmarkImm 105
  {UnmarkImm, "unmark", BUTTON, NULL, 0, 0},
#define FlipHoriz 11
  {FlipHoriz, "flipHoriz", BUTTON, FlipHorizM_bits, FlipHorizM_width, FlipHorizM_height},
#define Up 7
  {Up, "up", BUTTON, UpM_bits, UpM_width, UpM_height},
#define FlipVert 12
  {FlipVert, "flipVert", BUTTON, FlipVertM_bits, FlipVertM_width, FlipVertM_height},
#define Left 9
  {Left, "left", BUTTON, LeftM_bits, LeftM_width, LeftM_height},
#define Fold 99
  {Fold, "fold", BUTTON, FoldM_bits, FoldM_width, FoldM_height},
#define Right 10
  {Right, "right", BUTTON, RightM_bits, RightM_width, RightM_height},
#define RotateLeft 33
  {RotateLeft, "rotateLeft", BUTTON, RotateLeftM_bits, RotateLeftM_width, RotateLeftM_height},
#define Down 8
  {Down, "down", BUTTON, DownM_bits, DownM_width, DownM_height},
#define RotateRight 13
  {RotateRight, "rotateRight", BUTTON, RotateRightM_bits, RotateRightM_width, RotateRightM_height},
#define Point 14
  {Point, "point", TOGGLE, NULL, 0, 0},
#define Curve 41
  {Curve, "curve", TOGGLE, NULL, 0, 0},
#define Line 15
  {Line, "line", TOGGLE, NULL, 0, 0},
#define Rectangle 16
  {Rectangle, "rectangle", TOGGLE, NULL, 0, 0},
#define FilledRectangle 17
  {FilledRectangle, "filledRectangle", TOGGLE, NULL, 0, 0},
#define Circle 18
  {Circle, "circle", TOGGLE, NULL, 0, 0},
#define FilledCircle 19
  {FilledCircle, "filledCircle", TOGGLE, NULL, 0, 0},
#define FloodFill 20
  {FloodFill, "floodFill", TOGGLE, NULL, 0, 0},
#define SetHotSpot 21
  {SetHotSpot, "setHotSpot", TOGGLE, NULL, 0, 0},
#define ClearHotSpot 22
  {ClearHotSpot, "clearHotSpot", BUTTON, NULL, 0, 0},
#define Undo 25
  {Undo, "undo", BUTTON, NULL, 0, 0},
};

#include "Dialog.h"

Widget 
    top_widget, 
    parent_widget,
    formy_widget,
    infoButton_widget,
    fileButton_widget, fileMenu_widget,
    editButton_widget, editMenu_widget,
    fgButton_widget, fgMenu_widget,
    status_widget, statusb_widget,
    pane_widget, 
    form_widget,
    pixmap_widget,
    image_shell,
    image_widget;

Display *dpy;
int screen;
int ncolors;
int current_color;
Colormap cmap;

/* picked up from rgb.c of Xpm lib */
#ifndef UNUSE_XPM
typedef struct {  /* rgb values and ascii names (from rgb text file) */
   int  r, g, b;  /* rgb values, range of 0 -> 65535 */
   char *name;    /* color mnemonic of rgb value */
} RgbName;
#define MAX_RGBNAMES 1024
#endif UNUSE_XPM
RgbName rgb_table[MAX_RGBNAMES];

int max_ncolors;
char *colorInMenu;
char *hints_cmt, *colors_cmt, *pixels_cmt;
PWColorInfo **colorTable;

Boolean image_visible = False;
Pixmap check_mark;
Dialog info_dialog, input_dialog, error_dialog, qsave_dialog, file_dialog;
String filename = "", format = "";
char message[80];

extern int xpmReadRgbNames();
extern char *xpmGetRgbName();

void FixMenu(), SwitchImage(), SwitchGrid(), SwitchAxes(), 
  SwitchProportional(),   SwitchZoom(), DoLoad(), DoInsert(), DoSave(), 
  DoSaveAs(), DoResize(), DoRescale(), DoFilename(), DoHintsCmt(), 
  DoColorsCmt(), DoPixelsCmt(), DoQuit(), DoAddColor(), DoSymbolicName(), 
  DoMonochromeName(), DoGrey4Name(), DoGreyName(), DoColorName(), DoCut(), 
  DoCopy(), DoPaste();

void TheCallback();

static XtActionsRec actions_table[] = {
  {"fix-menu", FixMenu},
  {"switch-image", SwitchImage},
  {"switch-grid", SwitchGrid},
  {"switch-axes", SwitchAxes},
  {"switch-proportional", SwitchProportional},
  {"switch-zoom", SwitchZoom},
  {"do-load", DoLoad},
  {"do-insert", DoInsert},
  {"do-save", DoSave},
  {"do-save-as", DoSaveAs},
  {"do-resize", DoResize},
  {"do-rescale", DoRescale},
  {"do-filename", DoFilename},
  {"do-hintscmt", DoHintsCmt},
  {"do-colorscmt", DoColorsCmt},
  {"do-pixelscmt", DoPixelsCmt},
  {"do-quit", DoQuit},
  {"do-addcolor", DoAddColor},
  {"do-symbname", DoSymbolicName},
  {"do-mononame", DoMonochromeName},
  {"do-grey4name", DoGrey4Name},
  {"do-greyname", DoGreyName},
  {"do-colorname", DoColorName},
  {"do-cut", DoCut},
  {"do-copy", DoCopy},
  {"do-paste", DoPaste}
};

static Atom protocols[2];

void client_message(w, tag, event, b)
     Widget w;
     char *tag;
     XEvent *event;
     Boolean *b;
{
  if (event->xclient.message_type == protocols[1] &&
      event->xclient.data.l[0] == protocols[0])
    /* the widget got a kill signal */
    {
      if (w == image_shell) 
	SwitchImage();
    }
}

void unsetKillfromWM(w)
     Widget w;
{
  
  /* set WM property to receive a window deletion and avoid getting killed */
  protocols[0] = XInternAtom(dpy, "WM_DELETE_WINDOW", True);
  protocols[1] = XInternAtom(dpy, "WM_PROTOCOLS", True);
  XSetWMProtocols(dpy, XtWindow(w), protocols, 1);
  /* add handler to get WM's client message */
  XtAddEventHandler(w, ClientMessage, True, 
		    (XtEventHandler)client_message, NULL);
}


FixColorEntryName(old_name, name)
     char *old_name;
     char *name;
{
  char *widget_name = XtMalloc(strlen(old_name) + 2); /* \0 + * */
  Widget w;
  Arg wargs[1];
  
  sprintf(widget_name, "*%s", old_name);
  w = XtNameToWidget(fgMenu_widget, widget_name);
  if (!w) return;
#ifndef USE_ATHENA
  XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR(name, CHARSET));
#else USE_ATHENA
  XtSetArg(wargs[0], XtNlabel, name);
#endif
  XtSetValues(w, wargs, 1);
  XtSetValues(fgButton_widget, wargs, 1);
}


AddColorNotifyProc colorToAdd(w, pixel, name)
     Widget w;
     Pixel pixel;
     char *name;
{
  Widget wi;
  int max_colors = 1<<DisplayPlanes(dpy,screen);
  
  if ((pixel < 0) || (pixel >= max_colors)) 
    printf(
    "Warning: Color pixel %d out of bounds for display (pixel range [0->%d]\n",
	   pixel, max_colors-1);
  else if (colorInMenu[pixel] == 0)
    {
      int *id = (int *)malloc(sizeof(int));
      char *menu_name;
      XColor color;
      Widget w;
      Arg wargs[1];
      int no_name = 0;
      
      *id = pixel + Color_offset;
      colorInMenu[pixel] = '1';
      color.pixel = pixel;
      XQueryColor(dpy, DefaultColormap(dpy, screen), &color);

#ifndef USE_ATHENA
      if (pixel == BlackPixel(dpy, screen))
	XtSetArg(wargs[0], XmNforeground, WhitePixel(dpy, screen));
      XtSetArg(wargs[0], XmNarmColor, pixel);
#else USE_ATHENA
      if (pixel == WhitePixel(dpy, screen))
	XtSetArg(wargs[0], XtNforeground, BlackPixel(dpy, screen));
      else XtSetArg(wargs[0], XtNforeground, pixel);
#endif

      if (!name)
	{
	  if (!(menu_name = xpmGetRgbName(rgb_table, max_ncolors,
					   (int) color.red, (int) color.green,
					   (int)color.blue)))
	    {
	      no_name++;
	      menu_name = (char *) malloc(15*sizeof(char));
	      sprintf(menu_name, "#%04X%04X%04X", 
		      color.red, color.green, color.blue);
	    }
	}
      else menu_name = name;

#ifdef DEBUG
      printf("Adding color %d %s to menu\n", pixel, name);
#endif DEBUG

      if ((color.red != 0) || (color.green != 0) || (color.blue != 0) ||
	  (color.pixel == BlackPixel(dpy, screen)))
	{
#ifndef USE_ATHENA
	  wi = XmCreatePushButtonGadget(fgMenu_widget, menu_name, wargs, 1);
	  XtAddCallback(wi, XmNactivateCallback, TheCallback, id);
	  XtManageChild(wi);
#else USE_ATHENA
	  wi = XtCreateManagedWidget(menu_name, smeBSBObjectClass, 
				    fgMenu_widget, wargs, 1);
	  XtAddCallback(wi, XtNcallback, TheCallback, id);
#endif
	}
      
      if (no_name) free(menu_name);
    }
#ifdef DEBUG
  else printf("Already used color!\n");
#endif DEBUG
}


void FixImage(w)
     Widget w;
{
    Pixmap image, image_mask;
    Window root;
    unsigned int width, height, border_width, depth;
    int x, y;

    if (!image_visible)
      {
#ifndef USE_ATHENA
	XmToggleButtonGadgetSetState(edit_menu[0].widget, False, False);
#endif
	return;
      }
#ifndef USE_ATHENA
    else XmToggleButtonGadgetSetState(edit_menu[0].widget, True, False);
#endif
    
    PWGetUnzoomedPixmap(pixmap_widget, &image, &image_mask);
    XGetGeometry(XtDisplay(image_widget), image, &root, &x, &y, 
		 &width, &height, &border_width, &depth);
    XtResizeWidget(image_widget, width, height, border_width);
    XResizeWindow(XtDisplay(image_widget), XtWindow(image_shell), 
		  width, height);
    
    XSetWindowBackgroundPixmap(XtDisplay(pixmap_widget), 
			       XtWindow(image_widget), image); 

    if (image_mask) 
      {
	XShapeCombineMask(XtDisplay(image_widget), 
			  XtWindow(image_widget), ShapeBounding,
			  0, 0, image_mask, ShapeSet);
	XShapeCombineMask(XtDisplay(pixmap_widget), 
			  XtWindow(image_shell), ShapeBounding,
			  0, 0, image_mask, ShapeSet);
      }
    else 
      {
	XShapeCombineMask(XtDisplay(image_widget), 
			  XtWindow(image_widget), ShapeBounding,
			  0, 0, None, ShapeSet);
	XShapeCombineMask(XtDisplay(pixmap_widget), 
			  XtWindow(image_shell), ShapeBounding,
			  0, 0, None, ShapeSet);
      }

    XClearWindow(XtDisplay(pixmap_widget), XtWindow(image_widget));
    XFreePixmap(XtDisplay(pixmap_widget), image);
    if (image_mask) XFreePixmap(XtDisplay(image_widget), image_mask);
}

void FixEntry(w, id)
    Widget w;
    int *id;
{
    int n;
    Arg wargs[2];


    n = 0;
    switch (*id) {
	
    case Image:
#ifndef USE_ATHENA
        XtSetArg(wargs[n], XmNset, image_visible); n++;
#else USE_ATHENA
	XtSetArg(wargs[n], XtNleftBitmap, 
		 image_visible ? check_mark : None); n++;
#endif 
	break;
	
    case Grid:
#ifndef USE_ATHENA
        XtSetArg(wargs[n], XmNset, PWQueryGrid(pixmap_widget)); n++;
#else USE_ATHENA
	XtSetArg(wargs[n], XtNleftBitmap, 
		 PWQueryGrid(pixmap_widget) ? check_mark : None); n++;
#endif
	break;

    case Axes:
#ifndef USE_ATHENA
        XtSetArg(wargs[n], XmNset, PWQueryAxes(pixmap_widget)); n++;
#else USE_ATHENA
	XtSetArg(wargs[n], XtNleftBitmap, 
		 PWQueryAxes(pixmap_widget) ? check_mark : None); n++;
#endif
	break;
	
    case Proportional:
#ifndef USE_ATHENA
        XtSetArg(wargs[n], XmNset, PWQueryProportional(pixmap_widget)); n++;
#else USE_ATHENA
	XtSetArg(wargs[n], XtNleftBitmap, 
		 PWQueryProportional(pixmap_widget) ? check_mark : None); n++;
#endif
	break;
	
    case Zoom:
#ifndef USE_ATHENA
        XtSetArg(wargs[n], XmNset, PWQueryZooming(pixmap_widget)); n++;
#else USE_ATHENA
	XtSetArg(wargs[n], XtNleftBitmap, 
		 PWQueryZooming(pixmap_widget) ? check_mark : None); n++;
#endif
	break;

    case Copy:
    case Cut:
#ifndef USE_ATHENA
	XtSetArg(wargs[n], XmNsensitive, PWQueryMarked(pixmap_widget)); n++;
#else USE_ATHENA
	XtSetArg(wargs[n], XtNsensitive, PWQueryMarked(pixmap_widget)); n++;
#endif
	break;

    case Paste:
#ifndef USE_ATHENA
	XtSetArg(wargs[n], XmNsensitive, PWQueryStored(pixmap_widget)); n++;
#else USE_ATHENA
	XtSetArg(wargs[n], XtNsensitive, PWQueryStored(pixmap_widget)); n++;
#endif
	break;

    default:
	return;
    }
    
    XtSetValues(w, wargs, n);
}

void FixMenu(w, event)
    Widget w;
    XEvent *event;
{
    int i;

    for (i = 0; i < XtNumber(edit_menu); i++)
	FixEntry(edit_menu[i].widget, &edit_menu[i].id);
}

FixStatus()
{
    int n;
    Arg wargs[2];
    String str;
    char fname[256], *fn = fname;
    char size[50];

    str = PWUnparseStatus(pixmap_widget);

    n = 0;
#ifndef USE_ATHENA
    while ((*str != ' ') || (*(str-1) == ':')) *fn++ = *str++;
    *fn = 0;
    strcpy(size, str);
    XtSetArg(wargs[n], XmNlabelString, 
	     XmStringCreateLtoR(fname, XmSTRING_DEFAULT_CHARSET)); n++;
    XtSetValues(status_widget, wargs, n);
    XtSetArg(wargs[n], XmNlabelString, 
	     XmStringCreateLtoR(size, XmSTRING_DEFAULT_CHARSET)); n++;
    XtSetValues(statusb_widget, wargs, n);
#else USE_ATHENA
    XtSetArg(wargs[n], XtNlabel, str); n++;
    XtSetValues(status_widget, wargs, n);
#endif
}

UseColorNotifyProc FixColor(w, current)
     Widget w;
     Pixel current;
{
  Arg wargs[1];

  current_color = current;
  colorTable = PWGetColorTable(w);
#ifndef USE_ATHENA
  XtSetArg(wargs[0], XmNlabelString, 
	   XmStringCreateLtoR(colorTable[current_color]->c_name, CHARSET));
#else USE_ATHENA  
  XtSetArg(wargs[0], XtNlabel, colorTable[current_color]->c_name);
#endif
  XtSetValues(fgButton_widget, wargs, 1);
}

static int zero = 0;
#define Plain  &zero,sizeof(int)

void TheCallback(w, client_data, call_data)
     Widget w;
     XPointer client_data, call_data;
{
    int   *id = (int *)client_data;
    
    switch (*id) {
      
    case Load:
      DoLoad();
      break;
      
    case Insert:
      DoInsert();
      break;
      
    case Save:
      DoSave();
      break;
      
    case SaveAs:
      DoSaveAs();
      break;
      
    case Filename:
      DoFilename();
      break;
      
    case Hints:
      break;
    case Colors:
      DoColorsCmt();
      break;
    case Pixels:
      DoPixelsCmt();
      break;
      
    case Quit:
      DoQuit();
      break;
      
    case ColorPixel:
      DoAddColor();
      break;
    case ColorSname:
      DoSymbolicName();
      break;
    case ColorMname:
      DoMonochromeName();
      break;
    case ColorG4name:
      DoGrey4Name();
      break;
    case ColorGname:
      DoGreyName();
      break;
    case ColorCname:
      DoColorName();
      break;
      
    case Image:
      SwitchImage();
      break;
      
    case Grid:
      SwitchGrid();
      break;
      
    case Axes:
      SwitchAxes();
      break;	
      
    case Proportional:
      SwitchProportional();
      break;
      
    case Zoom:
      SwitchZoom();
      break;
      
    case Resize:
      DoResize();
      break;
      
    case Rescale:
      DoRescale();
      break;
      
    case Copy:
      DoCopy();
      break;
      
    case Cut:
      DoCut();
      break;
      
    case Paste:
      DoPaste();
      break;
      
    case Clear:
      PWStoreToBuffer(pixmap_widget);
      PWClear(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case Set:
      PWStoreToBuffer(pixmap_widget);
      PWSet(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case Redraw:
      PWRedraw(pixmap_widget);
      FixImage();
      break;
      
    case CopyImm:
      PWRemoveAllRequests(pixmap_widget);
      if (PWQueryMarked(pixmap_widget)) {
	PWAddRequest(pixmap_widget, MarkRequest, False, Plain);
	PWEngageRequest(pixmap_widget, CopyRequest, True, Plain);
      }
      else {
	PWEngageRequest(pixmap_widget, MarkRequest, False, Plain);
	PWAddRequest(pixmap_widget, CopyRequest, True, Plain);
      }
      break;
      
    case MoveImm:
      PWRemoveAllRequests(pixmap_widget);
      if (PWQueryMarked(pixmap_widget)) {
	PWAddRequest(pixmap_widget, MarkRequest, False, Plain);
	PWEngageRequest(pixmap_widget, MoveRequest, True, Plain);
      }
      else {
	PWEngageRequest(pixmap_widget, MarkRequest, False, Plain);
	PWAddRequest(pixmap_widget, MoveRequest, True, Plain);
      }
      break;
      
    case MarkImm:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, MarkRequest, True, Plain);
      break;
      
    case UnmarkImm:
      PWUnmark((Widget)pixmap_widget);
      break;
      
    case Up:
      PWStoreToBuffer(pixmap_widget);
      PWUp(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case Down:
      PWStoreToBuffer(pixmap_widget);
      PWDown(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case Left:
      PWStoreToBuffer(pixmap_widget);
      PWLeft(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case Right:
      PWStoreToBuffer(pixmap_widget);
      PWRight(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case Fold:
      PWStoreToBuffer(pixmap_widget);
      PWFold(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case FlipHoriz:
      PWStoreToBuffer(pixmap_widget);
      PWFlipHoriz(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case FlipVert:
      PWStoreToBuffer(pixmap_widget);
      PWFlipVert(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case RotateRight:
      PWStoreToBuffer(pixmap_widget);
      PWRotateRight(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case RotateLeft:
      PWStoreToBuffer(pixmap_widget);
      PWRotateLeft(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      break;
      
    case Point:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, PointRequest, True, Plain);
      break;
      
    case Curve:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, CurveRequest, True, Plain);
      break;
      
    case Line:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, LineRequest, True, Plain);
      break;
      
    case Rectangle:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, RectangleRequest, True, Plain);
      break;
      
    case FilledRectangle:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, FilledRectangleRequest, True, Plain);
      break;
      
    case Circle:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, CircleRequest, True, Plain);
      break;
      
    case FilledCircle:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, FilledCircleRequest, True, Plain);
      break;
      
    case FloodFill:
      PWRemoveAllRequests(pixmap_widget);
      PWEngageRequest(pixmap_widget, FloodFillRequest, True, Plain);
      break;
      
    case SetHotSpot:
	PWRemoveAllRequests(pixmap_widget);
	PWEngageRequest(pixmap_widget, HotSpotRequest, True, Plain);
	break;
	
    case ClearHotSpot:
	PWStoreToBuffer(pixmap_widget);
	PWClearHotSpot(pixmap_widget);
	PWChangeNotify(pixmap_widget, NULL, NULL);
	PWSetChanged(pixmap_widget);
	break;

    case Undo:
      PWUndo(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      FixStatus();
      break;	
    case Color: /* Start of colors management */
    default: /* Don't change this default section: color management */
      PWSetForeground(pixmap_widget,(*id - Color_offset));
      FixColor(pixmap_widget, *id - Color_offset);
      break; 
    }
}


void DoLoad()
{
  if (PWQueryChanged(pixmap_widget)) {
    PWGetFilename(pixmap_widget, &filename);
  RetryLoadSave:
    switch (PopupDialog(qsave_dialog, "Save file before loading?",
			filename, &filename, XtGrabExclusive)) {
    case Yes:
      if (PWWriteFile(pixmap_widget, filename) 
	  != XpmPixmapSuccess) {
	sprintf(message, "Can't write file: %s", filename);
	if (PopupDialog(error_dialog, message, 
			NULL, NULL, XtGrabExclusive) == Retry) 
	  goto RetryLoadSave;
      }
      break;
      
    case Cancel:
      return;
    }
  }
  PWGetFilepath(pixmap_widget, &filename);
 RetryLoad:
  if (PopupDialog(file_dialog, "Load file:", 
		  filename, &filename, XtGrabExclusive) == Okay) {
    if (PWReadFile(pixmap_widget, filename) != XpmPixmapSuccess) {
      sprintf(message, "Can't read file: %s", filename);
      if (PopupDialog(error_dialog, message, 
		      NULL, NULL, XtGrabExclusive) == Retry)
	goto RetryLoad;
    }
    else {
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWClearChanged(pixmap_widget);
      FixStatus();
    }
  }
}

void DoInsert()
{
  PWGetFilepath(pixmap_widget, &filename);
 RetryInsert:
  if (PopupDialog(file_dialog, "Insert file:", 
		  filename, &filename, XtGrabExclusive) == Okay) {
    if (PWStoreFile(pixmap_widget, filename) != XpmPixmapSuccess) {
      sprintf(message, "Can't read file: %s", filename);
      if (PopupDialog(error_dialog, message, 
		      NULL, NULL, XtGrabExclusive) == Retry)
	goto RetryInsert;
    }
    else {
      PWEngageRequest(pixmap_widget, RestoreRequest, False, Plain);
    }
  }
}

void DoSave()
{
  PWGetFilename(pixmap_widget, &filename);
  if (PWWriteFile(pixmap_widget, filename) != 
      XpmPixmapSuccess) 
    {
      sprintf(message, "Can't write file: %s", filename);
      if (PopupDialog(error_dialog, message, 
		      NULL, NULL, XtGrabExclusive) == Retry) 
	DoSaveAs();
    }
  else {
    PWClearChanged(pixmap_widget);
  }
}

void DoSaveAs()
{
  PWGetFilename(pixmap_widget, &filename);
 RetrySave:
  if (PopupDialog(file_dialog, "Save file:", 
		  filename, &filename, XtGrabExclusive) == Okay) {
    if (PWWriteFile(pixmap_widget, filename) != XpmPixmapSuccess) {
      sprintf(message, "Can't write file: %s", filename);
      if (PopupDialog(error_dialog, message, 
		      NULL, NULL, XtGrabExclusive) == Retry)
	goto RetrySave;
    }
    else {
      PWClearChanged(pixmap_widget);
      FixStatus();
    }
  }
}

void DoFilename()
{
  PWGetFilename(pixmap_widget, &filename);
  if (PopupDialog(file_dialog, "Change filename:", 
		  filename, &filename, XtGrabExclusive) == Okay) {
    PWChangeFilename(pixmap_widget, filename);
    FixStatus();
  }
}

void DoHintsCmt()
{
  PWComments(pixmap_widget, &hints_cmt, &colors_cmt, &pixels_cmt);
  if (PopupDialog(input_dialog, "Pixmap hints comment:", 
		  (hints_cmt ? hints_cmt : ""), &hints_cmt, 
		  XtGrabExclusive) == Okay)
    PWComments(pixmap_widget, &hints_cmt, &colors_cmt, &pixels_cmt);
  if (hints_cmt) XtFree(hints_cmt);
  if (colors_cmt) XtFree(colors_cmt);
  if (pixels_cmt) XtFree(pixels_cmt);
  hints_cmt = 0;
  colors_cmt = 0;
  pixels_cmt = 0;
}

void DoColorsCmt()
{
  PWComments(pixmap_widget, &hints_cmt, &colors_cmt, &pixels_cmt);
  if (PopupDialog(input_dialog, "Pixmap colors comment:", 
		  (colors_cmt ? colors_cmt : ""), &colors_cmt, 
		  XtGrabExclusive) == Okay)
    PWComments(pixmap_widget, &hints_cmt, &colors_cmt, &pixels_cmt);
  if (hints_cmt) XtFree(hints_cmt);
  if (colors_cmt) XtFree(colors_cmt);
  if (pixels_cmt) XtFree(pixels_cmt);
  hints_cmt = 0;
  colors_cmt = 0;
  pixels_cmt = 0;
}

void DoPixelsCmt()
{
  PWComments(pixmap_widget, &hints_cmt, &colors_cmt, &pixels_cmt);
  if (PopupDialog(input_dialog, "Pixmap pixels comment:", 
		  (pixels_cmt ? pixels_cmt : ""), &pixels_cmt, 
		  XtGrabExclusive) == Okay)
    PWComments(pixmap_widget, &hints_cmt, &colors_cmt, &pixels_cmt);
  if (hints_cmt) XtFree(hints_cmt);
  if (colors_cmt) XtFree(colors_cmt);
  if (pixels_cmt) XtFree(pixels_cmt);
  hints_cmt = 0;
  colors_cmt = 0;
  pixels_cmt = 0;
}

void DoQuit()
{
  if (PWQueryChanged(pixmap_widget)) {
    PWGetFilename(pixmap_widget, &filename);
  RetryQuit:
    switch (PopupDialog(qsave_dialog, "Save file before quitting?",
			filename, &filename, XtGrabExclusive)) {
    case Yes:
      if (PWWriteFile(pixmap_widget, filename) 
	  != XpmPixmapSuccess) {
	sprintf(message, "Can't write file: %s", filename);
	if (PopupDialog(error_dialog, message, 
			NULL, NULL, XtGrabExclusive) == Retry) 
	  goto RetryQuit;
	else return;
      }
      break;
      
    case Cancel:
      return;
    }
  }
  exit(0);
}

void DoAddColor()
{
  char *cname;
  
  if (PopupDialog(input_dialog, "Color name (name or #rgb) to add:",
		  "", &cname, XtGrabExclusive) == Okay)
    {
      XColor color;
      
      if ((!XParseColor(dpy, cmap, cname, &color)) ||
	  (!XAllocColor(dpy, cmap, &color)))
	{
	  fprintf(stderr, 
		  "Warning: color %s could not be parsed/allocated!",
		  cname);
	}
      else
	{
	  Pixel transparent;
	  Arg wargs[1];
	  
	  XtSetArg(wargs[0], XtNtransparent, &transparent);
	  XtGetValues(pixmap_widget, wargs, 1);

	  colorToAdd(pixmap_widget, color.pixel, 
		     ((cname[0] == '#') ? 
		      xpmGetRgbName(rgb_table, max_ncolors, 
				    (int) color.red,
				    (int) color.green,
				    (int) color.blue) : cname));
	  if (color.pixel != transparent)
	    PWUseColorInTable(pixmap_widget, color.pixel, NULL, NULL, NULL,
			      NULL, NULL, 
			      ((cname[0] == '#') ? 
			       xpmGetRgbName(rgb_table, max_ncolors, 
					     (int) color.red,
					     (int) color.green,
					     (int) color.blue) : cname));
	}
    }
}

void DoSymbolicName()
{
  char *s_name;
  char message[80];
  
  colorTable = PWGetColorTable(pixmap_widget);
  sprintf(message, "Color `%s' [%d] symbolic name:", 
	  colorTable[current_color]->c_name, current_color);
  
  if (PopupDialog(input_dialog, message, 
		  (colorTable[current_color]->s_name ? 
		   colorTable[current_color]->s_name : ""), 
		  &s_name,
		  XtGrabExclusive) == Okay)
    PWUpdateColorInTable(pixmap_widget, 
			 current_color,
			 colorTable[current_color]->symbol,
			 s_name,
			 colorTable[current_color]->m_name,
			 colorTable[current_color]->g4_name,
			 colorTable[current_color]->g_name,
			 colorTable[current_color]->c_name);
}

void DoMonochromeName()
{
  char *m_name;
  char message[80];
  
  colorTable = PWGetColorTable(pixmap_widget);
  sprintf(message, "Color `%s' [%d] monochrome display name:", 
	  colorTable[current_color]->c_name, current_color);
  
  if (PopupDialog(input_dialog, message, 
		  (colorTable[current_color]->m_name ? 
		   colorTable[current_color]->m_name : ""), 
		  &m_name,
		  XtGrabExclusive) == Okay)
    PWUpdateColorInTable(pixmap_widget, 
			 current_color,
			 colorTable[current_color]->symbol,
			 colorTable[current_color]->s_name,
			 m_name,
			 colorTable[current_color]->g4_name,
			 colorTable[current_color]->g_name,
			 colorTable[current_color]->c_name);
}

void DoGrey4Name()
{
  char *g4_name;
  char message[80];
  
  colorTable = PWGetColorTable(pixmap_widget);
  sprintf(message, "Color `%s' [%d] grey scale 4 display name:", 
	  colorTable[current_color]->c_name, current_color);
  
  if (PopupDialog(input_dialog, message, 
		  (colorTable[current_color]->g4_name ? 
		   colorTable[current_color]->g4_name : ""), 
		  &g4_name,
		  XtGrabExclusive) == Okay)
    PWUpdateColorInTable(pixmap_widget, 
			 current_color,
			 colorTable[current_color]->symbol,
			 colorTable[current_color]->s_name,
			 colorTable[current_color]->m_name,
			 g4_name,
			 colorTable[current_color]->g_name,
			 colorTable[current_color]->c_name);
}

void DoGreyName()
{
  char *g_name;
  char message[80];
  
  colorTable = PWGetColorTable(pixmap_widget);
  sprintf(message, "Color `%s' [%d] grey scale display name:", 
	  colorTable[current_color]->c_name, current_color);
  
  if (PopupDialog(input_dialog, message, 
		  (colorTable[current_color]->g_name ? 
		   colorTable[current_color]->g_name : ""), 
		  &g_name,
		  XtGrabExclusive) == Okay)
    PWUpdateColorInTable(pixmap_widget, 
			 current_color,
			 colorTable[current_color]->symbol,
			 colorTable[current_color]->s_name,
			 colorTable[current_color]->m_name,
			 colorTable[current_color]->g4_name,
			 g_name,
			 colorTable[current_color]->c_name);
}

void DoColorName()
{
  char *c_name;
  char message[80];
  
  colorTable = PWGetColorTable(pixmap_widget);
  sprintf(message, "Color `%s' [%d] color display name:", 
	  colorTable[current_color]->c_name, current_color);
 RetryColorCname:
  if ((PopupDialog(input_dialog, message, 
		   (colorTable[current_color]->c_name ? 
		    colorTable[current_color]->c_name : ""), 
		   &c_name,
		   XtGrabExclusive) == Okay) && (c_name))
    {
      Pixel transparent;
      Arg wargs[1];
      
      XtSetArg(wargs[0], XtNtransparent, &transparent);
      XtGetValues(pixmap_widget, wargs, 1);
      if ((transparent != NotSet) && 
	  (strcasecmp(c_name, NoColorName) == 0))
	{
	  if (PopupDialog(error_dialog, "Can't have towo None colors",
			  NULL, NULL, XtGrabExclusive) == Retry)
	    goto RetryColorCname;
	}
      else
	{
	  FixColorEntryName(colorTable[current_color]->c_name, c_name);
	  PWUpdateColorInTable(pixmap_widget, 
			       current_color,
			       colorTable[current_color]->symbol,
			       colorTable[current_color]->s_name,
			       colorTable[current_color]->m_name,
			       colorTable[current_color]->g4_name,
			       colorTable[current_color]->g_name,
			       c_name);
	}
    }
}

void SwitchImage()
{
    if (image_visible) {
      XtPopdown(image_shell);
      image_visible = False;
    }
    else {
      Position image_x, image_y;
      int n;
      Arg wargs[3];
      
      XtTranslateCoords(pixmap_widget, 
			10, 10, &image_x, &image_y);
      
      n = 0;
      XtSetArg(wargs[n], XtNx, image_x); n++;
      XtSetArg(wargs[n], XtNy, image_y); n++;
      XtSetValues(image_shell, wargs, n);
      
      image_visible = True;
      
      XtPopup(image_shell, XtGrabNone);
      unsetKillfromWM(image_shell);
      FixImage();
    }
}

void SwitchGrid()
{
  PWSwitchGrid(pixmap_widget);
}

void SwitchAxes()
{
  PWSwitchAxes(pixmap_widget);
}

void SwitchProportional()
{
  PWSwitchProportional(pixmap_widget);
}

void SwitchZoom()
{
  if (PWQueryZooming(pixmap_widget)) {
    PWZoomOut(pixmap_widget);
    PWChangeNotify(pixmap_widget, NULL, NULL);
  }
  else {
    if (PWQueryMarked(pixmap_widget)) {
      PWStoreToBuffer(pixmap_widget);
      PWZoomMarked(pixmap_widget);
      PWChangeNotify(pixmap_widget, NULL, NULL);
    }
    else {
      PWEngageRequest(pixmap_widget, ZoomInRequest, False, Plain);
    }
  }
}

void DoResize()
{
  char x;
  int width, height;

  format = "";
 RetryResize:
  if (PopupDialog(input_dialog, "Resize to WIDTHxHEIGHT:", 
		  format, &format, XtGrabExclusive) == Okay) {
    sscanf(format, "%d%c%d", &width, &x, &height);
    if ((width >0) && (height > 0) && (x == 'x')) {
      PWResize(pixmap_widget, (Dimension)width, (Dimension)height);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      FixStatus();
    }
    else {
      sprintf(message, "Wrong format: %s", format);
      if (PopupDialog(error_dialog, message, 
		      NULL, NULL, XtGrabExclusive) == Retry)
	goto RetryResize;
    }
  }
}

void DoRescale()
{
  char x;
  int width, height;

  format = "";
 RetryRescale:
  if (PopupDialog(input_dialog, "Rescale to WIDTHxHEIGHT:", 
		  format,	&format, XtGrabExclusive) == Okay) {
    sscanf(format, "%d%c%d", &width, &x, &height);
    if ((width >0) && (height > 0) && (x == 'x')) {
      PWRescale(pixmap_widget, (Dimension)width, (Dimension)height);
      PWChangeNotify(pixmap_widget, NULL, NULL);
      PWSetChanged(pixmap_widget);
      FixStatus();
    }
    else {
      sprintf(message, "Wrong format: %s", format);
      if (PopupDialog(error_dialog, message, 
		      NULL, NULL, XtGrabExclusive) == Retry)
	goto RetryRescale;
    }
  }
}

void DoCut()
{
  PWStore(pixmap_widget);
  PWClearMarked(pixmap_widget);
  PWUnmark(pixmap_widget);
  PWChangeNotify(pixmap_widget, NULL, NULL);
}

void DoCopy()
{
  PWStore(pixmap_widget);
  PWUnmark(pixmap_widget);
}

void DoPaste()
{
  PWEngageRequest(pixmap_widget, RestoreRequest, False, Plain);
}

void InfoCallback()
{
    PopupDialog(info_dialog, info,
		NULL, NULL, XtGrabExclusive);
}


/* loading pixmap editors colors */
loadPixEditColors()
{
  FILE *colorfile;
  static char *fname = ".pixmap";
  char filename[256];
  Pixel transparent;
  Arg args[1];

  /* get transparent pixel from pixmap_widget */
  XtSetArg(args[0], XtNtransparent, &transparent);
  XtGetValues(pixmap_widget, args, 1);


  /* first try to open in local dir */
  if (!(colorfile = fopen(fname, "r")))
    { /* try in homedir */
      sprintf(filename, "%s/%s", getenv("HOME"), fname);
      if (!(colorfile = fopen(filename, "r")))
	{ /* try in X11 lib dir */
	  sprintf(filename, "/usr/lib/X11/app-defaults/%s", fname);
	  colorfile = fopen(filename, "r");
	}
    }
  
  if (colorfile) 
    { /* parse it, i.e., read name, try to alloc in cmap, add menu entry */
      char cname[512];
      int status;
      XColor color;
      
      while ((status = fscanf(colorfile,"%[^\n]\n", cname)) && (status != EOF))
	{
	  if (!XParseColor(dpy, cmap, cname, &color)) continue;
	  if (!XAllocColor(dpy, cmap, &color))
	    {
	      fprintf(stderr, "Warning: color %s could not be allocated!",
		      cname);
	      continue;
	    }
	  else
	    {
	      colorToAdd(pixmap_widget, color.pixel, 
			 ((cname[0] == '#') ? 
			  xpmGetRgbName(rgb_table, max_ncolors, 
					 (int) color.red,
					 (int) color.green,
					 (int) color.blue) : cname));
	      if (color.pixel != transparent)
                PWUseColorInTable(pixmap_widget, color.pixel, NULL, NULL, NULL,
				  NULL, NULL, 
				  ((cname[0] == '#') ? 
				   xpmGetRgbName(rgb_table, max_ncolors, 
						 (int) color.red,
						 (int) color.green,
						 (int) color.blue) : cname));
	    }
	}
      fclose(colorfile);
    }
  else
    { /* load min(ncolors, 30) allocated in colormap */
      int i;
      
      for (i = 0; i < (ncolors > 30 ? 30 : ncolors); i++)
	{
	  XColor color;
	  color.pixel = i;
	  XQueryColor(dpy, DefaultColormap(dpy, screen), &color);
	  
	  colorToAdd(pixmap_widget, i, xpmGetRgbName(rgb_table, max_ncolors, 
				       (int) color.red,
				       (int) color.green,
				       (int) color.blue));
          if (i != transparent)
	    PWUseColorInTable(pixmap_widget, i, NULL, NULL, NULL,
			      NULL, NULL, 
			      xpmGetRgbName(rgb_table, max_ncolors, 
					    (int) color.red,
					    (int) color.green,
					    (int) color.blue));
      }
    }
}


void main(argc, argv)
    int    argc;
    char  *argv[];
{
    int i, n;
    Arg wargs[5];
    Widget w;
#ifndef USE_ATHENA
    Widget formh;
    static int Resize_id = Resize, Filename_id = Filename;
#else USE_ATHENA
    Widget radio_group; XPointer radio_data;
#endif
    int x, y, width = 0, height = 0;
    Pixmap pixmap;
    Pixel fg, bg;
    Screen *scrptr;

    top_widget = XtInitialize(NULL, "Pixmap", 
			      options, XtNumber(options), 
			      &argc, argv);

    scrptr = XtScreen(top_widget);

    if (argc > 1) {
      if ((argc ==3) && (strcmp(argv[1], "-size") == 0))
	  XParseGeometry(argv[2], &x, &y, &width, &height);
      else
	{
	  fprintf(stderr, "%s %s", argv[0], usage);
	  exit (-1);
	}
    }

    /* some intializations */
    dpy = XtDisplay(top_widget);
    screen = DefaultScreen(dpy);
    ncolors = 1<<DisplayPlanes(dpy,screen);
#ifdef DEBUG
    printf("Num colors %d\n",ncolors);
#endif DEBUG
    cmap = DefaultColormap(dpy,screen);
    colorInMenu = (char *) malloc(ncolors*sizeof(char));
    bzero(colorInMenu, ncolors*sizeof(char));
    max_ncolors = xpmReadRgbNames(rgb_fname, rgb_table);

#ifdef USE_ATHENA
    check_mark = XCreateBitmapFromData(XtDisplay(top_widget),
				      RootWindowOfScreen(XtScreen(top_widget)),
				      xlogo16_bits, 
				      xlogo16_width, 
				      xlogo16_height);
#endif
    XtAddActions(actions_table, XtNumber(actions_table));
#ifndef USE_ATHENA
    parent_widget = XtCreateManagedWidget("parent", xmRowColumnWidgetClass,
					 top_widget, NULL, 0);

    formy_widget = XmCreateMenuBar(parent_widget, "formy", NULL, 0);
    XtManageChild(formy_widget);

    infoButton_widget = XmCreateCascadeButtonGadget(formy_widget, 
						 "infoButton", NULL, 0);
    XtManageChild(infoButton_widget);
    XtAddCallback(infoButton_widget, XmNactivateCallback, InfoCallback, NULL);
    
    fileMenu_widget = XmCreatePulldownMenu(formy_widget, "fileMenu", 
					   NULL, 0);
    XtManageChild(fileMenu_widget);
    XtSetArg(wargs[0], XmNsubMenuId, fileMenu_widget);
    fileButton_widget = XmCreateCascadeButtonGadget(formy_widget, 
						    "fileButton", wargs, 1);
    XtManageChild(fileButton_widget);

    for (i = 0; i < XtNumber(file_menu); i++) {
      if (file_menu[i].id == Dummy)
	w = XmCreateSeparatorGadget(fileMenu_widget, file_menu[i].name, 
				    NULL, 0);
      else if (file_menu[i].trap == TOGGLE)
	{
	  w = XmCreateToggleButtonGadget(fileMenu_widget, file_menu[i].name, 
					 NULL, 0);
	  XtAddCallback(w, XmNvalueChangedCallback, TheCallback, 
			&file_menu[i].id);
	}
      else 
	{
	  w = XmCreatePushButtonGadget(fileMenu_widget, file_menu[i].name, 
				   NULL, 0);
	  XtAddCallback(w, XmNactivateCallback, TheCallback, &file_menu[i].id);
	}

      file_menu[i].widget = w;
      XtManageChild(w);
    }
        
    editMenu_widget = XmCreatePulldownMenu(formy_widget, "editMenu", 
					   NULL, 0);
    XtManageChild(editMenu_widget);
    
    XtSetArg(wargs[0], XmNsubMenuId, editMenu_widget);
    editButton_widget = XmCreateCascadeButtonGadget(formy_widget, 
						    "editButton", 
						    wargs, 1);
    XtManageChild(editButton_widget);
    XtAddCallback(editButton_widget, XmNcascadingCallback, FixMenu);

    for (i = 0; i < XtNumber(edit_menu); i++) {
      if (edit_menu[i].id == Dummy)
	w = XmCreateSeparatorGadget(editMenu_widget, edit_menu[i].name, 
				    NULL, 0);
      else if (edit_menu[i].trap == TOGGLE)
	{
	  w = XmCreateToggleButtonGadget(editMenu_widget, edit_menu[i].name, 
					 NULL, 0);
	  XtAddCallback(w, XmNvalueChangedCallback, TheCallback, 
			&edit_menu[i].id);
	}
      else 
	{
	  w = XmCreatePushButtonGadget(editMenu_widget, edit_menu[i].name, 
				   NULL, 0);
	  XtAddCallback(w, XmNactivateCallback, TheCallback, &edit_menu[i].id);
	}

      edit_menu[i].widget = w;
      XtManageChild(w);
    }

    fgMenu_widget = XmCreatePulldownMenu(formy_widget, "fgMenu", 
					 NULL, 0);
    XtManageChild(fgMenu_widget);
    
    XtSetArg(wargs[0], XmNsubMenuId, fgMenu_widget);
    fgButton_widget = XmCreateCascadeButtonGadget(formy_widget, "fgButton", 
						  wargs, 1);
    XtManageChild(fgButton_widget);

    status_widget = XmCreateCascadeButtonGadget(formy_widget, "status",
				                NULL, 0);
    XtManageChild(status_widget);
    XtAddCallback(status_widget, XmNactivateCallback, TheCallback, 
                  &Filename_id);
    statusb_widget = XmCreateCascadeButtonGadget(formy_widget, "statusb",
			 	                 NULL, 0);
    XtManageChild(statusb_widget);
    XtAddCallback(statusb_widget, XmNactivateCallback, TheCallback, 
                  &Resize_id);

    pane_widget = XmCreateForm(parent_widget, "pane", NULL, 0);
    XtManageChild(pane_widget);

    form_widget = XmCreateRadioBox(pane_widget, "form", NULL, 0);
    XtSetArg(wargs[0], XmNisHomogeneous, False);
    XtSetValues(form_widget, wargs, 1);
    XtManageChild(form_widget);
        
    for (i = 0; i < XtNumber(buttons); i++) {
      if (buttons[i].trap == TOGGLE)
	{
	  w = XmCreateToggleButtonGadget(form_widget, buttons[i].name, 
					 NULL, 0);
	  XtAddCallback(w, XmNvalueChangedCallback, TheCallback, 
			&buttons[i].id);
	}
      else 
	{
	  if ((!strcmp(buttons[i].name, "flipHoriz")) ||
	      (!strcmp(buttons[i].name, "left")) ||
	      (!strcmp(buttons[i].name, "rotateLeft")))
	    {
	      formh = form_widget;
	      form_widget = XmCreateRowColumn(formh, "formh", NULL, 0);
	      XtManageChild(form_widget);
	    }
	  
          if ( buttons[i].bits != NULL ) {

              XtVaGetValues(form_widget, XmNforeground, &fg,
		      XmNbackground, &bg, NULL);

              pixmap = XCreatePixmapFromBitmapData(dpy,
                  RootWindowOfScreen(scrptr), buttons[i].bits,
                  buttons[i].width, buttons[i].height, fg, bg,
                  DefaultDepthOfScreen(scrptr));
              XtSetArg(wargs[0], XmNlabelType, XmPIXMAP);
              XtSetArg(wargs[1], XmNlabelPixmap, pixmap);
	      w = XmCreatePushButtonGadget(form_widget, buttons[i].name,  
                 wargs, 2);
          } else {
	      w = XmCreatePushButtonGadget(form_widget, buttons[i].name,NULL,0);
          }
	  XtAddCallback(w, XmNactivateCallback, TheCallback, &buttons[i].id);

	  if ((!strcmp(buttons[i].name, "flipVert")) ||
	      (!strcmp(buttons[i].name, "right")) ||
	      (!strcmp(buttons[i].name, "rotateRight")))
	    form_widget = formh;
	}

      buttons[i].widget = w;
      XtManageChild(w);
      
      if (buttons[i].id == Point) 
	XmToggleButtonGadgetSetState(buttons[i].widget, True, False);
    }
    
    n = 0;
    XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(wargs[n], XmNleftWidget, form_widget); n++;
#else USE_ATHENA
    parent_widget = XtCreateManagedWidget("parent", panedWidgetClass,
					 top_widget, NULL, 0);

    formy_widget = XtCreateManagedWidget("formy", formWidgetClass,
				       parent_widget, NULL, 0);

    infoButton_widget = XtCreateManagedWidget("infoButton",
					commandWidgetClass, 
					formy_widget, NULL, 0);

    XtAddCallback(infoButton_widget, XtNcallback, InfoCallback, NULL);
    
    fileMenu_widget = XtCreatePopupShell("fileMenu", 
					 simpleMenuWidgetClass, 
					 formy_widget, NULL, 0);
    
    fileButton_widget = XtCreateManagedWidget("fileButton",
					      menuButtonWidgetClass, 
					      formy_widget, NULL, 0);

    for (i = 0; i < XtNumber(file_menu); i++) {
	w = XtCreateManagedWidget(file_menu[i].name, 
				  ((file_menu[i].id != Dummy) ? 
				   smeBSBObjectClass : smeLineObjectClass),
				  fileMenu_widget, NULL, 0),
	XtAddCallback(w,
		      XtNcallback,
		      TheCallback,
		      &file_menu[i].id);
	
	file_menu[i].widget = w;
    }
        
    editMenu_widget = XtCreatePopupShell("editMenu", 
					 simpleMenuWidgetClass, 
					 formy_widget, NULL, 0);
    
    editButton_widget = XtCreateManagedWidget("editButton", 
					      menuButtonWidgetClass, 
					      formy_widget, NULL, 0);

    for (i = 0; i < XtNumber(edit_menu); i++) {
	w = XtCreateManagedWidget(edit_menu[i].name, 
				  ((edit_menu[i].id != Dummy) ? 
				   smeBSBObjectClass : smeLineObjectClass),
				  editMenu_widget, NULL, 0),
	XtAddCallback(w,
		      XtNcallback,
		      TheCallback,
		      &edit_menu[i].id);
	
	edit_menu[i].widget = w;
    }

    fgMenu_widget = XtCreatePopupShell("fgMenu", 
					 simpleMenuWidgetClass, 
					 formy_widget, NULL, 0);
    
    fgButton_widget = XtCreateManagedWidget("fgButton", 
					      menuButtonWidgetClass, 
					      formy_widget, NULL, 0);

    status_widget = XtCreateManagedWidget("status", labelWidgetClass,
					  formy_widget, NULL, 0);

    pane_widget = XtCreateManagedWidget("pane", panedWidgetClass,
					parent_widget, NULL, 0);

    form_widget = XtCreateManagedWidget("form", formWidgetClass, 
					pane_widget, NULL, 0);
        
    for (i = 0; i < XtNumber(buttons); i++) {
	w = XtCreateManagedWidget(buttons[i].name, 
				  ((buttons[i].trap == TOGGLE) ? 
				   toggleWidgetClass : commandWidgetClass),
				  form_widget, NULL, 0);

	XtAddCallback(w,
		      XtNcallback,
		      TheCallback,
		      &buttons[i].id);

	buttons[i].widget = w;

	if (buttons[i].id == Point) {
	    radio_group = buttons[i].widget;
	    radio_data  = buttons[i].name;
	}
    }
    
    n = 0;
#endif
    XtSetArg(wargs[n], XtNaddColorNtfyProc, colorToAdd); n++;
    if (width) XtSetArg(wargs[n], XtNpixmapWidth, width), n++;
    if (height) XtSetArg(wargs[n], XtNpixmapHeight, height), n++;

    XtVaGetValues(top_widget, XmNforeground, &fg,
	  XmNbackground, &bg, NULL);

    pixmap = XCreatePixmapFromBitmapData(dpy,
      RootWindowOfScreen(scrptr), Stipple_bits,
      Stipple_width, Stipple_height, BlackPixel(dpy, screen), 
      WhitePixel(dpy, screen), 1);

    XtSetArg(wargs[n], XtNstipple, pixmap), n++;
    pixmap_widget = XtCreateManagedWidget("pixmap", pixmapWidgetClass,
					  pane_widget, wargs, n);
    XtSetKeyboardFocus(top_widget, pixmap_widget);
    
    PWSetForeground(pixmap_widget,BlackPixel(dpy, screen));
    current_color = BlackPixel(dpy, screen);
    
    XtRealizeWidget(top_widget);

    loadPixEditColors();
    
    image_shell = XtCreatePopupShell("image", transientShellWidgetClass,
				     top_widget, NULL, 0);
#ifndef USE_ATHENA
    XtSetArg(wargs[0], XmNlabelString, 
	     XmStringCreateLtoR("", XmSTRING_DEFAULT_CHARSET));
    image_widget = XmCreateLabel(image_shell, "label", wargs, 1);
    XtManageChild(image_widget);
#else USE_ATHENA
    image_widget = XtCreateManagedWidget("label", labelWidgetClass,
					 image_shell, NULL, 0);
    XtRealizeWidget(image_shell);
#endif

    Notify(pixmap_widget, FixImage);
    ColorNotify(pixmap_widget, FixColor);

    FixStatus();
    
    info_dialog = CreateDialog(top_widget, "info", Okay);
    input_dialog = CreateDialog(top_widget, "input", Okay | Cancel);
    error_dialog = CreateDialog(top_widget, "error", Abort | Retry);    
    qsave_dialog = CreateDialog(top_widget, "qsave", Yes | No | Cancel);
    file_dialog = CreateDialog(top_widget, "file", Okay | Cancel);

#ifdef USE_ATHENA
    XawToggleSetCurrent(radio_group, radio_data);
#endif
    PWEngageRequest(pixmap_widget, PointRequest, True, Plain);

    XtMainLoop();
}



