/* libpbm5.c - pbm utility library part 5
**
** Font routines.
**
** Copyright (C) 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "pbm.h"
#include "pbmfont.h"

/* The default font, packed in hex so this source file doesn't get huge.
** You can replace this with your own font using pbm_dumpfont().
*/
#define DEFAULTFONT_ROWS 155
#define DEFAULTFONT_COLS 112
static unsigned long defaultfont_bits[DEFAULTFONT_ROWS][(DEFAULTFONT_COLS+31)/32] = {
    {0x00000000,0x20000c00,0x10000000,0x00000000},
    {0xc600a000,0x42000810,0x00000002,0x00000063},
    {0x6c00a000,0x45000810,0x00000002,0x00000036},
    {0x6c00a000,0x88800808,0xf2e1dee2,0x00000036},
    {0x54000000,0x80000800,0x11122442,0x0000002a},
    {0x54000001,0x00000800,0x11122442,0x0000002a},
    {0x54000001,0x00000800,0x11122282,0x0000002a},
    {0x44000102,0x00000800,0x11122382,0x00000022},
    {0xee000102,0x00000800,0x11e1e102,0x00000077},
    {0x00000204,0x00000800,0x11002102,0x00000000},
    {0x00000000,0x00000c00,0x11002102,0x00000000},
    {0x00000000,0x003f8000,0xe3807600,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x02000080,0x00040000,0x00120000,0x00000001},
    {0x04000082,0x828e1838,0x20210100,0x00000002},
    {0x04000082,0x82912448,0x20210100,0x00000002},
    {0x08000082,0x8fd01940,0x404087c2,0x00000004},
    {0x08000080,0x050c0622,0x00408102,0x00000004},
    {0x10000080,0x05061874,0x0040828f,0x00008008},
    {0x10000080,0x1f912688,0x00408002,0x00000008},
    {0x20000000,0x0a11098c,0x00408002,0x00000010},
    {0x20000080,0x0a0e0672,0x00210000,0x00000010},
    {0x40000000,0x00040000,0x00210000,0x00000020},
    {0x00000000,0x00000000,0x00120000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x004e0838,0x7023e1cf,0x00008000},
    {0x00000000,0x00913844,0x88620208,0x00008000},
    {0x08000000,0x00910844,0x08a20401,0x00000004},
    {0x10000000,0x01110844,0x08a20401,0x00000008},
    {0x20000000,0x01110808,0x3123c781,0x00000010},
    {0x400003e0,0x02110810,0x0a202441,0x00000020},
    {0x20000000,0x02110820,0x0bf02442,0x00000010},
    {0x10008000,0x04110844,0x88242442,0x00000008},
    {0x08008002,0x040e3e7c,0x7073c382,0x00000004},
    {0x00010000,0x08000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x0000e1c0,0x00000000,0x00000000,0x00000000},
    {0x00011220,0x00000000,0x70e38f87,0x00000000},
    {0x20011220,0x00020020,0x89108448,0x00008010},
    {0x10011220,0x00040010,0x09314448,0x00008008},
    {0x0800e221,0x02083e08,0x11514788,0x00000004},
    {0x040111e0,0x00100004,0x2153e448,0x00000002},
    {0x08011020,0x00083e08,0x213a2448,0x00008004},
    {0x10011040,0x02040010,0x01022448,0x00008008},
    {0x2000e381,0x02020020,0x20e77f87,0x00000010},
    {0x00000000,0x04000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x3803e7ef,0xc73bbe3d,0xdb863ce7,0x0000001c},
    {0x44011224,0x48910808,0x91036648,0x00008022},
    {0x4c011285,0x48910808,0xa1036648,0x00008026},
    {0x54011387,0x081f0808,0xc102a548,0x0000802a},
    {0x54011285,0x09910808,0xe102a548,0x0000802a},
    {0x4e011204,0x08910848,0x9112a4c8,0x00008027},
    {0x40011224,0x08910848,0x891224c8,0x00008020},
    {0x3803e7ef,0x073bbe31,0xcff77e47,0x0000001c},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000003,0x00000000},
    {0x0003e1cf,0x87bff7ef,0xdfbf77c2,0x00000000},
    {0x00013224,0x48a4a244,0x89122442,0x00000000},
    {0x00011224,0x4824a244,0xa8a14482,0x00000000},
    {0x00013227,0x8e04226c,0xa8414102,0x00000000},
    {0x0001e224,0x83842228,0xa8a08102,0x00000000},
    {0x00010224,0x40842228,0xd8a08242,0x00000000},
    {0x00010224,0x48843638,0x51108442,0x00000000},
    {0x0003c1ce,0x6f1f1c10,0x53b9c7c2,0x00000000},
    {0x00000060,0x00000000,0x00000002,0x00000000},
    {0x00000000,0x00000000,0x00000003,0x00000000},
    {0xfe000000,0x00000000,0x00000000,0x0000007f},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00010180,0x000000c0,0x003001c0,0x00000000},
    {0x08008081,0x00040040,0x00100200,0x00000004},
    {0x10008082,0x80040040,0x00100200,0x00000008},
    {0x10004084,0x40023c78,0x70f1c7c7,0x00004008},
    {0x10004080,0x00000244,0x89122208,0x00008008},
    {0x20002080,0x00001e44,0x8113e208,0x00008010},
    {0x10002080,0x00002244,0x81120208,0x00008008},
    {0x10001080,0x00002244,0x89122208,0x00008008},
    {0x10001080,0x00001db8,0x70e9c787,0x00008008},
    {0x10000880,0x00000000,0x00000000,0x00008008},
    {0x08000180,0x00000000,0x00000000,0x00008004},
    {0x00000000,0x1fc00000,0x00000007,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00030080,0x981c0000,0x00000000,0x00000000},
    {0x20010000,0x08040000,0x00000000,0x00000010},
    {0x10010000,0x08040000,0x00000000,0x00000008},
    {0x10016387,0x898474b8,0x72e1d5c7,0x00000008},
    {0x10019080,0x8a042a64,0x89122208,0x00008008},
    {0x08011080,0x8c042a44,0x89122207,0x00000004},
    {0x10011080,0x8a042a44,0x89122200,0x00008008},
    {0x10011080,0x89042a44,0x89122208,0x00008008},
    {0x1003bbe0,0x98dfebe6,0x71e1e787,0x00000008},
    {0x10000000,0x80000000,0x01002000,0x00000008},
    {0x20000000,0x80000000,0x01002000,0x00000010},
    {0x00000007,0x00000000,0x03807000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00008000,0x00000000,0x10410000,0x00000000},
    {0x00008000,0x00000000,0x20408000,0x00000000},
    {0x0001f66e,0xfdfbf77c,0x20408000,0x00000000},
    {0x24008224,0x488a2248,0x20408240,0x00000012},
    {0x54008224,0x4a842210,0x40404540,0x0000002a},
    {0x48008222,0x8a8a1420,0x20408480,0x00000024},
    {0x00008a23,0x85111c44,0x20408000,0x00000000},
    {0x000071d1,0x0531887c,0x20408000,0x00000000},
    {0x00000000,0x00000800,0x20408000,0x00000000},
    {0x00000000,0x00000800,0x10410000,0x00000000},
    {0x00000000,0x00003000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x00000000,0x00000000,0x00000000},
    {0x00000000,0x20000c00,0x10000000,0x00000000},
    {0xc600a000,0x42000810,0x00000002,0x00000063},
    {0x6c00a000,0x45000810,0x00000002,0x00000036},
    {0x6c00a000,0x88800808,0xf2e1dee2,0x00000036},
    {0x54000000,0x80000800,0x11122442,0x0000002a},
    {0x54000001,0x00000800,0x11122442,0x0000002a},
    {0x54000001,0x00000800,0x11122282,0x0000002a},
    {0x44000102,0x00000800,0x11122382,0x00000022},
    {0xee000102,0x00000800,0x11e1e102,0x00000077},
    {0x00000204,0x00000800,0x11002102,0x00000000},
    {0x00000000,0x00000c00,0x11002102,0x00000000},
    {0x00000000,0x003f8000,0xe3807600,0x00000000}
    };

bit**
pbm_defaultfont( fcolsP, frowsP )
    int* fcolsP;
    int* frowsP;
    {
    bit** defaultfont;
    int row, col, scol;
    unsigned long l;

    defaultfont = pbm_allocarray( DEFAULTFONT_COLS, DEFAULTFONT_ROWS );
    for ( row = 0; row < DEFAULTFONT_ROWS; ++row )
	{
	for ( col = 0; col < DEFAULTFONT_COLS; col += 32 )
	    {
	    l = defaultfont_bits[row][col / 32];
	    for ( scol = min( col + 32, DEFAULTFONT_COLS ) - 1;
		  scol >= col; --scol )
		{
		if ( l & 1 )
		    defaultfont[row][scol] = 1;
		else
		    defaultfont[row][scol] = 0;
		l >>= 1;
		}
	    }
	}

    *fcolsP = DEFAULTFONT_COLS;
    *frowsP = DEFAULTFONT_ROWS;
    return defaultfont;
    }

void
pbm_dissectfont( font, frows, fcols, char_heightP, char_widthP, char_row0, char_col0 )
    bit** font;
    int frows;
    int fcols;
    int* char_heightP;
    int* char_widthP;
    int char_row0[95];
    int char_col0[95];
    {
    /*
    ** This routine expects a font bitmap representing the following text:
    **
    ** (0,0)
    **    M ",/^_[`jpqy| M
    **
    **    /  !"#$%&'()*+ /
    **    < ,-./01234567 <
    **    > 89:;<=>?@ABC >
    **    @ DEFGHIJKLMNO @
    **    _ PQRSTUVWXYZ[ _
    **    { \]^_`abcdefg {
    **    } hijklmnopqrs }
    **    ~ tuvwxyz{|}~  ~
    **
    **    M ",/^_[`jpqy| M
    **
    ** The bitmap must be cropped exactly to the edges.
    **
    ** The dissection works by finding the first blank row and column; that
    ** gives the height and width of the maximum-sized character, which is
    ** not too useful.  But the distance from there to the opposite side is
    ** an integral multiple of the cell size, and that's what we need.  Then
    ** it's just a matter of filling in all the coordinates.
    */
    int brow, bcol, row, col, d, ch;
    bit b;

    /* Find first blank row. */
    for ( brow = 0; brow < frows / 6; ++brow )
	{
	b = font[brow][0];
	for ( col = 1; col < fcols; ++col )
	    if ( font[brow][col] != b )
		goto nextrow;
	goto gotblankrow;
    nextrow: ;
	}
    pm_error( "couldn't find blank row in font", 0,0,0,0,0 );

gotblankrow:
    /* Find first blank col. */
    for ( bcol = 0; bcol < fcols / 8; ++bcol )
	{
	b = font[0][bcol];
	for ( row = 1; row < frows; ++row )
	    if ( font[row][bcol] != b )
		goto nextcol;
	goto gotblankcol;
    nextcol: ;
	}
    pm_error( "couldn't find blank col in font", 0,0,0,0,0 );

gotblankcol:
    /* Now compute character cell size. */
    d = frows - brow;
    *char_heightP = d / 11;
    if ( *char_heightP * 11 != d )
	pm_error( "problem computing character cell height", 0,0,0,0,0 );
    d = fcols - bcol;
    *char_widthP = d / 15;
    if ( *char_widthP * 15 != d )
	pm_error( "problem computing character cell width", 0,0,0,0,0 );

    /* Now fill in the 0,0 coords. */
    row = *char_heightP * 2;
    col = *char_widthP * 2;
    for ( ch = 0; ch < 95; ++ch )
	{
	char_row0[ch] = row;
	char_col0[ch] = col;
	col += *char_widthP;
	if ( col >= *char_widthP * 14 )
	    {
	    col = *char_widthP * 2;
	    row += *char_heightP;
	    }
	}
    }

void
pbm_dumpfont( font, fcols, frows )
    bit** font;
    int fcols;
    int frows;
    {
    /* Dump out font as C source code. */
    int row, col, scol, lperrow;
    unsigned long l;

    printf( "#define DEFAULTFONT_ROWS %d\n", frows );
    printf( "#define DEFAULTFONT_COLS %d\n", fcols );
    printf( "static unsigned long defaultfont_bits[DEFAULTFONT_ROWS][(DEFAULTFONT_COLS+31)/32] = {\n" );
    for ( row = 0; row < frows; ++row )
	{
	lperrow = 0;
	for ( col = 0; col < fcols; col += 32 )
	    {
	    if ( lperrow == 0 )
		printf( "    {" );
	    else if ( lperrow % 6 == 0 )
		{
		printf( ",\n     " );
		lperrow = 0;
		}
	    else
		printf( "," );
	    l = 0;
	    for ( scol = col; scol < min( col + 32, fcols ); ++scol )
		{
		l <<= 1;
		if ( font[row][scol] )
		    l |= 1;
		}
	    printf( "0lx%08x", l );
	    ++lperrow;
	    }
	printf( "}%s\n", row == frows - 1 ? "" : "," );
	}
    printf( "    };\n" );
    }
