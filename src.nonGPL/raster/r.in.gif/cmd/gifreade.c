/*----------------------------------------------------------------------*/
/* Copyright (c) 1987							*/
/* by CompuServe Inc., Columbus, Ohio.  All Rights Reserved		*/
/* GIFREADE.C can be copied and distributed freely for any		*/
/* non-commercial purposes. GIFREADE.C can only be incorporated		*/
/* into commercial software with the permission of CompuServe Inc.	*/
/*----------------------------------------------------------------------*/

#include <stdio.h>
#include "gifbase.h"
#include "gifreade.h"
#include "gis.h"

#define TRUE	(-1)
#define FALSE	0
extern char *layer;
extern struct Colors color;

short ReadScreenDesc(ReadByte,Width,Height,ColorRez, FillColor,NumColors, ColorMap,ColorMapSize)
		     short		(*ReadByte)();
		     short		*Width;
		     short		*Height;
		     short		*ColorRez;
		     short		*FillColor;
		     short		*NumColors;
		     struct ColorEntry	ColorMap[];
		     short		ColorMapSize;
/*
 * Read the signature, the screen description, and the optional default
 * color map.
 */

    {
    unsigned char Buffer[16];

    short
	I, J,
	Status,
	HaveColorMap,
	NumPlanes;

    G_init_colors(&color);
    for (I = 0; I < 13; I++)
	{
	Status = (*ReadByte)();
	if (Status < 0) return FALSE;
	Buffer[I] = (unsigned char) Status;
	}

    for (I = 0; I < 6; I++)
	if (Buffer[I] != GIFsignature[I])
	    return FALSE;

    *Width = Buffer[6] | Buffer[7] << 8;
    *Height = Buffer[8] | Buffer[9] << 9;
    NumPlanes = (Buffer[10] & 0x0F) + 1;

    /* bit 3 should be 0 in rev 87a */

    *ColorRez = ((Buffer[10] & 0x70) >> 4) + 1;
    HaveColorMap = (Buffer[10] & 0x80) != 0;
    *NumColors = 1 << NumPlanes;
    *FillColor = Buffer[11];
/*  Reserved = Buffer[12]; */

    if (HaveColorMap)
	{
	for (I = 0; I < *NumColors; I++)
	    {
	    for (J = 0; J < 3; J++)
		{
		Status = (*ReadByte)();
		if (Status < 0) return FALSE;
		Buffer[J] = (unsigned char) Status;
		}

	    if (I < ColorMapSize)
		{
		ColorMap[I].red = Buffer[0];
		ColorMap[I].green = Buffer[1];
		ColorMap[I].blue = Buffer[2];
		G_set_color((CELL)I, Buffer[0], Buffer[1], Buffer[2], &color); 
		}
	    }
	}
    else
	*NumColors = 0;

    return TRUE;
    }

short ReadImageDesc(ReadByte,LeftEdge,TopEdge,Width,Height,Interlaced,NumColors,ColorMap,ColorMapSize)
		    short (*ReadByte)();
		    short *LeftEdge;
		    short *TopEdge;
		    short *Width;
		    short *Height;
		    short *Interlaced;
		    short *NumColors;
		    struct ColorEntry ColorMap[];
    		    short ColorMapSize;
/*
 * Read the image description and the optional color map.
 */
    {
    unsigned char Buffer[16];

    short
	I, J,
	NumPlanes,
	HaveColorMap,
	Status;

    for (I = 0; I < 9; I++)
	{
	if ((Status = (*ReadByte)()) < 0) return FALSE;
	Buffer[I] = (unsigned char) Status;
	}

    *LeftEdge = Buffer[0] | Buffer[1] << 8;
    *TopEdge = Buffer[2] | Buffer[3] << 8;
    *Width = Buffer[4] | Buffer[5] << 8;
    *Height = Buffer[6] | Buffer[7] << 8;
    NumPlanes = (Buffer[8] & 0x0F) + 1;
    *NumColors = 1 << NumPlanes;

    /* Bits 3, 4, and 5 should be zero (reserved) in rev 87a */

    *Interlaced = (Buffer[8] & 0x40) != 0;
    HaveColorMap = (Buffer[8] & 0x80) != 0;

    if (HaveColorMap)
	{
	for (I = 0; I < *NumColors; I++)
	    {
	    for (J = 0; J < 3; J++)
		{
		if ((Status = (*ReadByte)()) < 0) return FALSE;
		Buffer[J] = (unsigned char) Status;
		}

	    if (I < ColorMapSize)
		{
		ColorMap[I].red = Buffer[0];
		ColorMap[I].green = Buffer[1];
		ColorMap[I].blue = Buffer[2];
		}
	    }
	}
    else
	*NumColors = 0;

    return TRUE;
    }
