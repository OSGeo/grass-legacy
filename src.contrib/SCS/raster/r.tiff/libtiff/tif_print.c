
/*
 * Copyright (c) 1988, 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

/*
 * TIFF Library.
 *
 * Directory Printing Support
 */
#include <stdio.h>
#include "tiffio.h"

#define	FIELD(tif,f)	TIFFFieldSet(tif, CAT(FIELD_,f))

static char *ResponseUnitNames[] = {
	"#0",
	"10ths",
	"100ths",
	"1,000ths",
	"10,000ths",
	"100,000ths",
};
static	float ResponseUnit[] = { 1., .1, .01, .001, .0001, .00001 };
#define	MAXRESPONSEUNIT \
    (sizeof (ResponseUnitNames) / sizeof (ResponseUnitNames[0]))

/*
 * Print the contents of the current directory
 * to the specified stdio file stream.
 */
void
TIFFPrintDirectory(tif, fd, showstrips, showresponsecurve, showcolormap)
	TIFF *tif;
	FILE *fd;
	int showstrips, showresponsecurve, showcolormap;
{
	register TIFFDirectory *td;
	char *sep;
	int i;
	long n;
	float unit;

	fprintf(fd, "TIFF Directory at offset 0x%x\n", tif->tif_diroff);
	td = &tif->tif_dir;
	if (FIELD(tif,SUBFILETYPE)) {
		fprintf(fd, "  Subfile Type:");
		sep = " ";
		if (td->td_subfiletype & FILETYPE_REDUCEDIMAGE) {
			fprintf(fd, "%sreduced-resolution image", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_PAGE) {
			fprintf(fd, "%smulti-page document", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_MASK) {
			fprintf(fd, "%stransparency mask", sep);
			sep = "/";
		}
		fprintf(fd, " (%u = 0x%x)\n",
		    td->td_subfiletype, td->td_subfiletype);
	}
	if (FIELD(tif,IMAGEDIMENSIONS))
		fprintf(fd, "  Image Width: %u Image Length: %u\n",
		    td->td_imagewidth, td->td_imagelength);
	if (FIELD(tif,RESOLUTION)) {
		fprintf(fd, "  Resolution: %g, %g",
		    td->td_xresolution, td->td_yresolution);
		if (FIELD(tif,RESOLUTIONUNIT)) {
			switch (td->td_resolutionunit) {
			case RESUNIT_NONE:
				fprintf(fd, " (unitless)");
				break;
			case RESUNIT_INCH:
				fprintf(fd, " pixels/inch");
				break;
			case RESUNIT_CENTIMETER:
				fprintf(fd, " pixels/cm");
				break;
			default:
				fprintf(fd, " (unit %u = 0x%x)",
				    td->td_resolutionunit,
				    td->td_resolutionunit);
				break;
			}
		}
		fprintf(fd, "\n");
	}
	if (FIELD(tif,POSITION))
		fprintf(fd, "  Position: %g, %g\n",
		    td->td_xposition, td->td_yposition);
	if (FIELD(tif,BITSPERSAMPLE))
		fprintf(fd, "  Bits/Sample: %u\n", td->td_bitspersample);
	if (FIELD(tif,COMPRESSION)) {
		fprintf(fd, "  Compression Scheme: ");
		switch (td->td_compression) {
		case COMPRESSION_NONE:
			fprintf(fd, "none\n");
			break;
		case COMPRESSION_CCITTRLE:
			fprintf(fd, "CCITT modified Huffman encoding\n");
			break;
		case COMPRESSION_CCITTFAX3:
			fprintf(fd, "CCITT Group 3 facsimile encoding\n");
			break;
		case COMPRESSION_CCITTFAX4:
			fprintf(fd, "CCITT Group 4 facsimile encoding\n");
			break;
		case COMPRESSION_CCITTRLEW:
			fprintf(fd, "CCITT modified Huffman encoding %s\n",
			    "w/ word alignment");
			break;
		case COMPRESSION_PACKBITS:
			fprintf(fd, "Macintosh PackBits encoding\n");
			break;
		case COMPRESSION_THUNDERSCAN:
			fprintf(fd, "ThunderScan 4-bit encoding\n");
			break;
		case COMPRESSION_LZW:
			fprintf(fd, "Lempel-Ziv & Welch encoding\n");
			break;
		case COMPRESSION_PICIO:
			fprintf(fd, "Pixar picio encoding\n");
			break;
		case COMPRESSION_NEXT:
			fprintf(fd, "NeXT 2-bit encoding\n");
			break;
		default:
			fprintf(fd, "%u (0x%x)\n",
			    td->td_compression, td->td_compression);
			break;
		}
	}
	if (FIELD(tif,PHOTOMETRIC)) {
		fprintf(fd, "  Photometric Interpretation: ");
		switch (td->td_photometric) {
		case PHOTOMETRIC_MINISWHITE:
			fprintf(fd, "min-is-white\n");
			break;
		case PHOTOMETRIC_MINISBLACK:
			fprintf(fd, "min-is-black\n");
			break;
		case PHOTOMETRIC_RGB:
			fprintf(fd, "RGB color\n");
			break;
		case PHOTOMETRIC_PALETTE:
			fprintf(fd, "palette color (RGB from colormap)\n");
			break;
		case PHOTOMETRIC_MASK:
			fprintf(fd, "transparency mask\n");
			break;
		default:
			fprintf(fd, "%u (0x%x)\n",
			    td->td_photometric, td->td_photometric);
			break;
		}
	}
	if (FIELD(tif,MATTEING))
		fprintf(fd, "  Matteing: %s\n", td->td_matteing ?
		    "pre-multiplied with alpha channel" : "none");
	if (FIELD(tif,THRESHHOLDING)) {
		fprintf(fd, "  Thresholding: ");
		switch (td->td_threshholding) {
		case THRESHHOLD_BILEVEL:
			fprintf(fd, "bilevel art scan\n");
			break;
		case THRESHHOLD_HALFTONE:
			fprintf(fd, "halftone or dithered scan\n");
			break;
		case THRESHHOLD_ERRORDIFFUSE:
			fprintf(fd, "error diffused\n");
			break;
		default:
			fprintf(fd, "%u (0x%x)\n",
			    td->td_threshholding, td->td_threshholding);
			break;
		}
	}
	if (FIELD(tif,FILLORDER)) {
		fprintf(fd, "  FillOrder: ");
		switch (td->td_fillorder) {
		case FILLORDER_MSB2LSB:
			fprintf(fd, "msb-to-lsb\n");
			break;
		case FILLORDER_LSB2MSB:
			fprintf(fd, "lsb-to-msb\n");
			break;
		default:
			fprintf(fd, "%u (0x%x)\n",
			    td->td_fillorder, td->td_fillorder);
			break;
		}
	}
	if (FIELD(tif,PREDICTOR)) {
		fprintf(fd, "  Predictor: ");
		switch (td->td_predictor) {
		case 1:
			fprintf(fd, "none\n");
			break;
		case 2:
			fprintf(fd, "horizontal differencing\n");
			break;
		default:
			fprintf(fd, "%u (0x%x)\n",
			    td->td_predictor, td->td_predictor);
			break;
		}
	}
	if (FIELD(tif,ARTIST))
		fprintf(fd, "  Artist: \"%s\"\n", td->td_artist);
	if (FIELD(tif,DATETIME))
		fprintf(fd, "  Date & Time: \"%s\"\n", td->td_datetime);
	if (FIELD(tif,HOSTCOMPUTER))
		fprintf(fd, "  Host Computer: \"%s\"\n", td->td_hostcomputer);
	if (FIELD(tif,SOFTWARE))
		fprintf(fd, "  Software: \"%s\"\n", td->td_software);
	if (FIELD(tif,DOCUMENTNAME))
		fprintf(fd, "  Document Name: \"%s\"\n", td->td_documentname);
	if (FIELD(tif,IMAGEDESCRIPTION))
		fprintf(fd, "  Image Description: \"%s\"\n",
		    td->td_imagedescription);
	if (FIELD(tif,MAKE))
		fprintf(fd, "  Make: \"%s\"\n", td->td_make);
	if (FIELD(tif,MODEL))
		fprintf(fd, "  Model: \"%s\"\n", td->td_model);
	if (FIELD(tif,ORIENTATION)) {
		fprintf(fd, "  Orientation: ");
		switch (td->td_orientation) {
		case ORIENTATION_TOPLEFT:
			fprintf(fd, "row 0 top, col 0 lhs\n");
			break;
		case ORIENTATION_TOPRIGHT:
			fprintf(fd, "row 0 top, col 0 rhs\n");
			break;
		case ORIENTATION_BOTRIGHT:
			fprintf(fd, "row 0 bottom, col 0 rhs\n");
			break;
		case ORIENTATION_BOTLEFT:
			fprintf(fd, "row 0 bottom, col 0 lhs\n");
			break;
		case ORIENTATION_LEFTTOP:
			fprintf(fd, "row 0 lhs, col 0 top\n");
			break;
		case ORIENTATION_RIGHTTOP:
			fprintf(fd, "row 0 rhs, col 0 top\n");
			break;
		case ORIENTATION_RIGHTBOT:
			fprintf(fd, "row 0 rhs, col 0 bottom\n");
			break;
		case ORIENTATION_LEFTBOT:
			fprintf(fd, "row 0 lhs, col 0 bottom\n");
			break;
		default:
			fprintf(fd, "%u (0x%x)\n",
			    td->td_orientation, td->td_orientation);
			break;
		}
	}
	if (FIELD(tif,SAMPLESPERPIXEL))
		fprintf(fd, "  Samples/Pixel: %u\n", td->td_samplesperpixel);
	if (FIELD(tif,ROWSPERSTRIP)) {
		fprintf(fd, "  Rows/Strip: ");
		if (td->td_rowsperstrip == 0xffffffffL)
			fprintf(fd, "(infinite)\n");
		else
			fprintf(fd, "%u\n", td->td_rowsperstrip);
	}
	if (FIELD(tif,MINSAMPLEVALUE))
		fprintf(fd, "  Min Sample Value: %u\n", td->td_minsamplevalue);
	if (FIELD(tif,MAXSAMPLEVALUE))
		fprintf(fd, "  Max Sample Value: %u\n", td->td_maxsamplevalue);
	if (FIELD(tif,PLANARCONFIG)) {
		fprintf(fd, "  Planar Configuration: ");
		switch (td->td_planarconfig) {
		case PLANARCONFIG_CONTIG:
			fprintf(fd, "single image plane\n");
			break;
		case PLANARCONFIG_SEPARATE:
			fprintf(fd, "separate image planes\n");
			break;
		default:
			fprintf(fd, "%u (0x%x)\n",
			    td->td_planarconfig, td->td_planarconfig);
			break;
		}
	}
	if (FIELD(tif,PAGENAME))
		fprintf(fd, "  Page Name: \"%s\"\n", td->td_pagename);
	if (FIELD(tif,GRAYRESPONSEUNIT)) {
		fprintf(fd, "  Gray Response Unit: ");
		if (td->td_grayresponseunit < MAXRESPONSEUNIT)
			fprintf(fd, "%s\n",
			    ResponseUnitNames[td->td_grayresponseunit]);
		else
			fprintf(fd, "%u (0x%x)\n",
			    td->td_grayresponseunit, td->td_grayresponseunit);
	}
	if (FIELD(tif,GRAYRESPONSECURVE)) {
		fprintf(fd, "  Gray Response Curve: ");
		if (showresponsecurve) {
			fprintf(fd, "\n");
			unit = ResponseUnit[td->td_grayresponseunit];
			n = 1L<<td->td_bitspersample;
			for (i = 0; i < n; i++)
				fprintf(fd, "    %2d: %g (%u)\n",
				    i,
				    td->td_grayresponsecurve[i] * unit,
				    td->td_grayresponsecurve[i]);
		} else
			fprintf(fd, "(present)\n");
	}
	if (FIELD(tif,GROUP3OPTIONS)) {
		fprintf(fd, "  Group 3 Options:");
		sep = " ";
		if (td->td_group3options & GROUP3OPT_2DENCODING)
			fprintf(fd, "%s2-d encoding", sep), sep = "+";
		if (td->td_group3options & GROUP3OPT_FILLBITS)
			fprintf(fd, "%sEOL padding", sep), sep = "+";
		if (td->td_group3options & GROUP3OPT_UNCOMPRESSED)
			fprintf(fd, "%sno compression", sep), sep = "+";
		fprintf(fd, " (%u = 0x%x)\n",
		    td->td_group3options, td->td_group3options);
	}
	if (FIELD(tif,CLEANFAXDATA)) {
		fprintf(fd, "  Fax Data: ");
		switch (td->td_cleanfaxdata) {
		case CLEANFAXDATA_CLEAN:
			fprintf(fd, "clean\n");
			break;
		case CLEANFAXDATA_REGENERATED:
			fprintf(fd, "receiver regenerated\n");
			break;
		case CLEANFAXDATA_UNCLEAN:
			fprintf(fd, "uncorrected errors\n");
			break;
		default:
			fprintf(fd, "(%u = 0x%x)\n",
			    td->td_cleanfaxdata, td->td_cleanfaxdata);
			break;
		}
	}
	if (FIELD(tif,BADFAXLINES))
		fprintf(fd, "  Bad Fax Lines: %u\n", td->td_badfaxlines);
	if (FIELD(tif,BADFAXRUN))
		fprintf(fd, "  Consecutive Bad Fax Lines: %u\n",
		    td->td_badfaxrun);
	if (FIELD(tif,GROUP4OPTIONS))
		fprintf(fd, "  Group 4 Options: 0x%x\n", td->td_group4options);
	if (FIELD(tif,PAGENUMBER))
		fprintf(fd, "  Page Number: %u-%u\n",
		    td->td_pagenumber[0], td->td_pagenumber[1]);
	if (FIELD(tif,COLORRESPONSEUNIT)) {
		fprintf(fd, "  Color Response Unit: ");
		if (td->td_colorresponseunit < MAXRESPONSEUNIT)
			fprintf(fd, "%s\n",
			    ResponseUnitNames[td->td_colorresponseunit]);
		else
			fprintf(fd, "%u (0x%x)\n",
			    td->td_colorresponseunit, td->td_colorresponseunit);
	}
	if (FIELD(tif,COLORMAP)) {
		fprintf(fd, "  Color Map: ");
		if (showcolormap) {
			fprintf(fd, "\n");
			n = 1L<<td->td_bitspersample;
			for (i = 0; i < n; i++)
				fprintf(fd, "   %5d: %5u %5u %5u\n",
				    i,
				    td->td_redcolormap[i],
				    td->td_greencolormap[i],
				    td->td_bluecolormap[i]);
		} else
			fprintf(fd, "(present)\n");
	}
	if (FIELD(tif,COLORRESPONSECURVE)) {
		fprintf(fd, "  Color Response Curve: ");
		if (showresponsecurve) {
			fprintf(fd, "\n");
			unit = ResponseUnit[td->td_colorresponseunit];
			n = 1L<<td->td_bitspersample;
			for (i = 0; i < n; i++)
				fprintf(fd, "    %2d: %6.4f %6.4f %6.4f\n",
				    i,
				    td->td_redresponsecurve[i] * unit,
				    td->td_greenresponsecurve[i] * unit,
				    td->td_blueresponsecurve[i] * unit);
		} else
			fprintf(fd, "(present)\n");
	}
	if (showstrips && FIELD(tif,STRIPOFFSETS)) {
		fprintf(fd, "  %u Strips:\n", td->td_nstrips);
		for (i = 0; i < td->td_nstrips; i++)
			fprintf(fd, "    %3d: [%8u, %8u]\n",
			    i, td->td_stripoffset[i], td->td_stripbytecount[i]);
	}
}
