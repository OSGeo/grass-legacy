typedef unsigned char BYTE;
struct dev {
	short	filesize;	/* number of bytes in file	*/
				/* excluding dev part		*/
	short	res;		/* basic resolution in goobies/in */
	short	hor;		/* goobies horizontal		*/
	short	vert;		/* goobies vertical		*/
	short	unitwidth;	/* size at which widths are given */
	short	nfonts;		/* number fonts physically avail. */
	short	nsizes;		/* number of point sizes	*/
	short	sizescale;	/* scaling for fractional pointsizes */
	short	paperwidth;	/* max line width in units	*/
	short	paperlength;	/* max paper length in units	*/
	short	nchtab;		/* number of funny names in chtab */
	short	lchname;	/* length of chname table	*/
	short	spare1;		/* in case of expansion		*/
	short	spare2;		/* in case of expansion		*/
};

struct	font {		/* characteristics of a font */
	BYTE	nwfont;		/* number of width entries 	*/
	BYTE	specfont;	/* 1 == special font		*/
	BYTE	ligfont;	/* 1 == ligatures exist in this font */
	BYTE	spare1;		/* unused for now		*/
	char	namefont[10];	/* name of this font, e.g. R	*/
	char	intname[10];	/* internal name of this font	*/
};

# define MAX_FONTS 20
/* header part of DESC.out file */
	struct dev
device;		/* basic device table */
	short *
sizes;		/* list of point sizes of fonts */
	short *
chtab;		/* index to non-ascii entries in chname */
	char *
chname;		/* list of non-ascii id strings from \(xx */
/* end of header part of DESC.out file,
** beginning of font.out sections */
struct FONTTAB {
	struct   font *font;	/* font def (field sizes in ()'s) */
	BYTE *widths;	/* widths (font.nwfont) */
	BYTE *kerning;	/* kerning codes (font.nwfont) */
	BYTE *codes;	/* code (font.nwfont) */
	BYTE *index;	/* index table (96+device.nchtab) */
}
fonttab[MAX_FONTS];
	int
map[MAX_FONTS+1],	/* mapping of input font number to fonttab pos. */
H, V,			/* horizontal and vertical coordinates */
nfont,			/* font number */
size;			/* font point size */
	struct FONTTAB
font;
