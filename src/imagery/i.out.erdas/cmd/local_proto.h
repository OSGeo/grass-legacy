#ifndef LOCAL_PROTO_H
#define LOCAL_PROTO_H
/* do_color.c */
int do_color(char [], char [], int);
int do_label(char [], char [], int, int);

/* structs */
/******************* Structure for the header of the erdas file *********/
struct erdheader {
	char hdwrd[6];
	short pack,nbands;
	char fil1[6];
	long rcols, rrows, rx, ry;
	char fill1[56];
	short maptyp, nclass;
	char fill2[14];
	short utyp;
	float area, mx, my, xcel, ycel;
	
/*
  	char hdwrd[6];	"HEAD74"
	short pack, 	Pack 	0 =  8 bit
				1 =  4 bit
				2 = 16 bit
	  nbands;
	char fil1[6];   six empty bytes
	long rcols, rrows,	Number of rows and columns 	
		rx, ry;		UL pixel "database location"
	char fill1[56];         more empty bytes...
	short maptyp,           Map projection
		nclass;         Number of classes
	char fill2[14];         More empty bytes
	short utyp;             Units of area per pixel - 0=None 1=Acre 2=Hectare 3=Other
	float area,             Area per pixel
		mx, my,         Map coord of top left pixel
		xcel, ycel;     linear size of a pixel (feet/degrees/meters)
		
*/	
};
/********************** End structure ************************************/



/* main.c */
static void printhd (struct erdheader *hd) ; /* Routine that prints out the ERDAS file header data */



/*I had to macro it.... APC , Nov 20, 1999 */
#define XOR(a,b)  ((a && b) || (!a && !b)) ? 1 : 0


#endif
