/* do_color.c */
int do_color(char [], char [], int);
int do_label(char [], char [], int, int);


/*I had to macro it.... APC , Nov 20, 1999 */
#define XOR(a,b)  ((a && b) || (!a && !b)) ? 1 : 0

int ERDFTYP;   /* ERDAS file type ( 4 or other) */

/* 


 The ERDAS image LAN file format contains a header record (128 bytes), 
 followed by the image data. The image data is arranged in a 
 Band Interleaved by Line (BIL) format. Each file is virtually 
 unlimited in size - the file structure allows up to 274 billion bytes. 
 The file consists of 512-byte records.

                ERDAS IMAGE FILE FORMAT
 http://www2.erdas.com/SupportSite/documentation/files/erdas7xfiles.pdf
**/

/******************* Structure for the header of the erdas file *********/
struct edheader {
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
