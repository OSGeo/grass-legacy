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
 +-----------------------------------------------------------+
 |   Record 1 (bytes 1 to 128) Header                        
 |   --------------------------------                        
 |                                                           
 |    Bytes   Type   Contents                                
 |                                                           
 |     1- 6    ASCII  Descriptor         (HEAD74 or HEADER)  
 |     7- 8    I*2    Type of data:0=8 bit /1=4 bit/2=16 bit 
 |     9-10    I*2    Number of Channels                     
 |    11-16           Unused                                 
 |    17-20    I*4    Number of Pixels, if HEAD74            
 |            (R*4    Number of Pixels, if HEADER)           
 |    21-24    I*4    Number of Lines,  if HEAD74            
 |            (R*4    Number of Lines,  if HEADER)           
 |    25-28    I*4    X-coordinate of 1st pixel, if HEAD74   
 |            (R*4    X-coordinate of 1st pixel, if HEADER)  
 |    29-32    I*4    Y-coordinate of 1st pixel, if HEAD74   
 |            (R*4    Y-coordinate of 1st pixel, if HEADER)  
 |    33-88           Unused                                 
 |    89-90    I*2    Integer which indicates Map type (int, float)
 |    91-92    I*2    Number of classes in the data set      
 |    93-106          Unused                                 
 |   107-108   I*2    Units of area associated to each pixel 
 |                    0=NONE, 1=ACRE, 2=HECTAR, 3=OTHER      
 |   109-112   R*4    Number of pixel area units              
 |   113-116   R*4    Map X-coordinate of upper left corner  
 |   117-120   R*4    Map Y-coordinate of upper left corner  
 |   121-124   R*4    X-pixel size                           
 |   125-128   R*4    Y-pixel size                           
 |                                                           
 |   Data files values begin in bytes 129 and cross over     
 |   record boundaries as necessary.                         
 |   Data are arranged in following order:                     
 |                                                           
 |   L - Lines;  C - Channels;  P - Pixels per line;         
 |                                                           
 |   Pixels 1 through x of line 1, band 1                    
 |   Pixels 1 through x of line 1, band n                    
 |                                                           
 |   Pixels 1 through x of line 2, band 1                    
 |   Pixels 1 through x of line 2, band n                    
 |                                                           
 |   Pixels 1 through x of line y, band 1                    
 |   Pixels 1 through x of line y, band n                    
 +
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
