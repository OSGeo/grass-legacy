/* from xv.h */
typedef unsigned char byte;
byte colAllocOrder[256]; /* orter to allocate cols */
unsigned long cols[256]; /* maps pic pixel values to X pixel vals */
byte r[256]; 
byte g[256];
byte b[256];
int  numcols;
/*
unsigned long infobg;
*/



/* used in AllocColors */
/*
int	nfcols;
*/
int rwthistime;
unsigned long black, white;
byte    rdisp[256], gdisp[256], bdisp[256];
int     fc2pcol[256]; /* maps freecols into pic pixel values */
unsigned long freecols[256];    /* list of pixel values to free */
unsigned int  ncells;





