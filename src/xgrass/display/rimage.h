#include <X11/Xlib.h>
typedef struct _rimage {
	int    rid;
/* rdithered = 1: => no dither
   rdithered = 2: => dither */
	int    rdithered;
	int    ncolors;
	int    last_hclrcat;
	unsigned long last_r;
	unsigned long last_b;
	unsigned long last_g;
	unsigned long 	*lookup_tbl;
	XImage *rimage;
	struct _rimage *next;
} Rimage; 


struct EntireImage {
	int entirewFrameOn;
	int width, height;
	int rdithered;
	int ncolors;
	int last_hclrcat;
	unsigned long last_r;
	unsigned long last_b;
	unsigned long last_g;
	int rindicator;
	char rname[30];
	char rmapset[30];
	int vindicator;
	struct Vect *vects;
	int sindicator;
	struct Site *sites; 
	unsigned long *lookup_tbl;
	XImage	*eImage;
};








#define RIMAGE_SIZE 	sizeof(struct _rimage)
Rimage	*first_rimage, *cur_rimage;
struct  EntireImage eframe;
int	FirstImage;
char 	chentirevectname[100],
	chentirevectmapset[100];
	







