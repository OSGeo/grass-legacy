#include "image.h"
#include "externs.h"


unsigned short rbuf[8192];
unsigned short gbuf[8192];
unsigned short bbuf[8192];

void
ierrfunc(ebuf)
char *ebuf;
{
    fprintf(stderr, "%s\n",ebuf);
    return ;
}


write_rgb(l,b,r,t,name)
Screencoord l, b, r, t;
char *name;
{
    int y, x;
    int xsize, ysize;
    IMAGE *image;
    unsigned long *pixbuf;

    xsize = r - l + 1; 
    ysize = t - b + 1; 

    i_seterror(ierrfunc);

    if(NULL == (pixbuf = 
	    (unsigned long *)malloc(xsize * ysize * sizeof(unsigned long))))
	return (-1);

    if(NULL == (image = iopen(name,"w",RLE(1),3,xsize,ysize,3)))
	return (-1);

    readsource(SRC_FRONT);
    lrectread(l,b,r,t, pixbuf);
    for(y=0; y<ysize; y++) {

	for(x=0; x<xsize; x++){
	   
	    rbuf[x] = (pixbuf[y*xsize + x] & 0x000000FF);
	    gbuf[x] = (pixbuf[y*xsize + x] & 0x0000FF00)>>8;
	    bbuf[x] = (pixbuf[y*xsize + x] & 0x00FF0000)>>16;
	}

	putrow(image,rbuf,y,0);		/* red row */
	putrow(image,gbuf,y,1);		/* green row */
	putrow(image,bbuf,y,2);		/* blue row */
    }
    free(pixbuf);
    iclose(image);
    return(0);
}

void
save_window_img()
{
char filename[80];

    fprintf(stderr,"Enter name of rgb file for saved image: ");
    gets(filename);
    fprintf(stderr,"\n");
    
    if(filename[0] != '\0'){
	if(0 > write_rgb(left,bottom,right,top,filename))
	    fprintf(stderr,"Unable to save %s.\n", filename);
	else
	    fprintf(stderr,"%s saved.\n", filename);
    }
    else
	fprintf(stderr,"<request cancelled>\n");
}




