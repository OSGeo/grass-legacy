#include "image.h"

unsigned short rbuf[8192];
unsigned short gbuf[8192];
unsigned short bbuf[8192];

/************************************************************************/
gsd_getimage(pixbuf, xsize, ysize)
unsigned long **pixbuf;
int *xsize, *ysize;
{
GLuint l, r, b, t;

    {
    GLint tmp[4];

    glGetIntegerv(GL_VIEWPORT, tmp);
    l=tmp[0];
    r=tmp[0]+tmp[2]-1;
    b=tmp[1];
    t=tmp[1]+tmp[3]-1;
    }

    *xsize = r - l + 1; 
    *ysize = t - b + 1; 

    if(NULL == (*pixbuf = 
	    (unsigned long *)malloc(*xsize * *ysize * sizeof(unsigned long))))
	return (0);
    glReadBuffer(GL_FRONT);
    glReadPixels(l, b, (r)-(l)+1, (t)-(b)+1, GL_RGBA, 
		 GL_UNSIGNED_BYTE,  *pixbuf);
    return(1);
}

/************************************************************************/

void
ierrfunc(ebuf)
char *ebuf;
{
    fprintf(stderr, "%s\n",ebuf);
    return ;
}


GS_write_rgb(name)
char *name;
{
    int y, x;
    int xsize, ysize;
    IMAGE *image;
    unsigned long *pixbuf;

    gsd_getimage(&pixbuf, &xsize, &ysize);

    if(pixbuf){

	i_seterror(ierrfunc);
	if(NULL == (image = iopen(name,"w",RLE(1),3,xsize,ysize,3)))
	{
	    fprintf(stderr,"Unable to open %s for writing.\n", name);
	    return (-1);
	}

	for(y=0; y<ysize; y++) {

	    for(x=0; x<xsize; x++){
	       
		rbuf[x] = (pixbuf[y*xsize + x] & 0xFF000000)>>24;
		gbuf[x] = (pixbuf[y*xsize + x] & 0x00FF0000)>>16;
		bbuf[x] = (pixbuf[y*xsize + x] & 0x0000FF00)>>8;
	    }

	    putrow(image,rbuf,y,0);		/* red row */
	    putrow(image,gbuf,y,1);		/* green row */
	    putrow(image,bbuf,y,2);		/* blue row */
	}
	free(pixbuf);
	iclose(image);
	return(0);
    }
    return(-1);

}




