#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern int SCREEN_TOP ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_LEFT ;
extern int SCREEN_RIGHT ;

extern Display *dpy;
extern Window grwin;
extern Pixmap bkupmap;
extern GC     gc;

/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which is actually be used to store the
 * image.
 */
Panel_save(name, top, bottom, left, right)
	char *name ;
	int top,bottom,left,right;
{
	int fd ;
	int width, height, i;
	XImage *impanel;
	char *dpoint;
	char c; /*DEBUG ONLY */

/* Adjust panel edges if outside window necessary */
	if (top    < SCREEN_TOP)       top = SCREEN_TOP ;
	if (bottom > SCREEN_BOTTOM) bottom = SCREEN_BOTTOM ;
	if (left   < SCREEN_LEFT)     left = SCREEN_LEFT ;
	if (right  > SCREEN_RIGHT)   right = SCREEN_RIGHT ;
	right += 2 ; /* Grab just a little more for luck */

/* Adjust width to an even number of pixels (whole shorts), */
/*  but remain within edges of display space */
	height = bottom - top + 1;
	width = right - left ;
	if (! (width % 2))
	    if (right < SCREEN_RIGHT)
		right++ ;
	    else
		if (left > SCREEN_LEFT)
			left-- ;
		else
			right-- ;

/* Get the image off the window */
	impanel = XGetImage(dpy,bkupmap,left,top,width,height,
	                    AllPlanes,ZPixmap);
/* open the file */
	fd = creat(name, 0644) ;
/* write the lower coordinates and size of image */
	write(fd, &left, sizeof(left)) ;
	write(fd, &top, sizeof(top)) ;
	write(fd, &width, sizeof(width)) ;
	write(fd, &height, sizeof(height)) ;
/* write the rasters, one line atta time */
	dpoint = impanel->data;
	for (i=0; i < height; i++){
	    write(fd, dpoint, width);
	    dpoint += impanel->bytes_per_line;
	}

	close(fd) ;
	XDestroyImage(impanel);
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
	char *name ;
{
	int fd, i;
	int top, left, width, height ;
	char *data, *tdata, *malloc();
	XImage *newimage;
	XWindowAttributes xwa;
	char c ;	/* DEBUG ONLY */

/* open file, read the dimensions and location */
	if( (fd = open(name, 0)) == NULL){
	    fprintf(stderr,"Cannot open panel %s\n",name);
	    return(-1);
	}
	read(fd, &left, sizeof(left)) ;
	read(fd, &top, sizeof(top)) ;
	read(fd, &width, sizeof(width)) ;
	read(fd, &height, sizeof(height)) ;

/* allocate space and read the data points */
	data = malloc(width*height);
	tdata = data;
	for (i=0; i<height; i++){
	    read(fd,tdata,width);
	    tdata += width;
	}
	close(fd) ;

/* now that data in in memory, get the window's attributes
   and turn it into an image, then draw it. */
	if (XGetWindowAttributes(dpy,grwin,&xwa)==0) return(-1);
	newimage = XCreateImage(dpy,xwa.visual,8,ZPixmap,0,
				data,width,height,8,width);
	XPutImage(dpy,grwin,gc,newimage,0,0,left,top,width,height);
	XPutImage(dpy,bkupmap,gc,newimage,0,0,left,top,width,height);
	XDestroyImage(newimage);

}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
	char *name ;
{
	unlink(name) ;
}
