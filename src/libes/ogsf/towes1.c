/************************************************************************/
gsd_getimage(pixbuf, xsize, ysize)
unsigned long **pixbuf;
int *xsize, *ysize;
{
#ifdef USE_OGL
GLuint l, r, b, t;

	/* OGLXXX
	 * get GL_VIEWPORT:
	 * You can probably do better than this.
	 */
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
	/* OGLXXX lrectread: see man page for glReadPixels */
    glReadPixels(l, b, (r)-(l)+1, (t)-(b)+1, GL_RGBA, GL_UNSIGNED_BYTE,  *pixbuf);
    return(1);
#endif
    return(0);
}

/************************************************************************/
