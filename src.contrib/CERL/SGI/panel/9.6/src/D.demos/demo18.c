/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */
#include <stdio.h>
#include <gl.h>
#include <panel.h>

Actuator *filewindow;
Panel *defpanel();

#define FILE_WINDOW_BUFSIZ  16386

main() 
{
    foreground();
    noport();
    winopen("typeout demo");
    winconstraints();
    winconstraints();

    doublebuffer();
    gconfig();

    defpanel();

    for (;;) {
        pnl_dopanel();
	swapbuffers();
    }
}

void
cleanexit()
{
    exit(0);
}

void
getfile(a)
Actuator *a;
{
FILE *stream;
int n=0;
Typein *adin=(Typein *)a->data;
Typeout *adout=(Typeout *)filewindow->data;
char *p=adout->buf;
    
    stream=fopen(adin->str, "r");
    if (!stream) {
	(void) strcpy(adout->buf,"can't open file");
	goto end;
    }

    do {
	n=fread(p,sizeof(char),
	    FILE_WINDOW_BUFSIZ-(p-adout->buf),stream);
    	p+=n;
    } while (n>0);
    *p='\0';	/* mark the end */
    fclose(stream);

end:
    adout->start=0;
    adout->dot=0;
    pnl_fixact(filewindow);		    /* bake changes, mark dirty */
}

Panel
*defpanel()
{
Actuator *a;
Panel *p;

    p=pnl_mkpanel();
    p->label="file viewer";
    p->x=200;
    p->y=100;

    a=pnl_mkact(pnl_wide_button);
    a->x=6.0;
    a->y=0.0;
    a->label="exit";
    a->upfunc=cleanexit;
    pnl_addact(a,p);

    a=pnl_mkact(pnl_typein);
    a->x=2.0;
    a->y=1.0;
    a->upfunc=getfile;
    a->label="file";
    a->labeltype=PNL_LABEL_LEFT;
    PNL_ACCESS(Typein,a,len)=60;
    pnl_addact(a,p);

    filewindow=pnl_mkact(pnl_typeout);
    filewindow->x=0.0;
    filewindow->y=2.0;
    PNL_ACCESS(Typeout,filewindow,col)=80;
    PNL_ACCESS(Typeout,filewindow,lin)=40;
    PNL_ACCESS(Typeout,filewindow,size)=FILE_WINDOW_BUFSIZ;
    pnl_addact(filewindow,p);
}

