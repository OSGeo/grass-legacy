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
#include <fcntl.h>
#include <gl.h>
#include <panel.h>

Actuator *messagewindow,*gridfiletypein,*solnfiletypein;
Panel *defpanel();

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
cancelfunc()
{
    exit(0);
}

void
openfiles(a)
Actuator *a;
{
int gridfd, solnfd;
int n=0;
Typein *adgrid=(Typein *)gridfiletypein->data;
Typein *adsoln=(Typein *)solnfiletypein->data;
Typeout *adout=(Typeout *)messagewindow->data;
char *p=adout->buf;
    
    if (!strcmp(adgrid->str,"")) {
	ringbell();
	tprint(messagewindow,"null grid file name");
	return;
    }

    if (!strcmp(adsoln->str,"")) {
	ringbell();
	tprint(messagewindow,"null solution file name");
	return;
    }

    gridfd=open(adgrid->str, O_RDONLY);
    if (gridfd<0) {
	tprint(messagewindow,"can't open grid file");
	return;
    }

    solnfd=open(adsoln->str, O_RDONLY);
    if (solnfd<0) {
	tprint(messagewindow,"can't open solution file");
	return;
    }
}

Panel
*defpanel()
{
Actuator *a;
Panel *p;

    p=pnl_mkpanel();
    p->label="file window";
    p->x=200;
    p->y=100;

    a=pnl_mkact(pnl_wide_button);
    a->x=1.0;
    a->y=0.0;
    a->label="ok";
    a->upfunc=openfiles;
    pnl_addact(a,p);

    a=pnl_mkact(pnl_wide_button);
    a->x=6.0;
    a->y=0.0;
    a->label="cancel";
    a->upfunc=cancelfunc;
    pnl_addact(a,p);

    gridfiletypein=pnl_mkact(pnl_typein);
    gridfiletypein->x=1.0;
    gridfiletypein->y=1.0;
    gridfiletypein->label="grid file";
    gridfiletypein->labeltype=PNL_LABEL_LEFT;
    PNL_ACCESS(Typein,gridfiletypein,len)=60;
    pnl_addact(gridfiletypein,p);

    solnfiletypein=pnl_mkact(pnl_typein);
    solnfiletypein->x=1.0;
    solnfiletypein->y=1.5;
    solnfiletypein->label="solution file";
    solnfiletypein->labeltype=PNL_LABEL_LEFT;
    PNL_ACCESS(Typein,solnfiletypein,len)=60;
    pnl_addact(solnfiletypein,p);

    messagewindow=pnl_mkact(pnl_typeout);
    messagewindow->x=0.0;
    messagewindow->y=2.5;
    messagewindow->label="messages";
    PNL_ACCESS(Typeout,messagewindow,col)=80;
    PNL_ACCESS(Typeout,messagewindow,lin)=20;
    pnl_addact(messagewindow,p);
}

