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
#include <gl.h>
#include <panel.h>

Actuator *poemwindow;
Panel *defpanel();

#define POEM_WINDOW_BUFSIZ  16386
#define POEM \
"Mary had a little spam.\n\
It felt as good as gold.\n\
And when she left it out at night,\n\
It shivered in the cold."


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
addtext(a)
Actuator *a;
{
    strcat(PNL_ACCESS(Typeout, poemwindow, buf), "\n");
    strcat(PNL_ACCESS(Typeout, poemwindow, buf),
	   PNL_ACCESS(Typein, a, str));

    pnl_fixact(poemwindow);
}

void
setdelim(a)
Actuator *a;
{
  if (a->val==1.0) PNL_ACCESS(Typeout, poemwindow, delimstr) = "\t\n ";
  else		   PNL_ACCESS(Typeout, poemwindow, delimstr) = NULL;
}

void
setnocursor(a)
Actuator *a;
{
  if (a->val==1.0) PNL_ACCESS(Typeout, poemwindow, mode) |= PNL_TOM_NOCURSOR;
  else		   PNL_ACCESS(Typeout, poemwindow, mode) &= ~PNL_TOM_NOCURSOR;
  pnl_fixact(poemwindow);
}

void
setnoregion(a)
Actuator *a;
{
  if (a->val==1.0) PNL_ACCESS(Typeout, poemwindow, mode) |= PNL_TOM_NOREGION;
  else		   PNL_ACCESS(Typeout, poemwindow, mode) &= ~PNL_TOM_NOREGION;
  pnl_fixact(poemwindow);
}

Panel
*defpanel()
{
Actuator *a;
Panel *p;

    p=pnl_mkpanel();
    p->label="typeout demo";
    p->x=200;
    p->y=100;

    a=pnl_mkact(pnl_wide_button);
    a->x=1.5;
    a->y= -2.0;
    a->label="exit";
    a->upfunc=cleanexit;
    pnl_addact(a,p);

    a=pnl_mkact(pnl_toggle_button);
    a->x=4.5;
    a->y= -2.0;
    a->label="auto word select";
    a->downfunc=setdelim;
    pnl_addact(a,p);

    a=pnl_mkact(pnl_toggle_button);
    a->x=4.5;
    a->y= -2.5;
    a->label="no cursor";
    a->downfunc=setnocursor;
    pnl_addact(a,p);

    a=pnl_mkact(pnl_toggle_button);
    a->x=4.5;
    a->y= -3.0;
    a->label="no region";
    a->downfunc=setnoregion;
    pnl_addact(a,p);

    a=pnl_mkact(pnl_typein);
    a->x=1.0;
    a->y=0.0;
    a->upfunc=addtext;
    a->label="a typein actuator";
    PNL_ACCESS(Typein,a,len)=60;
    PNL_ACCESS(Typein,a,str)="Edit This String";
    PNL_ACCESS(Typein,a,mode)=PNL_TIM_TERM_ENTER;

    pnl_addact(a,p);

    poemwindow=pnl_mkact(pnl_typeout);
    poemwindow->x=1.0;
    poemwindow->y=1.0;
    poemwindow->labeltype=PNL_LABEL_TOP;
    poemwindow->label="a typeout actuator";
    PNL_ACCESS(Typeout,poemwindow,size)=POEM_WINDOW_BUFSIZ;
    pnl_addact(poemwindow,p); /* addact does the malloc */
    (void) strcpy(PNL_ACCESS(Typeout,poemwindow,buf), POEM);
    pnl_fixact(poemwindow);	    /* bake in the string */
}

