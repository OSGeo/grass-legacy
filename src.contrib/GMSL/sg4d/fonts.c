

/*
**  Written by Bill Brown, Fall 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/
/*
#include "gis.h"
*/
#include "externs.h"
#include <fmclient.h>

static fmfonthandle Curfont;
static fmfontinfo Curinfo;

void
set_font()
{
double siz;
char *s;
    
    if(Curfont){
	fmfreefont(Curfont);
	Curfont = NULL;
    }

    if(Aroman->val){
	if(Abold->val){
	    if(Aitalic->val){
		if(0 == (Curfont = fmfindfont("Times-BoldItalic")))
		    fprintf(stderr,"Times-BoldItalic font unavailable\n");
	    }
	    else{
		if(0 == (Curfont = fmfindfont("Times-Bold")))
		    fprintf(stderr,"Times-Bold font unavailable\n");
	    }
	}
	else if(Aitalic->val){
	    if(0 == (Curfont = fmfindfont("Times-Italic")))
		fprintf(stderr,"Times-Italic font unavailable\n");
	}
	else
	    if(0 == (Curfont = fmfindfont("Times-Roman")))
		fprintf(stderr,"Times-Roman font unavailable\n");
    }
    else if(Ahelvetica->val){
	if(Abold->val){
	    if(Aitalic->val){
		if(0 == (Curfont = fmfindfont("Helvetica-BoldOblique")))
		    fprintf(stderr,"Helvetica-BoldOblique font unavailable\n");
	    }
	    else{
		if(0 == (Curfont = fmfindfont("Helvetica-Bold")))
		    fprintf(stderr,"Helvetica-Bold font unavailable\n");
	    }
	}
	else if(Aitalic->val){
	    if(0 == (Curfont = fmfindfont("Helvetica-Oblique")))
		fprintf(stderr,"Helvetica-Oblique font unavailable\n");
	}
	else
	    if(0 == (Curfont = fmfindfont("Helvetica")))
		fprintf(stderr,"Helvetica font unavailable\n");
    }

    siz = 0.0;
    s = PNL_ACCESS(Typein, Afontsiz, str);
    if(s[0])
	sscanf(s,"%lf", &siz);
    if(siz < 1){
	sprintf(s,"12 ");
	pnl_fixact(Afontsiz);
	siz = 12;
    }
    else if (siz > 100){
	sprintf(s,"100");
	pnl_fixact(Afontsiz);
	siz = 100;
    }

    if(Curfont){    
	Curfont = fmscalefont(Curfont, siz);
	fmsetfont(Curfont);
	fmgetfontinfo(Curfont, &Curinfo);
    }
}

font_is_set()
{
    if(Curfont)
	return(1);
    return(0);
}

get_txtwidth()
{
char *s;

    s = PNL_ACCESS(Typein, Atext, str);        

    if(!Curfont) return(strwidth(s));
    return(fmgetstrwidth(Curfont, s));

}

get_txtheight()
{

    if(!Curfont) return(16); /* default font */

    return(Curinfo.ysize);
}

get_txtdescender()
{

    if(!Curfont) return(4);

    return(Curinfo.yorig);
}

get_txtxoffset()
{

    if(!Curfont) return(0);

    return(Curinfo.xorig);
}


