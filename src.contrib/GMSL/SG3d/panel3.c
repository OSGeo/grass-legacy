
/*
**  Written by Bill Brown, Fall 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "gis.h"
#include "externs.h"
#include "math.h"

void update_scale_color();
void update_scalesiz();
void pnl_discrete_hpalette();
void do_ruler();
void update_label_color();
void new_scalez();
void catrange_on();
void catlist_off();
void get_cat_list();
void do_site_labels();
extern void set_font();
extern void put_label();
extern void undo_label();
extern void put_legend();
extern void set_Ccolor();


/* Replaces num with next higher(> 1) or lower (< 1)  number 
that is some power of ten of 25, 50 or 100 */

make_nice_number(num)
float *num;
{
float newnum, nextnum;

    if(*num < 0) return(0);

    if(*num < 1){
	newnum = 1.;
	while(.5*newnum > *num){
	    nextnum = newnum/10.;
	    newnum /= 2.;
	    if(.5*newnum > *num)
		newnum /= 2.;
	    if(.5*newnum > *num)
		newnum = nextnum;
	}
    }
    else{
	newnum = 1.;
	while(2*newnum <= *num){
	    nextnum = newnum*10.;
	    newnum *= 2.5;
	    if(2*newnum <= *num)
		newnum *= 2.;
	    if(2*newnum <= *num)
		newnum = nextnum;
	}
	if (newnum == 2.5) newnum = 3;   
	/* 2.5 isn't nice, but .25, 25, 250 ... are */
    }

    *num = newnum;
    return(1);

}

make_nice_number1(num)
float *num;
{
float newnum, nextnum;

    if(*num < 0) return(0);

    if(*num < 1){
	newnum = 1.;
	while(newnum > *num){
	    nextnum = newnum/10.;
	    newnum /= 2.;
	    if(newnum > *num)
		newnum /= 2.;
	    if(newnum > *num)
		newnum = nextnum;
	}
    }
    else{
	newnum = 1.;
	while(newnum < *num){
	    nextnum = newnum*10.;
	    newnum *= 2.5;
	    if(newnum < *num)
		newnum *= 2.;
	    if(newnum < *num)
		newnum = nextnum;
	}
    }
    *num = newnum;
    return(1);

}


/* gets scale of some power of ten of 25, 50 or 100 */
float
get_default_scale(est_wrld)
float est_wrld;
{
float scale;

    scale = est_wrld/XYscale;

    make_nice_number(&scale);

    scale *= XYscale;
    return(scale);
}




install_scale_panel()
{
static char buf[20];
static char fontbuf[10];
Actuator *a;

    if (NULL == (P_Scale = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Scale->label = "Scale";
    P_Scale->visible = 0; 

    /* Put Scale */
    Aputscale=pnl_mkact (pnl_toggle_button);
    Aputscale->label = " place scale object:";
    Aputscale->x = 0.5;
    Aputscale->y = 7.75;
    pnl_addact (Aputscale, P_Scale);

    /* solid */
    Asolidscale=pnl_mkact (pnl_toggle_button);
    Asolidscale->label = " solid";
    Asolidscale->x = 0.5;
    Asolidscale->y = 7.0;
    pnl_addact (Asolidscale, P_Scale);

    /* wire */
    Awirescale=pnl_mkact (pnl_toggle_button);
    Awirescale->label = " wire";
    Awirescale->x = 0.5;
    Awirescale->y = 6.5;
    pnl_addact (Awirescale, P_Scale);

    /* 3d */
    A3dscale=pnl_mkact (pnl_toggle_button);
    A3dscale->label = " cube";
    A3dscale->x = 3.0;
    A3dscale->y = 7.0;
    pnl_addact (A3dscale, P_Scale);

    /* flat */
    Aflatscale=pnl_mkact (pnl_toggle_button);
    Aflatscale->label = " plane";
    Aflatscale->val = 1;
    Aflatscale->x = 3.0;
    Aflatscale->y = 6.5;
    pnl_addact (Aflatscale, P_Scale);

    /* show north  */
    Ashownorth=pnl_mkact (pnl_toggle_button);
    Ashownorth->label = " north arrow";
    Ashownorth->val = 1;
    Ashownorth->x = 0.5;
    Ashownorth->y = 5.9;
    pnl_addact (Ashownorth, P_Scale);

    /* ruler */
    Aruler=pnl_mkact (pnl_button);
    Aruler->label = " draw ruler:";
    Aruler->x = 0.5;
    Aruler->y = 4.75;
    Aruler->downfunc = do_ruler;
    pnl_addact (Aruler, P_Scale);

    /* northwest ticks  */
    Anwticks=pnl_mkact (pnl_toggle_button);
    Anwticks->labeltype = PNL_LABEL_LEFT;
    Anwticks->label = "NW ";
    Anwticks->x = 1.5;
    Anwticks->y = 4.0;
    pnl_addact (Anwticks, P_Scale);

    /* northeast ticks  */
    Aneticks=pnl_mkact (pnl_toggle_button);
    Aneticks->label = " NE";
    Aneticks->x = 2.0;
    Aneticks->y = 4.0;
    pnl_addact (Aneticks, P_Scale);

    /* southwest ticks  */
    Aswticks=pnl_mkact (pnl_toggle_button);
    Aswticks->labeltype = PNL_LABEL_LEFT;
    Aswticks->label = "SW ";
    Aswticks->val = 1;
    Aswticks->x = 1.5;
    Aswticks->y = 3.5;
    pnl_addact (Aswticks, P_Scale);

    /* southeast ticks  */
    Aseticks=pnl_mkact (pnl_toggle_button);
    Aseticks->label = " SE";
    Aseticks->x = 2.0;
    Aseticks->y = 3.5;
    pnl_addact (Aseticks, P_Scale);

    /* Scale Elev */
    Ascalez=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Ascalez, str)="";
    PNL_ACCESS(Typein, Ascalez, len)=10;
    Ascalez->label=" <-elev->";
    Ascalez->x=0.5;
    Ascalez->y=2.5;
    Ascalez->upfunc = new_scalez;
    Ascalez->labeltype = PNL_LABEL_RIGHT;
    pnl_addact(Ascalez, P_Scale);

    /* auto elev */
    Aautoz=pnl_mkact (pnl_toggle_button);
    Aautoz->label = "auto";
    Aautoz->labeltype = PNL_LABEL_BOTTOM;
    Aautoz->val = 1;
    Aautoz->x = 4.25;
    Aautoz->y = 2.5;
    pnl_addact (Aautoz, P_Scale);

    Scale_Size = get_default_scale((float)(X_Res * X_Size/15.));
    sprintf(buf,"%.2f", Scale_Size/XYscale );
    
    /* Scale Size */
    Ascalesiz=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Ascalesiz, str)=buf;
    PNL_ACCESS(Typein, Ascalesiz, len)=10;
    Ascalesiz->label=" size";
    Ascalesiz->x=0.5;
    Ascalesiz->y=2.0;
    Ascalesiz->labeltype = PNL_LABEL_RIGHT;
    Ascalesiz->upfunc = update_scalesiz;
    pnl_addact(Ascalesiz, P_Scale);

    Dcolor[SCALE_COLOR] = 0xFFFFFF;   /* white */

    /* scale color */
    Ascaleco = pnl_mkact (pnl_discrete_hpalette);
    Ascaleco->label = "    color = white   ";
    Ascaleco->minval = 0;
    Ascaleco->maxval = NUM_STANDARD_COLORS + NUM_CUSTOM_COLORS ;
    Ascaleco->x = 0.5;
    Ascaleco->y = 1.0;
    Ascaleco->w = 4.5;
    Ascaleco->h = 0.75;
    Ascaleco->activefunc = update_scale_color;
    pnl_addact (Ascaleco, P_Scale);

    /* Set Custom Color */
    AscaleCcset= pnl_mkact (pnl_button);
    AscaleCcset->label = "set";
    AscaleCcset->x = 5.25;
    AscaleCcset->y = 1.0;
    AscaleCcset->labeltype = PNL_LABEL_BOTTOM;
    AscaleCcset->downfunc = set_Ccolor;
    pnl_addact (AscaleCcset, P_Scale);

    /* CLOSE */
    make_close(2.0, 0.0, P_Scale);

    if (NULL == (P_Label = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Label->label = "Label";
    P_Label->visible = 0; 

    /* Put Label */
    Aputlabel=pnl_mkact (pnl_wide_button);
    Aputlabel->label = " place label";
    Aputlabel->downfunc = set_font;
    Aputlabel->upfunc = put_label;
    Aputlabel->x = 0.5;
    Aputlabel->y = -.75;
    pnl_addact (Aputlabel, P_Label);

    /* Text */
    Atext=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Atext, len)=32;
    Atext->label=" label text";
    Atext->x=0.5;
    Atext->y= -1.25;
    pnl_addact(Atext, P_Label);

    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- - - - - - - - - - - - - - - - -";
    a->x = .5;
    a->y = -2.1;
    pnl_addact (a, P_Label);

    sprintf(fontbuf,"12 ");

    {

	/* Frame for font stuff & undo */
	F_font=pnl_mkact(pnl_frame);
	F_font->x= .5;
/*
	F_font->data->mode = PNL_FM_FREE;
	F_font->data->offx = .25;
	F_font->data->offy = -1.75;
	F_font->data->minx = .25;
	F_font->data->maxx = 7.0;
	F_font->data->miny = -1.00;
	F_font->data->maxy = 1.75;
*/
	pnl_addact(F_font, P_Label);

	/* pt size */
	Afontsiz=pnl_mkact(pnl_typein);
	PNL_ACCESS(Typein, Afontsiz, len)=3;
	PNL_ACCESS(Typein, Afontsiz, str)=fontbuf;
	Afontsiz->labeltype = PNL_LABEL_LEFT;
	Afontsiz->label=" font size:";
	Afontsiz->x= 2.5;
	Afontsiz->y= 1.75;
	pnl_addsubact(Afontsiz, F_font);

	/* Undo */
	Aundolabel=pnl_mkact (pnl_wide_button);
	Aundolabel->label = " undo";
	Aundolabel->downfunc=undo_label;
	Aundolabel->x = 4.0;
	Aundolabel->y = 1.75;
	pnl_addsubact (Aundolabel, F_font);

	Dcolor[LABEL_COLOR] = 0xFFFFFF;   /* white */

	Alabelco = pnl_mkact (pnl_discrete_hpalette);
	Alabelco->label = "    color = white  ";
	Alabelco->minval = 0;
	Alabelco->maxval = NUM_STANDARD_COLORS + NUM_CUSTOM_COLORS ;
	Alabelco->x = .5;
	Alabelco->y = 1.0;
	Alabelco->w = 4.5;
	Alabelco->h = 0.5;
	Alabelco->activefunc = update_label_color;
	pnl_addsubact (Alabelco, F_font);

	/* Set Custom Color */
	AlabelCcset= pnl_mkact (pnl_button);
	AlabelCcset->label = "set";
	AlabelCcset->x = 5.25;
	AlabelCcset->y = 1.0;
	AlabelCcset->labeltype = PNL_LABEL_BOTTOM;
	AlabelCcset->downfunc = set_Ccolor;
	pnl_addsubact (AlabelCcset, F_font);

	/* Times-Roman font */
	Aroman=pnl_mkact (pnl_toggle_button);
	Aroman->label = " Times-Roman";
	Aroman->x = 0.5;
	Aroman->y = 0.0;
	pnl_addsubact (Aroman, F_font);

	/* Helvetica font */
	Ahelvetica=pnl_mkact (pnl_toggle_button);
	Ahelvetica->label = " Helvetica";
	Ahelvetica->x = 0.5;
	Ahelvetica->y = -0.5;
	Ahelvetica->val= 1;
	pnl_addsubact (Ahelvetica, F_font);

	/* Courier font */
	Acourier=pnl_mkact (pnl_toggle_button);
	Acourier->label = " Courier";
	Acourier->x = 0.5;
	Acourier->y = -1.0;
	Acourier->val= 0;
	pnl_addsubact (Acourier, F_font);


	/* italic font */
	Aitalic=pnl_mkact (pnl_toggle_button);
	Aitalic->label = " Italic";
	Aitalic->x = 4.0;
	Aitalic->y = 0.0;
	pnl_addsubact (Aitalic, F_font);

	/* bold font */
	Abold=pnl_mkact (pnl_toggle_button);
	Abold->label = " Bold";
	Abold->x = 4.0;
	Abold->y = -0.5;
	Abold->val = 1.0;
	pnl_addsubact (Abold, F_font);

    }

    /* put legend */
    Alegend=pnl_mkact (pnl_wide_button);
    Alegend->label = "Legend";
    Alegend->x = 0.5;
    Alegend->y = -2.75;
    Alegend->downfunc  = put_legend;
    pnl_addact (Alegend, P_Label);

    /* invert legend direction */
    Acatinv=pnl_mkact (pnl_toggle_button);
    Acatinv->label = " invert";
    Acatinv->x = 0.5;
    Acatinv->y = -3.35;
    Acatinv->val = 0.0;
    pnl_addact (Acatinv, P_Label);

    /* category values */
    Acatvals=pnl_mkact (pnl_toggle_button);
    Acatvals->label = " category values";
    Acatvals->x = 2.5;
    Acatvals->y = -2.75;
    Acatvals->val = 1.0;
    pnl_addact (Acatvals, P_Label);

    /* category labels */
    Acatlabs=pnl_mkact (pnl_toggle_button);
    Acatlabs->label = " category labels";
    Acatlabs->x = 2.5;
    Acatlabs->y = -3.25;
    Acatlabs->val = 1.0;
    pnl_addact (Acatlabs, P_Label);

    /* range of categories */
    Acatrange=pnl_mkact (pnl_toggle_button);
    Acatrange->label = " Use Range";
    Acatrange->x = 0.5;
    Acatrange->y = -4.00;
    Acatrange->val = 0.0;
    Acatrange->upfunc = catlist_off;
    pnl_addact (Acatrange, P_Label);

    /* low in range */
    Acatlow=pnl_mkact (pnl_typein);
    PNL_ACCESS(Typein, Acatlow, str)="";
    PNL_ACCESS(Typein, Acatlow, len)=10;
    Acatlow->label = " low: ";
    Acatlow->labeltype = PNL_LABEL_LEFT;
    Acatlow->x = 4.5;
    Acatlow->y = -4.00;
    Acatlow->upfunc = catrange_on;
    pnl_addact (Acatlow, P_Label);

    /* high in range */
    Acathigh=pnl_mkact (pnl_typein);
    PNL_ACCESS(Typein, Acathigh, str)="";
    PNL_ACCESS(Typein, Acathigh, len)=10;
    Acathigh->label = "high: ";
    Acathigh->labeltype = PNL_LABEL_LEFT;
    Acathigh->x = 4.5;
    Acathigh->y = -4.50;
    Acathigh->upfunc = catrange_on;
    pnl_addact (Acathigh, P_Label);

    /* all categories */
    Acatall=pnl_mkact (pnl_toggle_button);
    Acatall->label = " discrete categories";
    Acatall->x = .5;
    Acatall->y = -5.00;
    Acatall->val = 0.0;
    pnl_addact (Acatall, P_Label);

    /* list categories */
    Acatlist=pnl_mkact (pnl_toggle_button);
    Acatlist->label = " use list";
    Acatlist->x = 1.5;
    Acatlist->y = -5.5;
    Acatlist->val = 0.0;
    Acatlist->downfunc = get_cat_list;
    pnl_addact (Acatlist, P_Label);

    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- - - - - - - - - - - - - - - - -";
    a->x = .5;
    a->y = -5.9;
    pnl_addact (a, P_Label);

    /* put site labels */
    Aputsitelabels=pnl_mkact (pnl_wide_button);
    Aputsitelabels->label = "Label Sites";
    Aputsitelabels->x = 0.5;
    Aputsitelabels->y = -6.5;
    Aputsitelabels->downfunc  = do_site_labels;
    pnl_addact (Aputsitelabels, P_Label);

    /* center labels in box */
    Asitelabbox=pnl_mkact (pnl_toggle_button);
    Asitelabbox->label = " in box";
    Asitelabbox->x = 3.5;
    Asitelabbox->y = -6.5;
    Asitelabbox->val = 1.0;
    pnl_addact (Asitelabbox, P_Label);

    /* category values */
    Asitelabcats=pnl_mkact (pnl_toggle_button);
    Asitelabcats->label = " category"; 
    Asitelabcats->x = 0.5;
    Asitelabcats->y = -7.1;
    Asitelabcats->val = 1.0;
    pnl_addact (Asitelabcats, P_Label);

    /* strings */
    Asitelabstrings=pnl_mkact (pnl_toggle_button);
    Asitelabstrings->label = " strings";
    Asitelabstrings->x = 0.5;
    Asitelabstrings->y = -7.6;
    Asitelabstrings->val = 0.0;
    pnl_addact (Asitelabstrings, P_Label);

    /* string fields */
    Asitestrfields=pnl_mkact (pnl_typein);
    PNL_ACCESS(Typein, Asitestrfields, len)=10;
    PNL_ACCESS(Typein, Asitestrfields, str)="1";
    Asitestrfields->label = " fields:";
    Asitestrfields->labeltype = PNL_LABEL_LEFT;
    Asitestrfields->x = 4.5;
    Asitestrfields->y = -7.6;
    pnl_addact (Asitestrfields, P_Label);

    /* double values */
    Asitelabvals=pnl_mkact (pnl_toggle_button);
    Asitelabvals->label = " values";
    Asitelabvals->x = 0.5;
    Asitelabvals->y = -8.1;
    Asitelabvals->val = 0.0;
    pnl_addact (Asitelabvals, P_Label);

    /* double fields */
    Asitevalfields=pnl_mkact (pnl_typein);
    PNL_ACCESS(Typein, Asitevalfields, len)=10;
    PNL_ACCESS(Typein, Asitevalfields, str)="1";
    Asitevalfields->label = " fields:";
    Asitevalfields->labeltype = PNL_LABEL_LEFT;
    Asitevalfields->x = 4.5;
    Asitevalfields->y = -8.1;
    pnl_addact (Asitevalfields, P_Label);

    /* number of decimal places */
    Asitelabdec=pnl_mkact (pnl_typein);
    PNL_ACCESS(Typein, Asitelabdec, len)=10;
    PNL_ACCESS(Typein, Asitelabdec, str)="2";
    Asitelabdec->label = " precision: ";
    Asitelabdec->labeltype = PNL_LABEL_LEFT;
    Asitelabdec->x = 3.0;
    Asitelabdec->y = -8.7;
    pnl_addact (Asitelabdec, P_Label);

    /* piechart from double vals */
    Asitelabpie=pnl_mkact (pnl_toggle_button);
    Asitelabpie->label = " piechart";
    Asitelabpie->x = 0.5;
    Asitelabpie->y = -9.3;
    Asitelabpie->val = 0.0;
    pnl_addact (Asitelabpie, P_Label);

    /* piechart fields */
    Asitepiefields=pnl_mkact (pnl_typein);
    PNL_ACCESS(Typein, Asitepiefields, len)=10;
    PNL_ACCESS(Typein, Asitepiefields, str)="1";
    Asitepiefields->label = " fields:";
    Asitepiefields->labeltype = PNL_LABEL_LEFT;
    Asitepiefields->x = 4.5;
    Asitepiefields->y = -9.3;
    pnl_addact (Asitepiefields, P_Label);

    /* piechart colors default to custom colors in reverse order, then std */
    Apiegrey=pnl_mkact (pnl_toggle_button);
    Apiegrey->label = " grey scale";
    Apiegrey->x = 2.5;
    Apiegrey->y = -9.8;
    Apiegrey->val = 0.0;
    pnl_addact (Apiegrey, P_Label);

    /* piechart labels */
    Apielabel=pnl_mkact (pnl_toggle_button);
    Apielabel->label = " label values";
    Apielabel->x = 2.5;
    Apielabel->y = -10.3;
    Apielabel->val = 0.0;
    pnl_addact (Apielabel, P_Label);

    /* CLOSE */
    make_close(2.5, -11., P_Label);

}


void
catrange_on()
{
    if(!Acatrange->val){
	Acatrange->val = 1;
	pnl_fixact(Acatrange);
    }

    catlist_off();
}

void
catlist_off()
{
    if(Acatlist->val){
	Acatlist->val = 0.0;
	pnl_fixact(Acatlist);
	get_cat_list();
    }
}

get_scalez(scalez)
float *scalez;
{
char *buf;
float z;

    buf = PNL_ACCESS(Typein, Ascalez, str);
    if(buf[0]){
	sscanf(buf,"%f",&z);
	*scalez = (z - Zoff) * Z_exag;
	return(1);
    }
    return(0);
}

put_scalez(scalez)
float scalez;
{
char *buf;
float z;
    
    z = 0;
    if(Z_exag) z = scalez/Z_exag + Zoff;
    buf = PNL_ACCESS(Typein, Ascalez, str);
    sprintf(buf,"         ");
    if(z > 1 || z < -1)
	sprintf(buf,"%.2f", z);
    else
	sprintf(buf,"%.6f", z);
    pnl_fixact(Ascalez);
    return(1);
}

void
new_scalez()
{
    if(Aautoz->val){
	Aautoz->val = 0;
	pnl_fixact(Aautoz);
    }
}

void
update_scalesiz ()
{
char *buf;
float size;

    buf = PNL_ACCESS(Typein, Ascalesiz, str);
    if(buf[0]){
	sscanf(buf,"%f",&size);
	Scale_Size = size * XYscale;
    }
    else{
	sprintf(buf,"%.2f", Scale_Size/XYscale );
	pnl_fixact(Ascalesiz);
    }

}

void
update_scale_color ()
{
    static char buf[20];
    char cobuf[10];

    update_color(SCALE_COLOR, cobuf, (int)Ascaleco->val);
    sprintf(buf,"    color = %s",cobuf);

    Ascaleco->label = buf;
    pnl_fixact (Ascaleco);
}


void
do_ruler()
{
double siz;
float bot;
int up;

    siz = Scale_Size;
   
    if(Afringe->val){
	if(Anozero->val){
	    up = 0;
	    bot = (Z_Min_notzero - Zoff - Z_Span_real/4.) * Z_exag;
	}
	else{
	    up = 1;
	    bot = Z_Min - Z_exag * Z_Span_real/4;
	}
    }
    else{
	bot = (Anozero->val ? (Z_Min_notzero - Zoff) * Z_exag: Z_Min);
	up = !(Anozero->val);
    }


    if(Anwticks->val){
	display_ruler(X_Min,Y_Max,X_Max,Y_Max,bot,1,siz, Dcolor[SCALE_COLOR], up);
	display_ruler(X_Min,Y_Max,X_Min,Y_Min,bot,-1,siz, Dcolor[SCALE_COLOR], up);
    }
    if(Aseticks->val){
	display_ruler(X_Max,Y_Min,X_Min,Y_Min,bot,-1,siz, Dcolor[SCALE_COLOR], up);
	display_ruler(X_Max,Y_Min,X_Max,Y_Max,bot,1,siz, Dcolor[SCALE_COLOR], up);
    }
    if(Aneticks->val){
	display_ruler(X_Max,Y_Max,X_Min,Y_Max,bot,1,siz, Dcolor[SCALE_COLOR], up);
	display_ruler(X_Max,Y_Max,X_Max,Y_Min,bot,1,siz, Dcolor[SCALE_COLOR], up);
    }
    if(Aswticks->val){
	display_ruler(X_Min,Y_Min,X_Max,Y_Min,bot,-1,siz, Dcolor[SCALE_COLOR], up);
	display_ruler(X_Min,Y_Min,X_Min,Y_Max,bot,-1,siz, Dcolor[SCALE_COLOR], up);
    }

}

void 
update_label_color()
{
    static char lab_buf[30];
    char cobuf[10];

    update_color(LABEL_COLOR, cobuf, (int)Alabelco->val);
    sprintf(lab_buf,"    color = %s",cobuf);

    Alabelco->label = lab_buf;
    pnl_fixact (Alabelco);
}



