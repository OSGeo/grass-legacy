
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
#include "math.h"

void update_scale_color();
void update_scalesiz();
void pnl_discrete_hpalette();
void do_ruler();
void update_label_color();
void new_scalez();
extern void set_font();
extern void undo_label();


/* gets scale of some power of ten of 25, 50 or 100 */

float
get_default_scale(est_wrld)
float est_wrld;
{
float scale, newscale, nextscale;

    scale = est_wrld/XYscale;
    if(scale < 1){
	newscale = 1.;
	while(newscale > scale){
	    nextscale = newscale/10.;
	    newscale /= 2.;
	    if(newscale > scale)
		newscale /= 2.;
	    if(newscale > scale)
		newscale = nextscale;
	}
    }
    else{
	newscale = 1.;
	while(newscale < scale){
	    nextscale = newscale*10.;
	    newscale *= 2.5;
	    if(newscale < scale)
		newscale *= 2.;
	    if(newscale < scale)
		newscale = nextscale;
	}
    }
    newscale *= XYscale;
    return(newscale);
}



install_scale_panel()
{
static char buf[20];
static char fontbuf[10];

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

    Scale_Color = 0xFFFFFF;   /* white */

    /* scale color */
    Ascaleco = pnl_mkact (pnl_discrete_hpalette);
    Ascaleco->label = "    color = white   ";
    Ascaleco->minval = 0;
    Ascaleco->maxval = 9;
    Ascaleco->x = 0.5;
    Ascaleco->y = 1.0;
    Ascaleco->w = 4.5;
    Ascaleco->h = 0.75;
    Ascaleco->activefunc = update_scale_color;
    pnl_addact (Ascaleco, P_Scale);

    if (NULL == (P_Label = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Label->label = "Label";
    P_Label->visible = 0; 

    /* Put Label */
    Aputlabel=pnl_mkact (pnl_toggle_button);
    Aputlabel->label = " place label";
    Aputlabel->downfunc = set_font;
    Aputlabel->x = 0.5;
    Aputlabel->y = 3.25;
    pnl_addact (Aputlabel, P_Label);

    /* Label */
    Aundolabel=pnl_mkact (pnl_button);
    Aundolabel->label = " undo";
    Aundolabel->downfunc=undo_label;
    Aundolabel->x = 4.0;
    Aundolabel->y = 3.25;
    pnl_addact (Aundolabel, P_Label);

    /* Text */
    Atext=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Atext, len)=30;
    Atext->label=" text";
    Atext->x=0.5;
    Atext->y=2.5;
    pnl_addact(Atext, P_Label);

    sprintf(fontbuf,"12 ");

    /* pt size */
    Afontsiz=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Afontsiz, len)=3;
    PNL_ACCESS(Typein, Afontsiz, str)=fontbuf;
    Afontsiz->labeltype = PNL_LABEL_LEFT;
    Afontsiz->label=" size:";
    Afontsiz->x= 1.5;
    Afontsiz->y= 1.75;
    pnl_addact(Afontsiz, P_Label);

    Label_Color = 0xFFFFFF;   /* white */

    Alabelco = pnl_mkact (pnl_discrete_hpalette);
    Alabelco->label = "    color = white  ";
    Alabelco->minval = 0;
    Alabelco->maxval = 9;
    Alabelco->x = 0.5;
    Alabelco->y = 1.0;
    Alabelco->w = 4.5;
    Alabelco->h = 0.5;
    Alabelco->activefunc = update_label_color;
    pnl_addact (Alabelco, P_Label);

    /* Times-Roman font */
    Aroman=pnl_mkact (pnl_toggle_button);
    Aroman->label = " Times-Roman";
    Aroman->x = 0.5;
    Aroman->y = 0.0;
    pnl_addact (Aroman, P_Label);

    /* Helvetica font */
    Ahelvetica=pnl_mkact (pnl_toggle_button);
    Ahelvetica->label = " Helvetica";
    Ahelvetica->x = 0.5;
    Ahelvetica->y = -0.5;
    Ahelvetica->val= 1;
    pnl_addact (Ahelvetica, P_Label);

    /* italic font */
    Aitalic=pnl_mkact (pnl_toggle_button);
    Aitalic->label = " Italic";
    Aitalic->x = 4.0;
    Aitalic->y = 0.0;
    pnl_addact (Aitalic, P_Label);

    /* bold font */
    Abold=pnl_mkact (pnl_toggle_button);
    Abold->label = " Bold";
    Abold->x = 4.0;
    Abold->y = -0.5;
    Abold->val = 1.0;
    pnl_addact (Abold, P_Label);

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

    update_color(&Scale_Color, cobuf, (int)Ascaleco->val);
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
	display_ruler(X_Min,Y_Max,X_Max,Y_Max,bot,1,siz, Scale_Color, up);
	display_ruler(X_Min,Y_Max,X_Min,Y_Min,bot,-1,siz, Scale_Color, up);
    }
    if(Aseticks->val){
	display_ruler(X_Max,Y_Min,X_Min,Y_Min,bot,-1,siz, Scale_Color, up);
	display_ruler(X_Max,Y_Min,X_Max,Y_Max,bot,1,siz, Scale_Color, up);
    }
    if(Aneticks->val){
	display_ruler(X_Max,Y_Max,X_Min,Y_Max,bot,1,siz, Scale_Color, up);
	display_ruler(X_Max,Y_Max,X_Max,Y_Min,bot,1,siz, Scale_Color, up);
    }
    if(Aswticks->val){
	display_ruler(X_Min,Y_Min,X_Max,Y_Min,bot,-1,siz, Scale_Color, up);
	display_ruler(X_Min,Y_Min,X_Min,Y_Max,bot,-1,siz, Scale_Color, up);
    }

}

void 
update_label_color()
{
    static char lab_buf[30];
    char cobuf[10];

    update_color(&Label_Color, cobuf, (int)Alabelco->val);
    sprintf(lab_buf,"    color = %s",cobuf);

    Alabelco->label = lab_buf;
    pnl_fixact (Alabelco);
}



