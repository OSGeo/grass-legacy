
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

/*
** Enhancements made winter 1991 - 1992 by Bill Brown
** US Army Construction Engineering Research Lab
** Some code copied from NASA panels library
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/
/*
#include "gis.h"
*/
#include "externs.h"
#include "math.h"


void P_popmatrix();
void P_pushmatrix();
void P_rotate();
void P_translate();
void P_transform();
extern int whats_here();
extern void show_where_viewpt();
extern void check_under_model();
extern int ll_vertex();
extern int flat_vertex();
extern int ll_norm_vertex();
extern int flat_norm_vertex();
extern int v_ll_vertex();
extern int v_flat_vertex();
extern int vd_ll_vertex();
extern int vd_flat_vertex();
void redraw_event();

char Exag_value[20];

char Range_txt[][12] = {
    " 10000000.0",
    " 1000000.0 ",
    " 100000.0  ",
    " 10000.0   ",
    " 1000.0    ",
    " 100.0     ",
    " 10.0      ",
    " 1.0       ",
    " 0.1       ",
    " 0.01      ",
    " 0.001     ",
    " 0.0001    ",
    " 0.00001   ",
    " 0.000001  ",
    " 0.0000001 ",
};

float Range_val[] = {
    10000000.0,
    1000000.0,
    100000.0,
    10000.0,
    1000.0,
    100.0,
    10.0,
    1.0,
    0.1,
    0.01,
    0.001,
    0.0001,
    0.00001,
    0.000001,
    0.0000001,
};

float Range_val_ex[] = {
    1000.0,
    100.0,
    100.0,
    10.0,
    10.0,
    10.0,
    10.0,
    1.0,
    0.1,
    0.01,
    0.01,
    0.01,
    0.01,
    0.001,
    0.001,
};

void _two_buf();
void _one_buf();
void two_buf();
void one_buf();
void slow_start();
void update_vect_color();
void update_bg_color();
void update_site_color();
void update_xy ();
void update_height ();
void update_lights ();
void _update_lightpos ();
void update_ltheight ();
void update_lightxy ();
void _update_view_dir ();
void update_view_dir ();
void update_incl ();
void update_persp ();
void update_exag ();
void update_vectz ();
void do_type();
void do_globe();
void do_newdspf();
void do_setdspf();
void do_drawdspf();
void newcell ();
void newelev ();
void do_site_display();
void new_sites();
void look_center();
void new_vect ();
void do_fast_display();
extern void draw_model();
extern void position_model();
extern void save_settings();
extern void get_settings();
extern void _newvalhpalette();
extern void keep_focus();
extern void set_real_to();
extern void put_scale();
extern void put_label();
extern void save_window_img();
Panel * my_initscriptpanel ();



void
_drawdiscretehpalette(a, p)
Actuator *a;
Panel *p;
{
Coord y;
Colorindex col;
int i;
float width;


  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);
    
    width = a->w / (a->maxval - a->minval);
    for (i = 0,col = (Colorindex)a->minval; col < (Colorindex)a->maxval;
							++i, ++col){

	color(col);
	rectf(i*width, 0.0,(i+1)*width,a->h);
    }
    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}


void
pnl_discrete_hpalette(a)
Actuator *a;
{
  pnl_palette(a);
  a->type=PNL_HSLIDER;

  a->w=PNL_SLIDER_HEIGHT;
  a->h=PNL_SLIDER_WIDTH;
  a->newvalfunc=_newvalhpalette;
  a->drawfunc=_drawdiscretehpalette;
}


void
_drawpuckpointer(a, p, x, y, style)
Actuator *a;
Panel *p;
Coord x, y;
int style;
{
static Coord d=PNL_PUCK_SIZE/2.0;

  if (style==PNL_FILLED) {
    pmv2(-d,0.0);
    pdr2(-PNL_DIM_2,-PNL_DIM_2);
    pdr2(PNL_DIM_2,-PNL_DIM_2);
    pdr2(d,0.0);
    pdr2(PNL_DIM_2,PNL_DIM_2);
    pdr2(-PNL_DIM_2,PNL_DIM_2);
    pclos();
    pmv2(0.0,-d);
    pdr2(PNL_DIM_2,-PNL_DIM_2);
    pdr2(PNL_DIM_2,PNL_DIM_2);
    pdr2(0.0,d);
    pdr2(-PNL_DIM_2,PNL_DIM_2);
    pdr2(-PNL_DIM_2,-PNL_DIM_2);
    pclos();
  } else {
    move2(-d,0.0);
    draw2(-PNL_DIM_2, -PNL_DIM_2);
    draw2(0.0,-d);
    draw2(PNL_DIM_2,-PNL_DIM_2);
    draw2(d,0.0);
    draw2(PNL_DIM_2,PNL_DIM_2);
    draw2(0.0,d);
    draw2(-PNL_DIM_2,PNL_DIM_2);
    draw2(-d,0.0);
    move2(0.0,0.0);
    linewidth(3);
    draw2(d*fcos(Alook->val * PI/1800.0), d*fsin(Alook->val * PI/1800.0));
    linewidth(1);
  }
}

/*
Uncomment to show visible surface in XY position window
#define SHOW_VIS
*/

void
__drawpuck(a, p)
Actuator *a;
Panel *p;
{
Coord x,y;
static Coord d = PNL_PUCK_SIZE/2.0;


  if (!a->dirtycnt) return;

    PNL_ACCESS(Puck, a, x)=RANGE(PNL_ACCESS(Puck, a, x), a->minval, a->maxval);
    PNL_ACCESS(Puck, a, y)=RANGE(PNL_ACCESS(Puck, a, y), a->minval, a->maxval);

    color(pnl_normal_color);
    rectf(a->x,a->y,a->x+a->w,a->y+a->h);

#ifdef SHOW_VIS
    {
	int i,j;
	float xo,yo,xb,yb,xres,yres;
	static int cnt = 0;

	if(cnt > 3){
/*
	    set_mask(X_Mod, Y_Mod);
*/
	    color(pnl_other_color);
	    xo = a->x + a->w *((RANGE_FACTOR/2.0) / (1.0 + RANGE_FACTOR));
	    yo = a->y + a->h *((RANGE_FACTOR/2.0) / (1.0 + RANGE_FACTOR));
	    xres = a->w *((1.0/(1.0 + RANGE_FACTOR)) / MASKDIM); 
	    yres = a->h *((1.0/(1.0 + RANGE_FACTOR)) / MASKDIM); 
	    for(i=0; i< MASKDIM; i++){
		yb = yo + (MASKDIM-i)*yres;
		for(j=1; j< MASKDIMP; j++){
		    xb = xo + j*xres;
		    if(!P_mask[i][j])
			rectf(xb,yb,xb+xres,yb+yres);
		}
	    }
	}
	else cnt ++;
    }
#endif

    pushmatrix();
    x=a->w*(PNL_ACCESS(Puck, a, x)-a->minval)/(a->maxval-a->minval);
    y=a->h*(PNL_ACCESS(Puck, a, y)-a->minval)/(a->maxval-a->minval);

    translate(a->x+d, a->y+d, 0.0);
    translate(x*(a->w-PNL_PUCK_SIZE)/a->w,y*(a->h-PNL_PUCK_SIZE)/a->h,0.0);

    color(pnl_highlight_color);
    _drawpuckpointer(a,p,x,y,PNL_FILLED);
    color(pnl_black_color);
    _drawpuckpointer(a,p,x,y,PNL_OPEN);
    popmatrix();

    color(pnl_black_color);
    rect(a->x,a->y,a->x+a->w,a->y+a->h);
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

install_panels ()
{
    P_Script = my_initscriptpanel ();
    install_hidden_panels ();
    install_control_panel ();
    install_light_panel ();
    install_menus_panel ();
    install_movement_panel ();
}

static int dummy;

void
toggle_key()
{
    TOGGLE (P_Keyframe->visible);
    pnl_fixpanel (P_Keyframe);
}
void
toggle_scale()
{
    TOGGLE (P_Scale->visible);
    pnl_fixpanel (P_Scale);
}
void
toggle_label()
{
    TOGGLE (P_Label->visible);
    pnl_fixpanel (P_Label);
}
void
toggle_path()
{
    TOGGLE (P_Path->visible);
    pnl_fixpanel (P_Path);
}
void
toggle_script ()
{
    TOGGLE (P_Script->visible);
    pnl_fixpanel (P_Script);
}
void 
toggle_options ()
{
    TOGGLE (P_Options->visible);
    pnl_fixpanel (P_Options);
}
void 
toggle_sites ()
{
    TOGGLE (P_Sites->visible);
    pnl_fixpanel (P_Sites);
}
void 
toggle_animate ()
{
    TOGGLE (P_Animate->visible);
    pnl_fixpanel (P_Animate);
}
void 
toggle_vect ()
{
    TOGGLE (Amorvect->val);
    pnl_fixact (Amorvect);
    TOGGLE (P_Vect->visible);
    pnl_fixpanel (P_Vect);
}
void 
toggle_lights()
{
    TOGGLE (P_Lights->visible);
    pnl_fixpanel (P_Lights);
}

install_hidden_panels ()
{


    if (NULL == (P_Vect = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Vect->label = "Vectors";
    P_Vect->visible = 0;
    P_Vect->ppu *= .8;  /* shrink it */

    /* Drape vectors */
    Adrape = pnl_mkact (pnl_toggle_button);
    Adrape->label = " Drape ";
    Adrape->x = 0.5;
    Adrape->y = 1.0;
    pnl_addact (Adrape, P_Vect);

    /* Flat vectors */
    Aflat = pnl_mkact (pnl_toggle_button);
    Aflat->label = " Flat  ";
    Aflat->x = 0.5;
    Aflat->y = 0.5;
    pnl_addact (Aflat, P_Vect);

    /* vector color */
    Avcolor = pnl_mkact (pnl_discrete_hpalette);
    Avcolor->label = "color = white  ";
    Avcolor->minval = 0;
    Avcolor->maxval = 9;
    Avcolor->x = 0.5;
    Avcolor->y = -.75;
    Avcolor->w = 4.5;
    Avcolor->h = 0.75;
    Avcolor->activefunc = update_vect_color;
    pnl_addact (Avcolor, P_Vect);
    
    /* vector elevation */
    Avectz=pnl_mkact(pnl_vslider);
    Avectz->label="Vect Z";
    Avectz->x=6.75;
    Avectz->y= -1.0;
    Avectz->w = 0.75;
    Avectz->h = 3.0;
    Avectz->minval= 0.0;
    Avectz->maxval=1.0;
    Avectz->downfunc=_two_buf;
    Avectz->upfunc=_one_buf;
    Avectz->activefunc=update_vectz;
    Avectz->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Avectz, P_Vect);
    
    /* Dim vectors */
    Adim= pnl_mkact (pnl_toggle_button);
    Adim->label = "Darker";
    Adim->x = 5.25;
    Adim->y = -.75;
    Adim->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact (Adim, P_Vect);

    /* vwidth1 */
    Avwidth1=pnl_mkact (pnl_up_arrow_button);
    Avwidth1->label = " Line Width ";
    Avwidth1->x = 2.7;
    Avwidth1->y = 1.0;
    pnl_addact (Avwidth1, P_Vect);

    /* vwidth2 */
    Avwidth2=pnl_mkact (pnl_down_arrow_button);
    Avwidth2->label = "     1";
    Avwidth2->x = 2.7;
    Avwidth2->y = 0.5;
    pnl_addact (Avwidth2, P_Vect);

    /* New Vect */
    Anewvect=pnl_mkact (pnl_wide_button);
    Anewvect->label = "New Vect";
    Anewvect->x = 0.5;
    Anewvect->y = 1.8;
    Anewvect->downfunc = new_vect;
    pnl_addact (Anewvect, P_Vect);

    if (NULL == (P_Sites = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Sites->label = "Sites";
    P_Sites->visible = 0;
    P_Sites->ppu *= .8;  /* shrink it */

    /* new sites file */
    Anewsite=pnl_mkact (pnl_wide_button);
    Anewsite->label = " New Sites File ";
    Anewsite->x = 0.5;
    Anewsite->y = 5.0;
    pnl_addact (Anewsite, P_Sites);

    /* display x sites */
    Axsite=pnl_mkact (pnl_toggle_button);
    Axsite->label = " x";
    Axsite->val = 1;
    Axsite->x = 0.5;
    Axsite->y = 4.0;
    pnl_addact (Axsite, P_Sites);
    
    /* display sphere sites */
    Aspheresite=pnl_mkact (pnl_toggle_button);
    Aspheresite->label = " sphere";
    Aspheresite->x = 0.5;
    Aspheresite->y = 3.5;
    pnl_addact (Aspheresite, P_Sites);

    /* display octohedron sites */
    Aoctosite=pnl_mkact (pnl_toggle_button);
    Aoctosite->label = " octohedron";
    Aoctosite->x = 0.5;
    Aoctosite->y = 3.0;
    pnl_addact (Aoctosite, P_Sites);

    /* display conetree sites */
    Aconetree=pnl_mkact (pnl_toggle_button);
    Aconetree->label = " cone tree";
    Aconetree->val = 0;
    Aconetree->x = 3.5;
    Aconetree->y = 4.0;
    pnl_addact (Aconetree, P_Sites);
    
    /* display round tree sites */
    Arndtree=pnl_mkact (pnl_toggle_button);
    Arndtree->label = " round tree";
    Arndtree->x = 3.5;
    Arndtree->y = 3.5;
    pnl_addact (Arndtree, P_Sites);

    /* display conehead (glyph1) sites */
    Aglyph1site=pnl_mkact (pnl_toggle_button);
    Aglyph1site->label = " glyph1";
    Aglyph1site->val = 0;
    Aglyph1site->x = 3.5;
    Aglyph1site->y = 3.0;
    pnl_addact (Aglyph1site, P_Sites);
    
    /* display inverse conehead (glyph2) sites */
    Aglyph2site=pnl_mkact (pnl_toggle_button);
    Aglyph2site->label = " glyph2";
    Aglyph2site->val = 0;
    Aglyph2site->x = 3.5;
    Aglyph2site->y = 2.5;
    pnl_addact (Aglyph2site, P_Sites);
    
    /* display sites */
    Asitesiz=pnl_mkact(pnl_hslider);
    Asitesiz->label=" size ";
    Asitesiz->x= 0.5;
    Asitesiz->y= 2.0;
    Asitesiz->w= 4.0;
    Asitesiz->h= 0.3;
    Asitesiz->minval= 0.0;
    Asitesiz->maxval= 1.0;
    Asitesiz->val= 0.5;
    pnl_addact(Asitesiz, P_Sites);

    if(Site_cat_isZ){
    /* connect site to surface */
    Aconsite=pnl_mkact (pnl_toggle_button);
    Aconsite->label = " connect to surface";
    Aconsite->val = 1;
    Aconsite->x = 0.5;
    Aconsite->y = 1.0;
    pnl_addact (Aconsite, P_Sites);
    }

    Astcolor = pnl_mkact (pnl_discrete_hpalette);
    Astcolor->label = "    color = green  ";
    Astcolor->minval = 0;
    Astcolor->maxval = 9;
    Astcolor->x = 0.5;
    Astcolor->y = 0.0;
    Astcolor->w = 4.5;
    Astcolor->h = 0.5;
    Astcolor->activefunc = update_site_color;
    pnl_addact (Astcolor, P_Sites);
  
    if(Map_Sitecolor){
    /* use site color map (sctually color structure from raster map) */
    Amapsite=pnl_mkact (pnl_toggle_button);
    Amapsite->label = "map";
    Amapsite->labeltype = PNL_LABEL_BOTTOM;
    Amapsite->val = 1;
    Amapsite->x = 5.5;
    Amapsite->y = 0.0;
    pnl_addact (Amapsite, P_Sites);
    }

    /* level sites */
    Asitelevel=pnl_mkact (pnl_toggle_button);
    Asitelevel->label = " Constant Z";
    Asitelevel->x = 0.5;
    Asitelevel->y = -1.25;
    pnl_addact (Asitelevel, P_Sites);

    /* site Z */
    Asitez=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Asitez, str)="";
    PNL_ACCESS(Typein, Asitez, len)=10;
    Asitez->x= 4.0;
    Asitez->y= -1.25;
    pnl_addact(Asitez, P_Sites);

    if (NULL == (P_Options = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Options->label = "Options";
    P_Options->visible = 0; 

    Aread=pnl_mkact (pnl_wide_button);
    Aread->label = "    Load 3d View      ";
    Aread->x = 0.5;
    Aread->y = 1.25;
    pnl_addact (Aread, P_Options);

    Asave=pnl_mkact (pnl_wide_button);
    Asave->label = " Save Current 3d View ";
    Asave->x = 0.5;
    Asave->y = 0.75;
    pnl_addact (Asave, P_Options);

    /* New Cell */
    Anewcell=pnl_mkact (pnl_wide_button);
    Anewcell->label = " New Cell (color) File";
    Anewcell->x = 0.5;
    Anewcell->y = 0.0;
    pnl_addact (Anewcell, P_Options);

    /* New Elevation  */
    Anewelev=pnl_mkact (pnl_wide_button);
    Anewelev->label = "  New Elevation File  ";
    Anewelev->x = 0.5;
    Anewelev->y = -0.7;
    Anewelev->downfunc = newelev;
    pnl_addact (Anewelev, P_Options);

    /* Do Display_type */
    Adotype=pnl_mkact (pnl_toggle_button);
    Adotype->label = " animate display type";
    Adotype->x = 0.5;
    Adotype->y = -1.4;
    Adotype->downfunc = do_type;
    pnl_addact (Adotype, P_Options);

    /* no clear on redraw */
    Anoclear=pnl_mkact (pnl_toggle_button);
    Anoclear->label = " no clear on redraw";
    Anoclear->x = 0.5;
    Anoclear->y = -2.0;
    pnl_addact (Anoclear, P_Options);

    Abgcolor = pnl_mkact (pnl_discrete_hpalette);
    Abgcolor->label = "background color = black  ";
    Abgcolor->minval = 0;
    Abgcolor->maxval = 9;
    Abgcolor->x = 0.5;
    Abgcolor->y = -3.1;
    Abgcolor->w = 4.5;
    Abgcolor->h = 0.5;
    Abgcolor->activefunc = update_bg_color;
    pnl_addact (Abgcolor, P_Options);

    /* Do Globe */
    Aglobe=pnl_mkact (pnl_toggle_button);
    Aglobe->label = " map lat-long to globe";
    Aglobe->x = 0.5;
    Aglobe->y = -4.1;
    Aglobe->upfunc = do_globe;
    pnl_addact (Aglobe, P_Options);

    /* Scale... */
    Ascale=pnl_mkact (pnl_wide_button);
    Ascale->label = "Scale...";
    Ascale->x = 0.5;
    Ascale->y = -4.6;
    Ascale->downfunc = toggle_scale;
    pnl_addact (Ascale, P_Options);

    /* Label... */
    Alabel=pnl_mkact (pnl_wide_button);
    Alabel->label = "Label...";
    Alabel->x = 3.0;
    Alabel->y = -4.6;
    Alabel->downfunc = toggle_label;
    pnl_addact (Alabel, P_Options);

    /* new dspf */
    Anewdspf=pnl_mkact (pnl_wide_button);
    Anewdspf->label = "  New 3D Displayfile  ";
    Anewdspf->x = 0.5;
    Anewdspf->y = -5.5;
    Anewdspf->downfunc = do_newdspf;
    pnl_addact (Anewdspf, P_Options);

    /* set 3D data parameters*/
    Asetdspf=pnl_mkact (pnl_wide_button);
    Asetdspf->label = " set dspf parameters  ";
    Asetdspf->x = 0.5;
    Asetdspf->y = -6.0;
    Asetdspf->upfunc = do_setdspf;
    pnl_addact (Asetdspf, P_Options);

    /* DRAW dspf */
    Adrawdspf=pnl_mkact (pnl_wide_button);
    Adrawdspf->label = " DRAW 3D Displayfile  ";
    Adrawdspf->x = 0.5;
    Adrawdspf->y = -6.5;
    Adrawdspf->upfunc = do_drawdspf;
    pnl_addact (Adrawdspf, P_Options);

    /* show on redraw */
    Ashowdspf=pnl_mkact (pnl_toggle_button);
    Ashowdspf->label = " show 3D on redraw";
    Ashowdspf->x = 0.5;
    Ashowdspf->y = -7.0;
    pnl_addact (Ashowdspf, P_Options);

    /* do dspf lighting */
    Adspflights=pnl_mkact (pnl_toggle_button);
    Adspflights->label = " use showdspf lights";
    Adspflights->val = 1;
    Adspflights->x = 0.5;
    Adspflights->y = -7.5;
    pnl_addact (Adspflights, P_Options);

    install_anim_panels();
    install_scale_panel();
}


install_control_panel ()
{
    Panel *panel;


    if (NULL == (panel = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    panel->label = "Control";

    pnl_needredraw ();

    /* nozero */
    Anozero=pnl_mkact (pnl_toggle_button);
    Anozero->label = " No Zeros";
    Anozero->x = 4.0;
    Anozero->y = 0.25;
    pnl_addact (Anozero, panel);

    /* Shading */
    Ashading=pnl_mkact (pnl_toggle_button);
    Ashading->label = " Gouraud";
    Ashading->x = 1.0;
    Ashading->y = 1.75;
    pnl_addact (Ashading, panel);

    /* Polygon */
    Apoly=pnl_mkact (pnl_toggle_button);
    Apoly->label = " Polygon";
    Apoly->x = 1.0;
    Apoly->y = 4.25;
    pnl_addact (Apoly, panel);

    /* Grid/poly */
    Agpoly=pnl_mkact (pnl_toggle_button);
    Agpoly->label = " Wire/Poly";
    Agpoly->x = 1.0;
    Agpoly->y = 3.75;
    pnl_addact (Agpoly, panel);

    /* Grid */
    Agrid=pnl_mkact (pnl_toggle_button);
    Agrid->label = " Wire";
    Agrid->x = 1.0;
    Agrid->y = 3.25;
    pnl_addact (Agrid, panel);

    /* Colored Grid */
    Agridc=pnl_mkact (pnl_toggle_button);
    Agridc->label = " Col Wire";
    Agridc->x = 1.0;
    Agridc->y = 2.75;
    pnl_addact (Agridc, panel);

    /* Vect */
    Avect=pnl_mkact (pnl_wide_button);
    Avect->label = "  Vect ";
    Avect->x = 3.2;
    Avect->y = -0.5;
    pnl_addact (Avect, panel);

    /* Sites */
    Asites=pnl_mkact (pnl_wide_button);
    Asites->label = " Sites ";
    Asites->x = 3.2;
    Asites->y = -1.0;
    pnl_addact (Asites, panel);

    /* Fringe */
    Afringe=pnl_mkact (pnl_toggle_button);
    Afringe->label = " Fringe";
    Afringe->x = 1.0;
    Afringe->y = 1.25;
    pnl_addact (Afringe, panel);

    /* Quit */
    Aquit=pnl_mkact (pnl_wide_button);
    Aquit->label = " Quit ";
    Aquit->x = 5.5;
    Aquit->y = -0.5;
    pnl_addact (Aquit, panel);

    /* Dump */
    Adump=pnl_mkact (pnl_wide_button);
    Adump->label = "Img Dump";
    Adump->x = 5.5;
    Adump->y = -1.0;
    pnl_addact (Adump, panel);

    /* Draw */
    Adraw=pnl_mkact (pnl_wide_button);
    Adraw->label = " Draw ";
    Adraw->x = 1.0;
    Adraw->y = -0.5;
    pnl_addact (Adraw, panel);

    /* Cancel */
    Acancel=pnl_mkact (pnl_wide_button);
    Acancel->label = " Cancel ";
    Acancel->x = 1.0;
    Acancel->y = -1.0;
    pnl_addact (Acancel, panel);

    /* Surface */
    Asurface=pnl_mkact (pnl_toggle_button);
    Asurface->label = " Surface Only";
    Asurface->x = 1.0;
    Asurface->y = 0.25;
    pnl_addact (Asurface, panel);

    /*
    if(getgdesc(GD_BLEND)){
    }
    */

    /* Transparency */
    Atransp1=pnl_mkact(pnl_toggle_button);
    Atransp1->label = "<-solid ";
    Atransp1->labeltype = PNL_LABEL_LEFT;
    Atransp1->x= 5.4;
    Atransp1->y= 1.75;
    Atransp1->w= .25;
    Atransp1->h= .25;
    pnl_addact(Atransp1, panel);

    Atransp2=pnl_mkact(pnl_toggle_button);
    Atransp2->label = "Transparency";
    Atransp2->labeltype = PNL_LABEL_TOP;
    Atransp2->x= 5.75;
    Atransp2->y= 1.75;
    Atransp2->w= .25;
    Atransp2->h= .25;
    pnl_addact(Atransp2, panel);

    Atransp3=pnl_mkact(pnl_toggle_button);
    Atransp3->label = " clear->";
    Atransp3->labeltype = PNL_LABEL_RIGHT;
    Atransp3->x= 6.1;
    Atransp3->y= 1.75;
    Atransp3->w= .25;
    Atransp3->h= .25;
    pnl_addact(Atransp3, panel);

    /* Whats here?  */
    Awhat=pnl_mkact (pnl_toggle_button);
    Awhat->label = " What's here?";
    Awhat->x = 4.0;
    Awhat->y = 1.25;
    pnl_addact (Awhat, panel);

    /* fast1 */
    Afast1=pnl_mkact (pnl_up_arrow_button);
    Afast1->label = " Grid Resolution";
    Afast1->x = 4.0;
    Afast1->y = 4.25;
    pnl_addact (Afast1, panel);

    /* fast2 */
    Afast2=pnl_mkact (pnl_down_arrow_button);
    Afast2->label = "";
    Afast2->x = 4.0;
    Afast2->y = 3.75;
    pnl_addact (Afast2, panel);

    /* slow1 */
    Aslow1=pnl_mkact (pnl_up_arrow_button);
    Aslow1->label = " Poly Resolution";
    Aslow1->x = 4.0;
    Aslow1->y = 3.0;
    pnl_addact (Aslow1, panel);

    /* slow2 */
    Aslow2=pnl_mkact (pnl_down_arrow_button);
    Aslow2->label = "";
    Aslow2->x = 4.0;
    Aslow2->y = 2.5;
    pnl_addact (Aslow2, panel);

    /* triangle  */
    Atriangle=pnl_mkact (pnl_toggle_button);
    Atriangle->label = " Triangulate";
    Atriangle->x = 4.0;
    Atriangle->y = 0.75;
    pnl_addact (Atriangle, panel);
    
    /* Lights */
    Alight=pnl_mkact (pnl_toggle_button);
    Alight->label = " Lights";
    Alight->x = 1.0;
    Alight->y = 0.75;
    pnl_addact (Alight, panel);
}

install_menus_panel ()
{
    Panel *panel;


    if (NULL == (panel = P_Menus = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    panel->label = "Menus";
    panel->ppu *= .8;  /* shrink it */

    /* Lights...  */
    Amorlights=pnl_mkact (pnl_wide_button);
    Amorlights->label = "Lights... ";
    Amorlights->x = 0.0;
    Amorlights->y = 0.0;
    Amorlights->downfunc = toggle_lights;
    pnl_addact (Amorlights, panel);

    /* Vect...  */
    Amorvect=pnl_mkact (pnl_wide_button);
    Amorvect->label = " Vect...  ";
    Amorvect->x = 3.0;
    Amorvect->y = 0.0;
    Amorvect->downfunc = toggle_vect;
    pnl_addact (Amorvect, panel);

    /* Options... */
    Aoptions=pnl_mkact (pnl_wide_button);
    Aoptions->label = "Options...";
    Aoptions->x = 3.0;
    Aoptions->y = 0.5;
    Aoptions->downfunc = toggle_options;
    pnl_addact (Aoptions, panel);

    /* Script... */
    Ascript=pnl_mkact (pnl_wide_button);
    Ascript->label = "Script... ";
    Ascript->x = 0.0;
    Ascript->y = 0.5;
    Ascript->downfunc = toggle_script;
    pnl_addact (Ascript, panel);

    /* Animate... */
    Aanimate=pnl_mkact (pnl_wide_button);
    Aanimate->label = "Animate... ";
    Aanimate->x = 6.0;
    Aanimate->y = 0.5;
    Aanimate->downfunc = toggle_animate;
    pnl_addact (Aanimate, panel);

    /* Sites... */
    Amorsites=pnl_mkact (pnl_wide_button);
    Amorsites->label = "Sites...   ";
    Amorsites->x = 6.0;
    Amorsites->y = 0.0;
    Amorsites->downfunc = toggle_sites;
    pnl_addact (Amorsites, panel);

}

install_movement_panel ()
{
    Panel *panel;
    Actuator *a;

    if (NULL == (panel = P_Mvmt = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    panel->label = "Movement";
    panel->ppu *= .75;  /* shrink it */

    /* Where? */
    Awhere=pnl_mkact (pnl_wide_button);
    Awhere->label = "Where am I?";
    Awhere->x = 6.0;
    Awhere->y = 7.75;
    Awhere->downfunc = show_where_viewpt;
    pnl_addact (Awhere, panel);

    /* Reset */
    Areset=pnl_mkact (pnl_button);
    Areset->label = " Reset";
    Areset->x = 6.5;
    Areset->y = 6.0;
    Areset->labeltype = PNL_LABEL_TOP;
    pnl_addact (Areset, panel);

    /* range1 */
    Arange1=pnl_mkact (pnl_up_arrow_button);
    Arange1->label = " Z Scale";
    Arange1->x = 7.2;
    Arange1->y = 1.5;
    pnl_addact (Arange1, panel);

    /* range2 */
    Arange2=pnl_mkact (pnl_down_arrow_button);
    Arange2->label = "";
    Arange2->x = 7.2;
    Arange2->y = 1.0;
    pnl_addact (Arange2, panel);

    /* Z exageration */
    Aexag=pnl_mkact(pnl_vslider);
    Aexag->label="";
    Aexag->x=8.0;
    Aexag->y=3.0;
    Aexag->h=4.5;
    Aexag->minval= 0.0;
    Aexag->maxval= 10.0;
    Aexag->downfunc=two_buf;
    Aexag->upfunc=one_buf;
    Aexag->activefunc=update_exag;
    Aexag->labeltype = PNL_LABEL_TOP;
    pnl_addact(Aexag, panel);

    /* Exag Label */
    Aexaglab=pnl_mkact (pnl_label);
    Aexaglab->label = "Z exag:";
    Aexaglab->x = 7.7;
    Aexaglab->y = 2.5;
    pnl_addact (Aexaglab, panel);

    /* Exag Value */
    Aexagval=pnl_mkact (pnl_label);
    Aexagval->label = " ";
    Aexagval->x = 7.2;
    Aexagval->y = 2.0;
    pnl_addact (Aexagval, panel);

    /* XY */
    Axy=pnl_mkact(pnl_puck);
    Axy->label="XY position";
    Axy->x=0.5;
    Axy->y=3.0;
    Axy->minval= 0.0;
    Axy->maxval=1.0;
    Axy->downfunc=two_buf;
    Axy->upfunc=one_buf;
    Axy->activefunc=update_xy;
    Axy->drawfunc= __drawpuck;
    Axy->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Axy, panel);

    /* look label */
    a=pnl_mkact (pnl_label);
    a->label = "look:";
    a->x = 6.0;
    a->y = 5.0;
    pnl_addact (a, panel);

    /* focus */
    Afocus=pnl_mkact (pnl_toggle_button);
    Afocus->label = "here";
    Afocus->x = 6.0;
    Afocus->y = 4.5;
    Afocus->labeltype = PNL_LABEL_RIGHT;
    pnl_addact (Afocus, panel);

    /* center */
    Acenter=pnl_mkact (pnl_button);
    Acenter->label = "center";
    Acenter->x = 6.0;
    Acenter->y = 4.0;
    Acenter->downfunc = look_center;
    pnl_addact (Acenter, panel);

    /* clear focus */
    Anofocus=pnl_mkact (pnl_button);
    Anofocus->label = "cancel";
    Anofocus->x = 6.0;
    Anofocus->y = 3.5;
    pnl_addact (Anofocus, panel);

    /* height */
    Aheight=pnl_mkact(pnl_vslider);
    Aheight->label="Height";
    Aheight->x=4.7;
    Aheight->y=3.0;
    Aheight->h=3.8;
    Aheight->minval= 0.0;
    Aheight->maxval=1.0;
    Aheight->downfunc=two_buf;
    Aheight->upfunc=one_buf;
    Aheight->activefunc=update_height;
    Aheight->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Aheight, panel);

    /* view direction */
    Alook=pnl_mkact(pnl_dial);
    Alook->label="View Dir.";
    Alook->labeltype = PNL_LABEL_BOTTOM;
    Alook->x= 1.0;
    Alook->y= 7.5;
    Alook->minval= 0.0;
    Alook->maxval=3600.0;
    Alook->downfunc=two_buf;
    Alook->upfunc=one_buf;
    Alook->activefunc=update_view_dir;
    PNL_ACCESS (Dial, Alook, winds) = 1.;
    pnl_addact(Alook, panel);

    /* view inclination */
    Aincl=pnl_mkact(pnl_dial);
    Aincl->label="View Incline";
    Aincl->labeltype = PNL_LABEL_BOTTOM;
    Aincl->x= 4.0;
    Aincl->y= 7.5;
    Aincl->minval= -900.0;
    Aincl->maxval=  900.0;
    Aincl->downfunc=two_buf;
    Aincl->upfunc=one_buf;
    Aincl->activefunc=update_incl;
    PNL_ACCESS (Dial, Aincl, winds) = .5;
    pnl_addact(Aincl, panel);

    /* view Twist */
    Atwist=pnl_mkact(pnl_dial);
    Atwist->label="Twist";
    Atwist->labeltype = PNL_LABEL_TOP;
    Atwist->x= 2.5;
    Atwist->y= 7.5;
    Atwist->minval= 0.0;
    Atwist->maxval=3600.0;
    Atwist->downfunc=two_buf;
    Atwist->upfunc=one_buf;
    Atwist->activefunc=update_view_dir;
    PNL_ACCESS (Dial, Atwist, winds) = 1.;
    pnl_addact(Atwist, panel);

    /* Perspective */
    Apersp=pnl_mkact(pnl_hslider);
    Apersp->label="Perspective";
    Apersp->x=0.5;
    Apersp->y=1.5;
    Apersp->w=4.0;
    Apersp->h=0.75;
    Apersp->minval= 30.0;
    Apersp->maxval=1200.0;
    Apersp->downfunc=two_buf;
    Apersp->upfunc=one_buf;
    Apersp->activefunc=update_persp;
    Apersp->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Apersp, panel);

    /* ortho */
    Aortho=pnl_mkact (pnl_toggle_button);
    Aortho->label = "ortho";
    Aortho->x = 5.0;
    Aortho->y = 1.5;
    Aortho->activefunc=update_persp;
    Aortho->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact (Aortho, panel);


}

install_light_panel ()
{

    if (NULL == (P_Lights = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Lights->label = "Lights";
    P_Lights->ppu *= .75;  /* shrink it */
    P_Lights->visible = 0;

    /* follow */
    Afollow=pnl_mkact (pnl_toggle_button);
    Afollow->label = " Follow Viewpoint";
    Afollow->x = 1.0;
    Afollow->y = 2.7;
    pnl_addact (Afollow, P_Lights);

    /* show model */
    Ashowmod=pnl_mkact (pnl_toggle_button);
    Ashowmod->label = " Show Model";
    Ashowmod->x = 6.0;
    Ashowmod->y = 2.7;
    Ashowmod->downfunc = check_under_model;
    Ashowmod->upfunc = check_under_model;
    pnl_addact (Ashowmod, P_Lights);

    /* light brightness */
    Abright=pnl_mkact(pnl_hslider);
    Abright->label="Light Brightness";
    Abright->x= 1.0;
    Abright->y= 1.9;
    Abright->w= 5.0;
    Abright->h= 0.4;
    Abright->minval= 0.0;
    Abright->maxval= 1.0;
    Abright->activefunc=update_lights;
    Abright->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Abright, P_Lights);

    /* material shineyness */
    Ashine=pnl_mkact(pnl_hslider);
    Ashine->label="Surface Reflectivity";
    Ashine->x= 1.0;
    Ashine->y= -2.0;
    Ashine->w= 5.0;
    Ashine->h= 0.4;
    Ashine->minval= 0.0;
    Ashine->maxval= 1.0;
    Ashine->activefunc=update_lights;
    Ashine->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Ashine, P_Lights);

    /* material - ambient */
    Aambient=pnl_mkact(pnl_hslider);
    Aambient->label="Ambient Light";
    Aambient->x= 1.0;
    Aambient->y= -0.8;
    Aambient->w= 5.0;
    Aambient->h= 0.4;
    Aambient->minval= 0.0;
    Aambient->maxval= 1.0;
    Aambient->activefunc=update_lights;
    Aambient->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Aambient, P_Lights);

    /* surface color - red */
    Ared=pnl_mkact(pnl_hslider);
    Ared->label=" R";
    Ared->x= 1.5;
    Ared->y= 1.0;
    Ared->w= 4.0;
    Ared->h= 0.3;
    Ared->minval= 0.0;
    Ared->maxval= 1.0;
    Ared->activefunc=update_lights;
    Ared->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Ared, P_Lights);

    /* surface color - grn */
    Agrn=pnl_mkact(pnl_hslider);
    Agrn->label=" G";
    Agrn->x= 1.5;
    Agrn->y= 0.5;
    Agrn->w= 4.0;
    Agrn->h= 0.3;
    Agrn->minval= 0.0;
    Agrn->maxval= 1.0;
    Agrn->activefunc=update_lights;
    Agrn->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Agrn, P_Lights);

    /* surface color - blu */
    Ablu=pnl_mkact(pnl_hslider);
    Ablu->label=" B";
    Ablu->x= 1.5;
    Ablu->y= 0.0;
    Ablu->w= 4.0;
    Ablu->h= 0.3;
    Ablu->minval= 0.0;
    Ablu->maxval= 1.0;
    Ablu->activefunc=update_lights;
    Ablu->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Ablu, P_Lights);

    /* ltheight */
    Altheight=pnl_mkact(pnl_vslider);
    Altheight->label=" Height";
    Altheight->x= 6.5;
    Altheight->y= -1.5;
    Altheight->w= 0.8;
    Altheight->h= 3.8;
    Altheight->minval= 0.0;
    Altheight->maxval=2.0;
    Altheight->activefunc=update_ltheight;
    Altheight->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Altheight, P_Lights);

    /* XY */
    Alightxy=pnl_mkact(pnl_puck);
    Alightxy->label="XY position";
    Alightxy->x=7.8;
    Alightxy->y= -1.5;
    Alightxy->minval= 0.0;
    Alightxy->maxval=1.0;
    Alightxy->activefunc=update_lightxy;
    Alightxy->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Alightxy, P_Lights);

}


void
_update_xy ()
{
    float dx, dy;

    dx = FROM_TO[TO][X] - FROM_TO[FROM][X];
    dy = FROM_TO[TO][Y] - FROM_TO[FROM][Y];

    FROM_TO[FROM][X] = PNL_ACCESS (Point, Axy, x) * XRange + XBase;
    FROM_TO[FROM][Y] = PNL_ACCESS (Point, Axy, y) * YRange + YBase;

    FROM_TO[TO][X] = FROM_TO[FROM][X] + dx;
    FROM_TO[TO][Y] = FROM_TO[FROM][Y] + dy;
}

void
update_xy ()
{
    _update_xy ();

    if(InFocus)
	keep_focus();
    
    if(Afollow->val)
	_update_lightpos();

    update_projection ();
    do_fast_display ();

}

void
_update_lightpos ()
{
    LightPos[X] = FROM_TO[FROM][X] + To_Light[X];
    LightPos[Y] = FROM_TO[FROM][Y] + To_Light[Y];
    LightPos[Z] = FROM_TO[FROM][Z] + To_Light[Z];

    PNL_ACCESS (Point, Alightxy, x) = (LightPos[X] - XBase) / XRange;
    PNL_ACCESS (Point, Alightxy, y) = (LightPos[Y] - YBase) / YRange;
    
    Altheight->val = (LightPos[Z] - ZBase) / ZRange;

    pnl_fixact (Alightxy);
    pnl_fixact (Altheight);
}


void
update_lightxy ()
{
    if (Afollow->val)
    {
	Afollow->val = 0;
	pnl_fixact (Afollow);
    }
    LightPos[X] = PNL_ACCESS (Point, Alightxy, x) * XRange + XBase;
    LightPos[Y] = PNL_ACCESS (Point, Alightxy, y) * YRange + YBase;
    if(Ashowmod->val){
	if(New_view) position_model();
	check_under_model ();
	draw_model();
    }
}

void
_update_ltheight ()
{

    LightPos[Z] = Altheight->val * ZRange + ZBase;
}

void
update_ltheight ()
{
    _update_ltheight();

    if (Afollow->val)
    {
	Afollow->val = 0;
	pnl_fixact (Afollow);
    }
    if(Ashowmod->val){
	if(New_view) position_model();
	check_under_model ();
	draw_model();
    }
}

void
_update_height ()
{
    float dz;

    dz = FROM_TO[TO][Z] - FROM_TO[FROM][Z];

    FROM_TO[FROM][Z] = Aheight->val * ZRange + ZBase;
    FROM_TO[TO][Z] = FROM_TO[FROM][Z] + dz;  
}

void
update_height ()
{
    _update_height ();
    
    if(InFocus)
	keep_focus();

    if(Afollow->val)
	_update_lightpos();

    update_projection ();
    do_fast_display ();

}

void
update_lights ()
{
    if(Ashowmod->val){
	if(New_view) position_model();
	check_under_model ();
	draw_model();
    }
}
void look_center()
{
     if(LatLon)
	CenterSphere = 1;
     else{
	 if(!los_intersect(REAL_TO, FROM_TO))
		if(!get_centroid(REAL_TO)){
		      REAL_TO[X] = FROM_TO[TO][X];
		      REAL_TO[Y] = FROM_TO[TO][Y];
		      REAL_TO[Z] = FROM_TO[TO][Z];
		 }
	 draw_x (REAL_TO, 0, MARK_SIZ);
	 InFocus = 1;
     }
     New_view = 1;
     fprintf(stderr,"<new center set>\n");
     zclear();
}
void
_update_exag ()
{
    float dz;

    Z_exag = Aexag->val * XYscale;

    sprintf(Exag_value,"%07.08lf",Z_exag/XYscale);
    Aexagval->label = Exag_value;
    pnl_fixact(Aexagval);

}

void
update_exag ()
{
double r_t[3];

    if(InFocus){
	if(Z_exag)
	    REAL_TO[Z] /= Z_exag;
	else{
	    r_t[X] = REAL_TO[X];
	    r_t[Y] = REAL_TO[Y];
	    buf_interp((int)(REAL_TO[X]/X_Res),
		(int)(Y_Size-1 - (int)(REAL_TO[Y]/Y_Res)), r_t); 
	    REAL_TO[Z] = r_t[Z] - (double)Zoff;
	}
	_update_exag ();
	REAL_TO[Z] = (Z_exag? REAL_TO[Z]*Z_exag: 0); 
	keep_focus();
    }
    else
	_update_exag ();

    update_projection ();
    do_fast_display ();
}

void
update_vectz ()
{
    float dz;
    
    if(Aflat->val){
	Vect_z = Avectz->val * 4*Z_Span;
	Vect_z -= 2*Z_Span;
	do_fast_display ();
    }
}

void
_update_view_dir ()
{
    transform_fromto ();
    pnl_fixact(Axy);
}

void
update_view_dir ()
{
    transform_fromto ();
    do_fast_display ();
    pnl_fixact(Axy);
}

void
update_incl ()
{
    transform_fromto ();
    do_fast_display ();
}

void
_update_persp ()
{
    persp = Apersp->val;
}

void
update_persp ()
{
    _update_persp ();

    update_projection ();
    do_fast_display ();

}

void
_do_globe()
{
    if(Aglobe->val){
	vert_func = (Func_ptr *)ll_vertex;
	normvert_func = (Func_ptr *)ll_norm_vertex;
	v_vert_func = (Func_ptr *)v_ll_vertex;
	vd_vert_func = (Func_ptr *)vd_ll_vertex;
	clear_mask();      /* may write seperate masking routine later */
	InFocus = FALSE;
	CenterSphere = TRUE;
	LatLon = TRUE;
    }
    else{
	vert_func = (Func_ptr *)flat_vertex;
	normvert_func = (Func_ptr *)flat_norm_vertex;
	v_vert_func = (Func_ptr *)v_flat_vertex;
	vd_vert_func = (Func_ptr *)vd_flat_vertex;
	LatLon = FALSE;
    }

    if(P_Scale->visible){ 
	P_Scale->selectable = !LatLon;
	pnl_fixpanel (P_Scale);
    }
    Awhat->selectable =  Afringe->selectable = 
	Afocus->selectable = Ascale->selectable = 
	Aconetree->selectable = Arndtree->selectable = !LatLon;
    pnl_fixact(Awhat);
    pnl_fixact(Afringe);
    pnl_fixact(Afocus);
    pnl_fixact(Ascale);
    pnl_fixact(Aconetree);
    pnl_fixact(Arndtree);
}

void
do_globe()
{
    _do_globe();
    update_projection();
    do_fast_display();

}


#define POLY_PER_SEC 5000
void
do_type()
{
int check_poly = 0;
int check_grid = 0;

    if(Adotype->val){
	if(Agrid->val && ((X_Size/X_Mod)*(Y_Size/Y_Mod)) > .6 * POLY_PER_SEC){
	    ringbell();
	    fprintf(stderr, "WARNING: Drawing time may be very slow\n");
	    fprintf(stderr, "<try changing grid resolution>\n");
	}
	else if(Apoly->val && (X_Size/X_Modr)*(Y_Size/Y_Modr) > POLY_PER_SEC){
	    ringbell();
	    fprintf(stderr, "WARNING: Drawing time may be very slow\n");
	    fprintf(stderr, "<try changing polygon resolution>\n");
	}
	else if(Agpoly->val){
	    if((X_Size/X_Modr)*(Y_Size/Y_Modr) > .4 * POLY_PER_SEC)
		check_poly = 1;
	    if((X_Size/X_Mod)*(Y_Size/Y_Mod) > .3 * POLY_PER_SEC)
		check_grid = 1;
	    if(check_poly || check_grid){
		ringbell();
		fprintf(stderr, "WARNING: Drawing time may be very slow\n");
		if(check_poly)
		    fprintf(stderr,"<try changing polygon resolution>\n");
		if(check_grid)
		    fprintf(stderr,"<try changing grid resolution>\n");
	    }
	}
    }
}


do_radio(a, a1, a2)
Actuator *a, *a1, *a2;
{
    if (a == a1)
    {
	if (a2->val)
	{
	    a2->val = 0;
	    pnl_fixact (a2);
	}
    }
    if (a == a2)
    {
	if (a1->val)
	{
	    a1->val = 0;
	    pnl_fixact (a1);
	}
    }
}

do_radio3(a, a1, a2, a3)
Actuator *a, *a1, *a2, *a3;
{
    if (a == a1)
    {
	if (a2->val)
	{
	    a2->val = 0;
	    pnl_fixact (a2);
	}
	if (a3->val)
	{
	    a3->val = 0;
	    pnl_fixact (a3);
	}
    }
    if (a == a2)
    {
	if (a1->val)
	{
	    a1->val = 0;
	    pnl_fixact (a1);
	}
	if (a3->val)
	{
	    a3->val = 0;
	    pnl_fixact (a3);
	}
    }
    if (a == a3)
    {
	if (a1->val)
	{
	    a1->val = 0;
	    pnl_fixact (a1);
	}
	if (a2->val)
	{
	    a2->val = 0;
	    pnl_fixact (a2);
	}
    }
}

do_radio6(a, a1, a2, a3, a4, a5, a6)
Actuator *a, *a1, *a2, *a3, *a4, *a5, *a6;
{
Actuator *ta[6];
int i, j;

    ta[0] = a1;
    ta[1] = a2;
    ta[2] = a3;
    ta[3] = a4;
    ta[4] = a5;
    ta[5] = a6;

    for (i=0; i<6; i++){
	if(a == ta[i]){
	    for (j=0; j<6; j++){
		if(j != i){
		    if(ta[j]->val){
			ta[j]->val = 0;
			pnl_fixact (ta[j]);
		    }
		}
	    }
	}
    }

}


serve_actuator (a)
    Actuator *a;
{

    if (a == Aquit)
    {
	fprintf(stderr, "BYE!\n<%s>\n", G_recreate_command());
	leave ();
    }
    if (a == Aread)
    {
	get_settings(NULL);
	do_fast_display();
    }
    if (a == Asave)
    {
	save_settings();
    }
    if (a == Anewcell)
    {
	newcell ();	
    }
    if (a == Anewsite)
    {
	new_sites ();	
    }
    if (a == Aspheresite || a == Axsite || a == Aoctosite || a == Aglyph2site
		 || a == Aglyph1site || a == Aconetree || a == Arndtree){
	/* allow glyph1 & glyph2 to be on at same time */
	if(a == Aglyph1site)
	    do_radio6(a, Aspheresite, Aoctosite, Axsite, Aglyph1site,
					Aconetree, Arndtree);
	else if(a == Aglyph2site)
	    do_radio6(a, Aspheresite, Aoctosite, Axsite, Aglyph2site,
					Aconetree, Arndtree);
	else{
	    do_radio6(a, Aspheresite, Aoctosite, Axsite, Aglyph1site,
					Aconetree, Arndtree);
	    if(Aglyph2site->val){
		Aglyph2site->val = 0;
		pnl_fixact (Aglyph2site);
	    }
	}
	return(0);
    }

/*
    if (a == Anozero)
    {
	do_fast_display();
    }
Probably don't want to display, in case preparing to draw a second surface */

    if (a == Aputscale)
    {
	if(LatLon)
	    return(0);
	if(a->val){
	    frontbuffer(1);
	    put_scale();
	    frontbuffer(0);
	    Aputscale->val = 0;
	    pnl_fixact(Aputscale);
	}
	return(0);
    }
    if (a == Aputlabel)
    {
	if(a->val){
	    frontbuffer(1);
	    put_label();
	    frontbuffer(0);
	    Aputlabel->val = 0;
	    pnl_fixact(Aputlabel);
	}
	return(0);
    }
    if (a == Afocus)
    {
	if(LatLon)
	    return(0);
	if (a->val){
	    InFocus = 1;	
	    set_real_to();
	    if(InFocus) keep_focus();
	    update_projection();
	    do_fast_display();
	    Afocus->val = 0;
	    pnl_fixact(Afocus);
	}
	if(Alookforward->val){
	    Alookforward->val = 0;
	    pnl_fixact(Alookforward);
	}
	return (0);
    }
    if (a == Acenter){
	if(Alookforward->val){
	    Alookforward->val = 0;
	    pnl_fixact(Alookforward);
	}
	return (0);
    }
    if (a == Anofocus)
    {
	InFocus = 0;
	CenterSphere = 0;
	do_fast_display();
    }
    if (a == Alook || a == Aincl)
    {
	if (InFocus){
	    InFocus = 0;
	}
    }
    if (a == Awhat)
    {
	if(a->val){
	    whats_here();
	    Awhat->val = 0;
	    pnl_fixact (Awhat);
	}
    }
    if (a == Ashading)
    {
	Shading = a->val;
	if(Shading)
	    shademodel(GOURAUD);
	else
	    shademodel(FLAT);
	redraw_ok = 1;
	return (0);
    }
    if (a == Atransp1 || a == Atransp2 || a == Atransp3){
	do_radio3(a, Atransp1, Atransp2, Atransp3);
	return(0);
    }
    if (a == Adrape || a == Aflat){
	do_radio(a, Adrape, Aflat);
	if (a == Aflat){ /* reset z_up to 0? */
	    Avectz->val = 0.0;
	    pnl_fixact (Avectz);
	}
	return(0);
    }

    if (a == Agrid || a == Agridc)
    {
	if (!Agrid->val)
	{
	    Agrid->val = 1;
	    pnl_fixact (Agrid);
	}
	if (Apoly->val)
	{
	    Apoly->val = 0;
	    pnl_fixact (Apoly);
	}
	if (Agpoly->val)
	{
	    Agpoly->val = 0;
	    pnl_fixact (Agpoly);
	}
	if (Alight->val)
	{
	    Alight->val = 0;
	    pnl_fixact (Alight);
	}
	if (Asurface->val)
	{
	    Asurface->val = 0;
	    pnl_fixact (Asurface);
	}
	Display_type = D_GRID;
	return (0);
    }

    if (a == Agpoly)
    {
	if (Apoly->val)
	{
	    Apoly->val = 0;
	    pnl_fixact (Apoly);
	}
	if (Agrid->val)
	{
	    Agrid->val = 0;
	    pnl_fixact (Agrid);
	}
	if (Agridc->val)
	{
	    Agridc->val = 0;
	    pnl_fixact (Agridc);
	}
	Display_type = D_GPOLY;
	return (0);
    }
    if (a == Apoly)
    {
	if (Agrid->val)
	{
	    Agrid->val = 0;
	    pnl_fixact (Agrid);
	}
	if (Agpoly->val)
	{
	    Agpoly->val = 0;
	    pnl_fixact (Agpoly);
	}
	if (Agridc->val)
	{
	    Agridc->val = 0;
	    pnl_fixact (Agridc);
	}
	Display_type = D_POLY;
	return (0);
    }
    if (a == Adraw)
    {
	TOGGLE(P_Menus->selectable);
	pnl_fixpanel (P_Menus);
	TOGGLE(P_Mvmt->selectable);
	pnl_fixpanel (P_Mvmt);

	do_display (Display_type, (int)(!Anoclear->val), 1);
	Model_showing = 0;

	TOGGLE(P_Menus->selectable);
	pnl_fixpanel (P_Menus);
	TOGGLE(P_Mvmt->selectable);
	pnl_fixpanel (P_Mvmt);
	
	return (0);
    }
    if(a == Alookforward){
	InFocus = 0;
	CenterSphere = 0;
	return (0);
    }
    if(a == Aterrain || a == Alevel){
	do_radio(a,Aterrain,Alevel);
	return (0);
    }
    if(a == Aspline || a == Alinterp){
	do_radio(a,Aspline,Alinterp);
	return (0);
    }
    if(a == Asplpath || a == Alinpath){
	do_radio(a,Asplpath,Alinpath);
	return (0);
    }
/*
    if(a == Ashowpath){
	if(Ashowpath->val){
	    show_path();
	}
	return (0);
    }
*/
    if (a == Afringe)
    {
/* just turned on
	if (Afringe->val)
	    display_fringe (X_Modr, Y_Modr);
*/
	if (Afringe->val){
	    concave(1);
	    gconfig();
	}
	else{
	    concave(0);
	    gconfig();
	}
	Fringe_on = Afringe->val;
    }
    if (a == Avect)
    {
	if(Vect_file) do_vect_display ();
	return (0);
    }
    if (a == Asites)
    {
	if(Site_file) do_site_display ();
	return (0);
    }
    if (a == Adump)
    {
	save_window_img();
	return (0);
    }
    if (a == Afast1 || a == Afast2)
    {
	slow_start(a, Afast1, Afast2, &fast_res, 10);
	update_fast_res ();

	return (0);
    }
    if (a == Aslow1 || a == Aslow2)
    {
	slow_start(a, Aslow1, Aslow2, &slow_res, 10);
	update_slow_res ();

	return (0);
    }
    if (a == Avwidth1 || a == Avwidth2)
    {  
	slow_start(a, Avwidth1, Avwidth2, &V_Width, 10);
	update_vect_width ();

	return (0);
    }
    if (a == Arange1 || a == Arange2)
    {
	slow_start(a, Arange2, Arange1, &Range, 6);
	update_range();
	do_fast_display();

	return (0);
    }
    if (a == Areset)
    {
	initialize2 ();
	if(View_file)
	    get_settings(Viewname);
	do_fast_display();

	return (0);
    }
    if (a == Alight)
    {
	if(!Alight->val){     /* if turning off lights, turn off surf */
	    if (Asurface->val)
	    {
		Asurface->val = 0;
		pnl_fixact (Asurface);
	    }
	}
	return (0);
    }
    if (a == Asurface)
    {
	if (!Alight->val)
	{
	    Alight->val = 1;
	    pnl_fixact (Alight);
	}
	return (0);
    }
    if ((a == Altheight) || (a == Alightxy))
    {
	if (Afollow->val)
	{
	    Afollow->val = 0;
	    pnl_fixact (Afollow);
	}
	return (0);
    }
    if (a == Afollow)
    {
	if(Afollow->val){
	    To_Light[X] = LightPos[X] - FROM_TO[FROM][X];
	    To_Light[Y] = LightPos[Y] - FROM_TO[FROM][Y];
	    To_Light[Z] = LightPos[Z] - FROM_TO[FROM][Z];
	}
	return (0);
    }
    if(a == A3dscale || a == Aflatscale){
	do_radio(a, Aflatscale, A3dscale);
	return(0);
    }
    if(a == Anwticks || a == Aseticks)
    {
	if(Aneticks->val){
	    Aneticks->val=0;
	    pnl_fixact(Aneticks);
	}
	if(Aswticks->val){
	    Aswticks->val=0;
	    pnl_fixact(Aswticks);
	}
    }
    if(a == Aneticks || a == Aswticks)
    {
	if(Anwticks->val){
	    Anwticks->val=0;
	    pnl_fixact(Anwticks);
	}
	if(Aseticks->val){
	    Aseticks->val=0;
	    pnl_fixact(Aseticks);
	}
    }
    if(a == Aroman || a == Ahelvetica){
	do_radio(a, Ahelvetica, Aroman);
	return(0);
    }
    if(a == Apersp)
    {
	if(Aortho->val){
	    Aortho->val=0;
	    pnl_fixact(Aortho);
	}
    }


    return (0);
}


void 
_two_buf ()
{
    if(Aflat->val) two_buf();
}

void
two_buf ()
{
    if(!getdisplaymode()){  /* therefore singlebuffer */
	doublebuffer();
	gconfig ();
	/*   was clearing front buffer every time, but this is 
	     distracting when using several movement actuators, since
	     you get a flash each time the button goes down; the tradoff
	     is that after a "draw", the first time double buffering is
	     enacted there will be a disconcerting momentary color shift
	     of the surface before the wire frame appears.
	*/
    }
}

void 
_one_buf()
{
    if(Aflat->val) one_buf();
}

void
one_buf ()
{
    if(getdisplaymode()){
	singlebuffer();
	gconfig();
	do_fast_display ();
    }
}

update_fast_res ()
{
    static char fast_buf[10];

    if (fast_res > RES_MAX)
	fast_res = RES_MAX;
    if (fast_res < 1)
	fast_res = 1;

    sprintf (fast_buf, "%6d", fast_res);
    Afast2->label = fast_buf;
    pnl_fixact (Afast2);

    X_Mod = fast_res;
    Y_Mod = fast_res;
}

update_slow_res ()
{
    static char slow_buf[10];

    if (slow_res > RES_MAX)
	slow_res = RES_MAX;
    if (slow_res < 1)
	slow_res = 1;

    sprintf (slow_buf, "%6d", slow_res);
    Aslow2->label = slow_buf;
    pnl_fixact (Aslow2);

    X_Modr = slow_res;
    Y_Modr = slow_res;
}

update_vect_width ()
{
    static char vect_buf[10];

    if (V_Width > 15)
	V_Width = 15;
    if (V_Width < 1)
	V_Width = 1;

    sprintf (vect_buf, "%6d", V_Width);
    Avwidth2->label = vect_buf;
    pnl_fixact (Avwidth2);
}

update_color(colvar, stringbuf, val)
long *colvar;
char *stringbuf;
int val;
{

    switch (val){
	case 0:
	    sprintf (stringbuf, "black  \0");
	    *colvar = 0x000000;
	    break;
	case 1:
	    sprintf (stringbuf, "red    \0");
	    *colvar = 0x0000FF;
	    break;
	case 2:
	    sprintf (stringbuf, "green  \0");
	    *colvar = 0x00FF00;
	    break;
	case 3:
	    sprintf (stringbuf, "yellow \0");
	    *colvar = 0x00FFFF;
	    break;
	case 4:
	    sprintf (stringbuf, "blue   \0");
	    *colvar = 0xFF0000;
	    break;
	case 5:
	    sprintf (stringbuf, "magenta\0");
	    *colvar = 0xFF00FF;
	    break;
	case 6:
	    sprintf (stringbuf, "cyan   \0");
	    *colvar = 0xFFFF00;
	    break;
	case 8:
	    sprintf (stringbuf, "grey   \0");
	    *colvar = 0x777777;
	    break;
	default:
	    sprintf (stringbuf, "white  \0");
	    *colvar = 0xFFFFFF;
	    break;
    }
}


void
update_vect_color ()
{
    static char vect_buf[20];
    char cobuf[10];

    update_color(&V_Color, cobuf, (int)Avcolor->val);
    sprintf (vect_buf, "color = %s",cobuf);

    Avcolor->label = vect_buf;
    pnl_fixact (Avcolor);
}


void
update_bg_color ()
{
    static char bg_buf[30];
    char cobuf[10];

    update_color(&BGcolor, cobuf, (int)Abgcolor->val);
    sprintf (bg_buf, "background color = %s",cobuf);

    Abgcolor->label = bg_buf;
    pnl_fixact (Abgcolor);
}

void
update_site_color ()
{
    static char st_buf[30];
    char cobuf[10];

    update_color(&STcolor, cobuf, (int)Astcolor->val);
    sprintf (st_buf, "    color = %s",cobuf);

    Astcolor->label = st_buf;
    pnl_fixact (Astcolor);
}

update_range ()
{
    static char range_buf[10];

    if (Range > 14)
	Range = 14;
    if (Range < 0)
	Range = 0;
    /*sprintf (range_buf, "%s", Range_txt[Range]);*/
    /*Arange2->label = range_buf;*/
    Arange2->label = Range_txt[Range];
    pnl_fixact (Arange2);

    /* change ranges for exageration, height, and light height */

    Aexag->val = Aexag->val / Aexag->maxval;   /* temporary */
    Aexag->maxval = 10.0 * Range_val[Range];
    Aexag->val *= Aexag->maxval;
    pnl_fixact(Aexag);

    _update_exag ();
    _update_zbounds ();
    _update_height ();
    _update_ltheight ();
    _update_view_dir ();
}

/*
** this is only called when viewing position needs to be 
**  re-transformed  (i.g. for view dir or view incl)
**  simple translation does not require this to be called,
**  since that can be handled by simple addition.
**
**  FROM_TO is actual X,Y,Z locations of eye position and point
**   in space that we are looking towards.  The base position is
**   (1000,0,0)  as given in UNIT_FROM_TO
**  This is then translated the FROM_TO[FROM] position, and then
**   rotated to get the LOOK_TO position.
*/
transform_fromto ()
{

    P_popmatrix ();
    P_pushmatrix ();

	P_translate (FROM_TO[FROM][X],FROM_TO[FROM][Y],FROM_TO[FROM][Z]);
	P_rotate ((int) Alook->val, 'z');
	P_rotate ((int) Aincl->val, 'y');
	P_transform (2, UNIT_FROM_TO, FROM_TO);
	
	update_projection ();
}

/*
**  update the Z_Min and Z_Max values.
*/
_update_zbounds ()
{
    Z_Max = (Z_Max_real - Z_Min_real) * Z_exag;
    Z_Span = 4000.0;
    ZRange = 3000.0 ;
}


void
slow_start(a, upact, dnact, var, speed)
    Actuator *a, *upact, *dnact;
    int *var;
    int speed;
{
    static int dn_cnt, delay;

	
	if (pnl_justdown){
	    dn_cnt = 1;
	    delay = speed;
	    if (a == upact)
		*var = *var + 1;
	    else
		*var = *var - 1;
	}
	if (!(dn_cnt % delay)){
	    if (a == upact)
		*var = *var + 1;
	    else
		*var = *var - 1;
	    if(delay > 2)delay --;
	    dn_cnt = 1;
	}
	dn_cnt ++; 

}
