
/*
**  Written by Bill Brown, Spring 1994 
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "externs.h"
#include "math.h"

extern void pnl_discrete_hpalette();
extern void update_site_color();
extern void set_Ccolor();
extern void new_sitecol();
extern void sites_describe();
void update_sitefields();
void update_sitemods();

/* makes transform actuators for site values */
make_sitemod(y, sfield, P)
float *y;
int sfield;
Panel *P;
{
    
    *y -= .6;
    Asfmod_add[sfield]=pnl_mkact (pnl_typein);
    Asfmod_add[sfield]->x = 4.0;
    Asfmod_add[sfield]->y = *y;
    Asfmod_add[sfield]->labeltype = PNL_LABEL_LEFT;
    Asfmod_add[sfield]->label= "Transform:  + ";
    PNL_ACCESS(Typein, Asfmod_add[sfield], len)=8;
    PNL_ACCESS(Typein, Asfmod_add[sfield], str)="0";
    Asfmod_add[sfield]->upfunc = update_sitemods;
    pnl_addact (Asfmod_add[sfield], P);

    *y -= .5;
    Asfmod_mult[sfield]=pnl_mkact (pnl_typein);
    Asfmod_mult[sfield]->x = 4.0;
    Asfmod_mult[sfield]->y = *y;
    Asfmod_mult[sfield]->labeltype = PNL_LABEL_LEFT;
    Asfmod_mult[sfield]->label= "* ";
    PNL_ACCESS(Typein, Asfmod_mult[sfield], len)=8;
    PNL_ACCESS(Typein, Asfmod_mult[sfield], str)="1";
    Asfmod_mult[sfield]->upfunc = update_sitemods;
    pnl_addact (Asfmod_mult[sfield], P);

}

make_site_panel()
{
Actuator *a;
float ny = 15.;
int i;

    for(i=0; i<MAX_ST_ATTS; i++)
	Sfieldno[i] = 0;

    if (NULL == (P_Sites = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Sites->label = "Sites";
    P_Sites->visible = 0;
    P_Sites->ppu *= .8;  /* shrink it */

    /* new sites file */
    Anewsite=pnl_mkact (pnl_wide_button);
    Anewsite->label = " New Sites File ";
    Anewsite->x = 0.5;
    Anewsite->y = ny-=.5;
    pnl_addact (Anewsite, P_Sites);

    /* new sites file */
    Asiteinfo=pnl_mkact (pnl_wide_button);
    Asiteinfo->label = " Info";
    Asiteinfo->x = 4.6;
    Asiteinfo->y = ny;
    Asiteinfo->downfunc = sites_describe;
    pnl_addact (Asiteinfo, P_Sites);

    /* display x sites */
    Axsite=pnl_mkact (pnl_toggle_button);
    Axsite->label = " x";
    Axsite->val = 1;
    Axsite->x = 0.5;
    Axsite->y = ny-=1.;
    pnl_addact (Axsite, P_Sites);
    
    /* display conetree sites */
    Aconetree=pnl_mkact (pnl_toggle_button);
    Aconetree->label = " cone tree";
    Aconetree->val = 0;
    Aconetree->x = 3.5;
    Aconetree->y = ny;
    pnl_addact (Aconetree, P_Sites);
    
    /* display sphere sites */
    Aspheresite=pnl_mkact (pnl_toggle_button);
    Aspheresite->label = " sphere";
    Aspheresite->x = 0.5;
    Aspheresite->y = ny-=.5;
    pnl_addact (Aspheresite, P_Sites);

    /* display round tree sites */
    Arndtree=pnl_mkact (pnl_toggle_button);
    Arndtree->label = " round tree";
    Arndtree->x = 3.5;
    Arndtree->y = ny;
    pnl_addact (Arndtree, P_Sites);

    /* display octohedron sites */
    Aoctosite=pnl_mkact (pnl_toggle_button);
    Aoctosite->label = " octohedron";
    Aoctosite->x = 0.5;
    Aoctosite->y = ny-=.5;
    pnl_addact (Aoctosite, P_Sites);

    /* display conehead (glyph1) sites */
    Aglyph1site=pnl_mkact (pnl_toggle_button);
    Aglyph1site->label = " glyph1";
    Aglyph1site->val = 0;
    Aglyph1site->x = 3.5;
    Aglyph1site->y = ny;
    pnl_addact (Aglyph1site, P_Sites);
    
    /* display cylinder sites */
    Acylsite=pnl_mkact (pnl_toggle_button);
    Acylsite->label = " cylinder";
    Acylsite->x = 0.5;
    Acylsite->y = ny-=.5;
    pnl_addact (Acylsite, P_Sites);

    /* display inverse conehead (glyph2) sites */
    Aglyph2site=pnl_mkact (pnl_toggle_button);
    Aglyph2site->label = " glyph2";
    Aglyph2site->val = 0;
    Aglyph2site->x = 3.5;
    Aglyph2site->y = ny;
    pnl_addact (Aglyph2site, P_Sites);

    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- - - - -  SIZE   - - - - -";
    a->x = .5;
    a->y = ny-=.5;
    pnl_addact (a, P_Sites);
    
    /* size of sites */
    Asitesiz=pnl_mkact(pnl_hslider);
    Asitesiz->label=" size ";
    Asitesiz->x= 0.5;
    Asitesiz->y= ny-=.5;
    Asitesiz->w= 4.0;
    Asitesiz->h= 0.3;
    Asitesiz->minval= 0.0;
    Asitesiz->maxval= 1.0;
    Asitesiz->val= 0.5;
    pnl_addact(Asitesiz, P_Sites);

    /* size toggle - use cat */
    Asfsize_c=pnl_mkact (pnl_toggle_button);
    Asfsize_c->label = " Use category";
    Asfsize_c->x = 0.5;
    Asfsize_c->y = ny-=1.;
    pnl_addact (Asfsize_c, P_Sites);
    
    /* size field toggle */
    Asfsize=pnl_mkact (pnl_toggle_button);
    Asfsize->label = " Use value field:";
    Asfsize->x = 0.5;
    Asfsize->y = ny-=.5;
    Asfsize->downfunc = update_sitefields;
    pnl_addact (Asfsize, P_Sites);
    
    /* size field text */
    Asfsize_txt=pnl_mkact (pnl_typein);
    Asfsize_txt->x = 5.0;
    Asfsize_txt->y = ny;
    PNL_ACCESS(Typein, Asfsize_txt, len)=3;
    PNL_ACCESS(Typein, Asfsize_txt, str)="1";
    Asfsize_txt->upfunc = update_sitefields;
    pnl_addact (Asfsize_txt, P_Sites);

    make_sitemod(&ny, SF_SIZE, P_Sites);
    
    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- - - - ALT. SIZE - - - - -";
    a->x = .5;
    a->y = ny-=.5;
    pnl_addact (a, P_Sites);
    
    /* size2 toggle - use cat */
    Asfsize2_c=pnl_mkact (pnl_toggle_button);
    Asfsize2_c->label = " Use category";
    Asfsize2_c->x = 0.5;
    Asfsize2_c->y = ny-=.6;
    pnl_addact (Asfsize2_c, P_Sites);
    
    /* size2 field toggle */
    Asfsize2=pnl_mkact (pnl_toggle_button);
    Asfsize2->label = " Use value field:";
    Asfsize2->x = 0.5;
    Asfsize2->y = ny-=.5;
    Asfsize2->downfunc = update_sitefields;
    pnl_addact (Asfsize2, P_Sites);
    
    /* size2 field text */
    Asfsize2_txt=pnl_mkact (pnl_typein);
    Asfsize2_txt->x = 5.0;
    Asfsize2_txt->y = ny;
    PNL_ACCESS(Typein, Asfsize2_txt, len)=3;
    PNL_ACCESS(Typein, Asfsize2_txt, str)="1";
    Asfsize2_txt->upfunc = update_sitefields;
    pnl_addact (Asfsize2_txt, P_Sites);

    make_sitemod(&ny, SF_SIZE2, P_Sites);
    
    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- - - - -  COLOR  - - - - -";
    a->x = .5;
    a->y = ny-=.5;
    pnl_addact (a, P_Sites);
    
    Astcolor = pnl_mkact (pnl_discrete_hpalette);
    Astcolor->label = "    color = green  ";
    Astcolor->minval = 0;
    Astcolor->maxval = NUM_STANDARD_COLORS + NUM_CUSTOM_COLORS ;
    Astcolor->x = 0.5;
    Astcolor->y = ny-=.7;
    Astcolor->w = 4.5;
    Astcolor->h = 0.5;
    Astcolor->activefunc = update_site_color;
    pnl_addact (Astcolor, P_Sites);

    /* Set Custom Color */
    AstCcset= pnl_mkact (pnl_button);
    AstCcset->label = "set";
    AstCcset->x = 5.25;
    AstCcset->y = ny;
    AstCcset->labeltype = PNL_LABEL_BOTTOM;
    AstCcset->downfunc = set_Ccolor;
    pnl_addact (AstCcset, P_Sites);

    /* new sites color file */
    Anewsitecol=pnl_mkact (pnl_wide_button);
    Anewsitecol->label = " New Sites Color File ";
    Anewsitecol->x = 0.5;
    Anewsitecol->y = ny-=1.1;
    Anewsitecol->downfunc = new_sitecol;
    pnl_addact (Anewsitecol, P_Sites);

    /* color toggle - use cat */
    Asfcolor_c=pnl_mkact (pnl_toggle_button);
    Asfcolor_c->label = " Use category";
    Asfcolor_c->x = 0.5;
    Asfcolor_c->y = ny-=.6;
    pnl_addact (Asfcolor_c, P_Sites);
    
    /* color field toggle */
    /* use site color map (sctually color structure from raster map) */
    Asfcolor=pnl_mkact (pnl_toggle_button);
    Asfcolor->label = " Use value field:";
    Asfcolor->val = Map_Sitecolor;
    Asfcolor->x = 0.5;
    Asfcolor->y = ny-=.5;
    Asfcolor->downfunc = update_sitefields;
    pnl_addact (Asfcolor, P_Sites);
    
    /* color field text */
    Asfcolor_txt=pnl_mkact (pnl_typein);
    Asfcolor_txt->x = 5.0;
    Asfcolor_txt->y = ny;
    PNL_ACCESS(Typein, Asfcolor_txt, len)=3;
    if(Site_cat_isZ && Map_Sitecolor){
	Sfieldno[SF_COLOR] = 1;
	PNL_ACCESS(Typein, Asfcolor_txt, str)="2";
    }
    else
	PNL_ACCESS(Typein, Asfcolor_txt, str)="1";
    Asfcolor_txt->upfunc = update_sitefields;
    pnl_addact (Asfcolor_txt, P_Sites);
    
    make_sitemod(&ny, SF_COLOR, P_Sites);

    /* RGB values */
    AsfcolorRGB=pnl_mkact (pnl_toggle_button);
    AsfcolorRGB->label = " Read RGB";
    AsfcolorRGB->x = 0.5;
    AsfcolorRGB->y = ny-=.6;
    AsfcolorRGB->val = 0.0;
    pnl_addact (AsfcolorRGB, P_Sites);

    /* RGB fields */
    AsfRGBfields=pnl_mkact (pnl_typein);
    PNL_ACCESS(Typein, AsfRGBfields, len)=10;
    PNL_ACCESS(Typein, AsfRGBfields, str)="1 2 3";
    AsfRGBfields->label = " fields:";
    AsfRGBfields->labeltype = PNL_LABEL_LEFT;
    AsfRGBfields->x = 3.5;
    AsfRGBfields->y = ny-=.5;
    pnl_addact (AsfRGBfields, P_Sites);

    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- -  VERTICAL POSITION  - -";
    a->x = .5;
    a->y = ny-=.5;
    pnl_addact (a, P_Sites);
    
    /* z toggle - use cat */
    Asfz_c=pnl_mkact (pnl_toggle_button);
    Asfz_c->label = " Use category";
    Asfz_c->x = 0.5;
    Asfz_c->y = ny-=.6;
    pnl_addact (Asfz_c, P_Sites);
    
    /* z field toggle */
    Asfz=pnl_mkact (pnl_toggle_button);
    Asfz->label = " Use value field:";
    Asfz->x = 0.5;
    Asfz->y = ny-=.5;
    Asfz->downfunc = update_sitefields;
    Asfz->val = Site_cat_isZ;
    pnl_addact (Asfz, P_Sites);
    
    /* z field text */
    Asfz_txt=pnl_mkact (pnl_typein);
    Asfz_txt->x = 5.0;
    Asfz_txt->y = ny;
    PNL_ACCESS(Typein, Asfz_txt, len)=3;
    PNL_ACCESS(Typein, Asfz_txt, str)="1";
    Asfz_txt->upfunc = update_sitefields;
    pnl_addact (Asfz_txt, P_Sites);
    
    /* connect site to surface */
    Aconsite=pnl_mkact (pnl_toggle_button);
    Aconsite->label = " connect to surface";
    Aconsite->val = Site_cat_isZ;
    Aconsite->x = 1.5;
    Aconsite->y = ny-=.5;
    pnl_addact (Aconsite, P_Sites);

    make_sitemod(&ny, SF_Z, P_Sites);

#ifdef OLD
    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- - - - GLYPH SHAPE - - - -";
    a->x = .5;
    a->y = ny-=.5;
    pnl_addact (a, P_Sites);
    
    /* shape toggle - use cat */
    Asfshape_c=pnl_mkact (pnl_toggle_button);
    Asfshape_c->label = " Use category";
    Asfshape_c->x = 0.5;
    Asfshape_c->y = ny-=.6;
    pnl_addact (Asfshape_c, P_Sites);
    
    /* shape field toggle */
    Asfshape=pnl_mkact (pnl_toggle_button);
    Asfshape->label = " Use value field:";
    Asfshape->x = 0.5;
    Asfshape->y = ny-=.5;
    Asfshape->downfunc = update_sitefields;
    pnl_addact (Asfshape, P_Sites);
    
    /* shape field text */
    Asfshape_txt=pnl_mkact (pnl_typein);
    Asfshape_txt->x = 5.0;
    Asfshape_txt->y = ny;
    PNL_ACCESS(Typein, Asfshape_txt, len)=3;
    PNL_ACCESS(Typein, Asfshape_txt, str)="1";
    Asfshape_txt->upfunc = update_sitefields;
    pnl_addact (Asfshape_txt, P_Sites);
    
    make_sitemod(&ny, SF_SHAPE, P_Sites);
#endif

    /* seperator label */
    a=pnl_mkact (pnl_label);
    a->label = "- - - - DRAW ORDER  - - - -";
    a->x = .5;
    a->y = ny-=.5;
    pnl_addact (a, P_Sites);
    
    /* shape toggle - use cat */
    Asfdorder_c=pnl_mkact (pnl_toggle_button);
    Asfdorder_c->label = " Use category";
    Asfdorder_c->x = 0.5;
    Asfdorder_c->y = ny-=.6;
    pnl_addact (Asfdorder_c, P_Sites);
    
    /* draw order field toggle */
    Asfdorder=pnl_mkact (pnl_toggle_button);
    Asfdorder->label = " Use value field:";
    Asfdorder->x = 0.5;
    Asfdorder->y = ny-=.5;
    Asfdorder->downfunc = update_sitefields;
    pnl_addact (Asfdorder, P_Sites);
    
    /* draw order field text */
    Asfdorder_txt=pnl_mkact (pnl_typein);
    Asfdorder_txt->x = 5.0;
    Asfdorder_txt->y = ny;
    PNL_ACCESS(Typein, Asfdorder_txt, len)=3;
    PNL_ACCESS(Typein, Asfdorder_txt, str)="1";
    Asfdorder->upfunc = update_sitefields;
    pnl_addact (Asfdorder_txt, P_Sites);
    
    make_sitemod(&ny, SF_DORDER, P_Sites);

    /* CLOSE */
    make_close(2.5, ny-=.75, P_Sites);
  
}

/* return a valid field index in sf: return 0 if input illegal */
str_to_sitefield(str, sf)
char *str;
int *sf;
{
int ret;
    
    if(str){
	if(str[0] >= '1' && str[0] <= '9'){  /* no zeros or negatives */
	    sscanf(str,"%d",sf);
	    if(*sf > Snumvals || *sf > MAX_ST_ATTS){
		*sf = Snumvals + S_RESV;
		return (0);
	    }
	    *sf += S_RESV;
	    return(1);
	}
	switch(str[0]){
	    case 'X':
	    case 'x':
	    case 'e':
	    case 'E':
	    case 'w':
	    case 'W':
		*sf = S_X;
		return(1);
	    case 'Y':
	    case 'y':
	    case 'n':
	    case 'N':
	    case 's':
	    case 'S':
		*sf = S_Y;
		return(1);
	    case 'Z':
	    case 'z':
		if(Shas_z){
		    *sf = S_Z;
		    return(1);
		}
		break;
	    default:
		*sf = S_RESV+1;
		return(0);

	}
	*sf = S_RESV+1;
	return(0);
    }

}

#define SET_CHECK_SITEFIELD(a, SF)  \
	if(!str_to_sitefield(PNL_ACCESS(Typein, a, str), Sfieldno+SF)){ \
	    sprintf(PNL_ACCESS(Typein, a, str), "%d", Sfieldno[SF] - S_RESV); \
	}


void
update_sitefields(a)
Actuator *a;
{

    if (a == Asfsize_txt || a == Asfsize )
    {
	SET_CHECK_SITEFIELD(Asfsize_txt, SF_SIZE);
	pnl_fixact(Asfsize_txt);
    }
    else if (a == Asfsize2_txt || a == Asfsize2 )
    {
	SET_CHECK_SITEFIELD(Asfsize2_txt, SF_SIZE2);
	pnl_fixact(Asfsize2_txt);
    }
    else if (a == Asfcolor_txt || a == Asfcolor )
    {
	SET_CHECK_SITEFIELD(Asfcolor_txt, SF_COLOR);
	pnl_fixact(Asfcolor_txt);
    }
    else if (a == Asfz_txt || a == Asfz )
    {
	SET_CHECK_SITEFIELD(Asfz_txt, SF_Z);
	pnl_fixact(Asfz_txt);
    }
#ifdef OLD
    else if (a == Asfshape_txt || a == Asfshape )
    {
	SET_CHECK_SITEFIELD(Asfshape_txt, SF_SHAPE);
	pnl_fixact(Asfshape_txt);
    }
#endif
    else if (a == Asfdorder_txt || a == Asfdorder )
    {
	SET_CHECK_SITEFIELD(Asfdorder_txt, SF_DORDER);
	pnl_fixact(Asfdorder_txt);
    }
    pnl_fixact(a);

}

void
update_sitemods(a)
Actuator *a;
{
int i;
float tmp;

    i = sscanf(PNL_ACCESS(Typein, a, str), "%f", &tmp);

    if(i){
	for(i=0; i<NUM_ST_ATTS; i++){
	    if(a == Asfmod_mult[i]){
		Smult[i] = tmp;
		break;
	    }
	    if(a == Asfmod_add[i]){
		Sadd[i] = tmp;
		break;
	    }
	}
    }

}


