
/*
**  Written by Bill Brown   1994
**  US Army Construction Engineering Research Lab
*/

#include "externs.h"

void newelev();
void newcell();
void new_vect();
void new_sites();
void load_3dview();
void save_settings();
void save_window_img();
void do_leave();

void pop_lights();
void pop_vect();
void pop_sites();
void pop_options();
void pop_scale();
void pop_label();
void pop_key();
void pop_path();
void pop_script();
void pop_ep();


void
my_fixicon(a)
     Actuator *a;
{
  Icon *ad=(Icon *)a->data;
  char *buf;

#ifdef DEBUG
  fprintf (stdout,"_fixicon: id:%d \"%s\"\n", a->id, a->label?a->label:"<no label>");
#endif DEBUG

  /* added this line to fix bug with icon getting permanently
  big during reading of script */
  if(pnl_readscript && pnl_justup) return;

  a->w=MAX(a->w, a->lw+2*PNL_DIM_2);
  
  ad->xstowed=a->x;
  ad->ystowed=a->y;
  ad->wstowed=a->w;
  ad->hstowed=a->h;

  if (!a->ca) return;
  ad->wopen=a->ca->w;
  ad->hopen=a->ca->h;
  ad->xopen=ad->xstowed-(ad->wopen-ad->wstowed)/2.0; /* centered horizontally */
  ad->yopen=ad->ystowed+ad->hstowed-ad->hopen;	    /* 'pop-down' */

  buf=(char *) pnl_alloc(strlen(a->ca->label)+1);
  (void) strcpy(buf, a->ca->label);
  a->label=buf;
  pnl_labeldimensions(a);
  pnl_labeloffsets(a);
}


make_mainmenu(x, y, panel)
float x, y;
Panel *panel;
{
Actuator *a, *b, *AFMenu, *APMenu;

    AFMenu=pnl_mkact (pnl_icon_menu);
    AFMenu->label = " File...";
    AFMenu->x = x;
    AFMenu->y = y;
    AFMenu->w = 3.0;
    AFMenu->fixfunc = my_fixicon;
    pnl_addact (AFMenu, panel);

    a=pnl_mkact (pnl_menu_item);
    a->label = "New Elevation";
    a->upfunc = newelev;
    pnl_addsubact (a, AFMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "New Color";
    a->upfunc = newcell;
    pnl_addsubact (a, AFMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "New Vect";
    a->upfunc = new_vect;
    pnl_addsubact (a, AFMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "New Sites";
    a->upfunc = new_sites;
    pnl_addsubact (a, AFMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Load 3dview";
    a->upfunc = load_3dview;
    pnl_addsubact (a, AFMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Save 3dview";
    a->upfunc = save_settings;
    pnl_addsubact (a, AFMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Save Image";
    a->upfunc = save_window_img;
    pnl_addsubact (a, AFMenu);

    a=pnl_mkact (pnl_icon_menu);
    a->label = "Quit";
    a->fixfunc = my_fixicon;
    pnl_addsubact (a, AFMenu);

    b=pnl_mkact (pnl_menu_item);
    b->label = "Really Quit";
    b->upfunc = do_leave;
    pnl_addsubact (b, a);


    APMenu=pnl_mkact (pnl_icon_menu);
    APMenu->label = " Panels...";
    APMenu->x = x + 3.0;
    APMenu->y = y;
    APMenu->w = 2.75;
    APMenu->fixfunc = my_fixicon;
    pnl_addact (APMenu, panel);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Lights...";
    a->upfunc = pop_lights;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Vect...";
    a->upfunc = pop_vect;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Sites...";
    a->upfunc = pop_sites;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Options...";
    a->upfunc = pop_options;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Scales...";
    a->upfunc = pop_scale;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Labels...";
    a->upfunc = pop_label;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "KeyFrame...";
    a->upfunc = pop_key;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "VectPath...";
    a->upfunc = pop_path;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Script...";
    a->upfunc = pop_script;
    pnl_addsubact (a, APMenu);

    a=pnl_mkact (pnl_menu_item);
    a->label = "Position...";
    a->upfunc = pop_ep;
    pnl_addsubact (a, APMenu);
    
}

