#include "digit.h"
#include <gl.h>
#include <device.h>
#include <panel.h>
#include "actuators.h"
#include "ism.h"

static Panel *init_site_panel();

#define NUM_LINE_ROWS 10

struct Site_Acts {
    Actuator *Color;
    Actuator *Pattern;
    Actuator *Size;
};

static struct Site_Acts Site_Acts;
static Panel *P_Site_ann;
static int setup_act_values ();

static Actuator *A_Site_Prev, *A_Site_Next, *A_Site_Accept;
static Actuator *A_Site_info1, *A_Site_info2;
static Actuator *A_Site_info0;

static int Set_Up = 0;

void
ask_site_nolabel (map, style, color, size)
    struct Map_info *map;
    int *style, *color, *size;
{
    Actuator *a;
    int num_pages;
    int page = 0;

    if (!Set_Up)
    {
	Set_Up = 1;
	init_site_panel();
    }

    pnl_unselect_all (P_Site_ann);

    P_Site_ann->visible = 1;
    /*noborder ();*/
    pnl_fixpanel (P_Site_ann);

    while (1)
    {
	setup_act_values(map);

	a = pnl_dopanel ();
	if (a == A_Site_Accept)
	    break;
    }
    while (a->active)
	pnl_dopanel ();

    *style = Site_Acts.Pattern->val;
    *color = Site_Acts.Color->val;
    *size = Site_Acts.Size->val;

    P_Site_ann->visible = 0;
    pnl_fixpanel (P_Site_ann);

    pnl_select_all (P_Site_ann);
}


static
setup_act_values (map)
    struct Map_info *map;
{
    static char info1_buf[100];
    static char info2_buf[50];
    unsigned char *p;


    sprintf (info1_buf, "Select Symbol and color");
    A_Site_info1->label = info1_buf;
    pnl_setdirty (A_Site_info1);

    /*
    sprintf (info2_buf, "Page %3d of %3d", page+1, num_pages);
    A_Site_info2->label = info2_buf;
    pnl_setdirty (A_Site_info2);
    */

    Site_Acts.Color->label = ISM_Colors[(int) Site_Acts.Color->val];
    pnl_setdirty (Site_Acts.Color);
    /*Site_Acts.Color->u = &(Site_Acts.Color->val);*/

    Site_Acts.Pattern->label=ISM_Site_Symbols[(int)Site_Acts.Pattern->val];
    /*Site_Acts.Pattern->u = Site_Acts.Pattern->val;*/
    pnl_setdirty (Site_Acts.Pattern);

    Site_Acts.Size->label = ISM_Site_Size[(int) Site_Acts.Size->val];
    /*Site_Acts.Size->u = &(Site_Acts.Size->val);*/
    pnl_setdirty (Site_Acts.Size);
}

static Panel *
init_site_panel()
{
    Panel *p;

    /*noborder ();*/
    P_Site_ann =p= pnl_mkpanel();
    p->x = XMAXSCREEN/3;
    p->y = YMAXSCREEN/3;
    p->label="Site Attributes";
    p->ppu=50.0;
    p->upfunc=p->fixfunc;

    mk_site_other_acts (p);
    mk_site_stuff (p);

    p->visible = 0;

    return p;
}

static void rotate_color_labels();
static void rotate_pattern_labels();
static void rotate_size_labels();

static 
mk_site_stuff (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 0;

    Site_Acts.Color =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_color_labels;
    a->val = 1;			/* toggleable, see rotate_color_labels () */
    a->x = xoff+L_COL_POS;
    a->y = yoff+.5;
    a->label=ISM_Colors[1];	/* just something to set the size */
    pnl_addact(a, p);

    Site_Acts.Pattern =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_pattern_labels;
    a->val = 1;
    a->x = xoff+L_STYLE_POS;
    a->y = yoff+.5;
    a->label=ISM_Site_Symbols[1];
    pnl_addact(a, p);

    Site_Acts.Size =a= pnl_mkact (pnl_picklabel);
    a->val = 1;
    a->downfunc = rotate_size_labels;
    a->x = xoff+L_WEIGHT_POS;
    a->y = yoff+.5;
    a->label=ISM_Site_Size[1];
    pnl_addact(a, p);
}

static 
mk_site_other_acts (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 1;

/* INFO LINES */
    /*
    A_Site_info0 =a= pnl_mkact (pnl_label);
    a->x = 2;
    a->y = yoff+3;
    a->label="SITE ATTRIBUTES";
    pnl_addact(a, p);
    */

    A_Site_info1 =a= pnl_mkact (pnl_label);
    a->x = 4;
    a->y = yoff+1;
    a->label="#### Different Categories";
    pnl_addact(a, p);

    /*
    A_Site_info2 =a= pnl_mkact (pnl_label);
    a->x = 10;
    a->y = yoff+1;
    a->label="Page ### of ###";
    pnl_addact(a, p);
    */


/* Buttons */
    A_Site_Accept =a= pnl_mkact (pnl_wide_button);
    a->x = 10;
    a->y = yoff+1;
    a->label="ACCEPT";
    pnl_addact(a, p);


/* Column labels */
    a= pnl_mkact (pnl_label);
    a->x = xoff+L_COL_POS;
    a->y = yoff;
    a->label="Color";
    pnl_addact(a, p);

    a= pnl_mkact (pnl_label);
    a->x = xoff+L_STYLE_POS;
    a->y = yoff;
    a->label="Symbol";
    pnl_addact(a, p);

    a= pnl_mkact (pnl_label);
    a->x = xoff+L_WEIGHT_POS;
    a->y = yoff;
    a->label="Size";
    pnl_addact(a, p);
}

/*
#define XXX  (*(a->u))
*/
#define XXX  (a->val)

static void 
rotate_color_labels(a)
    Actuator *a;
{
    if (++(XXX) > MAX_AREA_COLOR)
	XXX = 1;

    a->label = ISM_Colors[(int)XXX];
    pnl_setdirty (a);
}

static void 
rotate_pattern_labels (a)
    Actuator *a;
{
    if (++(XXX) > MAX_AREA_PATTERN)
	XXX = 0;

    a->label = ISM_Site_Symbols[(int)XXX];
    pnl_setdirty (a);
}

static void 
rotate_size_labels(a)
    Actuator *a;
{
    if (++(XXX) > MAX_AREA_SIZE)
	XXX = 1;

    a->label = ISM_Site_Size[(int)XXX];
    pnl_setdirty (a);
}
