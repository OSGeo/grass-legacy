#include "digit.h"
#include <gl.h>
#include <device.h>
#include <panel.h>
#include "actuators.h"
#include "ism.h"

static Panel *init_area_panel();

#define NUM_LINE_ROWS 10

struct Area_Acts {
    Actuator *Color;
    Actuator *Pattern;
    Actuator *Size;
};

static struct Area_Acts Area_Acts;
static Panel *P_Area_ann;
static int setup_act_values ();

static Actuator *A_Area_Prev, *A_Area_Next, *A_Area_Accept;
static Actuator *A_Area_info1, *A_Area_info2;
static Actuator *A_Area_info0;

static int Set_Up = 0;

void
ask_area_nolabel (map, style, color, size)
    struct Map_info *map;
    int *style, *color, *size;
{
    Actuator *a;
    int num_pages;
    int page = 0;

    if (!Set_Up)
    {
	Set_Up = 1;
	init_area_panel();
    }

    pnl_unselect_all (P_Area_ann);

    P_Area_ann->visible = 1;
    /*noborder ();*/
    pnl_fixpanel (P_Area_ann);

    while (1)
    {
	setup_act_values(map);

	a = pnl_dopanel ();
	if (a == A_Area_Accept)
	    break;
    }
    while (a->active)
	pnl_dopanel ();

    *style = Area_Acts.Pattern->val;
    *color = Area_Acts.Color->val;
    *size = Area_Acts.Size->val;

    P_Area_ann->visible = 0;
    pnl_fixpanel (P_Area_ann);

    pnl_select_all (P_Area_ann);
}


static
setup_act_values (map)
    struct Map_info *map;
{
    static char info1_buf[100];
    static char info2_buf[50];
    unsigned char *p;


    sprintf (info1_buf, "Select Pattern and color");
    A_Area_info1->label = info1_buf;
    pnl_setdirty (A_Area_info1);

    /*
    sprintf (info2_buf, "Page %3d of %3d", page+1, num_pages);
    A_Area_info2->label = info2_buf;
    pnl_setdirty (A_Area_info2);
    */

    Area_Acts.Color->label = ISM_Colors[(int) Area_Acts.Color->val];
    pnl_setdirty (Area_Acts.Color);
    /*Area_Acts.Color->u = &(Area_Acts.Color->val);*/

    Area_Acts.Pattern->label=ISM_Area_Patterns[(int)Area_Acts.Pattern->val];
    /*Area_Acts.Pattern->u = Area_Acts.Pattern->val;*/
    pnl_setdirty (Area_Acts.Pattern);

    Area_Acts.Size->label = ISM_Area_Size[(int) Area_Acts.Size->val];
    /*Area_Acts.Size->u = &(Area_Acts.Size->val);*/
    pnl_setdirty (Area_Acts.Size);
}

static Panel *
init_area_panel()
{
    Panel *p;

    /*noborder ();*/
    P_Area_ann =p= pnl_mkpanel();
    p->x = XMAXSCREEN/3;
    p->y = YMAXSCREEN/3;
    p->label="Area Attributes";
    p->ppu=50.0;
    p->upfunc=p->fixfunc;

    mk_area_other_acts (p);
    mk_area_stuff (p);

    p->visible = 0;

    return p;
}

static void rotate_color_labels();
static void rotate_pattern_labels();
static void rotate_size_labels();

static 
mk_area_stuff (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 0;

    Area_Acts.Color =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_color_labels;
    a->val = 1;			/* toggleable, see rotate_color_labels () */
    a->x = xoff+L_COL_POS;
    a->y = yoff+.5;
    a->label=ISM_Colors[1];	/* just something to set the size */
    pnl_addact(a, p);

    Area_Acts.Pattern =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_pattern_labels;
    a->val = 0;
    a->x = xoff+L_STYLE_POS;
    a->y = yoff+.5;
    a->label=ISM_Area_Patterns[1];
    pnl_addact(a, p);

    Area_Acts.Size =a= pnl_mkact (pnl_picklabel);
    a->val = 1;
    a->downfunc = rotate_size_labels;
    a->x = xoff+L_WEIGHT_POS;
    a->y = yoff+.5;
    a->label=ISM_Area_Size[1];
    pnl_addact(a, p);
}

static 
mk_area_other_acts (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 1;

/* INFO LINES */
    /*
    A_Area_info0 =a= pnl_mkact (pnl_label);
    a->x = 2;
    a->y = yoff+3;
    a->label="AREA ATTRIBUTES";
    pnl_addact(a, p);
    */

    A_Area_info1 =a= pnl_mkact (pnl_label);
    a->x = 4;
    a->y = yoff+1;
    a->label="#### Different Categories";
    pnl_addact(a, p);

    /*
    A_Area_info2 =a= pnl_mkact (pnl_label);
    a->x = 10;
    a->y = yoff+1;
    a->label="Page ### of ###";
    pnl_addact(a, p);
    */


/* Buttons */
    A_Area_Accept =a= pnl_mkact (pnl_wide_button);
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
    a->label="Pattern";
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

    a->label = ISM_Area_Patterns[(int)XXX];
    pnl_setdirty (a);
}

static void 
rotate_size_labels(a)
    Actuator *a;
{
    if (++(XXX) > MAX_AREA_SIZE)
	XXX = 1;

    a->label = ISM_Area_Size[(int)XXX];
    pnl_setdirty (a);
}
