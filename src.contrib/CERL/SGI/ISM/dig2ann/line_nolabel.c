#include "digit.h"
#include <gl.h>
#include <device.h>
#include <panel.h>
#include "actuators.h"
#include "ism.h"

static Panel *init_line_panel();

#define NUM_LINE_ROWS 10

struct Line_Acts {
    Actuator *Color;
    Actuator *Pattern;
    Actuator *Size;
};

static struct Line_Acts Line_Acts;
static Panel *P_Line_ann;
static int setup_act_values ();

static Actuator *A_Line_Prev, *A_Line_Next, *A_Line_Accept;
static Actuator *A_Line_info1, *A_Line_info2;
static Actuator *A_Line_info0;

static int Set_Up = 0;

void
ask_line_nolabel (map, style, color, size)
    struct Map_info *map;
    int *style, *color, *size;
{
    Actuator *a;
    int num_pages;
    int page = 0;

    if (!Set_Up)
    {
	Set_Up = 1;
	init_line_panel();
    }

    pnl_unselect_all (P_Line_ann);

    P_Line_ann->visible = 1;
    /*noborder ();*/
    pnl_fixpanel (P_Line_ann);

    while (1)
    {
	setup_act_values(map);

	a = pnl_dopanel ();
	if (a == A_Line_Accept)
	    break;
    }
    while (a->active)
	pnl_dopanel ();

    *style = Line_Acts.Pattern->val;
    *color = Line_Acts.Color->val;
    *size = Line_Acts.Size->val;

    P_Line_ann->visible = 0;
    pnl_fixpanel (P_Line_ann);

    pnl_select_all (P_Line_ann);
}


static
setup_act_values (map)
    struct Map_info *map;
{
    static char info1_buf[100];
    static char info2_buf[50];
    unsigned char *p;


    sprintf (info1_buf, "Select Style and color");
    A_Line_info1->label = info1_buf;
    pnl_setdirty (A_Line_info1);

    Line_Acts.Color->label = ISM_Colors[(int) Line_Acts.Color->val];
    pnl_setdirty (Line_Acts.Color);
    /*Line_Acts.Color->u = &(Line_Acts.Color->val);*/

    Line_Acts.Pattern->label=ISM_Line_Styles[(int)Line_Acts.Pattern->val];
    /*Line_Acts.Pattern->u = Line_Acts.Pattern->val;*/
    pnl_setdirty (Line_Acts.Pattern);

    Line_Acts.Size->label = ISM_Line_Thick[(int) Line_Acts.Size->val];
    /*Line_Acts.Size->u = &(Line_Acts.Size->val);*/
    pnl_setdirty (Line_Acts.Size);
}

static Panel *
init_line_panel()
{
    Panel *p;

    /*noborder ();*/
    P_Line_ann =p= pnl_mkpanel();
    p->x = XMAXSCREEN/3;
    p->y = YMAXSCREEN/3;
    p->label="Line Attributes";
    p->ppu=50.0;
    p->upfunc=p->fixfunc;

    mk_line_other_acts (p);
    mk_line_stuff (p);

    p->visible = 0;

    return p;
}

static void rotate_color_labels();
static void rotate_pattern_labels();
static void rotate_size_labels();

static 
mk_line_stuff (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 0;

    Line_Acts.Color =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_color_labels;
    a->val = 1;			/* toggleable, see rotate_color_labels () */
    a->x = xoff+L_COL_POS;
    a->y = yoff+.5;
    a->label=ISM_Colors[1];	/* just something to set the size */
    pnl_addact(a, p);

    Line_Acts.Pattern =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_pattern_labels;
    a->val = 1;
    a->x = xoff+L_STYLE_POS;
    a->y = yoff+.5;
    a->label=ISM_Line_Styles[1];
    pnl_addact(a, p);

    Line_Acts.Size =a= pnl_mkact (pnl_picklabel);
    a->val = 1;
    a->downfunc = rotate_size_labels;
    a->x = xoff+L_WEIGHT_POS;
    a->y = yoff+.5;
    a->label=ISM_Line_Thick[1];
    pnl_addact(a, p);
}

static 
mk_line_other_acts (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 1;

/* INFO LINES */
    /*
    A_Line_info0 =a= pnl_mkact (pnl_label);
    a->x = 2;
    a->y = yoff+3;
    a->label="LINE ATTRIBUTES";
    pnl_addact(a, p);
    */

    A_Line_info1 =a= pnl_mkact (pnl_label);
    a->x = 4;
    a->y = yoff+1;
    a->label="#### Different Categories";
    pnl_addact(a, p);

    /*
    A_Line_info2 =a= pnl_mkact (pnl_label);
    a->x = 10;
    a->y = yoff+1;
    a->label="Page ### of ###";
    pnl_addact(a, p);
    */


/* Buttons */
    A_Line_Accept =a= pnl_mkact (pnl_wide_button);
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
    a->label="Style";
    pnl_addact(a, p);

    a= pnl_mkact (pnl_label);
    a->x = xoff+L_WEIGHT_POS;
    a->y = yoff;
    a->label="Weight";
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
    if (++(XXX) > MAX_LINE_COLOR)
	XXX = 1;

    a->label = ISM_Colors[(int)XXX];
    pnl_setdirty (a);
}

static void 
rotate_pattern_labels (a)
    Actuator *a;
{
    if (++(XXX) > MAX_LINE_STYLE)
	XXX = 0;

    a->label = ISM_Line_Styles[(int)XXX];
    pnl_setdirty (a);
}

static void 
rotate_size_labels(a)
    Actuator *a;
{
    if (++(XXX) > MAX_LINE_WEIGHT)
	XXX = 1;

    a->label = ISM_Line_Thick[(int)XXX];
    pnl_setdirty (a);
}
