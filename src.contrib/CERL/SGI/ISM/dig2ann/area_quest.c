#include "digit.h"
#include <gl.h>
#include <device.h>
#include <panel.h>
#include "actuators.h"
#include "gis.h"
#include "ism.h"

Panel *init_area_panel();

#define NUM_LINE_ROWS 10

struct Area_Acts {
    Actuator *Att;
    Actuator *Cat;
    Actuator *Color;
    Actuator *Pattern;
    Actuator *Size;
    Actuator *Label;
};

static struct Area_Acts Area_Acts[NUM_LINE_ROWS];
static Panel *P_Area_ann;
static int setup_act_values ();

static Actuator *A_Area_Prev, *A_Area_Next, *A_Area_Accept;
static Actuator *A_Area_info1, *A_Area_info2;
static Actuator *A_Area_info0;

static int Set_Up = 0;

void
do_user_area_panel (map, larray, labeled)
    struct Map_info *map;
    struct lineray *larray;
{
    Actuator *a;
    int num_pages;
    int i;
    int page = 0;

    if (!Set_Up)
    {
	Set_Up = 1;
	init_area_panel();
    }

    num_pages = (labeled / 10) + ((labeled % 10) ? 1 : 0);

    pnl_unselect_all (P_Area_ann);

    P_Area_ann->visible = 1;
    /*noborder ();*/
    pnl_fixpanel (P_Area_ann);

    while (1)
    {
	setup_act_values(map,larray, labeled, num_pages, page);

	a = pnl_dopanel ();
	if (a == A_Area_Accept)
	    break;
	if (a == A_Area_Next)
	{
	    page++;
	    while (a->active)
		pnl_dopanel ();
	}
	if (a == A_Area_Prev)
	{
	    page--;
	    while (a->active)
		pnl_dopanel ();
	}

	if (page < 0) page = 0;
	if (page >= num_pages) page = num_pages-1;

    }
    while (a->active)
	pnl_dopanel ();

    P_Area_ann->visible = 0;
    pnl_fixpanel (P_Area_ann);

    pnl_select_all (P_Area_ann);
}



static
setup_act_values (map, larray, total, num_pages, page)
    struct Map_info *map;
    struct lineray *larray;
{
    int i, j, start_line, stop_line;
    static char Att_buf[NUM_LINE_ROWS][100];
    static char Cat_buf[NUM_LINE_ROWS][CAT_LENGTH+5];
    static char info1_buf[100];
    static char info2_buf[50];
    unsigned char *p;


    if (page >= num_pages) page = num_pages-1;
    if (page < 0) page = 0;
    start_line = page * NUM_LINE_ROWS;
    stop_line =  start_line + NUM_LINE_ROWS - 1;
    if (stop_line > total-1)
	stop_line = total-1;
	

    sprintf (info1_buf, "%4d Different Categories", total);
    A_Area_info1->label = info1_buf;
    pnl_setdirty (A_Area_info1);

    sprintf (info2_buf, "Page %3d of %3d", page+1, num_pages);
    A_Area_info2->label = info2_buf;
    pnl_setdirty (A_Area_info2);

    /* for (j=0,i = start_line ; i <= stop_line ; i++,j++) */
    for (j=NUM_LINE_ROWS-1,i = start_line ; i <= stop_line ; i++,j--)
    {
	sprintf (Att_buf[j], "%5d", larray[i].att);
	Area_Acts[j].Att->label = Att_buf[j];
	pnl_setdirty (Area_Acts[j].Att);

	Area_Acts[j].Cat->label = Cat_buf[j];
	strncpy (Cat_buf[j], G_get_cat (larray[i].att, &Cats), CAT_LENGTH);
	Cat_buf[j][CAT_LENGTH] = 0;
	pnl_setdirty (Area_Acts[j].Cat);

	Area_Acts[j].Color->label = 
	    ISM_Colors[((char *)&(larray[i].flags))[AREA_COLOR]];
	Area_Acts[j].Color->u = 
	    &(((char *)&(larray[i].flags))[AREA_COLOR]);
	pnl_setdirty (Area_Acts[j].Color);

	Area_Acts[j].Pattern->label = 
	    ISM_Area_Patterns[((char *)&larray[i].flags)[AREA_PATTERN]];
	Area_Acts[j].Pattern->u = 
	    &(((char *)&larray[i].flags)[AREA_PATTERN]);
	pnl_setdirty (Area_Acts[j].Pattern);

	Area_Acts[j].Size->label = 
	    ISM_Area_Size[((char *)&larray[i].flags)[AREA_SIZE]];
	Area_Acts[j].Size->u = 
	    &(((char *)&larray[i].flags)[AREA_SIZE]);
	pnl_setdirty (Area_Acts[j].Size);

	Area_Acts[j].Label->label = 
	    ISM_YN_Strings[((char *)&larray[i].flags)[AREA_YN]];
	Area_Acts[j].Label->u = 
	    &(((char *)&larray[i].flags)[AREA_YN]);
	pnl_setdirty (Area_Acts[j].Label);

	Area_Acts[j].Att->selectable = 1;
	Area_Acts[j].Cat->selectable = 1;
	Area_Acts[j].Color->selectable = 1;
	Area_Acts[j].Pattern->selectable = 1;
	Area_Acts[j].Size->selectable = 1;
	Area_Acts[j].Label->selectable = 1;
    }
    for ( ; j >= 0 ; j--)
    {
	Area_Acts[j].Att->selectable = 0;
	Area_Acts[j].Cat->selectable = 0;
	Area_Acts[j].Color->selectable = 0;
	Area_Acts[j].Pattern->selectable = 0;
	Area_Acts[j].Size->selectable = 0;
	Area_Acts[j].Label->selectable = 0;

	Area_Acts[j].Att->label = "";
	Area_Acts[j].Cat->label = "";
	Area_Acts[j].Color->label = "";
	Area_Acts[j].Pattern->label = "";
	Area_Acts[j].Size->label = "";
	Area_Acts[j].Label->label = "";

	pnl_setdirty (Area_Acts[j].Att);
	pnl_setdirty (Area_Acts[j].Cat);
	pnl_setdirty (Area_Acts[j].Color);
	pnl_setdirty (Area_Acts[j].Pattern);
	pnl_setdirty (Area_Acts[j].Size);
	pnl_setdirty (Area_Acts[j].Label);
    }
}

static Panel *
init_area_panel()
{
    Panel *p;
    int i;

    /*noborder ();*/
    P_Area_ann =p= pnl_mkpanel();
    p->x = XMAXSCREEN/4;
    p->y = YMAXSCREEN/3;
    p->label="Area Attributes";
    p->ppu=50.0;
    p->upfunc=p->fixfunc;

    mk_area_other_acts (p);
    for (i = 0 ; i < NUM_LINE_ROWS ; i++)
	mk_area_stuff (i, p);

    p->visible = 0;

    return p;
}

static void rotate_color_labels();
static void rotate_pattern_labels();
static void rotate_size_labels();
static void rotate_yn_labels();

static 
mk_area_stuff (i, p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 2;

    Area_Acts[i].Att =a= pnl_mkact (pnl_label);
    a->x = xoff+L_ATT_POS;
    a->y = yoff+.5*i;
    a->label = "     ";
    pnl_addact(a, p);

    Area_Acts[i].Cat =a= pnl_mkact (pnl_label);
    a->x = xoff+L_CAT_POS;
    a->y = yoff+.5*i;
    a->label="             ";	/* TODO */
    pnl_addact(a, p);

    Area_Acts[i].Color =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_color_labels;
    a->val = 1;			/* toggleable, see rotate_color_labels () */
    a->x = xoff+L_COL_POS;
    a->y = yoff+.5*i;
    a->label=ISM_Colors[1];	/* just something to set the size */
    pnl_addact(a, p);

    Area_Acts[i].Pattern =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_pattern_labels;
    a->val = 1;
    a->x = xoff+L_STYLE_POS;
    a->y = yoff+.5*i;
    a->label=ISM_Area_Patterns[1];
    pnl_addact(a, p);

    Area_Acts[i].Size =a= pnl_mkact (pnl_picklabel);
    a->val = 1;
    a->downfunc = rotate_size_labels;
    a->x = xoff+L_WEIGHT_POS;
    a->y = yoff+.5*i;
    a->label=ISM_Area_Size[1];
    pnl_addact(a, p);

    Area_Acts[i].Label =a= pnl_mkact (pnl_picklabel);
    a->val = 1;
    a->downfunc = rotate_yn_labels;
    a->x = xoff+L_YN_POS + .4;
    a->y = yoff+.5*i;
    a->label=ISM_YN_Strings[0];
    pnl_addact(a, p);
}

static 
mk_area_other_acts (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 7;

/* INFO LINES */
    A_Area_info0 =a= pnl_mkact (pnl_label);
    a->x = 2;
    a->y = yoff+3;
    a->label="AREA ATTRIBUTES";
    pnl_addact(a, p);

    A_Area_info1 =a= pnl_mkact (pnl_label);
    a->x = 2;
    a->y = yoff+1;
    a->label="#### Different Categories";
    pnl_addact(a, p);

    A_Area_info2 =a= pnl_mkact (pnl_label);
    a->x = 10;
    a->y = yoff+1;
    a->label="Page ### of ###";
    pnl_addact(a, p);


/* Buttons */
    A_Area_Prev =a= pnl_mkact (pnl_wide_button);
    a->x = 8;
    a->y = yoff+2;
    a->label="PREV";
    pnl_addact(a, p);

    A_Area_Next =a= pnl_mkact (pnl_wide_button);
    a->x = 10;
    a->y = yoff+2;
    a->label="NEXT";
    pnl_addact(a, p);

    A_Area_Accept =a= pnl_mkact (pnl_wide_button);
    a->x = 13;
    a->y = yoff+2;
    a->label="ACCEPT";
    pnl_addact(a, p);


/* Column labels */
    a= pnl_mkact (pnl_label);
    a->x = xoff+L_ATT_POS;
    a->y = yoff;
    a->label="Attr";
    pnl_addact(a, p);

    a= pnl_mkact (pnl_label);
    a->x = xoff+L_CAT_POS;
    a->y = yoff;
    a->label="Label";
    pnl_addact(a, p);

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

    a= pnl_mkact (pnl_label);
    a->x = xoff+L_YN_POS;
    a->y = yoff;
    a->label="Disp Label";
    pnl_addact(a, p);
}

#define XXX  (*(a->u))

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

static void 
rotate_yn_labels(a)
    Actuator *a;
{
    XXX = (XXX == 0);
    a->label = ISM_YN_Strings[(int)XXX];
    pnl_setdirty (a);
}
