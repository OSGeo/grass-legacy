#include "digit.h"
#include <gl.h>
#include <device.h>
#include <panel.h>
#include "actuators.h"
#include "gis.h"
#include "ism.h"

Panel *init_site_panel();

#define NUM_LINE_ROWS 10

struct Site_Acts {
    Actuator *Att;
    Actuator *Cat;
    Actuator *Color;
    Actuator *Symbol;
    Actuator *Size;
    Actuator *Label;
};

static struct Site_Acts Site_Acts[NUM_LINE_ROWS];
static Panel *P_Site_ann;
static int setup_act_values ();

static Actuator *A_Site_Prev, *A_Site_Next, *A_Site_Accept;
static Actuator *A_Site_info1, *A_Site_info2;
static Actuator *A_Site_info0;

static int Set_Up = 0;

void
do_user_site_panel (map, larray, labeled)
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
	init_site_panel();
    }

    num_pages = (labeled / 10) + ((labeled % 10) ? 1 : 0);

    pnl_unselect_all (P_Site_ann);

    P_Site_ann->visible = 1;
    /*noborder ();*/
    pnl_fixpanel (P_Site_ann);

    while (1)
    {
	setup_act_values(map,larray, labeled, num_pages, page);

	a = pnl_dopanel ();
	if (a == A_Site_Accept)
	    break;
	if (a == A_Site_Next)
	{
	    page++;
	    while (a->active)
		pnl_dopanel ();
	}
	if (a == A_Site_Prev)
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

    P_Site_ann->visible = 0;
    pnl_fixpanel (P_Site_ann);

    pnl_select_all (P_Site_ann);
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
    A_Site_info1->label = info1_buf;
    pnl_setdirty (A_Site_info1);

    sprintf (info2_buf, "Page %3d of %3d", page+1, num_pages);
    A_Site_info2->label = info2_buf;
    pnl_setdirty (A_Site_info2);

    /* for (j=0,i = start_line ; i <= stop_line ; i++,j++) */
    for (j=NUM_LINE_ROWS-1,i = start_line ; i <= stop_line ; i++,j--)
    {
	sprintf (Att_buf[j], "%5d", larray[i].att);
	Site_Acts[j].Att->label = Att_buf[j];
	pnl_setdirty (Site_Acts[j].Att);

	Site_Acts[j].Cat->label = Cat_buf[j];
	strncpy (Cat_buf[j], G_get_cat (larray[i].att, &Cats), CAT_LENGTH);
	Cat_buf[j][CAT_LENGTH] = 0;
	pnl_setdirty (Site_Acts[j].Cat);

	Site_Acts[j].Color->label = 
	    ISM_Colors[((char *)&(larray[i].flags))[SITE_COLOR]];
	Site_Acts[j].Color->u = 
	    &(((char *)&(larray[i].flags))[SITE_COLOR]);
	pnl_setdirty (Site_Acts[j].Color);

	Site_Acts[j].Symbol->label = 
	    ISM_Site_Symbols[((char *)&larray[i].flags)[SITE_SYMBOL]];
	Site_Acts[j].Symbol->u = 
	    &(((char *)&larray[i].flags)[SITE_SYMBOL]);
	pnl_setdirty (Site_Acts[j].Symbol);

	Site_Acts[j].Size->label = 
	    ISM_Site_Size[((char *)&larray[i].flags)[SITE_SIZE]];
	Site_Acts[j].Size->u = 
	    &(((char *)&larray[i].flags)[SITE_SIZE]);
	pnl_setdirty (Site_Acts[j].Size);

	Site_Acts[j].Label->label = 
	    ISM_YN_Strings[((char *)&larray[i].flags)[SITE_YN]];
	Site_Acts[j].Label->u = 
	    &(((char *)&larray[i].flags)[SITE_YN]);
	pnl_setdirty (Site_Acts[j].Label);

	Site_Acts[j].Att->selectable = 1;
	Site_Acts[j].Cat->selectable = 1;
	Site_Acts[j].Color->selectable = 1;
	Site_Acts[j].Symbol->selectable = 1;
	Site_Acts[j].Size->selectable = 1;
	Site_Acts[j].Label->selectable = 1;
    }
    for ( ; j >= 0 ; j--)
    {
	Site_Acts[j].Att->selectable = 0;
	Site_Acts[j].Cat->selectable = 0;
	Site_Acts[j].Color->selectable = 0;
	Site_Acts[j].Symbol->selectable = 0;
	Site_Acts[j].Size->selectable = 0;
	Site_Acts[j].Label->selectable = 0;

	Site_Acts[j].Att->label = "";
	Site_Acts[j].Cat->label = "";
	Site_Acts[j].Color->label = "";
	Site_Acts[j].Symbol->label = "";
	Site_Acts[j].Size->label = "";
	Site_Acts[j].Label->label = "";

	pnl_setdirty (Site_Acts[j].Att);
	pnl_setdirty (Site_Acts[j].Cat);
	pnl_setdirty (Site_Acts[j].Color);
	pnl_setdirty (Site_Acts[j].Symbol);
	pnl_setdirty (Site_Acts[j].Size);
	pnl_setdirty (Site_Acts[j].Label);
    }
}

static Panel *
init_site_panel()
{
    Panel *p;
    int i;

    /*noborder ();*/
    P_Site_ann =p= pnl_mkpanel();
    p->x = XMAXSCREEN/4;
    p->y = YMAXSCREEN/3;
    p->label="Site Attributes";
    p->ppu=50.0;
    p->upfunc=p->fixfunc;

    mk_site_other_acts (p);
    for (i = 0 ; i < NUM_LINE_ROWS ; i++)
	mk_site_stuff (i, p);

    p->visible = 0;

    return p;
}

static void rotate_color_labels();
static void rotate_pattern_labels();
static void rotate_size_labels();
static void rotate_yn_labels();

static 
mk_site_stuff (i, p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 2;

    Site_Acts[i].Att =a= pnl_mkact (pnl_label);
    a->x = xoff+L_ATT_POS;
    a->y = yoff+.5*i;
    a->label = "     ";
    pnl_addact(a, p);

    Site_Acts[i].Cat =a= pnl_mkact (pnl_label);
    a->x = xoff+L_CAT_POS;
    a->y = yoff+.5*i;
    a->label="             ";	/* TODO */
    pnl_addact(a, p);

    Site_Acts[i].Color =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_color_labels;
    a->val = 1;			/* toggleable, see rotate_color_labels () */
    a->x = xoff+L_COL_POS;
    a->y = yoff+.5*i;
    a->label=ISM_Colors[1];	/* just something to set the size */
    pnl_addact(a, p);

    Site_Acts[i].Symbol =a= pnl_mkact (pnl_picklabel);
    a->downfunc = rotate_pattern_labels;
    a->val = 1;
    a->x = xoff+L_STYLE_POS;
    a->y = yoff+.5*i;
    a->label=ISM_Site_Symbols[1];
    pnl_addact(a, p);

    Site_Acts[i].Size =a= pnl_mkact (pnl_picklabel);
    a->val = 1;
    a->downfunc = rotate_size_labels;
    a->x = xoff+L_WEIGHT_POS;
    a->y = yoff+.5*i;
    a->label=ISM_Site_Size[1];
    pnl_addact(a, p);

    Site_Acts[i].Label =a= pnl_mkact (pnl_picklabel);
    a->val = 1;
    a->downfunc = rotate_yn_labels;
    a->x = xoff+L_YN_POS + .4;
    a->y = yoff+.5*i;
    a->label=ISM_YN_Strings[0];
    pnl_addact(a, p);
}

static 
mk_site_other_acts (p)
    Panel *p;
{
    Actuator *a;
    int xoff, yoff;

    xoff  = 0;
    yoff  = 7;

/* INFO LINES */
    A_Site_info0 =a= pnl_mkact (pnl_label);
    a->x = 2;
    a->y = yoff+3;
    a->label="SITE ATTRIBUTES";
    pnl_addact(a, p);

    A_Site_info1 =a= pnl_mkact (pnl_label);
    a->x = 2;
    a->y = yoff+1;
    a->label="#### Different Categories";
    pnl_addact(a, p);

    A_Site_info2 =a= pnl_mkact (pnl_label);
    a->x = 10;
    a->y = yoff+1;
    a->label="Page ### of ###";
    pnl_addact(a, p);


/* Buttons */
    A_Site_Prev =a= pnl_mkact (pnl_wide_button);
    a->x = 8;
    a->y = yoff+2;
    a->label="PREV";
    pnl_addact(a, p);

    A_Site_Next =a= pnl_mkact (pnl_wide_button);
    a->x = 10;
    a->y = yoff+2;
    a->label="NEXT";
    pnl_addact(a, p);

    A_Site_Accept =a= pnl_mkact (pnl_wide_button);
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
    a->label="Symbol";
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
    if (++(XXX) > MAX_SITE_COLOR)
	XXX = 1;

    a->label = ISM_Colors[(int)XXX];
    pnl_setdirty (a);
}

static void 
rotate_pattern_labels (a)
    Actuator *a;
{
    if (++(XXX) > MAX_SITE_SYMBOL)
	XXX = 0;

    a->label = ISM_Site_Symbols[(int)XXX];
    pnl_setdirty (a);
}

static void 
rotate_size_labels(a)
    Actuator *a;
{
    if (++(XXX) > MAX_SITE_SIZE)
	XXX = 1;

    a->label = ISM_Site_Size[(int)XXX];
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
