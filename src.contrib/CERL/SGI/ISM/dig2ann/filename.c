#include "digit.h"
#include <gl.h>
#include <device.h>
#include <panel.h>
#include "actuators.h"
#include "gis.h"
#include "ism.h"

Panel *init_file_panel();

#define NUM_LINE_ROWS 10

static Panel *P_File;
static int setup_act_values ();

static Actuator *A_Accept, *A_input, *A_output;
static Actuator *A_info, *A_msg;

static int Set_Up = 0;
	
panel_enter_filenames (dig_name, ism_name, msg)
    char **dig_name, **ism_name;
    char *msg;
{
    Actuator *a;

    if (!Set_Up)
    {
	Set_Up = 1;
	init_file_panel();
    }

    pnl_unselect_all (P_File);

    P_File->visible = 1;
    /*noborder ();*/
    pnl_fixpanel (P_File);

    setup_act_values(dig_name, ism_name, msg);

    while (1)
    {
	a = pnl_dopanel ();
	if (a == A_Accept)
	{
	    if (*(PNL_ACCESS (Typein, A_input, str)) == 0 || 
		*(PNL_ACCESS (Typein, A_output, str)) == 0)
	    {

		A_msg->label = "Specify BOTH file names";
		pnl_setdirty (A_msg);
		continue;
	    }
	    else
		break;
	}
    }
    while (a->active)
	pnl_dopanel ();

    *dig_name = G_store (PNL_ACCESS (Typein, A_input, str));
    *ism_name = G_store (PNL_ACCESS (Typein, A_output, str));
    P_File->visible = 0;
    /*noborder ();*/
    pnl_fixpanel (P_File);

    pnl_select_all (P_File);
}



static
setup_act_values (dig_name, ism_name, msg)
    char **dig_name, **ism_name;
    char *msg;
{
    if (*dig_name)
    {
	strncpy (PNL_ACCESS (Typein, A_input, str), *dig_name, 40);
	pnl_setdirty (A_input);
    }

    if (*ism_name)
    {
	strncpy (PNL_ACCESS (Typein, A_output, str), *ism_name, 40);
	pnl_setdirty (A_output);
    }
    
    A_msg->label = msg;
    pnl_setdirty (A_msg);
}

static Panel *
init_file_panel()
{
    Panel *p;

    /*noborder ();*/
    P_File =p= pnl_mkpanel();
    p->x = XMAXSCREEN/4;
    p->y = YMAXSCREEN/3;
    p->label="Enter File names";
    p->ppu=50.0;
    p->upfunc=p->fixfunc;

    mk_file_acts (p);


    p->visible = 0;

    return p;
}


static 
mk_file_acts (p)
    Panel *p;
{
    Actuator *a;

    A_info =a= pnl_mkact (pnl_label);
    a->x = -1.5;
    a->y = 2;
    a->label="Enter file names";
    pnl_addact(a, p);

    A_msg =a= pnl_mkact (pnl_label);
    a->x = -1;
    a->y = 1.5;
    a->label="";
    pnl_addact(a, p);

    A_Accept =a= pnl_mkact (pnl_wide_button);
    a->x = 5;
    a->y = 2;
    a->label="ACCEPT";
    pnl_addact(a, p);

    A_input =a= pnl_mkact (pnl_typein);
    a->x = 1;
    a->y = .5;
    a->labeltype=PNL_LABEL_LEFT;
    a->label = "DIG Input:  ";
    pnl_addact(a, p);

    A_output =a= pnl_mkact (pnl_typein);
    a->x = 1;
    a->y = 0;
    a->labeltype=PNL_LABEL_LEFT;
    a->label = "ANN Output: ";
    pnl_addact(a, p);
}
