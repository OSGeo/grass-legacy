

/*
**  Written by Bill Brown, Spring 1994 
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "externs.h"
#include "math.h"

#ifdef EP

void read_set_ep_panel();
void update_ep_panel();

install_ep_panel()
{

    if (NULL == (P_Exact = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Exact->label = "Exact Position";
    P_Exact->visible = 0; 
    P_Exact->downfunc=update_ep_panel;


    /* "exact" perspective */
    Aeppersp=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Aeppersp, str)="";
    PNL_ACCESS(Typein, Aeppersp, len)=16;
    Aeppersp->label=" Perspective:      ";
    Aeppersp->upfunc=read_set_ep_panel;
    Aeppersp->x=0.5;
    Aeppersp->y=3.5;
    Aeppersp->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Aeppersp, P_Exact);

    /* Viewer Easting */
    Avieweast=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Avieweast, str)="";
    PNL_ACCESS(Typein, Avieweast, len)=16;
    Avieweast->label=" VIEWER:   easting ";
    Avieweast->upfunc=read_set_ep_panel;
    Avieweast->x=0.5;
    Avieweast->y=3.0;
    Avieweast->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Avieweast, P_Exact);

    /* Viewer Northing */
    Aviewnorth=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Aviewnorth, str)="";
    PNL_ACCESS(Typein, Aviewnorth, len)=16;
    Aviewnorth->label=" northing ";
    Aviewnorth->upfunc=read_set_ep_panel;
    Aviewnorth->x=0.5;
    Aviewnorth->y=2.5;
    Aviewnorth->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Aviewnorth, P_Exact);

    /* Viewer Height */
    Aviewhgt=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Aviewhgt, str)="";
    PNL_ACCESS(Typein, Aviewhgt, len)=16;
    Aviewhgt->label=" height ";
    Aviewhgt->upfunc=read_set_ep_panel;
    Aviewhgt->x=0.5;
    Aviewhgt->y=2.0;
    Aviewhgt->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Aviewhgt, P_Exact);

    /* Lookto Easting */
    Alooktoeast=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Alooktoeast, str)="";
    PNL_ACCESS(Typein, Alooktoeast, len)=16;
    Alooktoeast->label=" LOOK-TO:  easting ";
    Alooktoeast->upfunc=read_set_ep_panel;
    Alooktoeast->x=0.5;
    Alooktoeast->y=1.5;
    Alooktoeast->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Alooktoeast, P_Exact);

    /* Lookto Northing */
    Alooktonorth=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Alooktonorth, str)="";
    PNL_ACCESS(Typein, Alooktonorth, len)=16;
    Alooktonorth->label=" northing ";
    Alooktonorth->upfunc=read_set_ep_panel;
    Alooktonorth->x=0.5;
    Alooktonorth->y=1.0;
    Alooktonorth->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Alooktonorth, P_Exact);

    /* Lookto Height */
    Alooktohgt=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Alooktohgt, str)="";
    PNL_ACCESS(Typein, Alooktohgt, len)=16;
    Alooktohgt->label=" height ";
    Alooktohgt->upfunc=read_set_ep_panel;
    Alooktohgt->x=0.5;
    Alooktohgt->y=0.5;
    Alooktohgt->labeltype = PNL_LABEL_LEFT;
    pnl_addact(Alooktohgt, P_Exact);
    
    /* CLOSE */
    make_close(-0.5, -0.25, P_Exact);

}

void read_set_ep_panel()
{
float tf;
char *buf;
extern void keep_focus();

    buf = PNL_ACCESS(Typein, Avieweast, str);
    if(buf[0]){
        sscanf(buf,"%f",&tf);
	FROM_TO[FROM][X] = (tf - wind.west) * XYscale;
    }
    buf = PNL_ACCESS(Typein, Aviewnorth, str);
    if(buf[0]){
        sscanf(buf,"%f",&tf);
	FROM_TO[FROM][Y] = (tf - wind.south) * XYscale;
    }
    buf = PNL_ACCESS(Typein, Aviewhgt, str);
    if(buf[0]){
        sscanf(buf,"%f",&tf);
	FROM_TO[FROM][Z] = (tf - Z_Min_real) * Z_exag;
    }
    buf = PNL_ACCESS(Typein, Alooktoeast, str);
    if(buf[0]){
        sscanf(buf,"%f",&tf);
	REAL_TO[X] = (tf - wind.west) * XYscale;
    }
    buf = PNL_ACCESS(Typein, Alooktonorth, str);
    if(buf[0]){
        sscanf(buf,"%f",&tf);
	REAL_TO[Y] = (tf - wind.south) * XYscale;
    }
    buf = PNL_ACCESS(Typein, Alooktohgt, str);
    if(buf[0]){
        sscanf(buf,"%f",&tf);
	REAL_TO[Z] = (tf - Z_Min_real) * Z_exag;
    }

    InFocus = 1;	
    keep_focus();

    PNL_ACCESS (Point, Axy, x) =
		    (FROM_TO[FROM][X] - XBase)/XRange;
    PNL_ACCESS (Point, Axy, y) =
		    (FROM_TO[FROM][Y] - YBase)/YRange;
    pnl_fixact (Axy);

    Aheight->val = (FROM_TO[FROM][Z] - ZBase)/ ZRange;
    pnl_fixact (Aheight);

    buf = PNL_ACCESS(Typein, Aeppersp, str);
    if(buf[0]){
        sscanf(buf,"%f",&tf);
	Apersp->val = tf * 10.0;
	Aortho->val = 0;
	_update_persp ();
	pnl_fixact (Apersp);
	pnl_fixact (Aortho);
    }
    
    update_projection();
    do_fast_display();

}


void update_ep_panel()
{
float tf;
char buf[40];

    if(P_Exact->visible){

	tf = FROM_TO[FROM][X]/XYscale + wind.west;
	sprintf(buf, "%15f", tf);
	strcpy(PNL_ACCESS(Typein, Avieweast, str),buf);

	tf = FROM_TO[FROM][Y]/XYscale + wind.south;
	sprintf(buf, "%15f", tf);
	strcpy(PNL_ACCESS(Typein, Aviewnorth, str),buf);

	tf = Z_exag? FROM_TO[FROM][Z]/Z_exag + Z_Min_real: 0.0;
	sprintf(buf, "%15f", tf);
	strcpy(PNL_ACCESS(Typein, Aviewhgt, str),buf);

	tf =InFocus? REAL_TO[X]/XYscale + wind.west: 
		     FROM_TO[TO][X]/XYscale + wind.west;
	sprintf(buf, "%15f", tf);
	strcpy(PNL_ACCESS(Typein, Alooktoeast, str),buf);

	tf =InFocus? REAL_TO[Y]/XYscale + wind.south: 
		     FROM_TO[TO][Y]/XYscale + wind.south;
	sprintf(buf, "%15f", tf);
	strcpy(PNL_ACCESS(Typein, Alooktonorth, str),buf);

	tf = Z_exag? (InFocus? REAL_TO[Z]/Z_exag + Z_Min_real: 
		      FROM_TO[TO][Z]/Z_exag + Z_Min_real): 0.0;
	sprintf(buf, "%15f", tf);
	strcpy(PNL_ACCESS(Typein, Alooktohgt, str),buf);

	tf = Apersp->val / 10.0;
	if(Aortho->val)
	    sprintf(buf, "");
	else
	    sprintf(buf, "%15.1f", tf);
	strcpy(PNL_ACCESS(Typein, Aeppersp, str),buf);

	pnl_fixact (Aviewnorth);
	pnl_fixact (Avieweast);
	pnl_fixact (Aviewhgt);
	pnl_fixact (Alooktonorth);
	pnl_fixact (Alooktoeast);
	pnl_fixact (Alooktohgt);
	pnl_fixact (Aeppersp);

    }

}

#endif /* EP */


