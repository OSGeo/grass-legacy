/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */
#include <gl.h>
#include <panel.h>

struct view    /* user-defined struct with an Actuator as its FIRST element */
{
  Actuator act;
  float x_angle;
  float y_angle;
  float z_angle;
};

func (v)
struct view *v;	    /* here the parameter is a user-defined structure */
{
  (void) printf("it is %f\n", v->x_angle);
}

callit (a)
Actuator *a;	    /* and here its an Actuator */
{
  (*a->downfunc)(a);
}

main()
{
struct view *v = PNL_MKUSERACT (view, pnl_button);

  v->act.label   = "test";
  v->x_angle = 10.0;
  v->y_angle = 20.0;
  v->z_angle = 30.0;
  
  v->act.downfunc = (void (*)()) func;

  callit (v);
}


