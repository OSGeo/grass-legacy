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
/*
 * Stepmap -> A simple program which draws an empty window and then
 *            uses a control panel with a pair of radio buttons, a
 *            button, and a slider, to control which color the
 *            background is drawn it.
 */

#include <stdio.h>
#include <gl.h>
#include <device.h>
#include <panel.h>

#define MIN_COLOR 0		/* Limit the range of the background */
#define MAX_COLOR 1023
#define MIN_RGB 0
#define MAX_RGB 255

#define METER_SIZE 2.0

int Next_Color = 0;		/* Color to draw the background in */
int Delta = 1;			/* 1 if Up pushed, -1 if Down pushed */
char Amount[5];			/* Label for color slider */

Panel *My_Panel;
Actuator *Color_Slider;		/* Pointer to color slider */
Actuator *Toggle_Button;	/* Pointer to animate toggle */
Actuator *Red_Meter;		/* Pointer to meter structure */
Actuator *Green_Meter;
Actuator *Blue_Meter;
Actuator *Red_Strip;
Actuator *Blue_Strip;
Actuator *Green_Strip;
#define STRIP "                   "
char Red_Label[20] = "Red=xxx";
char Blue_Label[20] = "Blue=xxx";
char Green_Label[20] = "Green=xxx";
char Red_S_Label[20] = STRIP;
char Blue_S_Label[20] = STRIP;
char Green_S_Label[20] = STRIP;

void My_Draw()			/* Simply erase in the background color */
{
  color(Next_Color);
  clear();
  swapbuffers();
}

void Set_Meters()
{
  short red, green, blue;
  getmcolor(Next_Color,&red,&green,&blue);
  Red_Meter->val = red;
  Red_Meter->dirtycnt = 2;
  Blue_Meter->val = blue;
  Blue_Meter->dirtycnt = 2;
  Green_Meter->val = green;
  Green_Meter->dirtycnt = 2;

  Red_Strip->val=red;	    pnl_fixact(Red_Strip);
  Green_Strip->val=green;   pnl_fixact(Green_Strip);
  Blue_Strip->val=blue;	    pnl_fixact(Blue_Strip);

  (void) sprintf(Red_Label,"Red=%d",red);
  (void) sprintf(Green_Label,"Green=%d",green);
  (void) sprintf(Blue_Label,"Blue=%d",blue);
  (void) sprintf(Red_S_Label,"Red=(%d,%d)",
	  (int)Red_Strip->minval, (int)Red_Strip->maxval);
  (void) sprintf(Green_S_Label,"Green=(%d,%d)",
	  (int)Green_Strip->minval, (int)Green_Strip->maxval);
  (void) sprintf(Blue_S_Label,"Blue=(%d,%d)",
	  (int)Blue_Strip->minval, (int)Blue_Strip->maxval);
}

void Eraser()			/* Pick the color to erase */
{				/* Called indirectly by STEP button */

  Next_Color += Delta;		/* Go to next color */

  if (Next_Color < MIN_COLOR)	/* Make sure it is in range */
    Next_Color = MAX_COLOR;
  if (Next_Color > MAX_COLOR)
    Next_Color = MIN_COLOR;

  Color_Slider->val = Next_Color; /* Update the slider */
  Color_Slider->dirtycnt = 2;
  Set_Meters();
  (void) sprintf(Amount,"%4d",Next_Color); /* And its label */
}

void setup()			/* Cause STEP to step up */
{				/* Called by Up radio button */
  Delta = 1;
}

void setdown()
{				/* Cause STEP to step down */
  Delta = -1;			/* Called by Down radio button */
}

void Slider_Color()
{
  Next_Color = Color_Slider->val;
  Set_Meters();
  (void) sprintf(Amount,"%4d",Next_Color);
}

void My_Exit()			/* Clean up and Exit */
{				/* Called by Exit button */
  exit(0);
}

Panel *Make_Panel()		/* Prepare the control panel */
{
  Panel *p;
  Actuator *a;

  My_Panel = p = pnl_mkpanel();	/* Control Panel */
  p->label="Map Stepper";

  a = pnl_mkact(pnl_wide_button);	/* Exit button */
  a->label = "exit";
  a->x = 11.4;
  a->y = 0.0;
  a->downfunc=My_Exit;
  pnl_addact(a, p);

  Color_Slider = pnl_mkact(pnl_vslider); /* Color Slider */
  (void) sprintf(Amount,"%4d",Next_Color);
  Color_Slider->label = Amount;
  Color_Slider->x = 0.0;
  Color_Slider->y = 0.0;
  Color_Slider->minval = MIN_COLOR;
  Color_Slider->maxval = MAX_COLOR;
  Color_Slider->val = 0.0;
  Color_Slider->activefunc = Slider_Color;
  pnl_addact(Color_Slider, p);

  Red_Meter = a = pnl_mkact(pnl_analog_bar); /* Red Analog meter */
  a->label  = Red_Label;
  a->x = 1.0;
  a->y = 0.0;
  a->minval = MIN_RGB;
  a->maxval = MAX_RGB;
  pnl_addact(a, p);

  Blue_Meter = a = pnl_mkact(pnl_meter); /* Blue Analog Meter */
  a->label  = Blue_Label;
  a->x = 1.0;
  a->y = 5.0;
  a->minval = MIN_RGB;
  a->maxval = MAX_RGB;
  pnl_addact(a, p);

  Green_Meter = a = pnl_mkact(pnl_meter); /* Green Analog Meter */
  a->label  = Green_Label;
  a->x = 1.0;
  a->y = 2.5;
  a->minval = MIN_RGB;
  a->maxval = MAX_RGB;
  pnl_addact(a, p);

  Red_Strip = a = pnl_mkact(pnl_scale_chart);
  a->label = Red_S_Label;
  a->x = 4.0;
  a->y = 0.0;
  a->minval = MIN_RGB;
  a->maxval = MAX_RGB;
  pnl_addact(a, p);

  Blue_Strip = a = pnl_mkact(pnl_scale_chart);
  a->label = Blue_S_Label;
  a->x = 4.0;
  a->y = 5.0;
  ((Stripchart *)a->data)->Bind_Low = TRUE;
  pnl_addact(a, p);

  Green_Strip = a = pnl_mkact(pnl_strip_chart);
  a->label = Green_S_Label;
  a->x = 4.0;
  a->y = 2.5;
  a->minval = MIN_RGB;
  a->maxval = MAX_RGB;
  pnl_addact(a, p);

  a= pnl_mkact(pnl_button);		/* Step button */
  a->label = "Step";
  a->x = 11.4;
  a->y = 1.0;
  a->activefunc=Eraser;
  pnl_addact(a, p);

  Toggle_Button = pnl_mkact(pnl_toggle_button);
  Toggle_Button->label = "Animate";
  Toggle_Button->x = 11.4;
  Toggle_Button->y = 0.5;
  pnl_addact(Toggle_Button, p);

  a=pnl_mkact(pnl_radio_button);	/* UP button */
  a->label = "UP";
  a->val = 1.0;
  a->x = 11.4;
  a->y = 2.5;
  a->downfunc = setup;
  setup();
  pnl_addact(a, p);

  a=pnl_mkact(pnl_radio_button);	/* Down button */
  a->label = "DOWN";
  a->x = 11.4;
  a->y = 2.0;
  a->downfunc = setdown;
  pnl_addact(a, p);

  pnl_endgroup(p);			/* Finish up, jack */
  Set_Meters();
}

#define Draw_Twice My_Draw(); My_Draw();

main()
{
  foreground();			/* Only for debugging */
  keepaspect(1,1);
  winopen("My Panel");		/* Window to draw in */
  doublebuffer();		/* Run double buffered */
  gconfig();

  Make_Panel();			/* Initiate the panel */
  pnl_needredraw();			/* Force it to be drawn */

  ortho2(0.0,1.0,0.0,1.0);
  Draw_Twice;			/* Initial draw of my output */
  while (1) {			/* Classic busy loop */
    if (Toggle_Button->val == 1.0) {
      Eraser();
    }
    if (pnl_dopanel()) {		/* Some panel activity occured */
      do {
	My_Draw();		/* Redraw with new parameters */
      } while (pnl_dopanel());	/* As long as panel activity occurs */
      My_Draw();		/* Redraw with new parameters */
      pnl_drawpanel();		/* Redraw the panel */
      My_Draw();
    }
    if (pnl_userredraw()) {		/* Moved or reshaped my window */
      reshapeviewport();
      ortho2(0.0,1.0,0.0,1.0);
      Draw_Twice;
    }
    My_Draw();
  }
}
