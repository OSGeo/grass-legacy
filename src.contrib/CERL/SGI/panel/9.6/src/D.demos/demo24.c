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
#include	<stdio.h>
#include	<gl.h>
#include	<device.h>
#include	<panel.h>

#define	BACK_COLOR_INDEX	99
#define	MAX_INTENSITY	255.0


Actuator		*pal;

int		cur_index = BACK_COLOR_INDEX;

float		back_r;
float		back_g;
float		back_b;

Actuator	*back_r_label;
Actuator	*back_g_label;
Actuator	*back_b_label;

Actuator	*back_r_slider;
Actuator	*back_g_slider;
Actuator	*back_b_slider;


main()
{
	void	make_back_color_panel();



	foreground();
	noport();
	winopen("Testpan");
	doublebuffer();
	gconfig();
	mapcolor(BACK_COLOR_INDEX, 250, 200, 150);
	make_back_color_panel();


	while (1)
	{
		pnl_dopanel();
		swapbuffers();
	}

}

void make_back_color_panel()
{
	extern Actuator	*back_r_slider;
	extern Actuator	*back_g_slider;
	extern Actuator	*back_b_slider;
	
	extern Actuator	*back_r_label;
	extern Actuator	*back_g_label;
	extern Actuator	*back_b_label;
	
	Panel			*back_color_panel;
	short			r;
	short			g;
	short			b;
	static char		rchar[10];
	static char		gchar[10];
	static char		bchar[10];
	static char		cbuf[20];

	void			update_back_color();

	extern Actuator	*pal;

	Actuator		*map;
	void			set_cur_index();


	getmcolor(BACK_COLOR_INDEX, &r, &g, &b);

	back_color_panel = pnl_mkpanel();
	back_color_panel -> label = "Background";
	back_color_panel -> ppu = 40;

	(void) sprintf(rchar, "%3.0f%%", (float) (100.0 * r / MAX_INTENSITY));
	back_r_label = pnl_mkact(pnl_label);
	back_r_label -> label = rchar;
	back_r_label -> y = 3.05;
	pnl_addact(back_r_label, back_color_panel);

	(void) sprintf(gchar, "%3.0f%%", (float) (100.0 * g / MAX_INTENSITY));
	back_g_label = pnl_mkact(pnl_label);
	back_g_label -> label = gchar;
	back_g_label -> x = 1.0;
	back_g_label -> y = 3.05;
	pnl_addact(back_g_label, back_color_panel);

	(void) sprintf(bchar, "%3.0f%%", (float) (100.0 * b / MAX_INTENSITY));
	back_b_label = pnl_mkact(pnl_label);
	back_b_label -> label = bchar;
	back_b_label -> x = 2.0;
	back_b_label -> y = 3.05;
	pnl_addact(back_b_label, back_color_panel);

	back_r_slider = pnl_mkact(pnl_slider);
	back_r_slider -> label = "R";
	back_r_slider -> minval = 0.0;
	back_r_slider -> maxval = MAX_INTENSITY;
	back_r_slider -> val = (float) r;
	back_r_slider -> h = 3.0;
	back_r_slider -> u = (char *) back_r_label;
	back_r_slider -> activefunc = update_back_color;
	pnl_addact(back_r_slider, back_color_panel);

	back_g_slider = pnl_mkact(pnl_slider);
	back_g_slider -> label = "G";
	back_g_slider -> minval = 0.0;
	back_g_slider -> maxval = MAX_INTENSITY;
	back_g_slider -> val = (float) g;
	back_g_slider -> x = 1.0;
	back_g_slider -> h = 3.0;
	back_g_slider -> u = (char *) back_g_label;
	back_g_slider -> activefunc = update_back_color;
	pnl_addact(back_g_slider, back_color_panel);

	back_b_slider = pnl_mkact(pnl_slider);
	back_b_slider -> label = "B";
	back_b_slider -> minval = 0.0;
	back_b_slider -> maxval = MAX_INTENSITY;
	back_b_slider -> val = (float) b;
	back_b_slider -> x = 2.0;
	back_b_slider -> h = 3.0;
	back_b_slider -> u = (char *) back_b_label;
	back_b_slider -> activefunc = update_back_color;
	pnl_addact(back_b_slider, back_color_panel);

	pal = pnl_mkact(pnl_palette);
	(void) sprintf(cbuf, "Index = %4d", BACK_COLOR_INDEX);
	pal -> label = cbuf;
	pal -> minval = (float) BACK_COLOR_INDEX;
	pal -> maxval = (float) BACK_COLOR_INDEX;
	pal -> y = 4.0;
	pal -> w = 3.0;
	pal -> h = 3.0;
	pnl_addact(pal, back_color_panel);

	map = pnl_mkact(pnl_palette);
	map -> minval = 0.0;
	map -> maxval = 1023.0;
	map -> x = -1.0;
	map -> activefunc = set_cur_index;
	pnl_addact(map, back_color_panel);


}

void update_back_color(a)
Actuator	*a;
{
	switch (*a->label)
	{
		case 'R':
			back_r = a -> val;
			break;
			
		case 'G':
			back_g = a -> val;
			break;
			
		case 'B':
			back_b = a -> val;
			break;

		default:
			return;
	}

	mapcolor(	(Colorindex) cur_index,
			(Colorindex) back_r,
			(Colorindex) back_g,
			(Colorindex) back_b);

	(void) sprintf(((Actuator *)(a -> u)) -> label, "%3.0f%%",
		(100.0 * a -> val / MAX_INTENSITY));
	pnl_fixact((Actuator *) a -> u);

}


void set_cur_index(a)
Actuator	*a;
{
	extern int		cur_index;

	static char		rchar[10];
	static char		gchar[10];
	static char		bchar[10];
	static char		cbuf[20];

	short			r;
	short			g;
	short			b;

	extern float	back_r;
	extern float	back_g;
	extern float	back_b;

	extern Actuator	*back_r_label;
	extern Actuator	*back_g_label;
	extern Actuator	*back_b_label;
	extern Actuator	*back_r_slider;
	extern Actuator	*back_g_slider;
	extern Actuator	*back_b_slider;

	cur_index = (int) (pal -> minval = pal -> maxval = a -> val);
	(void) sprintf(cbuf, "Index = %4d", cur_index);
	pal->label = cbuf;
	pnl_fixact(pal);

	getmcolor((Colorindex) cur_index, &r, &g, &b);

	back_r_slider->val = back_r = (float) r;
	back_g_slider->val = back_g = (float) g;
	back_b_slider->val = back_b = (float) b;
	pnl_fixact(back_r_slider);
      pnl_fixact(back_g_slider);
      pnl_fixact(back_b_slider);

	(void) sprintf(rchar, "%3.0f%%", (100.0 * back_r / MAX_INTENSITY));
	back_r_label -> label = rchar;
	pnl_fixact(back_r_label);

	(void) sprintf(gchar, "%3.0f%%", (100.0 * back_g / MAX_INTENSITY));
	back_g_label -> label = gchar;
	pnl_fixact(back_g_label);

	(void) sprintf(bchar, "%3.0f%%", (100.0 * back_b / MAX_INTENSITY));
	back_b_label -> label = bchar;
	pnl_fixact(back_b_label);

}
