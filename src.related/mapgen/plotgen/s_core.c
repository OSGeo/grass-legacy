#ifndef lint
static char *SCCSID = "@(#)s_core.c	OEMG v.1.5";
#endif
#define EXTDEV
#define PLOTTER
#include <graphics.h>
#include <usercore.h>
#include <stdio.h>
#include <signal.h>

#define MAPSIZE			128

#define menu_1 1
#define menu_2 2
#define menu_3 3
#define work_s 9
extern FILE *logfid;
static int cg1dd(), cg2dd(), cg4dd(), cgpixwindd(), gp1dd(), gp1pixwindd();
static int colorflag, initlev = 0;
int pixwindd();
struct vwsurf vwsurf = DEFAULT_VWSURF(pixwindd);
static float x_image_base, y_image_base;
/*  globals: */
float	wx, wy, tx, ty;
float	x_size, y_size;
/* end globals */
	void
Bomb(mess, lev) char *mess; {
	if (initlev == 3) {
		terminate_device(KEYBOARD, 1);
		terminate_device(LOCATOR, 1);
	}
	if (initlev > 1) {
		deselect_view_surface(&vwsurf);
		set_pick_id(1);
		terminate_view_surface(&vwsurf);
	}
	if (initlev)
		terminate_core();
	if (lev)
		emess(1,"szoom in sunCore",(char *)0);
	else
		locexit();
}
static float redtex[] =
{0., 1.,.99,.99,.90,.00,.00,.10,.99,.50,.00,.50,.50,.99};
static float grntex[] =
{0., 1.,.00,.70,.90,.90,.90,.10,.00,.99,.50,.00,.99,.99};
static float blutex[] = 
{0., 1.,.00,.00,.00,.00,.90,.99,.99,.50,.99,.99,.00,.99};
	void
SetUpCore(argv) char **argv; {
	get_view_surface(&vwsurf,argv);
	vwsurf.cmapsize = MAPSIZE;
	vwsurf.cmapname[0] = '\0';
	if (initialize_core(DYNAMICC, SYNCHRONOUS, THREED))
		Bomb("failure to init Core",1);
	initlev = 1;
	if (initialize_view_surface(&vwsurf, FALSE))
		Bomb("failure to init view surface",2);
	initlev=2;
	initialize_device(BUTTON, 1);
	initialize_device(BUTTON, 2);
	initialize_device(BUTTON, 3);
	initialize_device(LOCATOR, 1);
	initialize_device(PICK, 1);
	initialize_device(KEYBOARD, 1);
	initlev=3;
	set_keyboard(1, 80, " ", 1);
	set_echo_position(LOCATOR, 1, 0., 0.);
	set_echo_surface(LOCATOR, 1, &vwsurf);
	set_echo_surface(PICK, 1, &vwsurf);
	set_echo_surface(KEYBOARD, 1, &vwsurf);
	set_pick(1,0.01);
	set_window(0.,1000.,0.,750.);
	set_viewport_2(.0,.9999,.0, .74999);
	set_window_clipping(FALSE);
	set_output_clipping(TRUE);
	select_view_surface(&vwsurf);
}
	int
DoZoomPan(mode) {
	int button;
	float x, y;
	
	set_echo(LOCATOR, 1, 1);
	do {
		await_any_button_get_locator_2(1000000,1,&button, &x, &y);
		if (button == 3) return;
	} while (button != 1);
	set_echo_position(LOCATOR,1,x,y);
	map_ndc_to_world_2(x, y, &wx, &wy);
	wx = 2.*(wx - x_image_base);
	wy = 2.*(wy - y_image_base);
	move_abs_2(wx, wy);
	set_echo(LOCATOR, 1, mode ? 6 : 2);
	do {
		await_any_button_get_locator_2(10000000,1,&button,&x,&y);
		if (button == 3) return;
	} while (button != 1);
	set_echo_position(LOCATOR,1,x,y);
	map_ndc_to_world_2(x, y, &tx, &ty);
	tx = 2.*(tx - x_image_base);
	ty = 2.*(ty - y_image_base);
	return (zoompan(wx, wy, tx, ty, mode));
}
	void
DoLine(clos) {
	int button;
	float x, y, fx, fy;
	
	set_echo(LOCATOR, 1, 1);
	do {
		await_any_button_get_locator_2(1000000,1,&button, &x, &y);
		if (button == 3) return;
	} while (button != 1);
	set_echo_position(LOCATOR,1,x,y);
	map_ndc_to_world_2(x, y, &fx, &fy);
	fx = 2.*(fx - x_image_base);
	fy = 2.*(fy - y_image_base);
	fprintf(logfid,"-L 0\n");
	scprint(fx,fy);
	move_abs_2(fx, fy);
	set_echo(LOCATOR, 1, 2);
	for (;;) {
		do {
			await_any_button_get_locator_2(10000000,1,&button,&x,&y);
			if (button == 3) {
				if (clos) {
					line_abs_2(fx, fy);
					scprint(fx,fy);
				}
				fprintf(logfid,".\n");
				return;
			}
		} while (button != 1);
		set_echo_position(LOCATOR,1,x,y);
		map_ndc_to_world_2(x, y, &wx, &wy);
		wx = 2.*(wx - x_image_base);
		wy = 2.*(wy - y_image_base);
		line_abs_2(wx, wy);
		scprint(wx,wy);
	}
}
DoText(type) {
	int len, button;
	float x, y;
	double ang, atan2();
	char string[81];
	
	set_echo(LOCATOR, 1, 1);
	do {
		await_any_button_get_locator_2(1000000,1,&button, &x, &y);
		if (button == 3) return;
	} while (button != 1);
	set_echo_position(LOCATOR,1,x,y);
	map_ndc_to_world_2(x, y, &wx, &wy);
	wx = 2.*(wx - x_image_base);
	wy = 2.*(wy - y_image_base);
	move_abs_2(wx, wy);
	marker_abs_2(wx, wy);
	if (type == 2) {
		fprintf(logfid,"-xy ");
		scprint(wx, wy);
		return;
	}
	if (type) {
		set_echo(LOCATOR, 1, 2);
		do {
			await_any_button_get_locator_2(10000000,1,&button,&x,&y);
			if (button == 3) {
				return;
			}
		} while (button != 1);
		set_echo_position(LOCATOR,1,x,y);
		map_ndc_to_world_2(x, y, &tx, &ty);
		tx = 2.*(tx - x_image_base);
		ty = 2.*(ty - y_image_base);
	}
	if (type) {
		ang = atan2(ty-wy, tx-wx);
		line_abs_2(tx, ty);
	} else
		ang = 0.;
	fprintf(logfid,"-r %g -xy ",ang * 57.29578);
	scprint(wx,wy);
	fprintf(logfid,"-t\n");
	set_segment_visibility(menu_2, TRUE);
	set_echo_position(KEYBOARD,1,.0,.05);
	set_echo(KEYBOARD,1,1);
	do {
		set_keyboard(1,80,">>",2);
		await_keyboard(100000000,1,string,&len);
		fprintf(logfid,"%s",string);
	} while (string[0] != '.');
	set_segment_visibility(menu_2, FALSE);
}
	void
DoControl() {
	int len;
	char string[81];

	set_segment_visibility(menu_3, TRUE);
	set_echo_position(KEYBOARD,1,.0,.0);
	set_echo(KEYBOARD,1,1);
	do {
		set_keyboard(1,80,">#",2);
		await_keyboard(100000000,1,string,&len);
		fprintf(logfid,"%s",string);
	} while (string[0] != '\n');
	set_segment_visibility(menu_3, FALSE);
}
	static void
makeplot(opt) {
	double floor();

	create_retained_segment(work_s);
	set_segment_image_transformation_2(work_s,.5,.5,0.,.1,.05);
	if (opt) {
		map_ndc_to_world_2(.1,.05,&x_image_base,&y_image_base);
		map_ndc_to_world_2(.999,.7499,&wx,&wy);
		x_size = floor(2.*(wx - x_image_base));
		y_size = floor(2.*(wy - y_image_base));
	}
	set_segment_detectability(work_s, 50);
	set_segment_visibility(work_s, TRUE);
	doplot();
}
	int
s_core(argc, argv) int argc; char **argv; {
	int done=0, segnam, pickid;
	float white = 1.;

	SetUpCore(argv);
	if((vwsurf.dd == cg1dd) || (vwsurf.dd == cgpixwindd) ||
		(vwsurf.dd==cg2dd) || (vwsurf.dd==cg4dd) ||
		(vwsurf.dd==gp1dd) || (vwsurf.dd==gp1pixwindd)) {
		define_color_indices(&vwsurf,MAPSIZE-1,MAPSIZE-1,
			&white,&white,&white);
		define_color_indices(&vwsurf,0,11,redtex,grntex,blutex);
		colorflag=TRUE;
	} else
		colorflag=FALSE;
	MakeMenus();
	set_image_transformation_type(XFORM2);
	makeplot(1);
	set_segment_visibility(menu_1, TRUE);
	signal(SIGINT, SIG_IGN);
	while (!done) {
		set_echo(LOCATOR,1, 1);
		set_echo(PICK, 1,2);
		await_pick(1000000, 1, &segnam, &pickid);
		if (segnam == menu_1) {
			set_segment_visibility(menu_1, FALSE);
			switch (pickid) {
			case 1:
				delete_retained_segment(work_s);
				Bomb("",0); break;
			case 2: /* Zoom */
				if (DoZoomPan(1)) {
					delete_retained_segment(work_s);
					makeplot(0);
				}
				break;
			case 3: /* Pan */
				if (DoZoomPan(0)) {
					delete_retained_segment(work_s);
					makeplot(0);
				}
				break;
			case 4: /* mark */
				DoText(2);
				break;
			case 5: /* Text */
				DoText(0);
				break;
			case 6: /* Text */
				DoText(1);
				break;
			case 7: /* go back to base/no-zoom level */
				if (homer()) {
					delete_retained_segment(work_s);
					makeplot(0);
				}
				break;
			case 8: /* deZoom */
				if (dezoom()) {
					delete_retained_segment(work_s);
					makeplot(0);
				}
				break;
			case 9: /* output line */
				DoLine(0);
				break;
			case 10: /* poly line */
				DoLine(1);
				break;
			case 11: /* redraw image */
				delete_retained_segment(work_s);
				makeplot(0);
				break;
			case 12: /* enter control text */
				DoControl();
				break;
			default:
				set_segment_visibility(menu_1, FALSE);
				new_frame();
				break;
			}
			set_segment_visibility(menu_1, TRUE);
		}
	}
	Bomb("Normal", 0);
}
MakeMenus() {
	int i, j;
	float white=1.0;

	set_image_transformation_type(XLATE2);
	/* menu 1 */
	create_retained_segment(menu_1);
	set_primitive_attributes(&PRIMATTS);
	set_font(1); set_charsize(4.,3.);
	move_abs_2(6., 725.); set_pick_id(1); text("Exit");
	move_abs_2(6., 675.); set_pick_id(2); text("Zoom");
	move_abs_2(6., 625.); set_pick_id(3); text("Pan");
	move_abs_2(6., 575.); set_pick_id(4); text("Mark");
	move_abs_2(6., 525.); set_pick_id(5); text("Text");
	move_abs_2(6., 475.); set_pick_id(6); text("< Text");
	move_abs_2(6., 425.); set_pick_id(7); text("Home");
	move_abs_2(6., 375.); set_pick_id(8); text("deZoom");
	move_abs_2(6., 325.); set_pick_id(9); text("Line");
	move_abs_2(6., 275.); set_pick_id(10); text("Poly");
	move_abs_2(6., 225.); set_pick_id(11); text("RePlt");
	move_abs_2(6., 175.); set_pick_id(12); text("ctl text");
	close_retained_segment();
	set_segment_detectability(menu_1, 100);
	set_segment_visibility(menu_1, FALSE);

	create_retained_segment(menu_2);
	set_primitive_attributes(&PRIMATTS);
	set_font(1); set_charsize(4.,3.);
	move_abs_2(6., 27.); text("Enter text line(s).  Terminate with . line");
	close_retained_segment();
	set_segment_detectability(menu_2, 100);
	set_segment_visibility(menu_2, FALSE);

	create_retained_segment(menu_3);
	set_primitive_attributes(&PRIMATTS);
	set_font(1); set_charsize(4.,3.);
	move_abs_3(6., 27.); text("Enter control text line(s).  Terminate with null line");
	close_retained_segment();
	set_segment_detectability(menu_3, 100);
	set_segment_visibility(menu_3, FALSE);
}
#define IN(x) if ((x = getc(infile)) == EOF) return(1)
	int
splot(infile) FILE *infile; {
	int i;
	int c, d;
	long t;
	float x, y;

	while ((c = getc(infile)) != EOF)
		switch (c) {
		case _BOP:
			break;
		case _EOP:
			return(0); /* normal completion */
		case _PEN:
			IN(d); t = d << 8;
			IN(d); t += d;
			if (colorflag)
				set_line_index(t %8+1);
			break;
		case _MOVE:
			IN(d); t = d << 8;
			IN(d); x = t + d;
			IN(d); t = d << 8;
			IN(d); y = t + d;
			move_abs_2(x,y);
			break;
		case _DRAW:
			IN(d); t = d << 8;
			IN(d); x = t + d;
			IN(d); t = d << 8;
			IN(d); y = t + d;
			line_abs_2(x,y);
			break;
		default:
			return(2);
		}
	return(3);
}
