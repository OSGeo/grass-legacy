#ifndef lint
static char *SCCSID = "@(#)sundump.c	OEMG v.1.2";
#endif
/* signal that this is external device */
#define EXTDEV
# include <stdio.h>
# include <graphics.h>
#include <usercore.h>
#define MAPSIZE 128
#define IN(x) if ((x = getc(infile)) == EOF) done(0)
static float redtex[] =
{0., 1.,.99,.99,.90,.00,.00,.10,.99,.50,.00,.50,.50,.99};
static float grntex[] =
{0., 1.,.00,.70,.90,.90,.90,.10,.00,.99,.50,.00,.99,.99};
static float blutex[] = 
{0., 1.,.00,.00,.00,.00,.90,.99,.99,.50,.99,.99,.00,.99};
static FILE *infile = stdin;
int pixwindd();
struct vwsurf vwsurf = DEFAULT_VWSURF(pixwindd);
static int cg1dd();
static int cg2dd();
static int cg4dd();
static int cgpixwindd();
static int gp1dd();
static int gp1pixwindd();
static int colorflag;
	static char
*name;
	static char
*error[] = {
	"EOF during sub-field input",
	"Invalid control code",
	"EOF detected before End Of Plot"
};
	static void
done(err) {
	if (err >= 0)
		fprintf(stderr,"driver %s failure because %s\n",name,error[err]);
	terminate_device(BUTTON,3);
	terminate_device(LOCATOR,1);
	close_temporary_segment(1);
	deselect_view_surface(&vwsurf);
	terminate_core();
	exit(err < 0? 0:1);
}
	int
main(argc, argv) char **argv; {
	char in[20];
	float white=1.;
	int i;
	int c, d;
	long t;
	float x, y;

	name = *argv; /* save for error reference */
	while ((c = getc(infile)) != EOF)
		switch (c) {
		case _BOP:
			get_view_surface(&vwsurf, argv);
			vwsurf.cmapsize = MAPSIZE;
			vwsurf.cmapname[0] = '\0';
			if((vwsurf.dd == cg1dd) || (vwsurf.dd == cgpixwindd) ||
				(vwsurf.dd==cg2dd) || (vwsurf.dd==cg4dd) ||
				(vwsurf.dd==gp1dd) || (vwsurf.dd==gp1pixwindd)) {
				colorflag=TRUE;
			} else {
				colorflag=FALSE;
			}
			if(initialize_core(DYNAMICC,SYNCHRONOUS,TWOD)) {
				printf("init_c error\n");
				exit(1);
			}
			if(initialize_view_surface(&vwsurf, FALSE)) {
				printf("init_v error\n");
				exit(1);
			}
			select_view_surface(&vwsurf);
			set_viewport_2(.0,1.,.0,.75);
			set_window(0.,4000.,0.,3000.);
			define_color_indices(&vwsurf,MAPSIZE-1,MAPSIZE-1,
				&white,&white,&white);
			define_color_indices(&vwsurf,0,11,redtex,grntex,blutex);
			set_image_transformation_type(XLATE2);
			create_temporary_segment(1);
			set_line_index(1);
			break;
		case _EOP:
			initialize_device(BUTTON,3);
			set_echo(BUTTON,3,1);
			initialize_device(LOCATOR,1);
			set_echo(LOCATOR,1,1);
			set_echo_surface(LOCATOR,1,&vwsurf);
			set_echo_surface(BUTTON,3,&vwsurf);
			await_any_button(1000000000,&i);
			done(-1); /* normal completion */
		case _PEN:
			IN(d); t = d << 8;
			IN(d); t += d;
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
			done(1);
		}
	done(2);
}
