#include "externs.h"
#include "math.h"

do_label_display(pt, s)
float pt[3];
char *s;
{

	zbuffer(0);
	cpack(Dcolor[LABEL_COLOR]);
	cmov(pt[X], pt[Y], pt[Z]);
	if(font_is_set())
	    fmprstr(s);
	else
	    charstr(s);
	zbuffer(1);

}

/* center label on point */
do_label_displaycenter(pt, s, do_box)
float pt[3];
char *s;
int do_box;
{
short lx, ly;
int xoff, yoff;
float world[4];
int screen[2];


	world[X] = pt[X];
	world[Y] = pt[Y];
	world[Z] = pt[Z];
	if(LatLon){
	    world[X] = (world[X])/XYscale+wind.west;
	    world[Y] = (world[Y])/XYscale+wind.south;
	    latlon_to_sphere(world, RADIUS);
	}
	get_screen_pos(world, screen);
	xoff = (get_txtwidth(s)+get_txtxoffset()) / 2 ;
	yoff = (get_txtheight()-get_txtdescender()) / 2 ;
	lx = screen[X] - xoff;
	ly = screen[Y] - yoff;

	if(lx < 0 || lx > (right-left) || ly < 0 || ly > (top-bottom))
	    return (0);
	/* cpos would be undefined, so chars wouldn't be printed anyway
	but part of box may still be visible, then drawn empty */ 

	zbuffer(0);
	pushviewport();
	pushmatrix();
	loadmatrix(ID_matrix);
	ortho2(-0.5, (right - left)+0.5, -0.5, (top - bottom)+0.5);

	if(do_box){
	float t, b, l, r, v1[2], v2[2];

	    set_transparency();

	    if(IS_DARK(Dcolor[LABEL_COLOR]))
		cpack(Blend | 0xD8D8D8); /* very light gray */
	    else
		cpack(Blend | 0x181818); /* very dark gray */

	    b = ly - get_txtdescender() - 3.;
	    l = lx - get_txtxoffset() - 3.;
	    t = b + get_txtheight() + 6.;
	    r = lx + get_txtwidth(s) + get_txtxoffset() + 3.;
	    v1[X] = l;
	    v1[Y] = b;
	    bgnpolygon();
	    v2f(v1);
	    v2[X] = l;
	    v2[Y] = t;
	    v2f(v2);
	    v2[X] = r;
	    v2f(v2);
	    v2[Y] = b;
	    v2f(v2);
	    v2f(v1);
	    endpolygon();

	    unset_transparency();
	}

	cmov2i(lx ,ly);
	cpack(Dcolor[LABEL_COLOR]);
	
	if(s){
	    if(font_is_set())
		fmprstr(s);
	    else
		charstr(s);
	}

	popmatrix();
	popviewport();
	update_projection ();
	zbuffer(1);
	
	return(1);
}

static int Pie_col[MAX_ST_ATTS];

static int Std_col[6] = {
	0xFF00FF, 
	0xFF0000, 
	0x00FF00, 
	0x0000FF,
	0x00FFFF, 
	0xFFFF00
	};

set_piechart_colors(num, greyscale)
int num, greyscale;
{
int i, glevel;
short r, g, b;

    if(num > MAX_ST_ATTS) num = MAX_ST_ATTS;

    for (i=0; i<num; i++){
	if(greyscale){
	    glevel = i * 255/num;
	    RGB_TO_INT(glevel, glevel, glevel, Pie_col[i]);
	}
	else if(i < NUM_CUSTOM_COLORS){
	    getmcolor(Ccolori[NUM_CUSTOM_COLORS - i - 1], &r, &g, &b);
	    RGB_TO_INT(r, g, b, Pie_col[i]);
	}
	else
	    Pie_col[i] = Std_col[i%6];
    }

}

/* center piechart on point and display on view plane
 * parts are vals between 0.-1.
 * num is number of parts for chart
 * If parts add up to > 1, only first parts that add up to
 *    less than 1 are shown and error returned (-1)
 * If parts total < 1, remainder of pie is filled white.
 * If any illegal values (negative or > 1.) are found, drawing
      is cancelled and error returned */
do_piechart_displaycenter(pt, parts, num, siz)
float pt[3], siz;
double *parts;
int num;
{
short cx, cy;
int xoff, yoff, bad_val, i, ret;
float world[4];
int screen[2];
double sum;
char lstr[12];


	world[X] = pt[X];
	world[Y] = pt[Y];
	world[Z] = pt[Z];
	if(LatLon){
	    world[X] = (world[X])/XYscale+wind.west;
	    world[Y] = (world[Y])/XYscale+wind.south;
	    latlon_to_sphere(world, RADIUS);
	}
	get_screen_pos(world, screen);
	cx = screen[X];
	cy = screen[Y];

	zbuffer(0);
	pushviewport();
	pushmatrix();
	loadmatrix(ID_matrix);
	ortho2(-0.5, (right - left)+0.5, -0.5, (top - bottom)+0.5);

	if(num){
	Coord r, c[2];
	Angle arc1, arc2;

	    set_transparency();

	    c[X] = cx;
	    c[Y] = cy;
	    r = siz * (right - left)/12.;

	    cpack(Blend | 0xFFFFFF); 
	    circf(c[X], c[Y], r);
           
	    bad_val = 0;
	    arc1 = arc2 = 0;
	    sum = 0;
	    for (i=0; i<num; i++){
		if(parts[i] < 0 || parts[i] > 1.){
		    bad_val = 1;
fprintf(stderr,"ERROR - site val out of range: %.4lf\n", parts[i]);
		    break;
		}
		sum+=parts[i];
		arc2 = sum * 3600;
		if(arc2 <= 3600 && parts[i] != 0.0){
		    cpack(Blend | Pie_col[i]); 
		    arcf(c[X], c[Y], r, arc1, arc2);
		    arc1 = arc2;
		}
		else if(arc2 > 3600){
fprintf(stderr,"ERROR - site vals sum > 100%% %.4lf\n", sum);
		    cpack(Blend | Pie_col[i]); 
		    arcf(c[X], c[Y], r, arc1, arc2);
		    arc1 = arc2;
/*
		    ret = -1;
		    break;
*/
		}
	    }
	    if(Apielabel->val){
		arc1 = arc2 = 0;
		sum = 0;

		for (i=0; i<num; i++){
		    if(parts[i] < 0 || parts[i] > 1.)
			break;
		    sum+=parts[i];
		    arc2 = sum * 3600;
		    if(arc2 <= 3600 && parts[i] != 0.0){
			sprintf(lstr,"%d ",(int)(parts[i]*100+.5)); /* round */
/*
			xoff = (get_txtwidth(lstr)+get_txtxoffset()) / 2 ;
			yoff = (get_txtheight()-get_txtdescender()) / 2 ;
*/
			xoff = (get_txtwidth(lstr)/2. - get_txtxoffset());
			yoff = (get_txtheight()/2. - get_txtdescender());
			cx = screen[X] - xoff + .55*r*cos(PI*(arc1+arc2)/3600.);
			cy = screen[Y] - yoff + .55*r*sin(PI*(arc1+arc2)/3600.);
/*
c[X] = cx + xoff;
c[Y] = cy + yoff;
cpack(Blend | ~Dcolor[LABEL_COLOR]); 
circf(c[X], c[Y], (Coord)xoff);
*/
			cmov2i(cx ,cy);
			cpack(Dcolor[LABEL_COLOR]);
			if(font_is_set())
			    fmprstr(lstr);
			else
			    charstr(lstr);
			arc1 = arc2;
		    }
		    else
			break;
		}
	    }
	    unset_transparency();

	}

	
	if(bad_val){
	    ret = -1;
	    sprintf(lstr,"?");
	    xoff = (get_txtwidth(lstr)+get_txtxoffset()) / 2 ;
	    yoff = (get_txtheight()-get_txtdescender()) / 2 ;
	    cx = screen[X] - xoff;
	    cy = screen[Y] - yoff;

	    cmov2i(cx ,cy);
	    cpack(Dcolor[LABEL_COLOR]);
	    if(font_is_set())
		fmprstr(lstr);
	    else
		charstr(lstr);

	}

	popmatrix();
	popviewport();
	update_projection ();
	zbuffer(1);
	
	return(ret);
}

