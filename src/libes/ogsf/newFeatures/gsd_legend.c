/*********************/
DOING:  port this SG3D code to nviz - BB
/*********************/

#include <gstypes.h>


extern void gsd_set_font();

static float *Listcats;
static int Listnum=0;

#define SAVE_UNDER
#define MAX_LEGEND 256

#ifdef OLD
#include <device.h>
/*****************************************************************/

static
bigger(f1, f2)
float *f1, *f2;
{
    return( *f1 < *f2? -1: (*f1 > *f2) );

}

/*****************************************************************/
/* static float *Listcats; was static float Listcats[MAX_LEGEND] 
   when still using this */

void
get_cat_list()
{

    if(Acatlist->val){

	if(!Acatall->val){
	    Acatall->val = 1;
	    pnl_fixact(Acatall);
	}

	fprintf(stderr,"\nEnter list of category values:\n");
	fprintf(stderr,"<value> <ENTER> <value> <ENTER> ...\n");
	fprintf(stderr,"<period> <ENTER> to end list\n");

	for(Listnum=0; Listnum<MAX_LEGEND; Listnum++)
	    if(1 != scanf("%f", Listcats+Listnum)) break;
	fprintf(stderr,"<done>\n");
	fflush(stdin);

	qsort(Listcats, Listnum, sizeof(float), bigger);
	if(Listnum == MAX_LEGEND)
	    fprintf(stderr,"Warning - Category List limit reached!\n");
    }
    else{
	Listnum = 0;
    }

}

/*****************************************************************/
#endif /* OLD */

#ifdef TO_MOVE
{
/*****************************************************************/
/* move to GRASS_dependant */
/*****************************************************************/
Gs_get_colors_for_data(gs, fdata, nd, r, g, b)
struct Colors colors;
	if (G_read_colors(map_name, mapset, &colors) == -1)
	{
	    fprintf(stderr,"Color file for [%s] not available\n", map_name) ;
	    return;
	}
	G_get_color (tcell, &red, &green, &blue, &colors);
	G_free_colors (&colors);
/*****************************************************************/
/* use_cats, use_vals */
Gs_get_labels_for_data(gs, att, fdata, nd, use_cats, labels)
char *mapset, *map_name;
struct Categories cats;
	mapset = G_find_cell (map_name, "") ;
	if (mapset == NULL)
	{
	    fprintf(stderr,"Raster file [%s] not available\n", map_name);
	    return;
	}
	    if (G_read_cats(map_name, mapset, &cats) == -1)
	    {
		fprintf(stderr,"Category file for [%s] not available\n", 
			map_name) ;
		cat_labs = 0;
	    }
	    for(k=0; k<numlabs; k++){
		tcell= discrete? Listnum? Listcats[k]: min + k: labvals[k];
		if(cat_labs && !cat_vals)
		    sprintf(buff, "%s", G_get_cat(tcell, &cats));
		else{
		    if(cat_labs && cat_vals){
			cstr = G_get_cat(tcell, &cats);
			if(cstr[0])
			    sprintf(buff, "%2d) %s", tcell, cstr);
			else
			    sprintf(buff, "%2d", tcell);
		    }
		    else if(cat_vals)
			sprintf(buff, "%2d", tcell);
		}
	    G_free_cats (&cats);
/*****************************************************************/
/*****************************************************************/
/*****************************************************************/

/*****************************************************************/
/* move to primitives */
/*****************************************************************/
gsd_bgn_legend_viewport(wl, wb, wr, wt)
int wl, wb, wr, wt;
{
	pushviewport();
	pushmatrix();
	frontbuffer(1);
	backbuffer(0);
	readsource(SRC_FRONT);
	linewidth(1);

	save_under(wl, wb, wr, wt);
	viewport(wl, wr, wb, wt);
	ortho2(-0.5, (wr - wl)+0.5, 
	       -0.5, (wt - wb)+0.5); 
}
gsd_end_legend_viewport()
{
    popviewport();
    popmatrix();
    if(getdisplaymode()) {
	frontbuffer(0);
	backbuffer(1);
    }

}
/*****************************************************************/
}
#endif

void
gs_put_legend(gs, sl, sr, sb, st, legend_type, range, list, nlist)
geosurf *gs;
int sl, sr, sb, st;  /* bounding box for colors */
int legend_type;  /* discrete, continuous, list, range, inverted, etc 
		     - to be defined */
float *range, *list;
int nlist;
{
/*
struct Range range ;
*/
int cat_labs=0, cat_vals=0, do_invert=0, discrete=0;
int lowset=0, highset=0;
float lowcat, highcat; 
float min, max;


    
    gsd_set_font();

	
    if(legend_type & LT_SHOW_VALS)
	cat_vals = 1;

    if(legend_type & LT_SHOW_LABELS)
	cat_labs = 1;

    /* [get min-max] */
    /* allowing to be out of range for now */
    if(legend_type & LT_RANGE_LOW_HI == LT_RANGE_LOW_HI){
	lowcat = range[0]; 
	highcat = range[1]; 
    }
    else{
	lowcat = min;
	highcat = max;
	if(legend_type & LT_RANGE_LOWSET){
	    lowcat = range[0];
	}
	if(legend_type & LT_RANGE_HISET){
	    highcat = range[1];
	}
    }

    min = lowcat < highcat? lowcat: highcat;
    max = lowcat < highcat? highcat: lowcat;
    if(min == max) 
	Gs_warning("Range request error for legend");

    if(list && (legend_type & LT_LIST)){
	Listcats = list;
	Listnum = nlist;
	qsort(Listcats, Listnum, sizeof(float), bigger);
	discrete = 1;   /* list automatically discrete */
    }
    else
	Listnum = 0;

    if(legend_type & LT_DISCRETE)
	discrete = 1;

    if(legend_type & LT_INVERTED)
	do_invert = 1;

    horiz = (r-l > t-b);
    dividers = NULL;

    /* how many labels? */
/*
numlabs can't be = max - min + 1 any more because of floating point
maybe shouldn't allow discrete legend for floating point maps (unless list)
or else check number of different values in floating point map
and use each if "reasonable"
gs_get_values_in_range(gs, att, low, high, values, &nvals)
the nvals sent has a max number to return, nvals returned is the actual
number set in values, return val is 1 on success, -1 if > max vals found

might need to think about doing histograms first & use same routines here
could also have a LT_MOST that would limit # to some N most frequent

*/
    if(discrete){
	numlabs = Listnum? Listnum : max - min + 1;
	/* watch out for trying to display mega cats */
	if(numlabs < MAX_LEGEND) 
	    dividers = (float *)malloc(numlabs * sizeof(float));
    }
    else{
	numlabs = get_nice_range((float)min, (float)max, 4, labvals+1);
	labvals[0] = min;
	labvals[numlabs+1] = max;
	numlabs += 2;
    }
    {
    int i, k, lleg, horiz;
    int red, green, blue;
    float tcell, pcell;
    float vert1[2], vert2[2], vert3[2], vert4[2];
    float *dv1, *dv2; /* changing vertex coord */
    float *sv1, *sv2; /* stable vertex coord */
    float stab1, stab2;
    float *dividers;
    unsigned long colr;
    float labvals[12];
    int labw, maxlabw, numlabs;
    float labpos, labpt[3];
    char *cstr, buff[80];
    Screencoord wt, wb, wl, wr; /* Whole legend area, not just box */
    int xoff, yoff;
    int incr; /* for do_invert */
	
	/* find longest string, reset viewport & saveunder */
	maxlabw = 0;
	if(cat_labs || cat_vals){
	    for(k=0; k<numlabs; k++){
		tcell= discrete? Listnum? Listcats[k]: min + k: labvals[k];
		if(cat_labs && !cat_vals)
		    sprintf(buff, "%s", G_get_cat(tcell, &cats));
		else{
		    if(cat_labs && cat_vals){
			cstr = G_get_cat(tcell, &cats);
			if(cstr[0])
			    sprintf(buff, "%2d) %s", tcell, cstr);
			else
			    sprintf(buff, "%2d", tcell);
		    }
		    else if(cat_vals)
			sprintf(buff, "%2d", tcell);
		}
		labw = get_txtwidth(buff);
		if(labw > maxlabw){
		    maxlabw = labw;
		}
	    }
	}
	if(horiz){
	    xoff =  maxlabw/2 + get_txtxoffset();
	    wl = l - xoff;
	    wr = r + xoff;
	    yoff = 0; 
	    wb = b ;
	    wt = t + get_txtheight() + get_txtdescender() + 3;
	}
	else{
	    xoff = 0;
	    wl = l;
	    wr = r + maxlabw + get_txtxoffset() + 3;
	    yoff = get_txtheight()/2  + get_txtdescender();
	    wb = b - yoff;
	    wt = t + yoff;
	}

	gsd_bgn_legend_viewport(wl, wb, wr, wt);

	vert1[X] = vert2[X] = xoff;
	vert1[Y] = vert2[Y] =  yoff;
	if(horiz){
	    lleg = r-l;
	    dv1 = vert1+X;
	    dv2 = vert2+X;
	    sv1 = vert1+Y;
	    sv2 = vert2+Y;
	    stab2 = vert2[Y] = t - b + yoff;
	    stab1 = vert1[Y] = yoff;
	    if(do_invert)
		vert1[X] = vert2[X] = r - l + xoff;
	}
	else{
	    lleg = t-b;
	    dv1 = vert1+Y;
	    dv2 = vert2+Y;
	    sv1 = vert1+X;
	    sv2 = vert2+X;
	    stab2 = vert2[X] = r - l + xoff;
	    stab1 = vert1[X] = xoff;
	    if(do_invert)
		vert1[Y] = vert2[Y] = t - b + yoff;
	}

	if (discrete){
	    if(numlabs > lleg/5)
		G_warning("Too many categories to show as discrete!");
	    else if(numlabs > 1.2 * lleg/get_txtheight())
		G_warning("Try using smaller font!");
	}

	incr = do_invert? -1: 1 ;
	for (k = 0, i = 0; k < lleg; k++){
	    if(discrete && Listnum)
		tcell = Listcats[(int)((float)k*numlabs/lleg)];
	    else
		tcell = min + k * (double)(max - min + 1)/lleg;
	    if(k == 0 || tcell != pcell){
		RGB_TO_INT(red,green,blue,colr);
		if(discrete){  /* draw black-white-black seperator */
		    if (k > 0){
			*dv1 -= 2.*incr;
			*dv2 -= 2.*incr;
			cpack(0x0);
			bgnline();
			v2f(vert1);
			v2f(vert2);
			endline();

			*dv1 += 1.*incr;
			*dv2 += 1.*incr;
			if(dividers)
			    dividers[i++] = *dv1;

			*dv1 += 1.*incr;
			*dv2 += 1.*incr;
			cpack(0x0);
			bgnline();
			v2f(vert1);
			v2f(vert2);
			endline();

			*dv1 += 1.*incr;
			*dv2 += 1.*incr;
			pcell = tcell;
			continue; 
		    }
		}
	    }
	    cpack(colr);
	    bgnline();
	    v2f(vert1);
	    v2f(vert2);
	    endline();
	    *dv1 += 1.*incr;
	    *dv2 += 1.*incr;
	    pcell = tcell;
	}

	/* Black box */
	vert1[X] = vert2[X] = 1. + xoff;
	vert1[Y] = vert4[Y] = 1. + yoff;
	vert3[X] = vert4[X] = r - l - 1. + xoff;
	vert3[Y] = vert2[Y] = t - b - 1. + yoff;
	cpack(0x000000) ;
	bgnline();
	v2f(vert1);
	v2f(vert2);
	v2f(vert3);
	v2f(vert4);
	v2f(vert1);
	endline();

	/* White box */
	vert1[X] = vert2[X] = xoff ;
	vert1[Y] = vert4[Y] = yoff ;
	vert3[X] = vert4[X] = r - l + xoff ;
	vert3[Y] = vert2[Y] = t - b + yoff ;
	cpack(0xFFFFFF) ;
	bgnline();
	v2f(vert1);
	v2f(vert2);
	v2f(vert3);
	v2f(vert4);
	v2f(vert1);
	endline();
	
	/* draw discrete dividers */
	if(dividers){
	    cpack(0xFFFFFFFF);
	    *sv1 = stab1;
	    *sv2 = stab2;
	    for(k = 0; k < i; k++){
		*dv1 = *dv2 = dividers[k];
		bgnline();
		v2f(vert1);
		v2f(vert2);
		endline();
	    }
	}

	
	if(cat_labs || cat_vals){
	    labpt[Z] = 0;
	    for(k=0; k<numlabs; k++){
		if(discrete && Listnum){
		    tcell = Listcats[k];
		    labpos = (k+.5) / numlabs ;
		}
		else{
		    tcell = discrete? min + k: labvals[k];
		    labpos = (tcell - min + .5) / (max - min + 1);
		}
		if(do_invert) labpos = 1. - labpos;
		if(cat_labs && !cat_vals)
		    sprintf(buff, "%s", G_get_cat(tcell, &cats));
		else{
		    if(cat_labs && cat_vals){
			cstr = G_get_cat(tcell, &cats);
			if(cstr[0])
			    sprintf(buff, "%2d) %s", tcell, cstr);
			else
			    sprintf(buff, "%2d", tcell);
		    }
		    else if(cat_vals)
			sprintf(buff, "%2d", tcell);
		}
		if(horiz){
		    labpt[X] = labpos * (r - l) + xoff - 
				get_txtwidth(buff)/2 - get_txtxoffset();
		    labpt[Y] = t - b + yoff + 3 + get_txtheight()/2;
		}
		else{
		    labpt[X] = r - l + xoff + get_txtxoffset() + 3;
		    labpt[Y] = labpos * (t - b) + yoff - 
				get_txtheight()/2 + get_txtdescender();
		}
		do_label_display(labpt, buff);
		
	    }
	}

	if(dividers)
	    free(dividers);
    }


    gsd_end_legend_viewport();
    update_projection ();

}


/*****************************************************************/
/* Replaces num with next higher(> 1) or lower (< 1)  number 
that is some power of ten of 25, 50 or 100 */

make_nice_number(num)
float *num;
{
float newnum, nextnum;

    if(*num < 0) return(0);

    if(*num < 1){
	newnum = 1.;
	while(.5*newnum > *num){
	    nextnum = newnum/10.;
	    newnum /= 2.;
	    if(.5*newnum > *num)
		newnum /= 2.;
	    if(.5*newnum > *num)
		newnum = nextnum;
	}
    }
    else{
	newnum = 1.;
	while(2*newnum <= *num){
	    nextnum = newnum*10.;
	    newnum *= 2.5;
	    if(2*newnum <= *num)
		newnum *= 2.;
	    if(2*newnum <= *num)
		newnum = nextnum;
	}
	if (newnum == 2.5) newnum = 3;   
	/* 2.5 isn't nice, but .25, 25, 250 ... are */
    }

    *num = newnum;
    return(1);

}

/*****************************************************************/

int
get_nice_range(lownum, highnum, numvals, vals)
float lownum, highnum, *vals;
int numvals;
{
int num=0;
float curnum, step, start;

    if(!numvals) return(0);

    step = (highnum - lownum) / numvals;
    make_nice_number(&step);

    /* get a starting point */
    start = step * (int)(1 + lownum/step);
    if(start - lownum < .65*step) start+=step;

    for(curnum=start; curnum<(highnum-.65*step); curnum+=step){
        vals[num++] = curnum;
    }
    return(num);

}


/*****************************************************************/

/* need something like:
   GS_begin_rubberband_box(sx, sy)
   GS_redraw_rubberband_box(sx2, sy2)
   GS_end_rubberband_box(erase, &t, &b, &l, &r)
*/

/*****************************************************************/
int
get_box2(bleft,bright,bbottom,btop)
Screencoord *bleft, *bright, *bbottom, *btop;
{

    short val;
    long x, y, x1, y1, x2, y2;
    float vert1[2], vert2[2], vert3[2], vert4[2];

    fprintf(stderr,"\nleft mouse button : Sweep area for legend\n");
/*
    fprintf(stderr,"right mouse button : Cancel \n");
*/


    if(!isqueued(ESCKEY)) 
	qdevice(ESCKEY);
    if(!isqueued(LEFTMOUSE)) 
	qdevice(LEFTMOUSE);
    if(!isqueued(RIGHTMOUSE)) 
	qdevice(RIGHTMOUSE);

    loadmatrix(ID_matrix);
    drawmode(OVERDRAW);
    mapcolor(1, 0x44, 0x44, 0x77);  /* blue - grey */
    mapcolor(2, 0xDD, 0xDD, 0x44);  /* yellowish */
    ortho2(-0.5, (right - left)+0.5, -0.5, (top - bottom)+0.5); 
    deflinestyle(1, 0xF0F0);
    linewidth(1);

    
    while(TRUE){

	if (qtest())
	    switch (qread (&val)) {
		case LEFTMOUSE:
		    if(val){
			x1 = getvaluator(MOUSEX) - Ox;
			y1 = getvaluator(MOUSEY) - Oy;
			x2 = x1;
			y2 = y1;
			*bleft = x1 < x2 ? x1: x2;
			*bright = x1 > x2 ? x1: x2;
			*bbottom = y1 < y2 ? y1: y2;
			*btop = y1 > y2 ? y1: y2;
			while(getbutton(LEFTMOUSE)){
			    x = getvaluator(MOUSEX) - Ox;
			    y = getvaluator(MOUSEY) - Oy;
			    if(x2 != x || y2 != y){
				x2 = x;
				y2 = y;
				color(0);
				clear();

				*bleft = x1 < x2 ? x1: x2;
				*bright = x1 > x2 ? x1: x2;
				*bbottom = y1 < y2 ? y1: y2;
				*btop = y1 > y2 ? y1: y2;

				vert1[X] = vert2[X] = *bleft;
				vert1[Y] = vert4[Y] = *bbottom;
				vert3[X] = vert4[X] = *bright;
				vert3[Y] = vert2[Y] = *btop;

				setlinestyle(0);
				color(1);
				bgnline();
				v2f(vert1);
				v2f(vert2);
				v2f(vert3);
				v2f(vert4);
				v2f(vert1);
				endline();

				setlinestyle(1);
				color(2);
				bgnline();
				v2f(vert1);
				v2f(vert2);
				v2f(vert3);
				v2f(vert4);
				v2f(vert1);
				endline();
			    }
			}

			color(0);
			clear();
			drawmode(NORMALDRAW);
			setlinestyle(0);
			return(1);
		    }
		    break;
		case RIGHTMOUSE:
		case ESCKEY:
		    fprintf(stderr,"<done>\n");
		    color(0);
		    clear();
		    drawmode(NORMALDRAW);
		    setlinestyle(0);
		    return(0);
		    break;
	    }
    }

}

/*****************************************************************/

