/*
Added September 1994, Bill Brown
FP/NULL support added September 1995, Bill Brown
*/

#include "externs.h"
#include <device.h>
#include <stdlib.h>

#define SIGN(x) (x < 0? -1.: 1.)


extern void set_font();

static float Listcats[100];
static int Listnum=0;

static
bigger(f1, f2)
float *f1, *f2;
{
    return( *f1 < *f2? -1: (*f1 > *f2) );

}

void
get_cat_list()
{

    if(Acatlist->val){

	if(!Acatall->val){
	    Acatall->val = 1;
	    pnl_fixact(Acatall);
	}
	if(Acatrange->val){
	    Acatrange->val = 0.0;
	    pnl_fixact(Acatrange);
	}

	fprintf(stderr,"\nEnter list of category values:\n");
	fprintf(stderr,"<value> <ENTER> <value> <ENTER> ...\n");
	fprintf(stderr,"<period> <ENTER> to end list\n");

	for(Listnum=0; Listnum<100; Listnum++)
	    if(1 != scanf("%f", Listcats+Listnum)) break;
	fprintf(stderr,"<done>\n");
	fflush(stdin);

	qsort(Listcats, Listnum, sizeof(float), bigger);
	if(Listnum == 100)
	    fprintf(stderr,"Warning - Category List limit reached!\n");
    }
    else{
	Listnum = 0;
    }

}

void
put_legend()
{
Screencoord l, r, b, t;
char *mapset, *map_name;
struct Colors colors;
struct Categories cats;
struct Range range ;
struct FPRange fp_range ;
CELL min, max;
DCELL fmin, fmax;
int cat_labs, cat_vals, do_invert, show_all, is_fp, fprec, iprec;
int lowset, highset;
float lowcat, highcat; 

    
    if (Three_map){
	fprintf(stderr,
	"Legend only available when using a single cell file for color.\n");
	return;
    }
    map_name = Cellname[0];
    set_font();
    cat_labs = Acatlabs->val;
    cat_vals = Acatvals->val;
    show_all = Acatall->val; /* discrete */
    do_invert = Acatinv->val;

    if(Acatlist->val == 0.0)
	Listnum = 0;

    pushviewport();
    pushmatrix();
    frontbuffer(1);
    backbuffer(0);
    readsource(SRC_FRONT);
    linewidth(1);

    if(get_box2(&l, &r, &b, &t)){

	/* Make sure map is available */
	mapset = G_find_cell (map_name, "") ;
	if (mapset == NULL)
	{
	    fprintf(stderr,"Raster file [%s] not available\n", map_name);
	    return;
	}
	is_fp = G_raster_map_is_fp(map_name, mapset);
/*
fprintf(stderr,"IS_FP: %d\n", is_fp);
*/

	if (G_read_colors(map_name, mapset, &colors) == -1)
	{
	    fprintf(stderr,"Color file for [%s] not available\n", map_name) ;
	    return;
	}
	
	if(cat_labs)
	    if (G_read_cats(map_name, mapset, &cats) == -1)
	    {
		fprintf(stderr,"Category file for [%s] not available\n", 
			map_name) ;
		cat_labs = 0;
	    }
	
	if(!Listnum){
	    lowset = highset = 0;	
	    if(Acatrange->val){
		if(PNL_ACCESS(Typein, Acatlow, str)[0]){
		    lowset = 
		    (1 == sscanf(PNL_ACCESS(Typein, Acatlow, str),
			    "%f",&lowcat));
		}
		if(PNL_ACCESS(Typein, Acathigh, str)[0]){
		    highset = 
			    (1 == sscanf(PNL_ACCESS(Typein, Acathigh, str),
			    "%f",&highcat));
		}
/*
		if(PNL_ACCESS(Typein, Acatstep, str)[0]){
		    stepset = 
			    (1 == sscanf(PNL_ACCESS(Typein, Acatstep, str),
			    "%f",&stepcat));
		    if(!stepcat) stepcat = 1.;
		}
*/
	    }
	}
	else{
	    lowcat = Listcats[0];
	    lowset = 1;
	    highcat = Listcats[Listnum - 1];
	    highset = 1;
	}
	if(!lowset || !highset){
	    if(is_fp){
		if (G_read_fp_range(map_name, mapset, &fp_range) != 1 )
		{
		    fprintf(stderr,"Range info for [%s] not available\n",
				    map_name);
		    return;
		}
		G_get_fp_range_min_max (&fp_range, &fmin, &fmax);
		if(lowset) /* allowing to be out of range for now */
		    fmin = lowcat;
		if(highset)
		    fmax = highcat;
	    }
	    else{
		if (G_read_range(map_name, mapset, &range) == -1)
		{
		    fprintf(stderr,"Range info for [%s] not available\n",
				    map_name);
		    return;
		}
		G_get_range_min_max (&range, &min, &max);
		if(lowset) /* allowing to be out of range for now */
		    min = lowcat;
		if(highset)
		    max = highcat;
		fmin = min;
		fmax = max;
	    }
	}
	else{ /* idiot-proof */
	    fmin = lowcat < highcat? lowcat: highcat;
	    fmax = lowcat < highcat? highcat: lowcat;
	    min = fmin;
	    max = fmax;
	}
	if(is_fp? fmin >= fmax : min >= max){
	    fprintf(stderr,"Category range error.\n");
	    return;
	}
	/* set a reasonable precision */
	if(is_fp){
	float df;

	    df = fmax - fmin;
	    if(df < .1) fprec = 6;
	    else if(df < 1) fprec = 4;
	    else if(df < 10) fprec = 3;
	    else if(df < 100) fprec = 2;
	    else fprec = 1;
		
	}
	else{
	int tmp, p1, p2;
	   
	    iprec = p1 = p2 = 1;
	    if(max > 0)
		for (tmp = 1; tmp < max; tmp*=10, p1++);
	    if(min < 0)
		for (tmp = -1; tmp > min; tmp*=10, p2++);
	    
	    iprec = (p1 > p2? p1:p2);
	}

	{
	int i, k, lleg, horiz;
	int red, green, blue;
	CELL tcell;
	DCELL tdcell, pdcell;
	float vert1[2], vert2[2], vert3[2], vert4[2];
	float *dv1, *dv2; /* changing vertex coord */
	float *sv1, *sv2; /* stable vertex coord */
	float stab1, stab2;
	float *dividers;
	unsigned long colr;
	double labvals[12];
	int labw, maxlabw, numlabs;
	float labpos, labpt[3];
	char *cstr, buff[80];
	Screencoord wt, wb, wl, wr; /* Whole legend area, not just box */
	int xoff, yoff;
	int incr; /* for do_invert */
	    
	    horiz = (r-l > t-b);
	    dividers = NULL;

	    /* get label values */
	    if(show_all){
		numlabs = Listnum? Listnum : max - min + 1;
fprintf(stderr,"number of labels = %d\n", numlabs);
/* problem with core dump in 4.2 when want discrete boxes without a list */
		if(is_fp && !Listnum){
		    show_all = 0;   /* maybe later do stats & allow if few #s */
fprintf(stderr,"Unable to show discrete FP range (use list)\n");
		}
		/* watch out for trying to display mega cats */
		else if(numlabs < 1000) 
		    dividers = (float *)malloc((1+numlabs) * sizeof(float));
	    }
	    if(!show_all){  /* no else - can change above */
		numlabs = get_nice_range(fmin, fmax, 4, labvals+1);
		labvals[0] = fmin;
		labvals[numlabs+1] = fmax;
		numlabs += 2;
	    }

	    /* find longest string, reset viewport & saveunder */
	    maxlabw = 0;
	    if(cat_labs || cat_vals){
		for(k=0; k<numlabs; k++){
		    if(is_fp){
			tdcell= show_all? Listcats[k]: labvals[k];
			if(cat_labs){
			    cstr = G_get_d_raster_cat (&tdcell, &cats);
			}
			if(cat_labs && !cat_vals){
			    sprintf(buff, "%s", cstr);
			}
			else{
			    if(cat_labs && cat_vals){
				if(cstr)
				    sprintf(buff, "%.*lf) %s",  
					    fprec, tdcell, cstr);
				else
				    sprintf(buff, "%.*lf", fprec, tdcell);
			    }
			    else if(cat_vals)
				sprintf(buff, "%.*lf", fprec, tdcell);
			}
		    }
		    else{
			tcell= show_all? Listnum? 
			       Listcats[k]: min + k: labvals[k];
			if(cat_labs && !cat_vals)
			    sprintf(buff, "%s", G_get_cat(tcell, &cats));
			else{
			    if(cat_labs && cat_vals){
				cstr = G_get_cat(tcell, &cats);
				if(cstr[0])
				    sprintf(buff, "%*d) %s",iprec, tcell, cstr);
				else
				    sprintf(buff, "%d", tcell);
			    }
			    else if(cat_vals)
				sprintf(buff, "%d", tcell);
			}
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
	    save_under(wl, wb, wr, wt);
	    viewport(wl, wr, wb, wt);
	    ortho2(-0.5, (wr - wl)+0.5, 
		   -0.5, (wt - wb)+0.5); 

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

	    if (show_all){
		if(numlabs > lleg/5)
		    G_warning("Too many categories to show as discrete!");
		else if(numlabs > 1.2 * lleg/get_txtheight())
		    G_warning("Try using smaller font!");
	    }

	    incr = do_invert? -1: 1 ;
	    tdcell = pdcell = -1.0;
	    for (k = 0, i = 0; k < lleg; k++){
		if(show_all && Listnum)
		    tdcell = Listcats[(int)((float)k*numlabs/lleg)];
		else{
		    tcell = min + k * (max - min + 1)/lleg;
		    tdcell = fmin + k * (fmax - fmin)/lleg;
		    if(!is_fp) tdcell = tcell;
		}
		if(k == 0 || tdcell != pdcell ){

		    if(is_fp)
			G_get_d_raster_color (&tdcell, 
					      &red, &green, &blue, &colors);
		    else
			G_get_color((CELL)tdcell, 
				    &red, &green, &blue, &colors);

		    RGB_TO_INT(red,green,blue,colr);
		    if(show_all){  /* draw black-white-black seperator */
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
			    pdcell = tdcell;
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
		pdcell = tdcell;
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
		    if(is_fp){
			if(show_all && Listnum){
			    tdcell = Listcats[k];
			    labpos = (k+.5) / numlabs ;
			}
			else{ 
			    /* show_all not supported unless Listnum */
			    tdcell = labvals[k];
			    labpos = (tdcell - fmin) / (fmax - fmin);
			}
		    }
		    else{
			if(show_all && Listnum){
			    tcell = Listcats[k];
			    labpos = (k+.5) / numlabs ;
			}
			else{  
			    tcell = show_all? min + k: labvals[k];
			    labpos = (tcell - min + .5) / (max - min + 1);
			}
		    }
		    if(do_invert) labpos = 1. - labpos;
		    if(cat_labs){
			if(!is_fp)
			    cstr = G_get_cat(tcell, &cats);
			else
			    cstr = G_get_d_raster_cat (&tdcell, &cats);
		    }
		    if(cat_labs && !cat_vals)
			sprintf(buff, "%s", cstr);
		    else{
			if(cat_labs && cat_vals){
			    if(cstr)
				if(is_fp)
				    sprintf(buff, "%.*lf) %s", 
					    fprec, tdcell, cstr);
				else
				    sprintf(buff, "%*d) %s",iprec, tcell, cstr);
			    else
				if(is_fp)
				    sprintf(buff, "%.*lf", fprec, tdcell);
				else
				    sprintf(buff, "%d", tcell);
			}
			else if(cat_vals)
			    if(is_fp)
				sprintf(buff, "%.*lf", fprec, tdcell);
			    else
				sprintf(buff, "%d", tcell);
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

	if(cat_labs)
	    G_free_cats (&cats);
	G_free_colors (&colors);
    }

    popviewport();
    popmatrix();
    if(getdisplaymode()) {
	frontbuffer(0);
	backbuffer(1);
    }

    update_projection ();

}

int
get_box(bleft,bright,bbottom,btop)
Screencoord *bleft, *bright, *bbottom, *btop;
{

    short val, saved=0;
    long x, y, x1, y1, x2, y2;

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
    cpack(0xCC8888);  /* blue - grey */

    
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
				if(saved)
				    restore_under();

				*bleft = x1 < x2 ? x1: x2;
				*bright = x1 > x2 ? x1: x2;
				*bbottom = y1 < y2 ? y1: y2;
				*btop = y1 > y2 ? y1: y2;

				save_under(*bleft, *bbottom, *bright, *btop);
				viewport(*bleft, *bright, *bbottom, *btop);
				ortho2(-0.5, (*bright - *bleft)+0.5, 
				       -0.5, (*btop - *bbottom)+0.5); 
				clear();
				saved = 1;
			    }
			}

			restore_under();
			return(1);
		    }
		    break;
		case RIGHTMOUSE:
		case ESCKEY:
		    fprintf(stderr,"<done>\n");
		    restore_under();
		    return(0);
		    break;
	    }
    }

}

int
get_nice_range(lownum, highnum, numvals, vals)
double lownum, highnum, *vals;
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
