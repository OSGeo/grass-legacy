/* -*-c-basic-offset: 4; -*-
 *
 *
 * Pierre de Mouveaux - 27 avril 2000.
 */
#include <stdio.h>
#include <stdlib.h>
#include "includes.h"
#include "colors.h"
#include "layers.h"
#include "pad.h"
#include "../lib/graph.h"
#define SWITCHER
#include "../lib/driver.h"
#undef SWITCHER

#define TEST1 1

extern unsigned SC_WID, SC_HITE;
extern int NCOLORS;

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;

extern int backing_store;
extern u_long *xpixels;
extern Colormap fixedcmap;
extern int transparent;

static unsigned long erase_color 	= NOCOLOR;
static unsigned long current_color	= NOCOLOR;
static int current_id 				= 0;
static layer_t*	layer_stack[MAXLAYERS];
static layer_t*	offscreen_layer 	= NULL;
static layer_t*	scratch_layer 		= NULL;
static layer_t*	screen_layer 		= NULL;



int dirty = 0;


void store_top_layer() {
	layer_t *m, *l;

/*  	fprintf(stderr,"store_top_layer()\n"); */

/*  	fprintf(stderr,"SWITCHING layer: OFFSCREEN\n",current_id); */
	scratch_layer = offscreen_layer;

	l = get_scratch_layer();
	m = get_current_layer();
	m->visible = 1;
	m->used = 1;
  	if (!get_pad_rect(&(l->x),&(l->y),&(l->w),&(l->h))) 
  		fprintf(stderr,"ERROR: store_top_layer()\n"); 
/*
	l->flags |= TRANSPARENT;
	clear_mask(l);
	clear_layer(l);
*/
	

/*    	if (!dirty) */
/*    		return; */

#if !TEST1
	l = get_scratch_layer();
	m = get_current_layer();

	if ((l==NULL) ||(m==NULL)) {
		fprintf(stderr,"store_top_layer() NULL %p %p \n",l,m);
		return;
	}
	combine_layers(l, m);
	clear_mask(l);
	clear_layer(l);
	dirty=1;
#endif
}

void undo_top_layer() {
#if !TEST1
	display_all_layers(0,0);
	dirty = 0;
#endif
}

void display_all_layers(int force, int with_scratch)
{
	int i;
	Pixmap p=0;
	GC pgc=0;
	extern int depth;

/*  	fprintf(stderr,"display_all_layers() %d\n",current_id); */

	p  = XCreatePixmap(dpy, grwin,SC_WID,SC_HITE,depth);
	if (p==0) {
		pgc = 0;
		XSetForeground(dpy,gc,get_erase_color());
		XFillRectangle(dpy, grwin, gc,0,0,SC_WID,SC_HITE);
	} else {
		pgc = gc;
		XSetForeground(dpy,pgc,get_erase_color());
		XFillRectangle(dpy, p, pgc,0,0,SC_WID,SC_HITE);
	}

	for (i=0;i<MAXLAYERS;i++) {
		if (layer_stack[i] != NULL) {
			display_layer(layer_stack[i],p,pgc, force);
		}
#if !TEST1
		if ((i==current_id) && (with_scratch))
			display_layer(get_scratch_layer(),p,pgc,force);
#endif
	}

	if (p!=0) {
		XCopyArea(dpy,p,grwin,gc,0,0,SC_WID,SC_HITE,0,0);
		XFreePixmap(dpy,p);
	}
	XFlush(dpy);
}

void display_layer(layer_t* in, Pixmap p, GC pgc, int force) {

	if (in==NULL)
		return;

	if (!force)
		if ((!in->visible)/* || (!in->used)*/)
			return;
	
	fprintf(stderr,"(%d) ",(in->flags & TRANSPARENT));

	if ((p==0) || (pgc==0)) {

		if ((in->flags & TRANSPARENT) != 0) {
			XSetClipMask(dpy,gc,in->m);
			XSetClipOrigin(dpy,gc,0,0);
		} else {
			XSetClipMask(dpy,gc,0);
		}
		/* for security, to avoid erasing with the last used drawing color */
/*  		XSetForeground(dpy,gc,get_erase_color_from_pad()); */
/*  		XSetForeground(dpy,gc,get_erase_color()); */
		XCopyArea(dpy, in->l, grwin, gc, in->x, in->y,in->w,in->h, in->x, in->y);
		XSetClipMask(dpy,gc,0);
	} else {

		XSetForeground(dpy,pgc,get_erase_color());
		if ((in->flags & TRANSPARENT) != 0) {
			XSetClipMask(dpy,pgc,in->m);
			XSetClipOrigin(dpy,pgc,0,0);
		} else {
			XSetClipMask(dpy,pgc,0);
		}
		XCopyArea(dpy, in->l, p, pgc, in->x, in->y,in->w,in->h, in->x, in->y);
		XSetClipMask(dpy,pgc,0);
	}

}
		
void flush_top_layer() {
	int i;
	layer_t* l, *m;

/*  	fprintf(stderr,"flush_top_layer() %d\n",current_id); */
	if (dirty) {
		dirty = 0;
	}
#if TEST1

	l = get_scratch_layer();
	m = get_current_layer();

	if ((l==NULL) ||(m==NULL)) {
		fprintf(stderr,"store_top_layer() NULL %p %p \n",l,m);
		return;
	}
	XSetForeground(dpy,l->gc,get_erase_color());
	combine_layers(l, m);
	clear_mask(l);
	clear_layer(l);
	dirty=1;
#endif
/*  	clear_layer(screen_layer); */
	display_all_layers(0,1);
	
/*  	fprintf(stderr,"SWITCHING layer: SCREEN\n",current_id); */
	scratch_layer = screen_layer;
}


void combine_layers(layer_t* in, layer_t* out) {

	if ((in==NULL)||(out==NULL))
		return;

/*  	fprintf(stderr,"Transprent: in: %d out %d\n",in->flags,out->flags) ; */
	if (in->flags & TRANSPARENT) {
		XSetClipMask(dpy,out->gc,in->m);
		XSetClipOrigin(dpy,out->gc,0,0);
		if (out->flags & TRANSPARENT) {
			XCopyPlane(dpy, in->m, out->m, out->mgc, in->x, in->y,in->w, in->h, in->x, in->y, 1);	
		}
	} else {
		XSetClipMask(dpy,out->gc,0);
		out->flags &= ~TRANSPARENT;
	}
	XCopyArea(dpy, in->l, out->l, out->gc, in->x, in->y,in->w, in->h, in->x, in->y);
	XSetClipMask(dpy,out->gc,0);
/*  	fprintf(stderr,"Transprent: in: %d out %d\n",in->flags,out->flags) ; */
}

layer_t* get_current_layer()
{
	return layer_stack[current_id];
}

layer_t* get_scratch_layer()
{
	return scratch_layer;
}

layer_t* set_current_layer(int id)
{
	if (is_valid(id)>0) {
		current_id = id;
		return layer_stack[id];
	} else
		return layer_stack[current_id];
}

layer_t* get_layer(int id)
{
	if (!is_valid(id))
		return NULL;
	
	return layer_stack[id];
}

int free_layer_list() {
	int i;

	current_id = 0;

	delete_layer(scratch_layer);
	
	for (i=0; i<MAXLAYERS; i++) {
		delete_layer(layer_stack[i]);
	}
}


#if 1
static void create_screen_layer()
{
	layer_t* l;
	extern int depth;
	XGCValues gcv;

/*  	fprintf(stderr,"create_screen_layer()\n"); */

	l = (layer_t*)calloc(1,sizeof(struct layer_t));
	if (l==NULL)
		return ;
	l->l = grwin;
	l->m = XCreatePixmap(dpy, grwin, SC_WID,SC_HITE, 1);
	if (l->m == 0) {
		XFreePixmap(dpy,l->l);
		free(l);
		return ;
	}

	gcv.foreground=1;
   	gcv.background=0;
	gcv.line_width=0;
	gcv.graphics_exposures=False;
	l->mgc = XCreateGC(dpy, l->m, 
		GCForeground|GCBackground|GCLineWidth|GCGraphicsExposures, &gcv);
	if (l->mgc == NULL) {
		XFreePixmap(dpy,l->m);
		free(l);
		return ;
	}

	l->gc=gc;	/* XCreateGC + XCopyGC ? */
	l->w=SC_WID;
	l->h=SC_HITE;
	clear_mask(l);
	l->flags = TRANSPARENT;
	l->visible=1;
	l->used=1;

	screen_layer = l;
}
#endif

void build_layer_list() {
	extern int depth;
	int i;

	current_id = 0;
/*  	fprintf(stderr,"create_layerS()\n"); */

	delete_layer(offscreen_layer);

  	delete_layer(screen_layer);
	
	for (i=0; i<MAXLAYERS; i++) {
		delete_layer(layer_stack[i]);
	}

	scratch_layer = offscreen_layer = create_layer(depth, SC_WID, SC_HITE);
  	create_screen_layer();

	current_id = 0;

	for (i=0; i<MAXLAYERS; i++) {
		layer_stack[i] = create_layer(depth,SC_WID, SC_HITE);
		if (layer_stack[i] == NULL) {
			fprintf(stderr,"Error: allocation of layers memory.\n");
			exit(0);	/* Need's improvement ... ;-) */
		}
	}
}


layer_t* create_layer(int depth, int w, int h)
{
	layer_t* l;
	XGCValues gcv;
/*  	fprintf(stderr,"create_layer()\n"); */

	l = (layer_t*)calloc(1,sizeof(struct layer_t));
	if (l==NULL)
		return NULL;
	l->l =  XCreatePixmap(dpy, grwin, w, h,depth);
	if (l->l == 0) {
		free(l);
		return NULL;
	}
	l->m = XCreatePixmap(dpy, grwin, w, h, 1);
	if (l->m == 0) {
		XFreePixmap(dpy,l->l);
		free(l);
		return NULL;
	}

	gcv.foreground=1;
   	gcv.background=0;
	gcv.line_width=0;
	gcv.graphics_exposures=False;
	l->mgc = XCreateGC(dpy, l->m, 
		GCForeground|GCBackground|GCLineWidth|GCGraphicsExposures, &gcv);
	if (l->mgc == NULL) {
		XFreePixmap(dpy,l->l);
		XFreePixmap(dpy,l->m);
		free(l);
		return NULL;
	}

	l->gc=gc;	/* XCreateGC + XCopyGC ? */
	l->w=w;
	l->h=h;
	l->flags = TRANSPARENT;
	clear_mask(l);
	clear_layer(l);
	l->visible=1;
	l->used=0;
	return l;
}	

void delete_layer(layer_t* l)
{
	if (l==NULL)
		return;
	if (l->l != grwin)
		XFreePixmap(dpy,l->l);

	XFreePixmap(dpy,l->m);
/*  	XFreeGC(dpy,l->gc); */
	XFreeGC(dpy,l->mgc);
	free(l);
}

void clear_layer(layer_t* l)
{
	if (l == NULL)
		return;
	XSetForeground(dpy,l->gc,get_erase_color());
	XFillRectangle(dpy, l->l, l->gc, l->x, l->y, l->w,l->h); 
/*  	fprintf(stderr," Clear_leayer() [%d %d %d %d]\n",l->x,l->y,l->w,l->h); */
}

void clear_layer_area(layer_t* l, int x, int y, int w, int h)
{
	XGCValues gcv;
/*  	fprintf(stderr,"clear_layer_area()\n"); */
	if (l == NULL)
		return;
	XSetForeground(dpy,l->gc,get_erase_color());
	XFillRectangle(dpy, l->l, l->gc,x,y,w,h);
/*  	fprintf(stderr," [%d %d %d %d]\n",x,y,w,h); */
/*  	fprintf(stderr," <0 0 %d %d>\n",SC_WID, SC_HITE); */
}

void clear_mask(layer_t* l)
{
	if (l == NULL)
		return;
	XSetFunction(dpy,l->mgc,GXclear);
	XFillRectangle(dpy,l->m,l->mgc, l->x,l->y,l->w,l->h);
	XSetFunction(dpy,l->mgc,GXor);
/*  	fprintf(stderr," Clear_mask() [%d %d %d %d]\n",l->x,l->y,l->w,l->h); */
}

void clear_mask_area(layer_t* l,int x, int y, int w, int h)
{
	if (l == NULL)
		return;
	XSetFunction(dpy,l->mgc,GXclear);
	XFillRectangle(dpy,l->m,l->mgc, x,y,w,h);
	XSetFunction(dpy,l->mgc,GXor);
/*  	fprintf(stderr," [%d %d %d %d]\n",x,y,w,h); */
/*  	fprintf(stderr," <0 0 %d %d>\n",SC_WID, SC_HITE); */
}

int is_valid(int id) {

	if ((id >=MAXLAYERS) || (id < 0))
		return -2;
	if (layer_stack[id] == NULL)
		return -1;
	return 1;
}

int is_valid_index(int id) {

	if ((id < MAXLAYERS) || (id >= 0))
		return 1;
	return -1;
}

int select_layer(int layer) {
	set_current_layer(layer);
	return 1;
}

int show_layer(int id) {
	layer_t* l = get_layer(id);
	if (l==NULL)
		return -1;
	l->visible = 1;
/*  	display_all_layers(0,1); */
	return 1;
}

int hide_layer(int id) {
	layer_t* l = get_layer(id);
	if (l==NULL)
		return -1;
	l->visible = 0;
/*  	display_all_layers(0,1); */
	return 1;
}

int solo_layer(int id)
{
	int i;

	if ((id>=MAXLAYERS)|| (id<0))
		return -1;

	for (i=0; i< MAXLAYERS; i++) {
		if (i != id)
			hide_layer(i);
		else
			show_layer(i);
	}
	return 1;
}

int all_visibles()
{
	display_all_layers(0,1);
	return 1;
}

int all_layers()
{
	display_all_layers(1,1);
	return 1;
}

int get_current_layer_id()
{
	extern int current_id;
	return current_id;
}
int get_num_layers()
{
	return MAXLAYERS;
}

int get_pad_rect(int*x,int*y,int*w,int*h)
{
	PAD *pad;
	ITEM *item;
    LIST *list;

	pad = get_current_pad();

	if (pad == NULL) {
		*x=0;
		*y=0;
		*w=SC_WID;
		*h =SC_HITE;
		return 1;
	}

	item = find_item(pad, "d_win");

	if (item==NULL)
		return -1;

	list= item->list;
	if (list == NULL)
		return -1;

	if (list->value == NULL)
		return -1;

	sscanf(list->value,"%d %d %d %d",y,h,x,w);
	*h -= *y;
	*w -= *x;
/*  	fprintf(stderr,"%s [Get pad rect%d %d %d %d]\n",list->value,*x,*y,*w,*h); */
}

unsigned long get_erase_color() {
	if (erase_color == NOCOLOR)
		erase_color = _get_lookup_for_color(0,0,0);
	return erase_color;
}

void set_erase_color(unsigned long color)
{
	erase_color = color;
}

void set_current_color(unsigned long color)
{
	current_color = color;
}

unsigned long get_current_color()
{
	if (current_color == NOCOLOR)
		current_color = _get_lookup_for_color(0,0,0);
	return current_color;
}
unsigned long get_erase_color_from_pad()
{
	PAD *pad;
	ITEM *item;
    LIST *list;
	
	int color;

	/* NOT working: in fact, colors are stored 
	   in textual format, i.e. "blue" */

	pad = get_current_pad();

	if (pad == NULL) {
		fputc('*',stderr);
		return get_erase_color();
	}

	item = find_item(pad, "erase");

	if (item==NULL) {
		fputc('#',stderr);
		return get_erase_color();
	}

	list= item->list;
	if (list == NULL) {
		fputc('/',stderr);
		return get_erase_color();
	}
/* 	for (list = item->list; list != NULL; list = list->next) */

	if (list->value == NULL) {
		fputc('\\',stderr);
		return get_erase_color();
	}
	fputc('!',stderr);
	if (sscanf(list->value,"%d",color) == 1) {
		fprintf(stderr,"%s [Pad erase color: %d]\n",list->value,color);
		return (unsigned long) _get_color_index(color);
	}
	return get_erase_color();
}


void erase_window()
{
	fprintf(stderr,"erase_window()! SO SLoooooW!\n");
	XFillRectangle(dpy, grwin, gc,0,0,SC_WID,SC_HITE);
}



int XEraseAll () {
	layer_t *s;
	s=get_current_layer();
	s->flags |= TRANSPARENT;
	clear_layer(s);
	clear_mask(s);
	s->visible=0;
	s->used=0;
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;
	clear_layer(s);
	clear_mask(s);
	s->visible=0;
	s->used=0;
	return 0;
}


int XEraseArea (int x, int y, int w, int h) {
	layer_t *s;
	s=get_current_layer();
	s->flags |= TRANSPARENT;
	s->x=x;
	s->y=y;
	s->w=w;
	s->h=h;
	clear_layer_area(s,x,y,w,h);
	clear_mask_area(s,x,y,w,h);
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;
	s->x=x;
	s->y=y;
	s->w=w;
	s->h=h;
	clear_layer_area(s,x,y,w,h);
	clear_mask_area(s,x,y,w,h);
	return 0;
}

int erase_layer(int id) {
		
	layer_t *s;
	int x,y,w,h;
	
	if (get_pad_rect(&x,&y,&w,&h)) {
/*  		fprintf(stderr," [Erase : %d %d %d %d]\n",x,y,w,h); */
		s=get_scratch_layer();
		s->flags |= TRANSPARENT;
		s->x=x;
		s->y=y;
		s->w=w;
		s->h=h;
		clear_layer_area(s,x,y,w,h);
		clear_mask_area(s,x,y,w,h);
		if ((s= get_layer(id)) != NULL) {
			s->flags |= TRANSPARENT;
  			clear_layer_area(s,x,y,w,h);
			clear_mask_area(s,x,y,w,h);
		}				
	} else {
		s=get_scratch_layer();
		s->flags |= TRANSPARENT;
		clear_layer(s);
		clear_mask(s);
		if ((s= get_layer(id)) != NULL) {
			s->flags |= TRANSPARENT;
			clear_layer(s);
			clear_mask(s);
		}				
	}
	return 0;
}

int erase_layer_recursive() {
		
	layer_t *s;
	int x,y,w,h;
	int i;

	if (get_pad_rect(&x,&y,&w,&h)) {
/*  		fprintf(stderr," [Erase recursive: %d %d %d %d]\n",x,y,w,h); */
		s=get_scratch_layer();
		s->flags |= TRANSPARENT;
		s->x=x;
		s->y=y;
		s->w=w;
		s->h=h;
		clear_layer_area(s,x,y,w,h);
		clear_mask_area(s,x,y,w,h);
		for (i=0; i<MAXLAYERS; i++) {
			if ((s= get_layer(i)) != NULL) {
				s->flags |= TRANSPARENT;
				clear_layer_area(s,x,y,w,h);
				clear_mask_area(s,x,y,w,h);
			}				
		}
	} else {
		s=get_scratch_layer();
		s->flags |= TRANSPARENT;
		clear_layer(s);
		clear_mask(s);
		for (i=0; i<MAXLAYERS; i++) {
			if ((s= get_layer(i)) != NULL) {
				s->flags |= TRANSPARENT;
				clear_layer(s);
				clear_mask(s);
			}				
		}
	}
	return 0;
}






















