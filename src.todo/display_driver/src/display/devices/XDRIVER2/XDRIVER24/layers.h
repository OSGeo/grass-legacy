/* -*-c-basic-offset: 4; -*-
 *
 *
 * Pierre de Mouveaux - 27 avril 2000.
 */

#ifndef __LAYERS__
#define __LAYERS__

#include "includes.h"

#define	MAXLAYERS	5
#define TRANSPARENT	1
#define NOCOLOR 	0xFFFFFFFF

typedef struct layer_t {
	unsigned short 	xmin;
	unsigned short 	xmax;
	unsigned short 	ymin;
	unsigned short 	ymax;
	int 	x;
	int 	y;
	int 	w;
	int 	h;
	Pixmap			l;	
	Pixmap			m;
	GC				gc;
	GC				mgc;
	unsigned long	flags;
	unsigned short	id;
	unsigned char	used;
	unsigned char	visible;
	char			name[64];
} layer_t;


void store_top_layer();
void undo_top_layer();
void flush_top_layer();
void combine_layers(layer_t* in, layer_t* out);
layer_t* create_layer(int, int, int);
void delete_layer(layer_t*);
void clear_mask(layer_t*);
void clear_layer(layer_t*);
void clear_layer_area(layer_t* l, int x1, int x2, int y1, int y);
void clear_mask_area(layer_t* l, int x1, int x2, int y1, int y);
void erase_window();

layer_t* get_scratch_layer();
layer_t* get_current_layer();
layer_t* set_current_layer(int);

void display_layer(layer_t*, Pixmap,GC, int);
void display_all_layers(int,int);

void build_layer_list();
int free_layer_list();

unsigned long get_erase_color();
void set_erase_color(unsigned long);
unsigned long get_current_color();
void set_current_color(unsigned long);
unsigned long get_erase_color_from_pad();

int get_pad_rect(int*,int*,int*,int*);

int erase_layer(int);
int erase_layer_recursive();

layer_t* get_layer(int);
int is_valid(int);
int is_valid_index(int);
int select_layer(int);
int show_layer(int);
int hide_layer(int);
int solo_layer(int);
int all_visibles();
int all_layers();
int get_current_layer_id();

#if 0
layer_t* select_layer_by_id(int);
int select_layer_by_name(char*);
int insert_after(layer_t*);
int insert_before(layer_t*);
int append(layer_t*);
int remove_from_list(layer_t*);
#endif


#endif /* __LAYERS__ */

