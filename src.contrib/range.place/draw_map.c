#include "gis.h"
#include "menu.h"

struct box *draw_map(me)

        struct box *me;

{
        char *mapset;
        int offset;
        struct Colors colors;
        char buff[128];
	extern struct box *displayed_map_pointer;
	extern char displayed_map[];
	extern char buf[];
	extern char path[];
	extern int txt_a,txt_b;
	extern int display_delete_flag;
	extern int layer_count,perm_cell_count,user_cell_count;
	extern struct box *user_cell_ptr, *perm_cell_ptr;
	struct box *parent;	

	parent = me->parent;
	mapset = G_find_cell(me->text,"");
	
	if(display_delete_flag == 1){

	D_clear_window();
        D_offset_is(&offset) ;
        R_color_offset(offset) ;
/*
       if (G_get_colr(me->text, mapset, &colors) == -1)
      {
        sprintf(buff,"color file for [%s] not available", me->text) ;                     G_fatal_error(buff) ;
        }

       R_reset_colors(colors.red, colors.grn, colors.blu, colors.num+1);*/ 
/* Set the colors for the display */
	if (G_read_colors(me->text, mapset, &colors) == -1)
	{
		sprintf(buff,"Color file for [%s] not available", me->text) ;
			G_fatal_error(buff) ;
	}

	R_reset_colors(colors.min, colors.max,
		       colors.red, colors.grn, colors.blu) ;
	R_reset_colors(0, 0,
		       &colors.r0, &colors.g0, &colors.b0) ;

/* Go draw the cell file */
 
        cell_draw(me->text,mapset);
        D_set_cell_name(me->text);
	sprintf(displayed_map,me->text);
	displayed_map_pointer = me;
	return(me);
	}

	else{

	G__file_name(path,"cell", "", mapset);
	sprintf(buf,"rm  %s/%s",path,me->text);
	system(buf);	
	erase_ring(me);
	free_child_ring(parent);
	sprintf(buf,"ls %s",path);
	read_from_dir(parent);
	if(parent == perm_cell_ptr) perm_cell_count = layer_count;
	else if(parent == user_cell_ptr)
				user_cell_count = layer_count;
	
	scroll_enter(parent->child);
	draw_ring(parent->child);
	return(parent->child);
	}	

 
}

