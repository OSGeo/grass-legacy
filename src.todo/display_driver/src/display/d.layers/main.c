/* -*-c-basic-offset: 4;-*- */

#include "gis.h"
#include "display.h"
#include "D.h"
#include "raster.h"
#include "../../libes/raster/graph.h"

int main(int argc,char *argv[])
{
	struct Option *layer, *dev;
	struct Flag *solo, *all, *undo,  *select,*get, *all_visible, *x,*show, *hide;
	char name[256];
	char *err;
	char* driver;
	char l;
	G_gisinit(argv[0]);


	solo = G_define_flag();
    solo->key = 'o';
    solo->description = "Solo";

	all = G_define_flag();
    all->key = 'a';
    all->description = "All";

	all_visible = G_define_flag();
    all_visible->key = 'v';
    all_visible->description = "All visibles";

	select = G_define_flag();
    select->key = 't';
    select->description = "Target";

	show = G_define_flag();
    show->key = 's';
    show->description = "Show";

	hide = G_define_flag();
    hide->key = 'h';
    hide->description = "Hide";

	get = G_define_flag();
    get->key = 'g';
    get->description = "Get current layer";

	undo = G_define_flag();
    undo->key = 'u';
    undo->description = "Undo last drawing operation";

	x = G_define_flag();
    x->key = 'x';
    x->description = "Erase layer";

	layer = G_define_option();
	layer->key = "layer";
	layer->type = TYPE_INTEGER;
	layer->required = NO;
/*  	cmd->answer = "undo";	 *//* set default color! */
	layer->description = "Command to the Graphics driver";

    dev              = G_define_option();
    dev->key         = "device";
    dev->type        = TYPE_STRING;
    dev->required    = NO;
	/*dev->gisprompt   = "old,cell,raster" ;*/
    dev->description = "Graphice device";

   if (argc > 1 && G_parser(argc, argv))
        exit(-1);

	R_open_driver2(dev->answer);
	
	if (layer->answer) {
		if (sscanf(layer->answer,"%d",&l)!=1)
			l = -1;
	} else
		l = -1;

	if (solo->answer) {
/*  		if (l<0) goto BYE; */
		_send_ident(LAYER_SOLO);
		_send_char(&l);
	} else if (all->answer) {
		_send_ident(LAYER_ALL);
	} else if (all_visible->answer) {
		_send_ident(LAYER_ALL_VISIBLES);
	} else if (select->answer) {
/*  		if (l<0) goto BYE; */
		_send_ident(LAYER_SELECT);
		_send_char(&l);
	} else if (show->answer) {
/*  		if (l<0) goto BYE; */
		_send_ident(LAYER_SHOW);
		_send_char(&l);
	} else if (hide->answer) {
/*  		if (l<0) goto BYE; */
		_send_ident(LAYER_HIDE);
		_send_char(&l);
	} else if (get->answer) {
		char cl=-1;
		char nl=0;
		_send_ident(LAYER_GET_CURRENT);
		_get_char(&cl);
		_get_char(&nl);
		if (cl >= 0)
			printf("Currently selected layer: %d (Layers: 0 -> %d).\n",cl,nl-1);
		else
			printf("No layers there!\n");
	} else if (undo->answer) {
			_send_ident(UNDO_DRAWING);
	} else if (x->answer) {
			if (l<0) goto BYE;
		_send_ident(LAYER_ERASE);
		_send_char(&l);
	}
BYE:
	R_close_driver();

	exit(0);
}

