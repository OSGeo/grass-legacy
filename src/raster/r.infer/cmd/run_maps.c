#include "infer.h"

static struct Categories cats;
static int do_cats = 0;

int 
initialize_map_categories (void)
{
    do_cats = 1;
    G_init_cats ((CELL) 0, "infer", &cats);
    record_category_label (0, "no data");

    return 0;
}

int 
run_through_maps (void)
{
    struct Cell_head window ;
    CELL *new_row_buf ;
    CELL *buf_ptr ;
    int newfd ;
    int i ;
    int j ;

    setbuf(stderr,0) ;

    open_maps() ;

    if( (newfd = G_open_cell_new("infer")) == -1)
	G_fatal_error("Opening cell file to write");


    new_row_buf = G_allocate_cell_buf() ;

    G_get_window(&window) ;

#ifndef DEBUG
    fprintf(stderr,"Working to row %d\n", window.rows) ;
    fprintf(stderr,"At row: %4d ", 0) ;
#endif

    for(i = 0; i < window.rows; i++)
    {
#ifndef DEBUG
	fprintf(stderr,"\b\b\b\b\b%4d ", i+1) ;
#endif

	read_maps(i) ;

	buf_ptr = new_row_buf ;

	for(j = 0; j < window.cols; j++)
	{
		clear_truths() ;
		*buf_ptr++ = infer() ;
		incr_cols() ;
	}
#ifdef DEBUG
{
char buf[100];
MAPS *map;
fprintf (stdout,"\n");
for(j = 0; j < window.cols && j < 10; j++)
{
    for(map = MAP; map; map = map->next)
	fprintf (stdout,"%d:", map->rbuf[j]);
    fprintf (stdout,"%d:\n", new_row_buf[j]);
}
fprintf (stdout,"-->"); if (!gets(buf)) exit(0);
}
#endif
	G_put_raster_row(newfd, new_row_buf, CELL_TYPE);
    }
    close_maps() ;
    fprintf (stdout,"\nCreating support files\n");
    G_close_cell(newfd) ;
    G_write_cats ("infer", &cats);
    G_free_cats(&cats);
    do_cats = 0;

    return 0;
}

int 
test_infer (void)
{
    int val ;

    tell_open_maps() ;
    val = infer() ;
    fprintf(stderr,"New cell value = %d\n", val) ;

    return 0;
}

int 
open_maps (void)
{
    MAPS *map ;
    char *mapset ;
    char buff[128] ;

/* Open maps */
    for(map = MAP; map; map = map->next)
    {
	mapset = G_find_file("cell", map->p, "") ;
	if (! mapset)
	{
	    sprintf(buff,"Map [%s] not found to open", map->p) ;
	    G_fatal_error(buff) ;
	}
	if( (map->fd = G_open_cell_old(map->p,mapset )) == -1)
	{
	    sprintf(buff,"Map [%s] in mapset [%s] not available",
		    map->p, mapset) ;
	    G_fatal_error(buff) ;
	}
	fprintf(stderr,"Map [%s] in mapset [%s] opened\n", map->p, mapset) ;
	map->rbuf = G_allocate_cell_buf() ;
    }

    return 0;
}

int 
tell_open_maps (void)
{
    MAPS *map ;
    char *mapset ;

/* Open maps */
    fprintf(stderr,"\nMaps used:\n") ;
    for(map = MAP; map; map = map->next)
    {
	mapset = G_find_file("cell", map->p, "") ;
	if (! mapset)
	    fprintf(stderr,"Map [%s] not found\n", map->p) ;
	else
	    fprintf(stderr,"Map [%s] in mapset [%s]\n", map->p, mapset) ;
    }

    return 0;
}

int 
close_maps (void)
{
    MAPS *map ;

/* Close maps */
    for(map = MAP; map; map = map->next)
	G_close_cell (map->fd) ;

    return 0;
}

int 
read_maps (int row)
{
    MAPS *map ;

    for(map = MAP; map; map = map->next)
    {
	G_get_map_row(map->fd,map->rbuf,row);
	map->cptr = map->rbuf ;
    }

    return 0;
}

int 
incr_cols (void)
{
    MAPS *map ;

    for(map = MAP; map; map = map->next)
	map->cptr++ ;

    return 0;
}

int 
record_category_label (int n, char *label)
{
    if (do_cats && *label)
	G_set_cat ((CELL)n, label, &cats);

    return 0;
}
