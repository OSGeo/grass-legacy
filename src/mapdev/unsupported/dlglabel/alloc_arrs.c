/*  @(#)alloc_arrs.c	2.1  6/26/87  */
#include "dlg.h"

static int coors_allocated = 0 ;
static int nodes_allocated = 0 ;
static int areas_allocated = 0 ;
static int lines_allocated = 0 ;

#define CHUNK	512

char *falloc() ;
char *frealloc() ;

alloc_coors(num)
	int num ;
{
	int to_alloc ;

	if (num < coors_allocated)
		return ;
	
	to_alloc = coors_allocated ;
	while (num >= to_alloc)
		to_alloc += CHUNK ;

	if (coors_allocated == 0)
		coors = (double *) falloc(2 * to_alloc, sizeof(double)) ;
	else
		coors = (double *)frealloc(
			(char *)coors,
			2 * to_alloc,
			sizeof(double),
			2 * coors_allocated) ;
	
	coors_allocated = to_alloc ;
}

alloc_nodes(num)
	int num ;
{
	int to_alloc ;

	if (num < nodes_allocated)
		return ;
	
	to_alloc = nodes_allocated ;
	while (num >= to_alloc)
		to_alloc += CHUNK ;

	if (nodes_allocated == 0)
		node = (struct node *) falloc(to_alloc, sizeof(struct node)) ;
	else
		node = (struct node *)frealloc(
			(char *)node,
			to_alloc,
			sizeof(struct node),
			nodes_allocated) ;
	
	nodes_allocated = to_alloc ;
}

alloc_lines(num)
	int num ;
{
	int to_alloc ;

	if (num < lines_allocated)
		return ;
	
	to_alloc = lines_allocated ;
	while (num >= to_alloc)
		to_alloc += CHUNK ;

	if (lines_allocated == 0)
		line = (struct line *) falloc(to_alloc, sizeof(struct line)) ;
	else
	{
		line = (struct line *)frealloc(
			line,
			to_alloc,
			sizeof(struct line),
			lines_allocated) ;

	}
	lines_allocated = to_alloc ;
}

alloc_areas(num)
	int num ;
{
	int to_alloc ;

	if (num < areas_allocated)
		return ;
	
	to_alloc = areas_allocated ;
	while (num >= to_alloc)
		to_alloc += CHUNK ;

	if (areas_allocated == 0)
		area = (struct area *) falloc(to_alloc, sizeof(struct area)) ;
	else
		area = (struct area *)frealloc(
			(char *)area,
			to_alloc,
			sizeof(struct area),
			areas_allocated) ;
	
	areas_allocated = to_alloc ;
}
