/*  ./src/libes/dlg/dlg_read.c
 ***********************************************************************
 *  #include "dlg.h"
 *
 *  dlg_read (fd,dlg)
 *      FILE *fd ;
 *      struct dlg *dlg;
 *
 * This routine reads through a dlg file accumulating addresses about
 * nodes, areas, and lines.   These are stored in arrays within
 * the 'dlg' structure.
 *
 *   returns:   0  on a successful read
 *             -1  on error
 *
 **********************************************************************/

#include "gis.h"
#include "dlg.h"

#define ALLOC_AMT	512

#define GET(x,y) fread (x,y,1,fd)

dlg_read(fd,dlg)
	FILE *fd ;
	struct dlg *dlg;
{
	long ftell() ;
	int num ;
	char type ;
	char *check_alloc() ;

/* Make initial memory allocations */
	/* node, line, and area offsets */
	dlg->node_alloc = ALLOC_AMT ;
	dlg->node_off = (long *) G_calloc (ALLOC_AMT, sizeof (long));
	dlg->area_alloc = ALLOC_AMT ;
	dlg->area_off = (long *) G_calloc (ALLOC_AMT, sizeof (long));
	dlg->line_alloc = ALLOC_AMT ;
	dlg->line_off = (long *) G_calloc (ALLOC_AMT, sizeof (long));

	/* line and attribute buffers in node, area, and line structures */
	dlg->node.n_lines_alloc = ALLOC_AMT ;
	dlg->node.lines = (int *) G_calloc (ALLOC_AMT, sizeof (int));
	dlg->node.n_atts_alloc = ALLOC_AMT ;
	dlg->node.atts  = (int *) G_calloc (ALLOC_AMT, sizeof (int));
	dlg->area.n_lines_alloc = ALLOC_AMT ;
	dlg->area.lines = (int *) G_calloc (ALLOC_AMT, sizeof (int));
	dlg->area.n_atts_alloc  = ALLOC_AMT ;
	dlg->area.atts  = (int *) G_calloc (ALLOC_AMT, sizeof (int));
	dlg->line.n_coors_alloc = ALLOC_AMT ;
	dlg->line.coors = (double *) G_calloc (ALLOC_AMT, sizeof (double));
	dlg->line.n_atts_alloc  = ALLOC_AMT ;
	dlg->line.atts  = (int *) G_calloc (ALLOC_AMT, sizeof (int));

	dlg->max_nodes = 0 ;
	dlg->max_areas = 0 ;
	dlg->max_lines = 0 ;

#ifdef FOO
/*DEBUG*/ fprintf (stderr, "DLG_READ beginning\n", num);
#endif
	while(GET(&type, sizeof(type)))
	{
		switch(type)
		{
		case 'N':
			GET(&num, sizeof(num));
#ifdef FOO
/*DEBUG*/ fprintf (stderr, "Reading Node  num=%d\n", num);
#endif
			dlg->node_off = (long *)check_alloc(
				num,
				&dlg->node_alloc,
				(char *)dlg->node_off,
				sizeof(long)) ;
			dlg->node_off[num] = ftell(fd) ;
			if (dlg->max_nodes < num)
				dlg->max_nodes = num ;
			_dlg_read_node(&dlg->node, fd) ;
			break ;

		case 'A':
#ifdef FOO
/*DEBUG*/ fprintf (stderr, "Reading Area  num=%d\n", num);
#endif
			GET(&num, sizeof(num));
			dlg->area_off = (long *)check_alloc(
				num,
				&dlg->area_alloc,
				(char *)dlg->area_off,
				sizeof(long)) ;
			dlg->area_off[num] = ftell(fd) ;
			if (dlg->max_areas < num)
				dlg->max_areas = num ;
			_dlg_read_area(&dlg->area, fd) ;
			break ;

		case 'L':
#ifdef FOO
/*DEBUG*/ fprintf (stderr, "Reading Line  num=%d\n", num);
#endif
			GET(&num, sizeof(num));
			dlg->line_off = (long *)check_alloc(
				num,
				&dlg->line_alloc,
				(char *)dlg->line_off,
				sizeof(long)) ;
			dlg->line_off[num] = ftell(fd) ;
			if (dlg->max_lines < num)
				dlg->max_lines = num ;
			_dlg_read_line(&dlg->line, fd) ;
			break ;

		default:
			return(-1) ;
		}
	}
	return(0) ;
}

static
char *
check_alloc(num, n_alloc, arr, size)
	int num ;
	int *n_alloc ;
	char *arr ;
	int size ;
{
	int orig_alloc ;
	char *ptr ;
	char *calloc() ;

	if (num < *n_alloc)
		return (arr) ;

	orig_alloc = *n_alloc ;

	while (num >= *n_alloc)
		*n_alloc += ALLOC_AMT;

/* calloc()  initiliazes the space,  this is very important when the 
*  area and line numbers aren't sequential.
*/
	ptr = calloc (*n_alloc, size) ;
	if (ptr == NULL)
	{
		fprintf(stderr,"DLG check_alloc: ran out of memory [requested (%d %d)]\n", *n_alloc, size) ;
		exit(-1) ;
	}

	{
		register char *a ;
		register char *b ;
		register int n ;
		n = orig_alloc * size ;
		a = ptr ;
		b = arr ;
		while(n--)
			*a++ = *b++ ;
	}

	free(arr) ;
	return(ptr) ;
}

_dlg_read_node(node, fd)
	struct dlg_node *node ;
	FILE *fd ;
{
	char *check_alloc() ;

	GET(&node->x,      sizeof(node->x));
	GET(&node->y,      sizeof(node->y)) ;
	GET(&node->n_lines,sizeof(node->n_lines)) ;
	GET(&node->n_atts, sizeof(node->n_atts)) ;

	if (node->n_lines)
	{
		node->lines = (int *)check_alloc(
			node->n_lines,
			&node->n_lines_alloc,
			(char *)node->lines,
			sizeof(int)) ;
		fread ((char *)node->lines, sizeof(int), node->n_lines, fd) ;
	}
	if (node->n_atts)
	{
		node->atts = (int *)check_alloc(
			2 * node->n_atts,
			&node->n_atts_alloc,
			(char *)node->atts,
			sizeof(int)) ;
		fread ((char *)node->atts, sizeof(int), 2 * node->n_atts, fd) ;
	}
}

_dlg_read_area(area, fd)
	struct dlg_area *area ;
	FILE *fd ;
{
	GET(&area->x,      sizeof(area->x));
	GET(&area->y,      sizeof(area->y));
	GET(&area->n_lines,sizeof(area->n_lines));
	GET(&area->n_atts, sizeof(area->n_atts));
	GET(&area->n_isles,sizeof(area->n_isles));

	if (area->n_lines)
	{
		area->lines = (int *)check_alloc(
			area->n_lines,
			&area->n_lines_alloc,
			(char *)area->lines,
			sizeof(int)) ;
		fread ((char *)area->lines, sizeof(int), area->n_lines, fd) ;
	}
	if (area->n_atts)
	{
		area->atts = (int *)check_alloc(
			2 * area->n_atts,
			&area->n_atts_alloc,
			(char *)area->atts,
			sizeof(int)) ;
		fread ((char *)area->atts, sizeof(int), 2 * area->n_atts, fd) ;
	}
}

_dlg_read_line(line, fd)
	struct dlg_line *line ;
	FILE *fd ;
{
	GET (&line->start_node,sizeof(line->start_node));
	GET (&line->end_node,  sizeof(line->end_node));
	GET (&line->left_area, sizeof(line->left_area));
	GET (&line->right_area,sizeof(line->right_area));
	GET (&line->n_coors,   sizeof(line->n_coors));
	GET (&line->n_atts,    sizeof(line->n_atts));
	GET (&line->N,         sizeof(line->N));
	GET (&line->S,         sizeof(line->S));
	GET (&line->E,         sizeof(line->E));
	GET (&line->W,         sizeof(line->W));

	if (line->n_coors)
	{
		line->coors = (double *)check_alloc(
			2 * line->n_coors,
			&line->n_coors_alloc,
			(char *)line->coors,
			sizeof(double)) ;
		fread ((char *)line->coors, sizeof(double), 2 * line->n_coors, fd) ;
	}
	if (line->n_atts)
	{
		line->atts = (int *)check_alloc(
			2 * line->n_atts,
			&line->n_atts_alloc,
			(char *)line->atts,
			sizeof(int)) ;
		fread ((char *)line->atts, sizeof(int), 2 * line->n_atts, fd) ;
	}
}
