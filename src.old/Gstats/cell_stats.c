#include "global.h"

cell_stats (fd, verbose, non_zero, cell_counts, fmt)
    int fd[];
    char *fmt;
{
    CELL *cell[NFILES];
    CELL cat[NFILES], cat0, cat1;
    register int i;
    int row, col;
    register int p,q;
    int dir;
    CELL index_cat();
    NODE *pnode, *new_node;
    double unit_area;
    int planimetric;
    int use_areas;
    int count;
    double G_area_of_cell_at_row();

/* allocate i/o buffers for each cell file */
    for (i = 0; i < nfiles; i++)
	cell[i] = G_allocate_cell_buf();

/* if we want area totals, set this up.
 * distinguish projections which are planimetric (all cells same size)
 * from those which are not (e.g., lat-long)
 */
    if (cell_counts != 1)
    {
	switch (G_begin_cell_area_calculations())
	{
	case 0: /* areas don't make sense, but ignore this for now */
	case 1:
	    planimetric = 1;
	    unit_area = G_area_of_cell_at_row (0);
	    break;
	default:
	    planimetric = 0;
	    break;
	}
    }
    use_areas = (cell_counts != 1) && !planimetric;

/* here we go */
    plant_tree ();
    if (verbose)
	fprintf (stderr, "%s:  complete ... ", G_program_name());

    for (row = 0; row < nrows; row++)
    {
	if (use_areas)
	    unit_area = G_area_of_cell_at_row (row);

	if (verbose)
	    percent (row, nrows, 5);

/* read the primary file first, even if not first in the list */
	if (get_row (fd[primary], cell[0], row) < 0)
	    exit(1);

/* read the others */
	col = 1;
	for (i = 0; i < nfiles; i++)
	    if (i != primary && get_row (fd[i], cell[col++], row) < 0)
		exit(1);

/* read the mask file */
	if (maskfd >= 0)
	    G_get_map_row_nomask (maskfd, mask, row);

	col = 0;
	while (col < ncols)
	{
	    if (!mask[col])
	    {
		col++;
		continue;
	    }

	    for (i = 0; i < nfiles; i++)
		cat[i] = cell[i][col];
	    
	    count = 1;
	    while (++col < ncols)
	    {
		if (!mask[col]) continue;
		for (i = 0; i < nfiles; i++)
		    if(cat[i] != cell[i][col])
			break;
		if (i < nfiles) break;
		count++;
	    }

/* search for this value in the tree */
	    cat0 = cat[0];
	    cat1 = cat0 - NCATS;
	    q = 1;
	    while (q > 0)
	    {
		register CELL *t, *c;
		register CELL diff;
		pnode = &tree[p = q];
		t = pnode->cat;

/* search the tree */
		dir = FOUND ;
		if (*t > cat0)
		    dir =  LEFT;
		else if (*t++ <= cat1)
		    dir =  RIGHT;
		else
		{
		    c = cat+1;
		    for (i = 1; i < nfiles; i++)
			if ((diff = (*t++ - *c++)) > 0)
			{
			    dir = LEFT;
			    break;
			}
			else if (diff < 0)
			{
			    dir = RIGHT;
			    break;
			}
		}
		switch (dir)
		{
		case FOUND:
		    pnode->stats[cat0 - pnode->cat[0]].count += count;
		    if (use_areas)
			pnode->stats[cat0 - pnode->cat[0]].area += unit_area*count;
		    q = 0;
		    break;
		case LEFT:
		    q = pnode->left;
		    break;
		case RIGHT:
		    q = pnode->right;
		    break;
		}
	    }
	    if (dir == FOUND) continue;

/* add a new node to the tree and put this value into it */
	    N++;
/* grow the tree? */
	    if (N >= tlen)
	    {
		tree = (NODE *) G_realloc (tree, sizeof(NODE) * (tlen += INCR));
		pnode = &tree[p];  /* realloc moves tree, must reassign pnode */
	    }

	    new_node = &tree[N];
	    new_node -> cat   = (CELL *) G_calloc (nfiles, sizeof(CELL));
	    new_node -> stats = (STATS *) G_calloc (NCATS, sizeof (STATS));

	    if (cat0 < 0)
		cat0 = -((-cat0) >> SHIFT) - 1;
	    else
		cat0 = cat0 >> SHIFT;

	    if (cat0 < 0)
		cat0 = -((-cat0) << SHIFT) + 1;
	    else
		cat0 = cat0 << SHIFT ;

	    new_node->cat[0] = cat0;

	    for (i = 1; i < nfiles; i++)
		new_node->cat[i] = cat[i];

	    for (i = 0; i < NCATS; i++)
		new_node->stats[i].count = 0;
	    new_node->stats[cat[0]-cat0].count = count;
	    if (use_areas)
	    {
		for (i = 0; i < NCATS; i++)
		    new_node->stats[i].area = 0.0;
		new_node->stats[cat[0]-cat0].area = unit_area * count;
	    }

	    new_node->left = 0;

	    if (dir == LEFT)
	    {
		new_node->right = -p;            /* create thread */
		pnode->left  = N;             /* insert left */
	    }
	    else
	    {
		new_node->right = pnode->right; /* copy right link/thread */
		pnode->right = N;             /* add right */
	    }
	}
    }
    if (verbose)
	percent (nrows, nrows, 5);

    results (fmt, non_zero, cell_counts, planimetric, unit_area);
}
