 
#include <limits.h>
#include <string.h>
#include <unistd.h>

#include "btree.h"
#include "rowio.h"

#include "mapcalc.h"

/****************************************************************************/

typedef struct map
{
	const char *name;
	const char *mapset;
	int have_cats;
	int have_colors;
	int use_rowio;
	int min_row, max_row;
	int fd;
	struct Categories cats;
	struct Colors colors;
	BTREE btree;
	ROWIO rowio;
} map;

/****************************************************************************/

static map *maps;
static int num_maps;
static int max_maps;

static unsigned char *red, *grn, *blu;
static unsigned char *set;

static int min_row = INT_MAX;
static int max_row = -INT_MAX;
static int min_col = INT_MAX;
static int max_col = -INT_MAX;

static int max_rows_in_memory = 3;

static int read_row_type;

/****************************************************************************/

static int compare_ints(int *a, int *b)
{
	return *a - *b;
}

static int init_colors(map *m)
{
	if (!red) red = G_malloc(columns);
	if (!grn) grn = G_malloc(columns);
	if (!blu) blu = G_malloc(columns);
	if (!set) set = G_malloc(columns);

	if (G_read_colors((char *) m->name, (char *) m->mapset, &m->colors) < 0)
	{
		fprintf(stderr, "Error reading color file for [%s in %s]\n",
			m->name, m->mapset);
		return -1;
	}

	m->have_colors = 1;

	return 0;
}

static int init_cats(map *m)
{
	if (G_read_cats((char *) m->name, (char *) m->mapset, &m->cats) < 0)
	{
		fprintf(stderr, "Error reading category file for [%s in %s]\n",
			m->name, m->mapset);
		return -1;
	}

	if (!btree_create(&m->btree, compare_ints, 1))
	{
		fprintf(stderr, "Unable to create btree for [%s in %s]\n",
			m->name, m->mapset);
		return -1;
	}

	m->have_cats = 1;

	return 0;
}

int translate_from_colors(map *m, DCELL *rast, CELL *cell, int ncols, int mod)
{
	int i;

	G_lookup_d_raster_colors(rast, red, grn, blu, set, ncols, &m->colors);

	switch (mod)
	{
	case '#': /* grey */
		/* old weightings: R=0.177, G=0.813, B=0.011 */
		/* NTSC weightings: R=0.299, G=0.587, B=0.114 */
		for (i = 0; i < ncols; i++)
			cell[i] = (306*red[i] + 601*grn[i] + 117*blu[i] + 512) / 1024;
		break;
	case 'r':
		for (i = 0; i < ncols; i++)
			cell[i] = red[i];
		break;
	case 'g':
		for (i = 0; i < ncols; i++)
			cell[i] = grn[i];
		break;
	case 'b':
		for (i = 0; i < ncols; i++)
			cell[i] = blu[i];
		break;
	case 'M':
	case '@':
	default:
		fprintf(stderr, "Invalid map modifier: '%c'\n", mod);
		exit(1);
	}

	return 0;
}

/* convert cell values to double based on the values in the
 * category file.
 *
 * This requires performing sscanf() of the category label
 * and only do it it for new categories. Must maintain
 * some kind of maps of already scaned values.
 *
 * This maps is a hybrid tree, where the data in each node
 * of the tree is an array of, for example, 64 values, and
 * the key of the tree is the category represented by the 
 * first index of the data
 *
 * To speed things up a little, use shifts instead of divide or multiply
 * to compute the key and the index
 *
 * This uses the BTREE library to manage the tree itself
 * btree structure must already be intialized
 * pcats structure must already contain category labels
 */

#define SHIFT 6
#define NCATS (1<<SHIFT)

int translate_from_cats(map *m, CELL *cell, DCELL *xcell, int ncols)
{
	struct Categories *pcats;
	BTREE *btree;
	int i, idx;
	CELL cat, key;
	double vbuf[1<<SHIFT];
	double *values;
	char *label;

	btree = &m->btree;
	pcats = &m->cats;

	for (; ncols-- > 0; cell++, xcell++)
	{
		cat = *cell;
		if (IS_NULL_C(cell))
		{
			SET_NULL_D(xcell);
			continue;
		}

/* compute key as cat/NCATS * NCATS, adjusting down for negatives
 * and idx so that key+idx == cat
 */
		if (cat < 0)
			key = - (((-cat-1) >> SHIFT) << SHIFT) - NCATS;
		else
			key = (cat >> SHIFT) << SHIFT;
		idx = cat - key;

/* If key not already in the tree, sscanf() all cats for this key
 * and put them into the tree
 */
		if (!btree_find(btree, (char *)&key, (char **)&values))
		{
			values = vbuf;
			for (i = 0; i < NCATS; i++)
			{
				if ((label = G_get_cat((CELL)(i+key), pcats)) == NULL
				    || sscanf(label, "%lf", values) != 1)
					SET_NULL_D(values);
				values++;
			}
	    
			values = vbuf;
			btree_update(btree, (char *)&key, sizeof(key),
				      (char *)values, sizeof(vbuf));
		}

/* and finally lookup the translated value */
		if (IS_NULL_D(&values[idx]))
			SET_NULL_D(xcell);
		else
			*xcell = values[idx];
	}

	return 0;
}

static int column_shift(void *buf, int res_type, int col)
{
	CELL *ibuf;
	FCELL *fbuf;
	DCELL *dbuf;
	int i;

	/* if column offset, copy cell to itself shifting by col */
	if (col > 0)
	{
		switch (res_type)
		{
		case CELL_TYPE:
			for (i = 0; i < columns - col; i++)
			{
				if (IS_NULL_C(&ibuf[i + col]))
					SET_NULL_C(&ibuf[i]);
				else
					ibuf[i] = ibuf[i + col];
			}
			for ( ; i < columns; i++)
				SET_NULL_C(&ibuf[i]);

		case FCELL_TYPE:
			for (i = 0; i < columns - col; i++)
			{
				if (IS_NULL_F(&fbuf[i + col]))
					SET_NULL_F(&fbuf[i]);
				else
					fbuf[i] = fbuf[i + col];
			}
			for ( ; i < columns; i++)
				SET_NULL_F(&fbuf[i]);
			break;

		case DCELL_TYPE:
			for (i = 0; i < columns - col; i++)
			{
				if (IS_NULL_D(&dbuf[i + col]))
					SET_NULL_D(&dbuf[i]);
				else
					dbuf[i] = dbuf[i + col];
			}
			for ( ; i < columns; i++)
				SET_NULL_D(&dbuf[i]);
			break;
		}
	}
	else if (col < 0)
	{
		col = -col;
		switch (res_type)
		{
		case CELL_TYPE:
			for (i = columns - 1; i >= col; i--)
			{
				if (IS_NULL_C(&ibuf[i - col]))
					SET_NULL_C(&ibuf[i]);
				else
					ibuf[i] = ibuf[i - col];
			}
			for ( ; i >= 0; i--)
				SET_NULL_C(&ibuf[i]);
			break;

		case FCELL_TYPE:
			for (i = columns - 1; i >= col; i--)
			{
				if (IS_NULL_F(&fbuf[i - col]))
					SET_NULL_F(&fbuf[i]);
				else
					fbuf[i] = fbuf[i - col];
			}
			for ( ; i >= 0; i--)
				SET_NULL_F(&fbuf[i]);
			break;

		case DCELL_TYPE:
			for (i = columns - 1; i >= col; i--)
			{
				if (IS_NULL_D(&dbuf[i - col]))
					SET_NULL_D(&dbuf[i]);
				else
					dbuf[i] = dbuf[i - col];
			}
			for ( ; i >= 0; i--)
				SET_NULL_D(&dbuf[i]);
			break;
		}
	}

	return 0;
}

static void set_read_row_type(int res_type)
{
	read_row_type = res_type;
}

static int read_row(int fd, char *buf, int row, int dummy)
{
	return G_get_raster_row(fd, (DCELL *) buf, row, read_row_type) >= 0;
}

static int setup_map(map *m)
{
	int nrows = m->max_row - m->min_row + 1;
	int size = (sizeof(CELL) > sizeof(double))
		? sizeof(CELL)
		: sizeof(double);

	if (nrows > 1 && nrows <= max_rows_in_memory)
	{
		if (rowio_setup(&m->rowio, m->fd, nrows,
				columns * size, read_row, NULL) < 0)
			exit(1); /* out of memory - diagnostic printed by rowio */
		m->use_rowio = 1;
	}
	else
		m->use_rowio = 0;

	return 0;
}

static int read_map(map *m, void *buf, int res_type, int row, int col)
{
	CELL *ibuf = buf;
	FCELL *fbuf = buf;
	DCELL *dbuf = buf;
	void *bp;

	if (row < 0 || row >= rows)
	{
		int i;

		switch (res_type)
		{
		case CELL_TYPE:
			for (i = 0; i < columns; i++)
				SET_NULL_C(&ibuf[i]);
			break;
		case FCELL_TYPE:
			for (i = 0; i < columns; i++)
				SET_NULL_F(&fbuf[i]);
			break;
		case DCELL_TYPE:
			for (i = 0; i < columns; i++)
				SET_NULL_D(&dbuf[i]);
			break;
		default:
			return E_INV_TYPE;
		}

		return 0;
	}

	set_read_row_type(res_type);

	if (m->use_rowio)
	{
		bp = rowio_get(&m->rowio, row);
		if (!bp)
			return -1;

		G_copy(buf, bp, columns * G_raster_size(res_type));
	}
	else if (!read_row(m->fd, buf, row, 0))
		return 0;

	if (col)
		column_shift(buf, res_type, col);

	return 0;
}

static int close_map(map *m)
{
	if (m->fd < 0)
		return -1;

	G_close_cell (m->fd);

	if (m->have_cats)
	{
		btree_free(&m->btree);
		G_free_cats(&m->cats);
		m->have_cats = 0;
	}

	if (m->have_colors)
	{
		G_free_colors(&m->colors);
		m->have_colors = 0;
	}

	if (m->use_rowio)
	{
		rowio_release(&m->rowio);
		m->use_rowio = 0;
	}

	return 0;
}

/****************************************************************************/

int map_type(const char *name, int mod)
{
	char *mapset;
	int type;

	switch (mod)
	{
	case 'M':
		mapset = G_find_cell2((char *) name, "");
		return G_raster_map_type((char *) name, mapset);
	case '@': return DCELL_TYPE;
	case '#': return CELL_TYPE;
	case 'r': return CELL_TYPE;
	case 'g': return CELL_TYPE;
	case 'b': return CELL_TYPE;
	default:
		fprintf(stderr, "Invalid map modifier: '%c'\n", mod);
		return DCELL_TYPE;
	}
}

int open_map(const char *name, int mod, int row, int col)
{
	int i;
	char *mapset;
	int use_cats = 0;
	int use_colors = 0;
	map *m;

	if (row < min_row) min_row = row;
	if (row > max_row) max_row = row;
	if (col < min_col) min_col = col;
	if (col > max_col) max_col = col;

	mapset = G_find_cell2((char *) name, "");

	if (!mapset)
	{
		fprintf(stderr, "<%s> - not found\n", name);
		return -1;
	}

	switch (mod)
	{
	case 'M':			break;
	case '@': use_cats   = 1;	break;
	case '#': use_colors = 1;	break;
	case 'r': use_colors = 1;	break;
	case 'g': use_colors = 1;	break;
	case 'b': use_colors = 1;	break;
	default:
		fprintf(stderr, "Invalid map modifier: '%c'\n", mod);
		return -1;
	}

	for (i = 0; i < num_maps; i++)
	{
		m = &maps[i];

		if (strcmp(m->name, name) != 0 ||
		    strcmp(m->mapset, mapset) != 0)
			continue;

		if (row < m->min_row) m->min_row = row;
		if (row > m->max_row) m->max_row = row;

		if (use_cats && !m->have_cats)
			if (init_cats(m) < 0)
				return -1;

		if (use_colors && !m->have_colors)
			if (init_colors(m) < 0)
				return -1;

		return i;
	}


	if (num_maps >= max_maps)
	{
		max_maps += 10;
		maps = G_realloc(maps, max_maps * sizeof(map));
	}

	m = &maps[num_maps];

	m->name = name;
	m->mapset = mapset;
	m->have_cats = 0;
	m->have_colors = 0;
	m->use_rowio = 0;
	m->min_row = row;
	m->max_row = row;

	if (use_cats)
		if (init_cats(m) < 0)
			return -1;
	if (use_colors)
		if (init_colors(m) < 0)
			return -1;

	m->fd = G_open_cell_old((char *) name, mapset);

	if(m->fd < 0)
		return -1;

	return num_maps++;
}

int setup_maps(void)
{
	int i;

	for (i = 0; i < num_maps; i++)
		setup_map(&maps[i]);

	return 0;
}

int get_map_row(int idx, int mod, int row, int col, void *buf, int res_type)
{
	static CELL *ibuf;
	static DCELL *fbuf;
	map *m = &maps[idx];

	switch (mod)
	{
	case 'M':
		if (read_map(m, buf, res_type, row, col) < 0)
			return -1;
		break;
	case '@':
		if (!ibuf)
			ibuf = G_malloc(columns * sizeof(CELL));
		if (read_map(m, ibuf, 0, row, col) < 0)
			return -1;
		translate_from_cats(m, ibuf, buf, columns);
		break;
	case '#':
	case 'r':
	case 'g':
	case 'b':
		if (!fbuf)
			fbuf = G_malloc(columns * sizeof(CELL));
		if (read_map(m, fbuf, 0, row, col) < 0)
			return -1;
		translate_from_colors(m, fbuf, buf, columns, mod);
		break;
	default:
		fprintf(stderr, "Invalid map modifier: '%c'\n", mod);
		return -1;
	}

	return 0;
}

int close_maps(void)
{
	int i;

	for (i = 0; i < num_maps; i++)
		close_map(&maps[i]);

	num_maps = 0;

	return 0;
}

/****************************************************************************/

int open_output_map(const char *name, int res_type)
{
	int fd;

	if (res_type == CELL_TYPE)
		fd = G_open_cell_new((char *) name);
	else
		fd = G_open_fp_cell_new((char *) name);

	if (fd < 0)
	{
		fprintf(stderr, "Can't create output file [%s]\n", name);
		return -1;
	}

	return fd;
}

int put_map_row(int fd, void *buf, int res_type)
{
	int stat;

	stat = G_put_raster_row(fd, buf, res_type);

	if (stat < 0)
		return -1;

	return 0;
}

int close_output_map(int fd)
{
	return G_close_cell(fd);
}

/****************************************************************************/

