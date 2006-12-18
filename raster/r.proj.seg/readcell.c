#ifndef lint 
	static char *SCCSid= "@(#)readcell.c	v1.5 - 25 Jun 1995 	-emes-";
#endif
/*
 * readcell.c - reads an entire cell layer into a buffer
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "r.proj.h"

struct cache *readcell(int fdi)
{
	FCELL *tmpbuf;
	struct cache *c;
	int nrows;
	int ncols;
	int row;
	char *filename;
	int nx, ny;

	nrows = G_window_rows();
	ncols = G_window_cols();

	ny = (nrows + BDIM - 1) / BDIM;
	nx = (ncols + BDIM - 1) / BDIM;

	filename = G_tempfile();

	c = G_malloc(sizeof(struct cache));
	c->fd = open(filename, O_RDWR|O_CREAT|O_EXCL, 0600);
	c->stride = nx;
	c->nblocks = (nx + ny) * 2;	/* guess */
	c->grid = (block **) G_calloc(nx * ny, sizeof(block *));
	c->blocks = (block *) G_malloc(c->nblocks * sizeof(block));
	c->refs = (int *) G_malloc(c->nblocks * sizeof(int));

	if (c->fd < 0)
		G_fatal_error("Unable to open temporary file");

	remove(filename);

	G_message(_("Allocating memory and reading input map... "));
	G_percent(0, nrows, 5);

	tmpbuf = (FCELL *) G_malloc(nx * sizeof(block));

	for (row = 0; row < nrows; row += BDIM)
	{
		int x, y;

		for (y = 0; y < BDIM; y++)
		{
			G_percent(row + y, nrows - 1, 5);

			if (row + y >= nrows)
				break;

			if (G_get_f_raster_row(fdi, &tmpbuf[y * nx * BDIM], row + y) < 0)
				G_fatal_error("Error reading input");
		}

		for (x = 0; x < nx; x++)
		for (y = 0; y < BDIM; y++)
			if (write(c->fd, &tmpbuf[(y * nx + x) * BDIM], BDIM * sizeof(FCELL)) < 0)
				G_fatal_error("Error writing segment file");
	}

	G_free(tmpbuf);

	for (row = 0; row < c->nblocks; row++)
		c->refs[row] = -1;

	return c;
}

block *get_block(struct cache *c, int idx)
{
	int replace = rand() % c->nblocks;
	block *p = &c->blocks[replace];
	int ref = c->refs[replace];
	off_t offset = (off_t) idx * sizeof(FCELL) << L2BSIZE;

	if (ref >= 0)
		c->grid[ref] = NULL;

	c->grid[idx] = p;
	c->refs[replace] = idx;

	if (lseek(c->fd, offset, SEEK_SET) < 0)
		G_fatal_error("Error seeking on segment file");

	if (read(c->fd, p, sizeof(block)) < 0)
		G_fatal_error("Error writing segment file");

	return p;
}

