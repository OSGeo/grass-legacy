#include "segment.h"

#define TST(x)  /*	fprintf(stderr,"%s\n",(x));  */
#define CSEG	struct _cseg_
CSEG
{
	SEGMENT	*seg;
	int	fd;
	CELL	*buffer;
	char	put;
	char	*filename;
};

cseg_open(cseg, dim_row, dim_col, open_files)
CSEG	*cseg;
int	dim_row, dim_col, open_files;
{
	char	*filename;

	cseg->seg = (SEGMENT *)G_malloc(sizeof(SEGMENT));
	TST("cseg_open");
	cseg->buffer = NULL;
	TST("post cseg-buffer");
	cseg->put = 0;
	TST("pre Gtemp");
	filename = G_tempfile();
	TST("post G_temp");
	cseg->fd = creat(filename, 0666);
	TST("pre seg for");
	segment_format(cseg->fd,
			G_window_rows(),
			G_window_cols(),
			dim_row,
			dim_col,
			sizeof(CELL));
	TST("pre close");
	close(cseg->fd);
	cseg->fd = open(filename, 2);
	TST("pre seg init");
	segment_init(cseg->seg, cseg->fd, open_files);
	TST("end cseg open");
	cseg->filename = filename;
}

cseg_read_map(cseg, map_name)
CSEG	*cseg;
char	*map_name;
{
	char	*mapset;
	int	row, nrows;
	int	map_fd;
	char	buf[100];
	
	mapset = G_find_file2("cell", map_name, "");
	if(!mapset)
	{
		sprintf(buf, "file [%s] not found\n", map_name);
		G_fatal_error(buf);
		exit(1);
	}
	if((map_fd = G_open_cell_old(map_name, mapset)) < 0)
	{
		sprintf(buf, "unable to open file [%s]\n", map_name);
		G_fatal_error(buf);
		exit(1);
	}
	if(cseg->buffer == NULL)
	{
		cseg->buffer = G_allocate_cell_buf();
	}
	nrows = G_window_rows();
	for (row=0; row < nrows; row++)
	{
		if(G_get_map_row(map_fd, cseg->buffer, row) < 0)
		{
			sprintf(buf, "unable to read file [%s]\n", map_name);
			G_fatal_error(buf);
			exit(1);
		}
		segment_put_row(cseg->seg, cseg->buffer, row);
	}
	G_close_cell(map_fd);
}

cseg_allocate_row_buf(cseg)
CSEG	*cseg;
{
	cseg->buffer = G_allocate_cell_buf();
}

cseg_value_to_row(cseg, col, value)
CSEG	*cseg;
int	col;
CELL	value;
{
	if(cseg->buffer == NULL)
	{
		cseg->buffer = G_allocate_cell_buf();
	}
	cseg->buffer[col] = value;
}

cseg_row_to_seg(cseg, row)
CSEG	*cseg;
int	row;
{
	segment_put_row(cseg->seg, cseg->buffer, row);
}

cseg_free_buffer(cseg)
CSEG	*cseg;
{
	free(cseg->buffer);
}

CELL
cseg_value_get(cseg, row, col)
CSEG	*cseg;
int	row, col;
{
	CELL	value;

	segment_get(cseg->seg, &value, row, col);
	return(value);
}

cseg_value_put(cseg, value, row, col)
CSEG	*cseg;
CELL	value;
int	row, col;
{
	int check;

	cseg->put = 1;
	check = segment_put(cseg->seg, &value, row, col);
	if(check == -1)
		TST("segment put fails");
}

cseg_row_to_map(cseg, map_fd, row)
CSEG	*cseg;
int	map_fd;
int	row;
{
	if(cseg->put)
	{
		segment_flush(cseg->seg);
		cseg->put = 0;
	}
	segment_get_row(cseg->seg, cseg->buffer, row);
	G_put_map_row(map_fd, cseg->buffer, row);
}

cseg_to_map(cseg, mapname)
CSEG	*cseg;
char	*mapname;
{
	int	map_fd;
	int	row, nrows;

	TST("cseg to map");
	map_fd = G_open_cell_new(mapname);
	if (map_fd == -1)
	{
		fprintf(stderr,"unable to open new map layer [%s]\n",
			mapname);
	}
	else
	{
		if(cseg->put)
		{
			TST("cseg-put=1");
			segment_flush(cseg->seg);
			cseg->put = 0;
		}
		if(cseg->buffer == NULL)
		{
			cseg->buffer = G_allocate_cell_buf();
		}
		nrows = G_window_rows();
		TST("rowby row");
		for(row=0; row<nrows; row++)
		{
			segment_get_row(cseg->seg, cseg->buffer, row);
			G_put_map_row(map_fd, cseg->buffer, row);
		}
		G_close_cell(map_fd);
	}
}

cseg_release(cseg)
CSEG	*cseg;
{
	segment_release(cseg->seg);
	close(cseg->fd);
	unlink(cseg->filename);
	if(cseg->buffer != NULL)
	{
		free(cseg->buffer);
	}
	free(cseg);
}
