/* Line thinning program */
/*   Input/output and file support functions */

/* Mike Baba */
/* DBA Systems */
/* Fairfax, Va */
/* Jan 1990 */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Laboratory */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* January - February 1988 */

/* Entry points: */
/*   get_a_row     get row from temporary work file */
/*   open_file     open input cell file and read it into work file */
/*   close_file    copy work file into new cell file */
/*   map_size      get size of map and its pad */

/* Global variables: */
/*   row_io        place to store pointer to row manager stuff */
/*   n_rows        number of rows in the work file (includes pads) */
/*   n_cols        number of columns in the work file (includes pads) */

#include <math.h>
#include "gis.h"
#include "rowio.h"

#define PAD 2
#define MAX_ROW 7

extern int errno;
extern char *error_prefix;
static int n_rows, n_cols;
static int work_file;
static char *work_file_name;
static ROWIO row_io;

CELL *get_a_row(row)
int row;
{
	char *rowio_get();

	if (row < 0 || row >= n_rows)
		return(NULL);
	return((CELL *) rowio_get(&row_io,row));
}

put_a_row(row,buf)
int row;
CELL *buf;
{
	rowio_put(&row_io,buf,row);
}

read_row(file,buf,row,buf_len)
int file, row, buf_len;
char *buf;
{
	lseek(file,(long) row * buf_len,0);
	return(read(file,buf,buf_len) == buf_len);
}

write_row(file,buf,row,buf_len)
int file, row, buf_len;
char *buf;
{
	lseek(file,(long) row * buf_len,0);
	return(write(file,buf,buf_len) == buf_len);
}

open_file(name)
char *name;
{
	char *mapset, *G_find_cell();
	int cell_file,  read_row(), write_row(), buf_len;
	int i, row, col;
	char cell[100];
	CELL *buf;

	/* open cell file */
	strcpy (cell, name);
	if ((mapset = G_find_cell2(cell,"")) == NULL)
	{
		fprintf(stderr,"%s:  open_file:  cell file %s not found\n",error_prefix,name);
	        unlink(work_file_name);
		exit(-1);
	}
	if ((cell_file = G_open_cell_old(cell,mapset)) < 0)
	{
		fprintf(stderr,"%s:  open_file:  could not open cell file %s in %s\n",error_prefix,cell,mapset);
	        unlink(work_file_name);
		exit(-1);
	}
	n_rows = G_window_rows();
	n_cols = G_window_cols();
	fprintf(stdout,"File %s -- %d rows X %d columns\n",name,n_rows,n_cols);
	n_cols += (PAD << 1);
	/* copy cell file into our read/write file */
	work_file_name = G_tempfile();

	/* create the file and then open it for read and write */
	close(creat(work_file_name,0666));
	if ((work_file = open(work_file_name,2)) < 0)
	{
		fprintf(stderr,"%s:  open_file:  could not create temporary file %s\n",error_prefix,work_file_name);
		fprintf(stderr,"errno = %d\n",errno);
	        unlink(work_file_name);
		exit(-1);
	}
	buf = (CELL *) G_malloc(buf_len = n_cols * sizeof(CELL));
	for (col = 0; col < n_cols; col++)
		buf[col] = 0;
	for (i = 0; i < PAD; i++)
	{
		if (write(work_file,buf,buf_len) != buf_len)
		{
			fprintf(stderr,"%s:  open_file:  error writing temporary file\n",error_prefix);
	                unlink(work_file_name);
			exit(-1);
		}
	}
	for (row = 0; row < n_rows; row++)
	{
		if (G_get_map_row(cell_file,buf + PAD,row) < 0)
		{
			fprintf(stderr,"%s:  open_file:  error reading from %s in %s\n",error_prefix,cell,mapset);
	                unlink(work_file_name);
			exit(-1);
		}
		if (write(work_file,buf,buf_len) != buf_len)
		{
			fprintf(stderr,"%s:  open_file:  error writing temporary file\n",error_prefix);
	                unlink(work_file_name);
			exit(-1);
		}
	}
	for (col = 0; col < n_cols; col++)
		buf[col] = 0;
	for (i = 0; i < PAD; i++)
	{
		if (write(work_file,buf,buf_len) != buf_len)
		{
			fprintf(stderr,"%s:  open_file:  error writing temporary file\n",error_prefix);
	                unlink(work_file_name);
			exit(-1);
		}
	}
	n_rows += (PAD << 1);
	free(buf);
	G_close_cell(cell_file);
	rowio_setup(&row_io,work_file,MAX_ROW,n_cols*sizeof(CELL),read_row,write_row);
}

close_file(name)
char *name;
{
	int cell_file, row, k;
	int row_count, col_count;
	CELL *get_a_row(), *buf;

	if ((cell_file = G_open_cell_new(name)) < 0)
	{
		fprintf(stderr,"%s:  close_cell:  could not open output file %s\n",error_prefix,name);
	        unlink(work_file_name);
		exit(-1);
	}
	row_count = n_rows - (PAD << 1);
	col_count = n_cols - (PAD << 1);
	fprintf(stdout,"Output file %d rows X %d columns\n",row_count,col_count);
	fprintf(stdout,"Window %d rows X %d columns\n",G_window_rows(),G_window_cols());
	for (row = 0, k = PAD; row < row_count; row++, k++)
	{
		buf = get_a_row(k);
		G_put_map_row(cell_file,buf + PAD,row);
	}
	G_close_cell(cell_file);
	rowio_flush(&row_io);
	close(rowio_fileno(&row_io));
	rowio_release(&row_io); 
	unlink(work_file_name);
}

map_size(r,c,p)
int *r, *c, *p;
{
	*r = n_rows;
	*c = n_cols;
	*p = PAD;
}
