
/*  Gs.c 
    Bill Brown, USACERL  
    January 1993
*/

#include "gis.h"
#include "bitmap.h"

typedef int FILEDESC;

#define TRACE_FUNCS

#define NO_DATA_COL 0xffffff


/************************************************************************/

/* This should be a function variable that 
 * may be replaced by a user's function. 
 * Or else use G_set_error_routine.
*/

Gs_warning(str)
char *str;
{

    fprintf(stderr, "%s\n", str);

}

/************************************************************************/

/* This should be a function variable that 
 * may be replaced by a user's function. 
 * Or else use G_set_error_routine.
*/

Gs_status(str)
char *str;
{

    fprintf(stderr, "%s\n", str);

}

/************************************************************************/

/* Calling function must have already allocated space in buff for
   wind->rows * wind->cols integers.
   This routine simply loads the map into a 2d array by repetitve calls
   to get_map_row.
*/

int
Gs_loadmap_as_int(wind, map_name, buff)
struct Cell_head *wind;
char *map_name;
int *buff;
{

FILEDESC cellfile = NULL;
char *map_set;
char err_buff[100];
int offset, row;

int i;  /* TMP */

#ifdef TRACE_FUNCS
Gs_status("Gs_loadmap_as_int");
#endif

    map_set = G_find_file2 ("cell", map_name, "");

    if ((cellfile = G_open_cell_old(map_name, map_set)) == -1) {
        sprintf(err_buff,"Not able to open cellfile for [%s]", map_name);
        Gs_warning(err_buff);
	return(-1);
    }

    Gs_status("Loading Data");

    for (row = 0; row < wind->rows ; row++) {
	offset = row * wind->cols;
	G_get_map_row(cellfile, &(buff[offset]), row);
    }

    G_close_cell(cellfile);

    return(1);

}

/*********************************************************************/
/* returns -1 if G_read_range fails !! */

int
Gs_numbytes(filename, negflag)
char *filename;
int *negflag;
{
CELL max=0, min=0;
struct Range range;
char *mapset;
int shortbits, charbits, bitplace;
static int max_short, max_char; 
static int first=1;


    if(first){
	max_short = max_char = 1;
	shortbits =  8 * sizeof(short);
	for (bitplace = 1; bitplace < shortbits; ++bitplace) /*1 bit for sign*/
	    max_short *= 2;
	max_short -= 1;

	/* NO bits for sign, using unsigned char */
	charbits =  8 * sizeof(unsigned char);
	for (bitplace = 0; bitplace < charbits; ++bitplace)
	    max_char *= 2;
	max_char -= 1;

	first=0;
    }

    mapset = G_find_file2 ("cell", filename, "");
    if(-1 == G_read_range(filename, mapset, &range))
	return(-1);
    G_get_range_min_max(&range, &min, &max);
    *negflag = (min < 0);

    if(max < max_char && min > 0)
	return(sizeof(unsigned char));

    if(max < max_short && min > -max_short)
	return(sizeof(short));

    return(sizeof(int));


}

/************************************************************************/

/* Calling function must have already allocated space in buff for
   wind->rows * wind->cols shorts.  
   This routine simply loads the map into a 2d array by repetitve calls
   to get_map_row.
   Returns 1 on success, -1 on failure, -2 if read ok, but 1 or more values
   were too large (small) to fit into a short.
   (in which case the max (min) short is used)
*/

int
Gs_loadmap_as_short(wind, map_name, buff)
struct Cell_head *wind;
char *map_name;
short *buff;
{

FILEDESC cellfile = NULL;
char *map_set;
char err_buff[100];
int *ti, *tmp_buf;
int offset, row, col, val, max_short, overflow, shortsize, bitplace;
short *ts;

#ifdef TRACE_FUNCS
Gs_status("Gs_loadmap_as_short");
#endif

    overflow = 0;
    shortsize =  8 * sizeof(short);

    /* 1 bit for sign */
    for (max_short = bitplace = 1; bitplace < shortsize; ++bitplace)
	max_short *= 2;
    max_short -= 1;
	
    map_set = G_find_file2 ("cell", map_name, "");

    if ((cellfile = G_open_cell_old(map_name, map_set)) == -1) {
        sprintf(err_buff,"Not able to open cellfile for [%s]", map_name);
        Gs_warning(err_buff);
	return(-1);
    }

    if(NULL == (tmp_buf = (int *)G_malloc (wind->cols * sizeof(int)))) {
	sprintf(err_buff,"out of memory");
        Gs_warning(err_buff);
	return(-1);
    }

    Gs_status("Loading Data");


    for (row = 0; row < wind->rows ; row++) {
	offset = row * wind->cols;
	G_get_map_row(cellfile, tmp_buf, row);
	ts = &(buff[offset]);
	ti = tmp_buf;
	for (col = 0; col < wind->cols; col++) {
	    val = *ti;
	    if (abs(val) > max_short) {
		overflow = 1;
		/* assign floor/ceiling value?
		*/  
		*ts = (short)(max_short * val/abs(val));
	    }
	    else
		*ts = (short)val;
	    ti++;
	    ts++;
	}
    }


    G_close_cell(cellfile);
    free (tmp_buf);
    
    return(overflow ? -2 : 1);

}


/************************************************************************/

/* Calling function must have already allocated space in buff for
   wind->rows * wind->cols unsigned chars.  
   This routine simply loads the map into a 2d array by repetitve calls
   to get_map_row.
   Returns 1 on success, -1 on failure, -2 if read ok, but 1 or more values
   were too large (small) to fit into an unsigned char.
   (in which case the max (min) char is used)
   Since signs of chars can be tricky, we only load positive chars
   between 0-255.
*/

int
Gs_loadmap_as_char(wind, map_name, buff)
struct Cell_head *wind;
char *map_name;
char *buff;
{

FILEDESC cellfile = NULL;
char *map_set;
char err_buff[100];
int *ti, *tmp_buf;
int offset, row, col, val, max_char, overflow, charsize, bitplace;
unsigned char *tc;

#ifdef TRACE_FUNCS
Gs_status("Gs_loadmap_as_char");
#endif

    overflow = 0;
    charsize =  8 * sizeof(unsigned char);

    /* 0 bits for sign! */
    max_char = 1;
    for (bitplace = 0; bitplace < charsize; ++bitplace)
	max_char *= 2;
    max_char -= 1;
	
    map_set = G_find_file2 ("cell", map_name, "");

    if ((cellfile = G_open_cell_old(map_name, map_set)) == -1) {
        sprintf(err_buff,"Not able to open cellfile for [%s]", map_name);
        Gs_warning(err_buff);
	return(-1);
    }

    if(NULL == (tmp_buf = (int *)G_malloc (wind->cols * sizeof(int)))) {
	sprintf(err_buff,"out of memory");
        Gs_warning(err_buff);
	return(-1);
    }

    Gs_status("Loading Data");


    for (row = 0; row < wind->rows ; row++) {
	offset = row * wind->cols;
	G_get_map_row(cellfile, tmp_buf, row);
	tc = &(buff[offset]);
	ti = tmp_buf;
	for (col = 0; col < wind->cols; col++) {
	    val = *ti;
	    if (val > max_char) {
		overflow = 1;
		*tc = (unsigned char)max_char;
	    }
	    else if(val < 0) {
		overflow = 1;
		*tc = 0;
	    }
	    else
		*tc = (unsigned char)val;
	    ti++;
	    tc++;
	}
    }


    G_close_cell(cellfile);
    free (tmp_buf);
    
    return(overflow ? -2 : 1);

}


/************************************************************************/

/* Calling function must have already allocated space in buff for
   struct BM of wind->rows & wind->cols.
   This routine simply loads the map into the bitmap by repetitve calls
   to get_map_row.  Any value other than 0 in the map will set the bitmap.
   (may want to change later to allow specific value to set)
   Returns 1 on success, -1 on failure.
*/

int
Gs_loadmap_as_bitmap(wind, map_name, buff)
struct Cell_head *wind;
char *map_name;
struct BM *buff;
{

FILEDESC cellfile = NULL;
char *map_set;
char err_buff[100];
int *ti, *tmp_buf;
int row, col;

#ifdef TRACE_FUNCS
Gs_status("Gs_loadmap_as_bitmap");
#endif

	
    map_set = G_find_file2 ("cell", map_name, "");

    if ((cellfile = G_open_cell_old(map_name, map_set)) == -1) {
        sprintf(err_buff,"Not able to open cellfile for [%s]", map_name);
        Gs_warning(err_buff);
	return(-1);
    }

    if(NULL == (tmp_buf = (int *)G_malloc (wind->cols * sizeof(int)))) {
	sprintf(err_buff,"out of memory");
        Gs_warning(err_buff);
	return(-1);
    }

    Gs_status("Loading Data");


    for (row = 0; row < wind->rows ; row++) {
	G_get_map_row(cellfile, tmp_buf, row);
	ti = tmp_buf;
	for (col = 0; col < wind->cols; col++) {
	    BM_set (buff, col, row, *ti); 
	    ti++;
	}
    }


    G_close_cell(cellfile);
    free (tmp_buf);
    
    return(1);
}


/************************************************************************/

/* Calling function must have already allocated space in buff for range of
data (256 for now) - simply calls get_color for each cat in color range */

Gs_build_256lookup(filename, buff)
char *filename;
int *buff;
{
char *map;
struct Colors colrules;
CELL min, max, cats[256];
int i;
unsigned char r[256], g[256], b[256], set[256];

    Gs_status("building color table");

    map = G_find_file2 ("cell", filename, "");
    G_read_colors (filename, map, &colrules);
    G_get_color_range(&min, &max, &colrules);

    if(min < 0 || max > 255){
	fprintf(stderr, "mincol: %d, maxcol: %d\n",min,max);
	fprintf(stderr, "WARNING: color table range doesn't match data\n");
	/*  return (-1);  */
	min = min < 0 ? 0: min;
	max = max > 255 ? 255: max;
    }

    G_zero(cats, 256 * sizeof(CELL));
    for (i=min; i<=max; i++){
	cats[i] = i;
    }

    G_lookup_colors(cats, r, g, b, set, 256, &colrules);
    for (i=0; i<256; i++){
	if(set[i])
	    buff[i] = r[i] & 0xff | ((g[i] & 0xff)<<8) | ((b[i] & 0xff)<<16);
	else 
	    buff[i] = NO_DATA_COL;
    }
/*
print_256lookup(buff);
*/
    return(1);

}


/************************************************************************/
/* passed an array of 32 bit ints that is converted from cell values
   to packed colors (0xbbggrr) 
*/

Gs_pack_colors(filename, buff, rows, cols)
char *filename;
int *buff;
int rows, cols;
{
char *map;
struct Colors colrules;
unsigned char *r, *g, *b, *set;
int *cur, i, j;

    Gs_status("translating colors");
  
    r = (unsigned char *)G_malloc(cols);
    g = (unsigned char *)G_malloc(cols);
    b = (unsigned char *)G_malloc(cols);
    set = (unsigned char *)G_malloc(cols);
    map = G_find_file2 ("cell", filename, "");
    G_read_colors (filename, map, &colrules);

    cur = buff;
    for(i=0; i<rows; i++){
	G_lookup_colors (cur, r, g, b, set, cols, &colrules);
	for(j=0; j<cols; j++){
	    if(set[j])
		cur[j] = r[j] & 0xff | ((g[j] & 0xff)<<8) | ((b[j] & 0xff)<<16);
	    else
		cur[j] = NO_DATA_COL;
	}
	cur = &(cur[cols]);
    }

    G_free_colors (&colrules);
    free(r);
    free(g);
    free(b);
    free(set);

}


/************************************************************************/

