#include <stdio.h>

extern int SCREEN_TOP ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_LEFT ;
extern int SCREEN_RIGHT ;

extern struct vwsurf *suncolor ;
extern int NCOLORS ;

static struct
{
	int width, height, depth ;
	short *bits ;
} raster ;
static struct
{
	int type ;
	int nbytes ;
	char *data ;
} map ;

/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.
 */
Panel_save(name, top, bottom, left, right)
	char *name ;
{
	int fd ;
	float ndc_t, ndc_b, ndc_l, ndc_r ;

/* Adjust panel edges if outside window necessary */
	if (top    < SCREEN_TOP)       top = SCREEN_TOP ;
	if (bottom > SCREEN_BOTTOM) bottom = SCREEN_BOTTOM ;
	if (left   < SCREEN_LEFT)     left = SCREEN_LEFT ;
	if (right  > SCREEN_RIGHT)   right = SCREEN_RIGHT ;

/* Flip y coordinates */
	bottom = SCREEN_BOTTOM - bottom ;
	top    = SCREEN_BOTTOM - top    ;

/* establish raster to be written */
	map_world_to_ndc_2((float)left , (float)bottom, &ndc_l, &ndc_b) ;
	map_world_to_ndc_2((float)right, (float)top   , &ndc_r, &ndc_t) ;
	size_raster(suncolor, ndc_l, ndc_r, ndc_b, ndc_t, &raster) ;
	allocate_raster(&raster) ;
	get_raster(suncolor, ndc_l, ndc_r+5.0, ndc_b, ndc_t, 0, 0, &raster) ;
/* establish color table to be written */
	map.type = 1 ;
	map.nbytes = 0 ;
	map.data = NULL ;
/* open the file */
	fd = creat(name, 0644) ;
/* write the lower left coordinates */
	write(fd, &bottom, sizeof(bottom)) ;
	write(fd, &left, sizeof(left)) ;
/* write the raster */
	raster_to_file(&raster, &map, fd, 1) ;

	close(fd) ;
	free_raster(&raster) ;
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
	char *name ;
{
	int fd ;
	int bottom, left ;

	fd = open(name, 0) ;
	read(fd, &bottom, sizeof(int)) ;
	read(fd, &left, sizeof(int)) ;
	file_to_raster(fd, &raster, &map) ;
	close(fd) ;

/* This should work, but it leaves a line along the rightmost edge of the
   area being restored.
	move_abs_2((float)left, (float)bottom) ;
	put_raster(&raster) ;
	free_raster(&raster) ;
 
   instead we do the following nonsense; copy the real raster row by row
   to the sxreen.
*/
{
	int i, j ;
	unsigned char *a, *b ;

	struct
	{
		int width, height, depth ;
		short *bits ;
	} tmp_ras ;
	tmp_ras.width = raster.width - 10 ;
	tmp_ras.height = 1 ;
	tmp_ras.depth  = 8 ;
	tmp_ras.bits = (short *)calloc(raster.width/2+1, sizeof(short)) ;

	for(i=0; i<raster.height; i++)
	{
		a = raster.bits + raster.height * i ;
		b = tmp_ras.bits ;
		for(j=0; j<tmp_ras.width; j++)
			*b++ = *a++ ;
		move_abs_2((float)left, (float)(bottom+i)) ;
		put_raster(&tmp_ras) ;
	}
}
}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
	char *name ;
{
	unlink(name) ;
}
