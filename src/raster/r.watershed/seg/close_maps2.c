#include "Gwater.h"
#include <unistd.h>
    
int 
close_array_seg (void)
{
    struct Colors colors;
    int incr, max, red, green, blue, rd, gr, bl, flag;
    int c, r, map_fd;
    CELL *cellrow, value;
    CSEG *theseg;

    if (seg_flag || bas_flag || haf_flag) {
	if (seg_flag) 		theseg = &bas;
	else if (bas_flag) 	theseg = &bas;
	else 			theseg = &haf;
	max = -9;
	for (r=0; r<nrows; r++) {
	    for (c=0; c<ncols; c++) {
		cseg_get (theseg, &value, r, c);
		if(value > max)
		    max = value;
	    }
	}
	G_init_colors(&colors);
	G_make_random_colors(&colors, 1, max);
	G_set_color((CELL)0, 0, 0, 0, &colors);
	r = 1;
	incr = 0;
	while(incr >= 0) {
	    for (gr=130+incr; gr<=255; gr += 20) {
	        for (rd=90+incr; rd<=255; rd += 30) {
		    for (bl=90+incr; bl<=255; bl += 40) {
		        flag = 1;
		        while (flag) { 
			    G_get_color(r,&red,&green,&blue, &colors);
			    if((blue*.11 + red*.30 + green*.59) < 100) { 
				G_set_color(r, rd, gr, bl, &colors);
			        flag = 0;
		            }
			    if (++r > max) {
			        gr = rd = bl = 300;
				flag = 0;
			        incr = -1;
			    }
		        }
		    }
	        }
	    }
	    if (incr >= 0) {
		incr += 15;
		if (incr > 120)
		    incr = 7;
	    }
	}
    }
    if (seg_flag) {
	cellrow = (CELL *)G_malloc(ncols * sizeof(CELL));
        map_fd = G_open_cell_new(seg_name);
        for (r=0; r<nrows; r++) {
	    for (c=0; c<ncols; c++) {
		bseg_get (&swale, &value, r, c);
		if (value)
		    cseg_get (&bas, &(cellrow[c]), r, c);
		else cellrow[c] = 0;
	    }
            G_put_raster_row(map_fd, cellrow, CELL_TYPE);
        }
	G_free(cellrow);
        G_close_cell(map_fd);
	G_write_colors(seg_name, this_mapset, &colors);
    }
    if (bas_flag) {
	cseg_write_cellfile(&bas, bas_name);
	G_write_colors(bas_name, this_mapset, &colors);
    }
    if (haf_flag) {
	cseg_write_cellfile(&haf, haf_name);
	G_write_colors(haf_name, this_mapset, &colors);
    }
    if(seg_flag || bas_flag || haf_flag)
	G_free_colors(&colors);
    cseg_close(&haf);
    cseg_close(&bas);
    if(arm_flag) fclose(fp);
    close_maps();

    return 0;
}
