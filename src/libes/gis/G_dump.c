#include <stdio.h>
#include "G.h"
#include "glocale.h"
int G_dump (int fd){
    fprintf(stderr,"G_dump: memory allocated to G__\n");
    fprintf(stderr,_("Size of cell in fp maps = %d\n"), G__.fp_nbytes);
    fprintf(stderr,_("type for writing floating maps = %d\n"), G__.fp_type);
    fprintf(stderr,_("current window = %p\n"),&G__.window);
    fprintf(stderr,_("Flag: window set? %d\n"),G__.window_set);
    fprintf(stderr,_("File descriptor for automatic mask %d\n"), G__.mask_fd);
    fprintf(stderr,_("Flag denoting automatic masking %d\n"),G__.auto_mask); 
    fprintf(stderr,_("CELL mask buffer %p\n"),G__.mask_buf);
    fprintf(stderr,_("buffer for reading null rows %p\n"),G__.null_buf);
    fprintf(stderr,_("Pre/post compressed data buffer %p\n"),G__.compressed_buf);
    fprintf(stderr,_("sizeof compressed_buf %d\n"),G__.compressed_buf_size);
    fprintf(stderr,_("work data buffer %p\n"), G__.work_buf);
    fprintf(stderr,_("sizeof work_buf %d\n"), G__.work_buf_size);
    fprintf(stderr,_("sizeof null_buf %d\n"), G__.null_buf_size);
    fprintf(stderr,_("sizeof mask_buf %d\n"), G__.mask_buf_size);
    fprintf(stderr,_("Histogram request %d\n"),G__.want_histogram);

    fprintf(stderr,_("G_dump: file #%d\n"), fd);
        fprintf(stderr,_("open mode = %d\n"),G__.fileinfo[fd].open_mode);
        fprintf(stderr,_("Cell header %p\n"),&G__.fileinfo[fd].cellhd);
        fprintf(stderr,_("Table reclass %p\n"),&G__.fileinfo[fd].reclass);
	fprintf(stderr,_("Cell stats %p\n"),&G__.fileinfo[fd].statf);
	fprintf(stderr,_("Range structure %p\n"),&G__.fileinfo[fd].range);
	fprintf(stderr,_("float Range structure %p\n"),&G__.fileinfo[fd].fp_range);
        fprintf(stderr,_("max # bits used in lzw compression %d\n"),G__.fileinfo[fd].compression_bits);
	fprintf(stderr,_("want histogram?  %d\n"),G__.fileinfo[fd].want_histogram);
        fprintf(stderr,_("Automatic reclass flag %d\n"),G__.fileinfo[fd].reclass_flag);
        fprintf(stderr,_("File row addresses %p\n"),G__.fileinfo[fd].row_ptr);
        fprintf(stderr,_("Data to window col mapping %p\n"),G__.fileinfo[fd].col_map);
	fprintf(stderr,_("Data to window row constants %f,%f\n"),G__.fileinfo[fd].C1,G__.fileinfo[fd].C2);
        fprintf(stderr,_("Current data row in memory %d\n"),G__.fileinfo[fd].cur_row);
        fprintf(stderr,_("Current null row in memory %d\n"),G__.fileinfo[fd].null_cur_row);
	fprintf(stderr,_("nbytes per cell for current row %d\n"),G__.fileinfo[fd].cur_nbytes);
        fprintf(stderr,_("Decompressed data buffer %s\n"),G__.fileinfo[fd].data);
        fprintf(stderr,_("bytes per cell %d\n"),G__.fileinfo[fd].nbytes);
        fprintf(stderr,_("type: int, float or double map %d\n"),G__.fileinfo[fd].map_type);
       fprintf(stderr,_("Temporary name for NEW files %s\n"),G__.fileinfo[fd].temp_name);
        fprintf(stderr,_("Temporary name for NEW NULL files %s\n"),G__.fileinfo[fd].null_temp_name);
	fprintf(stderr,_("for existing raster maps %d\n"),G__.fileinfo[fd].null_file_exists);
        fprintf(stderr,_("Name of open file %s\n"),G__.fileinfo[fd].name);
        fprintf(stderr,_("Mapset of open file %s\n"),G__.fileinfo[fd].mapset);
        fprintf(stderr,_("io error warning given %d\n"),G__.fileinfo[fd].io_error);
        fprintf(stderr,_("xdr stream for reading fp %p\n"),&G__.fileinfo[fd].xdrstream);
        fprintf(stderr,_("NULL_ROWS array[%d] = %p\n"),NULL_ROWS_INMEM,G__.fileinfo[fd].NULL_ROWS);
        fprintf(stderr,_("data buffer for reading null rows %p\n"),G__.fileinfo[fd].null_work_buf);
        fprintf(stderr,_("Minimum row null row number in memory %d\n"),G__.fileinfo[fd].min_null_row);
        fprintf(stderr,"Quant ptr = %p\n",&G__.fileinfo[fd].quant);
	fprintf(stderr,"G_dump: end\n");

    return 0;
}
