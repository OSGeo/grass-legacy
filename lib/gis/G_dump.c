#include <stdio.h>
#include "G.h"
int G_dump (int fd){
    fprintf(stderr,"G_dump: memory allocated to G__\n");
    fprintf(stderr,"Size of cell in fp maps = %d\n", G__.fp_nbytes);
    fprintf(stderr,"type for writing floating maps = %d\n", G__.fp_type);
    fprintf(stderr,"current window = %p\n",&G__.window);
    fprintf(stderr,"Flag: window set? %d\n",G__.window_set);
    fprintf(stderr,"File descriptor for automatic mask %d\n", G__.mask_fd);
    fprintf(stderr,"Flag denoting automatic masking %d\n",G__.auto_mask); 
    fprintf(stderr,"CELL mask buffer %p\n",G__.mask_buf);
    fprintf(stderr,"buffer for reading null rows %p\n",G__.null_buf);
    fprintf(stderr,"Pre/post compressed data buffer %p\n",G__.compressed_buf);
    fprintf(stderr,"sizeof compressed_buf %d\n",G__.compressed_buf_size);
    fprintf(stderr,"work data buffer %p\n", G__.work_buf);
    fprintf(stderr,"sizeof work_buf %d\n", G__.work_buf_size);
    fprintf(stderr,"sizeof null_buf %d\n", G__.null_buf_size);
    fprintf(stderr,"sizeof mask_buf %d\n", G__.mask_buf_size);
    fprintf(stderr,"Histogram request %d\n",G__.want_histogram);

    fprintf(stderr,"G_dump: file #%d\n", fd);
        fprintf(stderr,"open mode = %d\n",G__.fileinfo[fd].open_mode);
        fprintf(stderr,"Cell header %p\n",&G__.fileinfo[fd].cellhd);
        fprintf(stderr,"Table reclass %p\n",&G__.fileinfo[fd].reclass);
	fprintf(stderr,"Cell stats %p\n",&G__.fileinfo[fd].statf);
	fprintf(stderr,"Range structure %p\n",&G__.fileinfo[fd].range);
	fprintf(stderr,"float Range structure %p\n",&G__.fileinfo[fd].fp_range);
        fprintf(stderr,"max # bits used in lzw compression %d\n",G__.fileinfo[fd].compression_bits);
	fprintf(stderr,"want histogram?  %d\n",G__.fileinfo[fd].want_histogram);
        fprintf(stderr,"Automatic reclass flag %d\n",G__.fileinfo[fd].reclass_flag);
        fprintf(stderr,"File row addresses %p\n",G__.fileinfo[fd].row_ptr);
        fprintf(stderr,"Data to window col mapping %p\n",G__.fileinfo[fd].col_map);
	fprintf(stderr,"Data to window row constants %f,%f\n",G__.fileinfo[fd].C1,G__.fileinfo[fd].C2);
        fprintf(stderr,"Current data row in memory %d\n",G__.fileinfo[fd].cur_row);
        fprintf(stderr,"Current null row in memory %d\n",G__.fileinfo[fd].null_cur_row);
	fprintf(stderr,"nbytes per cell for current row %d\n",G__.fileinfo[fd].cur_nbytes);
        fprintf(stderr,"Decompressed data buffer %s\n",G__.fileinfo[fd].data);
        fprintf(stderr,"bytes per cell %d\n",G__.fileinfo[fd].nbytes);
        fprintf(stderr,"type: int, float or double map %d\n",G__.fileinfo[fd].map_type);
       fprintf(stderr,"Temporary name for NEW files %s\n",G__.fileinfo[fd].temp_name);
        fprintf(stderr,"Temporary name for NEW NULL files %s\n",G__.fileinfo[fd].null_temp_name);
	fprintf(stderr,"for existing raster maps %d\n",G__.fileinfo[fd].null_file_exists);
        fprintf(stderr,"Name of open file %s\n",G__.fileinfo[fd].name);
        fprintf(stderr,"Mapset of open file %s\n",G__.fileinfo[fd].mapset);
        fprintf(stderr,"io error warning given %d\n",G__.fileinfo[fd].io_error);
        fprintf(stderr,"xdr stream for reading fp %p\n",&G__.fileinfo[fd].xdrstream);
        fprintf(stderr,"NULL_ROWS array[%d] = %p\n",NULL_ROWS_INMEM,G__.fileinfo[fd].NULL_ROWS);
        fprintf(stderr,"data buffer for reading null rows %p\n",G__.fileinfo[fd].null_work_buf);
        fprintf(stderr,"Minimum row null row number in memory %d\n",G__.fileinfo[fd].min_null_row);
        fprintf(stderr,"Quant ptr = %p\n",&G__.fileinfo[fd].quant);
	fprintf(stderr,"G_dump: end\n");

    return 0;
}
