#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "gis.h"
#include "G.h"
#include "glocale.h"


/*!
 *
 * \brief print debugging message
 * 
 * Print debugging message if environment variable GRASS_DEBUG_LEVEL
 * is set to level equal or greater  
 *
 * Levels: (recommended levels)<br>
 * 1 - message is printed once or few times per module<br>
 * 3 - each row (raster) or line (vector)<br>
 * 5 - each cell (raster) or point (vector) 
 *
 * \param int level
 * \param char *msg
 *
*/

static int grass_debug_level = -1;

int G_debug (int level, char *msg,...)
{
#ifdef GDEBUG
    char    *lstr, *filen;
    va_list ap;
    FILE    *fd;
   
    if (grass_debug_level < 0) {
        lstr = G__getenv( "DEBUG" );

        if ( lstr != NULL )
            grass_debug_level = atoi ( lstr );
        else
            grass_debug_level = 0;
    }
	
    if ( grass_debug_level >= level ) {
        va_start(ap, msg);

	filen =  getenv("GRASS_DEBUG_FILE"); 
        if ( filen != NULL ) {
	    fd = fopen (filen,"a");
            if ( !fd ) {
		G_warning ( _("Cannot open debug file '%s'"), filen);
		return 0;
	    }
	} else {
	    fd = stderr;
	}
        
	fprintf (fd, "D%d/%d: ", level, grass_debug_level);
	vfprintf (fd, msg, ap);
	fprintf (fd, "\n");
	
	if ( filen != NULL ) fclose ( fd );
	
	va_end(ap);
    }
#endif

    return 1;
}


/*!
 * \brief dumps status of various GIS parameters
 * 
 * Dumps status of various GIS parameters of a particular
 * file descriptor..
 *
 * \param int fd
 * \return int
 *
*/

int G_dump (int fd){
    G_message(_("G_dump: memory allocated to G__"));
    G_message(_("Size of cell in fp maps = %d"), G__.fp_nbytes);
    G_message(_("type for writing floating maps = %d"), G__.fp_type);
    G_message(_("current window = %p"), &G__.window);
    G_message(_("Flag: window set? %d"), G__.window_set);
    G_message(_("File descriptor for automatic mask %d"), G__.mask_fd);
    G_message(_("Flag denoting automatic masking %d"), G__.auto_mask); 
    G_message(_("CELL mask buffer %p"), G__.mask_buf);
    G_message(_("buffer for reading null rows %p"), G__.null_buf);
    G_message(_("Pre/post compressed data buffer %p"), G__.compressed_buf);
    G_message(_("sizeof compressed_buf %d"), G__.compressed_buf_size);
    G_message(_("work data buffer %p"), G__.work_buf);
    G_message(_("sizeof work_buf %d"), G__.work_buf_size);
    G_message(_("sizeof null_buf %d"), G__.null_buf_size);
    G_message(_("sizeof mask_buf %d"), G__.mask_buf_size);
    G_message(_("Histogram request %d"), G__.want_histogram);

    G_message(_("G_dump: file #%d"), fd);
    G_message(_("open mode = %d"), G__.fileinfo[fd].open_mode);
    G_message(_("Cell header %p"),&G__.fileinfo[fd].cellhd);
    G_message(_("Table reclass %p"), &G__.fileinfo[fd].reclass);
    G_message(_("Cell stats %p"), &G__.fileinfo[fd].statf);
    G_message(_("Range structure %p"), &G__.fileinfo[fd].range);
    G_message(_("float Range structure %p"), &G__.fileinfo[fd].fp_range);
    G_message(_("want histogram?  %d"), G__.fileinfo[fd].want_histogram);
    G_message(_("Automatic reclass flag %d"), G__.fileinfo[fd].reclass_flag);
    G_message(_("File row addresses %p"), G__.fileinfo[fd].row_ptr);
    G_message(_("Data to window col mapping %p"), G__.fileinfo[fd].col_map);
    G_message(_("Data to window row constants %f,%f"), G__.fileinfo[fd].C1,G__.fileinfo[fd].C2);
    G_message(_("Current data row in memory %d"), G__.fileinfo[fd].cur_row);
    G_message(_("Current null row in memory %d"), G__.fileinfo[fd].null_cur_row);
    G_message(_("nbytes per cell for current row %d"), G__.fileinfo[fd].cur_nbytes);
    G_message(_("Decompressed data buffer %s"), G__.fileinfo[fd].data);
    G_message(_("bytes per cell %d"), G__.fileinfo[fd].nbytes);
    G_message(_("type: int, float or double map %d"), G__.fileinfo[fd].map_type);
    G_message(_("Temporary name for NEW files %s"), G__.fileinfo[fd].temp_name);
    G_message(_("Temporary name for NEW NULL files %s"), G__.fileinfo[fd].null_temp_name);
    G_message(_("for existing raster maps %d"), G__.fileinfo[fd].null_file_exists);
    G_message(_("Name of open file %s"), G__.fileinfo[fd].name);
    G_message(_("Mapset of open file %s"), G__.fileinfo[fd].mapset);
    G_message(_("io error warning given %d"), G__.fileinfo[fd].io_error);
    G_message(_("xdr stream for reading fp %p"), &G__.fileinfo[fd].xdrstream);
    G_message(_("NULL_ROWS array[%d] = %p"), NULL_ROWS_INMEM, G__.fileinfo[fd].NULL_ROWS);
    G_message(_("data buffer for reading null rows %p"), G__.fileinfo[fd].null_work_buf);
    G_message(_("Minimum row null row number in memory %d"), G__.fileinfo[fd].min_null_row);
    G_message(_("Quant ptr = %p"), &G__.fileinfo[fd].quant);
    G_message(_("G_dump: end"));

    return 0;
}
