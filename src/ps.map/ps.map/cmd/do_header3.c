/* Functions: do_map_header, read_header_file
**
** Author: Paul W. Carlson	April 1992
*/

#include "ps_info.h"
#include "header.h"

do_map_header()
{
    char buf[100];
    char temp[100];
    double x, y, dy, fontsize;

    /* set font */
    fontsize = (double)hdr.fontsize;
    fprintf(PS.fp, "(%s) FN %.1lf SF\n", hdr.font, fontsize);

    /* set start of first line */
    dy = 1.5 * fontsize;
    y = 72.0 * (PS.page_height - PS.top_marg) - fontsize - 1.5;

    if (hdr.fp == NULL)
    {

    	if (PS.celltitle[0])
    	{
	    fprintf(PS.fp, "/t (TITLE:  %s) def\n", PS.celltitle);
	    fprintf(PS.fp, "t SW pop %.1lf XS D2 t exch %.1lf MS\n", 
		72 * PS.page_width, y);
	    y -= dy;
    	    strcpy(temp, G_myname());
    	    G_strip(temp);
    	    if (*temp == 0) strcpy(temp, G_location());
	    fprintf(PS.fp, "/t (LOCATION:  %s) def\n", temp);
	    fprintf(PS.fp, "t SW pop %.1lf XS D2 t exch %.1lf MS\n", 
		72 * PS.page_width, y);
	    y -= 0.25 * dy;
	}
    	if (PS.min_y > y) PS.min_y = y;
	return;
    }
	    
    x = 72.0 * PS.left_marg + 1.5;
    read_header_file();
    y -= 0.25 * dy;
    if (PS.min_y > y) PS.min_y = y;
}

    
read_header_file(){}
