#include "gis.h"
#include "glocale.h"

/***********************************************************

char *
G_adjust_Cell_head (cellhd, row_flag, col_flag)
    struct Cell_head *cellhd;

 This function fills in missing parts of the input cell header.
 It also makes projection-specific adjustments (such as lat-lon).

 The north, south, east, west parts of the header must be set
 before calling this routine.

 The other parts depend on row_flag and col_flag:

 row_flag TRUE
    the rows in the cell header is already set
    the n-s resolution is to be computed.

 row_flag FALSE
    the n-s resolution in the cell header is already set,
    the rows are to be computed.
    (the resolution may be adjusted to conform to the computed rows)

 col_flag TRUE
    the cols in the cell header is already set
    the e-w resolution is to be computed.

 col_flag FALSE
    the e-w resolution in the cell header is already set,
    the cols are to be computed.
    (the resolution may be adjusted to conform to the computed cols)

 returns an error message, or NULL if ok
 note: don't free this error message.
************************************************************/

/*!
 * \brief adjust cell header
 *
 * This function fills in missing parts of the input
 * cell header (or region).  It also makes projection-specific adjustments. The
 * <b>cellhd</b> structure must have its <i>north, south, east, west</i>,
 * and <i>proj</i> fields set. 
 * 
 * If <b>row_flag</b> is true, then the north-south resolution is computed 
 * from the number of <i>rows</i> in the <b>cellhd</b> structure. Otherwise the number of 
 * <i>rows</i> is computed from the north-south resolution in the structure, similarly for
 * <b>col_flag</b> and the number of columns and the east-west resolution. 
 *
 * If row_flag is true, top-bottom resolution is calculated from depths.
 * If row_flag are false, number of depths is calculated from top-bottom resolution.
 *
 * This routine returns NULL if execution occurs without error, otherwise it returns
 * an error message.
 *
 *  \param cellhd
 *  \param row_flag
 *  \param col_flag
 *  \return char * 
 */

char *G_adjust_Cell_head(struct Cell_head *cellhd,int row_flag,int col_flag)
{
    return G_adjust_Cell_head3 ( cellhd, row_flag, col_flag, row_flag );
}

/*!
 * \brief adjust cell header
 *
 * This function fills in missing parts of the input
 * cell header (or region).  It also makes projection-specific adjustments. The
 * <b>cellhd</b> structure must have its <i>north, south, east, west</i>,
 * and <i>proj</i> fields set. 
 * 
 * If <b>row_flag</b> is true, then the north-south resolution is computed 
 * from the number of <i>rows</i> in the <b>cellhd</b> structure. Otherwise the number of 
 * <i>rows</i> is computed from the north-south resolution in the structure, similarly for
 * <b>col_flag</b> and the number of columns and the east-west resolution. 
 *
 * If depth_flag is true, top-bottom resolution is calculated from depths.
 * If depth_flag are false, number of depths is calculated from top-bottom resolution.
 *
 * This routine returns NULL if execution occurs without error, otherwise it returns
 * an error message.
 *
 *  \param cellhd
 *  \param row_flag
 *  \param col_flag
 *  \return char * 
 */
char *G_adjust_Cell_head3(struct Cell_head *cellhd,int row_flag,int col_flag, int depth_flag)
{
    if (!row_flag)
    {
	if (cellhd->ns_res <= 0)
	    return (_("Illegal n-s resolution value"));
	if (cellhd->ns_res3 <= 0)
	    return (_("Illegal n-s3 resolution value"));
    }
    else
    {
	if (cellhd->rows <= 0)
	    return (_("Illegal row value"));
	if (cellhd->rows3 <= 0)
	    return (_("Illegal row3 value"));
    }
    if (!col_flag)
    {
	if (cellhd->ew_res <= 0)
	    return (_("Illegal e-w resolution value"));
	if (cellhd->ew_res3 <= 0)
	    return (_("Illegal e-w3 resolution value"));
    }
    else
    {
	if (cellhd->cols <= 0)
	    return (_("Illegal col value"));
	if (cellhd->cols3 <= 0)
	    return (_("Illegal col3 value"));
    }
    if (!depth_flag)
    {
	if (cellhd->tb_res <= 0)
	    return (_("Illegal t-b3 resolution value"));
    }
    else
    {
	if (cellhd->depths <= 0)
	    return (_("Illegal depths value"));
    }

/* for lat/lon, check north,south. force east larger than west */
    if (cellhd->proj == PROJECTION_LL)
    {
	if (cellhd->north > 90.0)
	    return (_("Illegal latitude for North"));
	if (cellhd->south < -90.0)
	    return (_("Illegal latitude for South"));
	while (cellhd->east <= cellhd->west)
	    cellhd->east += 360.0;
    }

/* check the edge values */
    if (cellhd->north <= cellhd->south)
    {
	if (cellhd->proj == PROJECTION_LL)
	    return (_("North must be north of South"));
	else
	    return (_("North must be larger than South"));
    }
    if (cellhd->east <= cellhd->west)
	return (_("East must be larger than West"));


/* compute rows and columns, if not set */
    if (!row_flag)
    {
	cellhd->rows = (cellhd->north - cellhd->south + cellhd->ns_res/2.0) / cellhd->ns_res;
	if (cellhd->rows == 0)
	    cellhd->rows = 1;

	cellhd->rows3 = (cellhd->north - cellhd->south + cellhd->ns_res3/2.0) / cellhd->ns_res3;
	if (cellhd->rows3 == 0)
	    cellhd->rows3 = 1;
    }
    if (!col_flag)
    {
	cellhd->cols = (cellhd->east - cellhd->west + cellhd->ew_res/2.0) / cellhd->ew_res;
	if (cellhd->cols == 0)
	    cellhd->cols = 1;

	cellhd->cols3 = (cellhd->east - cellhd->west + cellhd->ew_res3/2.0) / cellhd->ew_res3;
	if (cellhd->cols3 == 0)
	    cellhd->cols3 = 1;
    }

    if (!depth_flag)
    {
	cellhd->depths = (cellhd->top - cellhd->bottom + cellhd->tb_res/2.0) / cellhd->tb_res;
	if (cellhd->depths < 0)
	    cellhd->depths = 1;

    }


    if (cellhd->cols < 0 || cellhd->rows < 0 || cellhd->cols3 < 0 || cellhd->rows3 < 0 
	|| cellhd->depths < 0 )
    {
	return (_("Invalid coordinates"));
    }


/* (re)compute the resolutions */
    cellhd->ns_res = (cellhd->north - cellhd->south) / cellhd->rows;
    cellhd->ns_res3 = (cellhd->north - cellhd->south) / cellhd->rows3;
    cellhd->ew_res = (cellhd->east  - cellhd->west)  / cellhd->cols;
    cellhd->ew_res3 = (cellhd->east  - cellhd->west)  / cellhd->cols3;
    cellhd->tb_res = (cellhd->top  - cellhd->bottom)  / cellhd->depths;
	
    return NULL;
}

