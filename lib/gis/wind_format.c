
/**
 * \file wind_format.c
 *
 * \brief GIS Library - Window formatting functions.
 *
 * (C) 2001-2008 by the GRASS Development Team
 *
 * This program is free software under the GNU General Public License
 * (>=v2). Read the file COPYING that comes with GRASS for details.
 *
 * \author GRASS GIS Development Team
 *
 * \date 1999-2008
 */

#include <stdio.h>
#include <grass/gis.h>


static void format_double(double, char *, int);


/**
 * \brief Northing to ASCII.
 *
 * Converts the double representation of the <b>north</b> coordinate to 
 * its ASCII representation (into <b>buf</b>).
 *
 * \param[in] north northing
 * \param[in,out] buf buffer to hold formatted string
 * \param[in] projection, or -1 to force full precision FP
 * \return always returns 0
 */

int G_format_northing(double north, char *buf, int projection)
{
    if (projection == PROJECTION_LL)
	G_lat_format(north, buf);
    else if (projection == -1)
	format_double(north, buf, TRUE);
    else
	format_double(north, buf, FALSE);

    return 0;
}


/**
 * \brief Easting to ASCII.
 *
 * Converts the double representation of the <b>east</b> coordinate to
 * its ASCII representation (into <b>buf</b>).
 *
 * \param[in] east easting
 * \param[in,out] buf buffer to hold formatted string
 * \param[in] projection, or -1 to force full precision FP
 * \return always returns 0
 */

int G_format_easting(double east, char *buf, int projection)
{
    if (projection == PROJECTION_LL)
	G_lon_format(east, buf);
    else if (projection == -1)
	format_double(east, buf, TRUE);
    else
	format_double(east, buf, FALSE);

    return 0;
}


/**
 * \brief Resolution to ASCII.
 *
 * Converts the double representation of the <b>resolution</b> to its 
 * ASCII representation (into <b>buf</b>).
 *
 *  \param[in] resolution
 *  \param[in,out] buf buffer to hold formatted string
 *  \param[in] projection, or -1 to force full precision FP
 *  \return always returns 0
 */

int G_format_resolution(double res, char *buf, int projection)
{
    if (projection == PROJECTION_LL)
	G_llres_format(res, buf);
    else if (projection == -1)
	format_double(res, buf, TRUE);
    else
	format_double(res, buf, FALSE);

    return 0;
}

/*
 * 'full_prec' is boolean, FALSE uses %.8f,  TRUE uses %.15g
 * The reason to have this is that for lat/lon "%.8f" is not
 * enough to preserve fidelity once converted back into D:M:S,
 * which leads to rounding errors, especially for resolution.
 */
static void format_double(double value, char *buf, int full_prec)
{
    if (full_prec)
	sprintf(buf, "%.15g", value);
    else
	sprintf(buf, "%.8f", value);

    G_trim_decimal(buf);

    return;
}
