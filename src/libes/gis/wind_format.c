#include "gis.h"

static int format_double(double,char *);


/*!
 * \brief northing to ASCII
 *
 * Converts the double representation of the
 * <b>north</b> coordinate to its ASCII representation (into <b>buf</b>).
 *
 *  \param north
 *  \param buf
 *  \param projection
 *  \return int
 */

int G_format_northing ( double north, char *buf,int projection)
{
    if (projection == PROJECTION_LL)
	G_lat_format (north, buf);
    else
	format_double (north, buf);

    return 0;
}


/*!
 * \brief easting to ASCII
 *
 * Converts the double representation of the <b>east</b> coordinate to
 * its ASCII representation (into <b>buf</b>).
 *
 *  \param east
 *  \param buf
 *  \param projection
 *  \return int
 */

int G_format_easting ( double east, char *buf,int projection)
{
    if (projection == PROJECTION_LL)
	G_lon_format (east, buf);
    else
	format_double (east, buf);

    return 0;
}


/*!
 * \brief resolution to ASCII
 *
 *  Converts the double representation of the
 * <b>resolution</b> to its ASCII representation (into <b>buf</b>).
 *
 *  \param resolution
 *  \param buf
 *  \param projection
 *  \return int
 */

int G_format_resolution ( double res, char *buf,int projection)
{
    if (projection == PROJECTION_LL)
	G_llres_format (res, buf);
    else
	format_double (res, buf);

    return 0;
}

static int format_double ( double value, char *buf)
{
    sprintf (buf, "%.8f", value);
    G_trim_decimal (buf);

    return 0;
}
