
#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/graphics.h>

/*!
 * \brief flush graphics
 *
 * Send all pending graphics commands to the graphics driver.
 * This is done automatically when graphics input requests are made.
 * Generally this is only needed for interactive graphics.
 *
 *  \param void
 *  \return int
 */

void R_flush(void)
{
	R_stabilize();
}

static int (*update_function)(int, int);

/*!
 * \brief set update
 *
 * \param func
 * \return void
 */

void R_set_update_function(int (*func)(int, int))
{
	update_function = func;
}

/*!
 * \brief call update 
 *
 * \param wx
 * \param wy
 * \return int
 */

void R_call_update_function(int wx, int wy)
{
	if (update_function)
		(*update_function)(wx, wy);
}

/*!
 * \brief Is update function set?
 * \return 1 set 
 * \return 0 not set 
 */

int R_has_update_function(void)
{
	return update_function ? 1 : 0;
}

static int cancel;

/*!
 * \brief cancel 
 *
 *  \param v
 *  \return void
 */

void R_set_cancel(int v)
{
	cancel = v;
}

/*!
 * \brief 
 *
 *  \return int
 */

int R_get_cancel(void)
{
	return cancel;
}

/*!
 * \brief draw a raster
 *
 * Starting at the current position, the <b>num</b> colors represented
 * in the <b>raster</b> array are drawn for <b>nrows</b> consecutive pixel
 * rows.  The <b>withzero</b> flag is used to indicate whether 0 values are to
 * be treated as a color (1) or should be ignored (0). If ignored, those screen
 * pixels in these locations are not modified. This option is useful for graphic
 * overlays.
 *
 *  \param num
 *  \param nrows
 *  \param withzero
 *  \param ras raster
 *  \return int
 */

void R_raster(int num, int nrows, int withzero, const int *ras)
{
	static unsigned char *chararray;
	static int nalloc;
	int i;

	/* Check to see if char buffer can hold the int values */
	for (i = 0; i < num; i++)
	{
		int xc = ras[i];
		unsigned char cc = (unsigned char) xc;

		if (cc != xc)
		{
			R_raster_int(num, nrows, withzero, ras);
			return;
		}
	}

	/* If all one byte values, copy to char raster and call raster_char */
	if (num > nalloc)
	{
		chararray = G_realloc(chararray, num);
		nalloc = num;
	}

	for (i = 0; i < num; i++)
		chararray[i] = (unsigned char) ras[i];

	R_raster_char(num, nrows, withzero, chararray);
}

void R_pad_perror(const char *msg, int code)
{
	const char *err;

	switch (code)
	{
	case OK:		err = "";			break;
	case NO_CUR_PAD:	err = "no current pad";		break;
	case NO_PAD:		err = "pad not found";		break;
	case NO_MEMORY:		err = "out of memory";		break;
	case NO_ITEM:		err = "item not found";		break;
	case ILLEGAL:		err = "illegal request";	break;
	case DUPLICATE:		err = "duplicate name";		break;
	default:		err = "unknown error";		break;
	}

	fprintf(stderr, "%s%s%s\n", msg, *msg ? " : " : "", err);
}

void R_pad_freelist(char **list, int count)
{
	int i;

	if (count <= 0)
		return;

	for (i = 0; i < count; i++)
		G_free(list[i]);

	G_free(list);
}

