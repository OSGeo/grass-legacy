
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

