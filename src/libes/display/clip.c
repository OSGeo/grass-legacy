/* D_clip()  D_clips, if necessary, lines extending outside of a window.
 *
 * The limits of the window are described by:
 *    e:  east
 *    w:  west
 *    s:  south
 *    n:  north
 * Note that the following constraints must be true:
 *    ( w < e )     ( s < n )
 *
 *    x and c_x are values to be compared to w and e
 *    y and c_y are values to be compared to s and n
 *
 *    the x and c_x values returned lie between w and e
 *    the y and c_y values returned lie between s and n
 *
 * returns: 1 if any clipping occured, 0 otherwise
 */


/*!
 * \brief clip coordinates to window
 *
 * A line
 * represented by the coordinates <b>x, y</b> and <b>c_x, c_y</b> is clipped to
 * the window defined by <b>s</b> (south), <b>n</b> (north), <b>w</b>
 * (west), and <b>e</b> (east). Note that the following constraints must be
 * true:
 * w <e
 * s <n
 * The <b>x</b> and <b>c_x</b> are values to be compared to <b>w</b> and
<b>e.</b> The <b>y</b> and <b>c_y</b> are values to be compared to
<b>s</b> and <b>n.</b>
 * The <b>x</b> and <b>c_x</b> values returned lie between <b>w</b> and 
<b>e.</b> The <b>y</b> and <b>c_y</b> values returned lie between 
<b>s</b> and <b>n.</b>
 *
 *  \param s
 *  \param n
 *  \param w
 *  \param e
 *  \param x
 *  \param y
 *  \param c_x
 *  \param c_y
 *  \return int
 */

int D_clip(
	register double s,
	register double n,
	register double w,
	register double e,
	register double *x,
	register double *y,
	register double *c_x,
	register double *c_y )
{
	int mod ;

	mod = 0 ;

	if (*x < w)
	{
		if (*c_x != *x)
			*y = *y + (w - *x)/(*c_x - *x) * (*c_y - *y) ;
		*x = w ;
		mod = 1 ;
	}
	if (*x > e)
	{
		if (*c_x != *x)
			*y = *y + (e - *x)/(*c_x - *x) * (*c_y - *y) ;
		*x = e ;
		mod = 1 ;
	}
	if (*c_x < w)
	{
		if (*c_x != *x)
			*c_y = *c_y + (w - *c_x)/(*x - *c_x) * (*y - *c_y) ;
		*c_x = w ;
		mod = 1 ;
	}
	if (*c_x > e)
	{
		if (*c_x != *x)
			*c_y = *c_y + (e - *c_x)/(*x - *c_x) * (*y - *c_y) ;
		*c_x = e ;
		mod = 1 ;
	}
	if (*y < s)
	{
		if (*c_y != *y)
			*x = *x + (s - *y)/(*c_y - *y) * (*c_x - *x) ;
		*y = s ;
		mod = 1 ;
	}
	if (*y > n)
	{
		if (*c_y != *y)
			*x = *x + (n - *y)/(*c_y - *y) * (*c_x - *x) ;
		*y = n ;
		mod = 1 ;
	}
	if (*c_y < s)
	{
		if (*c_y != *y)
			*c_x = *c_x + (s - *c_y)/(*y - *c_y) * (*x - *c_x) ;
		*c_y = s ;
		mod = 1 ;
	}
	if (*c_y > n)
	{
		if (*c_y != *y)
			*c_x = *c_x + (n - *c_y)/(*y - *c_y) * (*x - *c_x) ;
		*c_y = n ;
		mod = 1 ;
	}

	return (mod) ;
}
