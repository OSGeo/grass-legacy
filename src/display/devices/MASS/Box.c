Box_abs (x1,y1,x2,y2)
{
    extern int SCREEN_BOTTOM;

    y1 = SCREEN_BOTTOM - y1;
    y2 = SCREEN_BOTTOM - y2;

    if (x1 > x2)
    {
	if (y1 > y2)
	    mgibox (x2, y2, x1, y1);
	else
	    mgibox (x2, y1, x1, y2);
    }
    else
    {
	if (y1 > y2)
	    mgibox (x1, y2, x2, y1);
	else
	    mgibox (x1, y1, x2, y2);
    }
}
