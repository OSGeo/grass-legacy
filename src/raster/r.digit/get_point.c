/* button 1 is whereami: no retur, keep loopingn
 *        2 is mark point, return 1 (ok)
 *        3 is done, return 0 (done)
 */

get_point(x, y, east, north)
    int *x ;
    int *y ;
    char *east ;
    char *north ;
{
    int button ;
    int curx, cury;

    curx = *x;
    cury = *y;
    do
    {
        if (curx >= 0 && cury >= 0)
            R_get_location_with_line(curx, cury, &curx, &cury, &button) ;
        else
            R_get_location_with_pointer(&curx, &cury, &button) ;

        if (button == 3)
            return(0) ;

	get_east_north (curx, cury, east, north);
        printf("EAST:  %-20s", east);
        printf("NORTH: %s\n", north);
        instructions(1) ;

    } while (button == 1) ;

    *x = curx;
    *y = cury;
    return(1) ;
}
