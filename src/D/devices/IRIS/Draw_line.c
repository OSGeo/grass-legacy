
/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

extern int SCREEN_BOTTOM;

draw_line(cur_x, cur_y, x, y)
{
    short Vert[2];

    bgnline ();
    Vert[0] = cur_x;
    Vert[1] =  SCREEN_BOTTOM -  cur_y;
    v2s (Vert);
    Vert[0] = x;
    Vert[1] =  SCREEN_BOTTOM -  y;
    v2s (Vert);
    endline ();
}
