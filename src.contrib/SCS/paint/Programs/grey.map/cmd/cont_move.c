static int curx = 0;
static int cury = 0;
move_abs (x, y)
{
    curx = x;
    cury = y;
}

cont_abs (x, y)
{
    draw_line (curx, cury, x, y);
    curx = x;
    cury = y;
}
