/* %W% %G% */
extern int cur_x, cur_y;

Move_rel(x, y)
int x, y;
{
    Move_abs(cur_x + x, cur_y + y);
}
