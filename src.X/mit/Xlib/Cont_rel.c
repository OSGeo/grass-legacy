/* %W% %G% */

extern int cur_x, cur_y;

Cont_rel(x, y)
int x, y;
{
    Cont_abs(cur_x + x, cur_y + y);
}
