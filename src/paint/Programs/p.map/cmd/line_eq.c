int 
line_eq (int x, int x0, int y0, int dx, int dy, int round)
{
    register int t;

    if((t = dy*(x-x0)) < 0) t-= round;
    else t += round;;

    return (y0 + t / dx);
}
