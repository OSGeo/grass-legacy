line_eq (x,x0,y0,dx,dy,round)
{
    register int t;

    if((t = dy*(x-x0)) < 0) t-= round;
    else t += round;;

    return (y0 + t / dx);
}
