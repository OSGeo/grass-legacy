

/*---------- Function: line ----------*/
line(x1, y1, x2, y2, color)
int x1, y1, x2, y2, color;
{
    put_chr('L');
    put_int(x1);
    put_int(y1);
    put_int(x2);
    put_int(y2);
    put_int(color);
}
