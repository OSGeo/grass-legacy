

/*---------- Function: paint ----------*/
paint(x, y, color)
int x, y, color;
{
    put_chr('P');
    put_int(x);
    put_int(y);
    put_int(color);
}
