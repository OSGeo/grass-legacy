color(n)
{
    float red,grn,blu;
    int r,g,b;
    unsigned char c;

    Pcolorvalue(n,&red,&grn,&blu);
    r = red * 255;
    g = grn * 255;
    b = blu * 255;

    Poutc(c=r);
    Poutc(c=g);
    Poutc(c=b);
}
