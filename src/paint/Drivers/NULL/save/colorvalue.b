/* this is for a 125 color system */
Pcolorvalue (n, red, grn, blu)
    float *red, *grn, *blu;
{
    int r,g,b;

    if (n < 0) n = 0;
    if (n > 124) n = 124;

    r = n / 25 ;
    g = (n % 25) / 5;
    b = n % 5 ;

    *red = r / 4.0 ;
    *grn = g / 4.0 ;
    *blu = b / 4.0 ;
}
