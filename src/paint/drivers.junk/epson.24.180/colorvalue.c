/* this is for a 125 color system */
Pcolorvalue (n, red, grn, blu)
    float *red, *grn, *blu;
{
    int r,g,b;

    if (n < 0) n = 0;
    else if (n > 124) n = 0;

    r = n / 25 ;
    g = (n % 25) / 5;
    b = n % 5 ;

    *red = (r + r + 1) / 10.0 ;
    *grn = (g + g + 1) / 10.0 ;
    *blu = (b + b + 1) / 10.0 ;
}
