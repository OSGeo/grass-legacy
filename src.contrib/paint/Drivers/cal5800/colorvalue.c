/* %W% %G% */
/* this is for a 216 color system */
Pcolorvalue (n, red, grn, blu)
    float *red, *grn, *blu;
{
    int r,g,b;

    if (n < 0) n = 0;
    if (n > 215) n = 215;

    r = n / 36 ;
    g = (n % 36) / 6;
    b = n % 6 ;

    *red = r / 5.0 ;
    *grn = g / 5.0 ;
    *blu = b / 5.0 ;
}
