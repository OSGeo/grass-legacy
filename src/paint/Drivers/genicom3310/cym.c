cym (n, cyan, yellow, magenta)
    unsigned char n;
    int *cyan, *yellow, *magenta;
{
    int red, grn, blu;

    if (n > 124) n = 0;

    red = n / 25 ;
    grn = (n % 25) / 5;
    blu = n % 5;

    *cyan    = 4 - red;
    *yellow  = 4 - blu;
    *magenta = 4 - grn;
}
