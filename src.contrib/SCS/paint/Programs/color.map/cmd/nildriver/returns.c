/* these simulate what the driver returns */
double Ptextscale()
{
    return 1.0;
}
Ptextfudge()
{
    return 0;
}
Ptextspace()
{
    return 2;
}
Pblocksize()
{
    return 35;
}
Pblockspace()
{
    return 25;
}
Pnblocks()
{
    return 10;
}
Pnchars()
{
    return 80;
}
double Phres()
{
    return 121.0;
}
double Pvres()
{
    return 120.0;
}
Pnpixels (rows, cols)
    int *rows, *cols;
{
    *rows = 0;
    *cols = 1024;
}
