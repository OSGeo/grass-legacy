int value (unsigned char *data, int n, int sflag)
{
    int v;

    v = *data++;
    if (sflag && v > 127) v -= 256;
    while (--n > 0) v = v * 256 + *data++;

    return v;
}
