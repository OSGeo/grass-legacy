D_scan_int (buf, f)
    char *buf;
    int *f;
{
    char dummy[2];

    *dummy = 0;
    return sscanf (buf, "%d%1s", f, dummy) == 1 && *dummy == 0 ;
}
