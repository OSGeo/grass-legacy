create_map(name, polyfile)
    char *name, *polyfile;
{
    char buf[1024];
    sprintf (buf, "r.in.poly i='%s' o='%s'", polyfile,name);
    printf ("Creating raster map %s\n", name);
    return system(buf);
}
