char *ps_devices(name)
char *name;
{
    static char path[1024];
    char *G_gisbase();

    sprintf(path, "%s/etc/paint/ps.devices/%s", G_gisbase(), name);
    return path;
}
