char *
mygets (buf)
    char *buf;
{
    if (!gets(buf))
    {
        printf ("EOF\n");
        exit(0);
    }
    G_strip(buf);
    return buf;
}
