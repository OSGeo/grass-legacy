char *
mygets (buf)
    char *buf;
{
    char *gets();
    if (!gets(buf))
    {
        printf ("EOF\n");
        exit(0);
    }
    G_strip (buf);
    return buf;
}
