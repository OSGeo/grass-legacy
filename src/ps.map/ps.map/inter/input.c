input (buf)
    char *buf;
{
    if (!gets(buf)) exit(0);
    G_strip (buf);
    if (strcmp (buf,"exit")==0) exit(0);
}
