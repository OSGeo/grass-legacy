input(buf) char *buf;
{
    do
    {
	if (isatty(0)) printf ("d> ");
    } while (!G_gets(buf));
    G_strip (buf);
    return (*buf != 'q');
}
