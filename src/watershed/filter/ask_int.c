ask_int (prompt)
    char *prompt;
{
    int n;
    char dummy[2];
    char buf[80];

    do
    {
        do printf ("\n%s: ",prompt);
        while (!G_gets(buf));

        *dummy = 0;
    }
    while (sscanf (buf, "%d%1s", &n, dummy) != 1 || *dummy != 0);

    return n;
}
