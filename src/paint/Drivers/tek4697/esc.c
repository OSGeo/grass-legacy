esc (s)
    char *s;
{
    Poutc ('\033');	/* ESC */
    Pouts (s);
}
