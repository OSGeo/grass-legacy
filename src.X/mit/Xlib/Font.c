set_Font(text)
char *text;
{
    if (-1 == init_font(text))
        init_font("romand");
}
