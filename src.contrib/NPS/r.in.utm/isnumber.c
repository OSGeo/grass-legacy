isnumber(s)
    char *s;
{
    if (*s == '-') s++;
    return (*s >= '0' && *s <= '9');
}
