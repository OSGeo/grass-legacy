int 
might_be_number (char *s)
{
    if (*s == '-') s++;
    return (*s >= '0' && *s <= '9');
}
