
struct_copy (To, From, size)
    char *To, *From;
    int size;
{
    for ( ; size ; size--)
	*To++ = *From++;
}
