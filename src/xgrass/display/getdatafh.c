
int
#ifdef _NO_PROTO
getdatafh (buf, k, d)
    char *buf;
    char **k;
    char **d;
#else
getdatafh (char *buf, char **k, char **d)
#endif
{
    char *key, *data;

    for (key=buf; *key; key++)
	if (*key != ' ' && *key != '\t')
	    break;
    if (*key == 0)
	return 0;
    for (data=key+1; *data; data++)
	if (*data == ' ' || *data == '\t')
	    break;
    if (*data)
	*data++ = 0;
    
    *k = key;
    *d = data;

    return 1;
}
