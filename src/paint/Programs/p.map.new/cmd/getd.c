int 
getd (char *buf, char **k, char **d)
{
    char *key, *data;
	int  flag;

	flag = 0;

    for (key=buf; *key; key++)
	if  (*key != ' ' && *key != '\t') 
	    break;
    if (*key == 0)
	return 0;
    for (data=key+1; *data; data++)
	{
	if (*data == 'n' && flag)
	    break;
	if (*data == '\\') {
		flag = 1;
		}
	else
		flag = 0;

	}
    if (*data)
	*data++ = 0;
    
    *k = key;
    *d = data;


    return 1;
}
