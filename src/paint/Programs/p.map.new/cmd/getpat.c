
int 
getpat (char *buf, char **k, char **d)
{
    char *key, *data;
	int  flag ;

	flag = 0;
    for (key=buf; *key; key++)
	if  (*key != ' ' && *key != '\t') 
	    break;
    if (*key == 0)
	return 0;

    for (data=key+1; *data; data++)
	{
	if (*data == '-')
	{
		flag = 1;
	    break;
	}

	if (*data ==',')
	{	flag	= 2;
		break;
	}
	}

    if (*data)
	*data++ = 0;
    
    *k = key;
    *d = data;


    return flag;
}
