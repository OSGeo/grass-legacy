/* %W% %G% */

scan_geo(buf, result)
    char *buf;
    double *result;
{
    char temp[3];
    int type;
    int d,m,s;

    if (sscanf (buf, "%d.%d.%d%1s", &d, &m, &s, temp) == 4)
	;
    else if (sscanf (buf, "%d.%d%1s", &d, &m, temp) == 3)
	s = 0 ;
    else if (sscanf (buf, "%d%1s", &d, temp) == 2)
	s = m = 0;
    else
	return 0;

    if (d < 0) return 0;
    if (m < 0 || m >= 60) return 0;
    if (s < 0 || s >= 60) return 0;

    if (*temp >= 'A' && *temp <= 'Z') *temp += 'a' -'A';
    if (*temp == 'e' || *temp == 'w')
	type = 1;
    else if (*temp == 'n' || *temp == 's')
	type = 2;
    else
	return 0;

    *result = d*3600 + m*60 + s;
    if (*temp == 'e' || *temp == 's')
    	*result = -(*result);
    return type;
}
