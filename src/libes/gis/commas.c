/* puts commas into numbers:
	1234567    becomes 1,234,567
	1234567.89 becomes 1,234,567.89
	12345      becomes 12,345
	1234       stays   1234
* doesn't work well with negative numbers (yet)
*/
G_insert_commas(buf)
    char *buf;
{
    char number[100];
    int i,len;
    int comma;

    while (*buf == ' ') buf++;
    strcpy (number, buf);
    for (len=0; number[len]; len++)
	if(number[len] == '.')
	    break;
    if (len < 5)
	return;
 
    i = 0;
    if (comma = len%3)
    {
	while (i < comma)
	    *buf++ = number[i++];
	*buf++ = ',';
    }
    for (comma = 0; number[i]; comma++)
    {
	if (number[i] == '.')
	    break;
	if (comma && (comma%3 == 0))
	    *buf++ = ',';
	*buf++ = number[i++];
    }
    while (number[i])
	*buf++ = number[i++];
    *buf = 0;
}

/* removes commas from strings representing a number 
	1,234,567    becomes 1234567
	1,234,567.89 becomes 1234567.89
	12,345      becomes 12345
	1234       stays   1234
*/
G_remove_commas(buf)
    char *buf;
{
    char *b;

    for (b=buf; *b; b++)
	if(*b != ',')
	    *buf++ = *b;

    *buf = 0;
    return;
}


