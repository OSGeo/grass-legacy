/***************************************************
 * these routines determine the printf format used
 * by floating point values
 *
 * format_parms() is called for each value.
 *     before first call set eformat=0,dp=6
 *
 * format_double() does the formating with the
 *     parms determined by format_parms()
 ***************************************************/
format_parms (v,n,dp, eformat, e_option)
    double v;
    int *n, *dp, *eformat, e_option;
{
    char buf[50];
    int orig_length, scient_dp;

    orig_length = *n;
    scient_dp = *dp;
    for(;;)
    {
	if(!*eformat)
	    format_double (v, buf, *n, *dp);
        else 
	    scient_format(v, buf, *n, *dp);

	if (strlen(buf) <= *n) break;

	if(*dp)
	{
	    *dp -= 1;
	}
	else
	{
	    if((e_option)&&(!*eformat))
	    {
	       *eformat=1;
	       *dp = scient_dp;
            }
            else
	       *n = strlen(buf);
	}
    }
}

scient_format(v, buf, n, dp)
    double v;
    char *buf;
{
    char temp[50];
    int i;

    sprintf(temp, "%#.*lg", dp, v);
    for (i=0; i<=n && temp[i] == ' ';i++) {}
    strcpy (buf, temp+i);
}

format_double (v, buf, n, dp)
    double v;
    char *buf;
{
    char fmt[15];
    char temp[100];
    int i,ncommas;

    sprintf (fmt, "%%%d.%dlf", n, dp);
    sprintf (temp, fmt, v);
    strcpy (buf, temp);
    G_insert_commas (temp);
    ncommas = strlen (temp) - strlen(buf);
    for (i=0; i<ncommas && temp[i] == ' ';i++) {}
    strcpy (buf, temp+i);
}
