
/****************************************************
legal_names.c

legal_SDTS_file_prefix (prefix)
   char *prefix       SDTS prefix to be checked

     returns 0 if name is OK
             1 if not OK:  if it contains anything other than
                            upper-case char, digit, or '_'
                           OR if it more than 4 characters long. 

******************************************************/

legal_SDTS_file_prefix (s)
   char *s;
{
	if (strlen (s) != 4)
	   return 1;

    if (*s == '.' || *s == 0 || *s == '_')
    return 1;

    for ( ; *s; s++)
    {
    if (isalpha (*s))
    {
       if (islower (*s))
        return 1;
    }
    else
    if (!isdigit(*s) && *s != '_')
        return 1;
    }
    return 0;
}

