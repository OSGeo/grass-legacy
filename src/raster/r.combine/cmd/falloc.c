char *
falloc(n, s, string) unsigned n, s ; char *string ;
{
    char *rval, *calloc() ;

    rval = calloc(n, s) ;

    if(!rval)
    {
	    cry_and_die(string) ;
    }
    else
    {
	    return(rval) ;
    }
}
