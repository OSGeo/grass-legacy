char *
falloc(n, s, string) unsigned n, s ; char *string ;
{
    char *rval, *calloc() ;

    rval = calloc(n, s) ;

    if(rval == 0)
        G_fatal_error(string) ;
    
    else
        return(rval) ;
}
