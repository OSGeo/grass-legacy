get_type()
{
    char buffer[256] ;

    for (;;)
    {
        G_clear_screen() ;
        printf("Please choose one of the following\n");
        printf("   A define an area\n") ;
        printf("   C define a circle\n") ;
        printf("   L define a line\n") ;
        printf("   Q quit (and create map)\n");
        printf("> ") ;
        if (!G_gets(buffer)) continue ;
        switch (buffer[0] & 0177)
        {
            case 'l': case 'L': return ('L') ;
            case 'a': case 'A': return ('A') ;
            case 'c': case 'C': return ('C') ;
            case 'q': case 'Q': return ('Q') ;
        }
    }
}
