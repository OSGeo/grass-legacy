
/*  @(#)rindex.c	1.0  6/26/87 
*   Created by:  CERL, original in mapdev/dlg_to_bdlg
*
*	there is no rindex() in STRINGS(3) in system V (att).
*	the calling  function should check to make sure the returning value
*	isn't false or an almost guaranted core dump will result.
*/

char	*
rindex(s, c)
	char	*s,  c ;
{
	register char	*p ;
	char	*t ;

	t = 0;
	for ( p=s ;  *p ; p++ )
		if( *p == c)
			t = p ;

	return (t) ;

}
