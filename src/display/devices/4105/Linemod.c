#define ESC	/033
Linemod(type)
	char *type ;
{
	fprintf (stdout,"%cMV%1d", (int)type) ;
}
