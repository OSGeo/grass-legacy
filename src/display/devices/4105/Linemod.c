#define ESC	/033
Linemod(type)
	char *type ;
{
	printf("%cMV%1d", (int)type) ;
}
