/*	This function sets the flag to indicate whether a layer has 	*/
/*	to be displayed or deleted.					*/

struct window *display_flag(ptr)

	struct window *ptr;
{
	extern int display_delete_flag;

	display_delete_flag = 1;
	return(ptr);
}


struct window *delete_flag(ptr)

	struct window *ptr;

{
	extern int display_delete_flag;
	display_delete_flag = 0;
	return(ptr);
}


