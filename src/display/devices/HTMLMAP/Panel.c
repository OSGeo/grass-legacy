/* 
 * HTMLMAP doesn't do panels
 */

int Panel_save (char *name, int top, int bottom, int left, int right)
{return 0;}

/* The saved panel associated with "name" is restored. */
int Panel_restore (char *name)
{return 0;}

/* The saved panel associated with "name" is deleted. */
int Panel_delete (char *name)
{return 0;}
