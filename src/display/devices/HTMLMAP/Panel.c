/* 
 * HTMLMAP doesn't do panels
 */

Panel_save(name, top, bottom, left, right)
	char *name ;
{}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
	char *name ;
{}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
	char *name ;
{}
