/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.   The last part of the name can be parsed off and used as
 * a pointer name to the saved image.
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
