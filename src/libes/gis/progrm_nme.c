/**********************************************************************
 *
 *   char *
 *   G_program_name()
 *
 *   returns the current program name
 *
 **********************************************************************
 *
 *   G_set_program_name(name)
 *        char *name 
 *
 *   program name set to name (name will be returned by G_program_name
 *
 **********************************************************************/

static char *name = "?" ;

char *
G_program_name()
{
    return name;
}

G_set_program_name (s)
    char *s;
{
    char *G_store();

    name = G_store (s);
}
