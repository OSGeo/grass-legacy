/*
 ***********************************************************************
 * prompt for vector files (digit)
 *
 *   char *
 *   G_ask_vector_new(prompt, name)) 
 *       asks user to input name of a new vector file
 *
 *   char *
 *   G_ask_vector_old(prompt, name) 
 *       asks user to input name of an existing vector file
 *
 *   char *
 *   G_ask_vector_any(prompt, name)
 *       asks user to input any vector name
 *
 *   char *
 *   G_ask_vector_in_mapset(prompt, name)
 *       asks user to input name of an existing vector file
 *       in current mapset
 *
 *   parms:
 *      char *prompt    optional prompt for user
 *      char *name      buffer to hold name of map found
 *
 *   returns:
 *      char *pointer to a string with name of mapset
 *       where file was found, or NULL if not found
 **********************************************************************/

char *
G_ask_vector_new (prompt,name)

	char *prompt;
	char *name;
{
	char *G_ask_new();

	return G_ask_new (prompt, name, "dig", "vector");
}

char *
G_ask_vector_old (prompt,name)

	char *prompt;
	char *name;
{
	char *G_ask_old();

	return G_ask_old (prompt, name, "dig", "vector");
}

char *
G_ask_vector_any (prompt,name)

	char *prompt;
	char *name;
{
	char *G_ask_any();

	return G_ask_any (prompt, name, "dig", "vector", 1);
}

char *
G_ask_vector_in_mapset (prompt,name)

	char *prompt;
	char *name;
{
	char *G_ask_in_mapset();

	return G_ask_in_mapset (prompt, name, "dig", "vector");
}
