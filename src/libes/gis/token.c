/* break buf into tokens. delimiters are replaced by NULLs
   and tokens array will point to varous locations in buf
   buf must not have a new line

   tokens = G_tokenize (string, delimstr);
   G_free_tokens (tokens);


   NOTE G_free_tokens() must be called when you are finished with tokens to
   release the memory
*/
/*   Given buf,  turn delimiters in '\0'  and place pointers to tokens
 *      in  tokens.
 */
#include "gis.h"

char **
G_tokenize (buf, delim)
	char *buf;
	char *delim;
{
	int i;
	char *G_index();
	char **tokens;

	i = 0;
	while (*buf == ' ' || *buf == '\t')  /* needed for free () */
		buf++;

	buf = G_store (buf);

	tokens = (char **) G_malloc (sizeof (char *));

	while (1)
	{
		while (*buf == ' ' || *buf == '\t')
			buf++;
		if (*buf == 0)
			break;
		tokens[i++] = buf;
		tokens = (char **) G_realloc (tokens, (i+1) * sizeof (char *));

		while (*buf && (G_index(delim,*buf) == NULL))
			buf++;
		if (*buf == 0)
			break;
		*buf++ = 0;
	}
	tokens[i] = NULL;

	return (tokens);
}

G_free_tokens (tokens)
    char **tokens;
{
    if (tokens[0] != NULL)
	free (tokens[0]);
    free (tokens);
    return (0);
}
