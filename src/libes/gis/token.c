#include <stdlib.h>
#include "gis.h"
/* break buf into tokens. delimiters are replaced by NULLs
   and tokens array will point to varous locations in buf
   buf must not have a new line

   tokens = G_tokenize (string, delimstr);
   ntokens = G_number_of_tokens (tokens);
   G_free_tokens (tokens);


   NOTE G_free_tokens() must be called when you are finished with tokens to
   release the memory
*/

/*   Given buf,  turn delimiters in '\0'  and place pointers to tokens
 *      in  tokens.
 */

char **G_tokenize ( char *buf, char *delim)
{
	int i;
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
		tokens = (char **) G_realloc ((char *) tokens, (i+1) * sizeof (char *));

		while (*buf && (G_index(delim,*buf) == NULL))
			buf++;
		if (*buf == 0)
			break;
		*buf++ = 0;
	}
	tokens[i] = NULL;

	return (tokens);
}

int G_number_of_tokens(char **tokens)
{
	int n;

	for (n = 0; tokens[n] != NULL ; n++)
        {
         /* nothing */
        }
        return n;
}

int G_free_tokens (char **tokens)
{
    if (tokens[0] != NULL)
	free (tokens[0]);
    free (tokens);
    return (0);
}
