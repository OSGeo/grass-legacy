/***********************************************************************

File		:	get_word.c
Function 	:	int GetWord (full_str, word)
Args		:	    char *full_str -- string to parse
			    char *word -- returned word
Author   	:	Frank Goodman
Last Revised 	:	27 September 1988

Abstract 	:	Function to extract the leftmost word from a string,
			eliminating both leading and trailing white space.
			The original full string is returned minus the
			extracted word string and leading white space.

Returns  	:	The length of the extracted string. 

***********************************************************************/
# include <stdio.h>
# include <strings.h>
# include <ctype.h>
# define STR_LEN 256

int GetWord(full_str, word)
    char    *full_str;
    char    *word;
    {
    char    full_buffer [STR_LEN];
    char    word_buffer [STR_LEN];
    char    *full_ptr;
    char    *word_ptr;

    strcpy (full_buffer, full_str);
    full_ptr = full_buffer;

    while ((isspace (*full_ptr)) && (*full_ptr != '\0'))    
    full_ptr++;                  

    if (*full_ptr == '\0')
    {
    *full_str = '\0';
    *word     = '\0';
    return (0);
    }
    word_ptr = word_buffer;         
    while (!(isspace (*full_ptr)) && (*full_ptr != '\0'))
    *word_ptr++ = *full_ptr++;
    *word_ptr = '\0';
    while ((isspace (*full_ptr)) && (*full_ptr != '\0'))
    full_ptr++;     
    strcpy (full_str, full_ptr);    
    strcpy (word,     word_buffer);
    return (strlen (word));
    }
