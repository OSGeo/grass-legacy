/*******************************************************************************
*	trim    	(see manual, Internal Functions)            	<in>   *
*	      		Trims off leading and trailing white space             *
*			and control and extension characters from character    *
*                       string.                                                *
*                                                                              *
*			Returns the trimmed string.                            *
*                       JPD 3/13/89                                            *
*******************************************************************************/

#include <string.h>
#include <stdio.h>

char *trim( str )
	char *str;
{
	long i, j, k, length;

	if(str == (char *) 0)
		return(str);

	length = strlen(str);

	/*    identify the first printable character (i)                      */
	for(i=0; (i<length)  &&  ((str[i]<33) || (str[i]>126)); i++);

	/*    identify the last printable character  (j)          	      */
	for(j=length; (j>0)  &&  ((str[j]<33) || (str[j]>126)); j--);

	/*    compute length of trimmed string       (length)                 */
	if(length>0) 
		length = j-i+1;

	/*    move printable characters to beginning of string                */
	if(length > 0) 
		for(k=0; k<length  ;k++)     
			str[k]=str[i+k];
	else                
		length = 0;

	/* 	append a NULL (0) to the end of the trimmed string characters */
	str[length] = (char) NULL;

	/*	return the trimmed string				      */
	return(str);
}
/*******************************************************************************
*       lowerc          (see manual, Internal Functions)                <in>   *
*                 This program changes all occurances of upper case letters    *
*                 in a string to lower case letters.  All other characters     *
*                 are unaffected.                                              *
*                                                                              *
*                 Only the string passed is changed.                           *
*                                                                              *
*                 On completion, the string pointer is returned.               *
*                                                                              *
*                      j.dabritz  3/7/89 - 5/8/89                              *
*******************************************************************************/

char *lowerc(string)
        char *string;
{
        long i;

                /*    test for null pointer          */
        if(string == (char *) 0)
                return(string);

                /*    convert string to lower case   */
        for(i=0; string[i] != 0 ; i++)
                if((string[i] >= 65) && (string[i] <= 90))
                        string[i] = string[i] + 32;

                /*    return string                   */
        return(string);
}

/*******************************************************************************
*       strsplit        (see manual, Internal Functions)                <in>   *
*                 This program splits a string into 2 parts divided at         *
*                 the first occurrance of a single delimiter character.        *
*                 Only the passed parameters are affected.                     *
*                                                                              *
*                 It returns a 0 if no delimiter is found, 1 if the delimiter  *
*                 is found on one end of the string, and 2 if it is found      *
*                 somewhere in the middle of the string.                       *
*                      j.dabritz  3/7/89 - 5/8/89                              *
*******************************************************************************/
#include <string.h>
#include <stdio.h>

long strsplit(ref,delim,part1,part2)
	char ref[];
	char delim;
	char part1[];
	char part2[];
{
	char *delimptr;
	long rtn;

		/*   initialize return value                   */
	rtn = 0;

		/*   check for legitimate parameters           */
	if(!ref || !part1 || !part2 || !delim)
		return(rtn);

		/*   initialize returned strings               */
	*part1 = 0;                /*  set to null  */
	*part2 = 0;                /*  set to null  */
	strcpy(part1, ref);

		/*   get pointer to delimeter                  */
	delimptr = strchr(part1, delim);

		/*   check to see if delim found               */
	if(delimptr)
	{		/*   split string                              */
		delimptr[0] = (char) NULL;
		delimptr++;
		strcpy(part2, delimptr);
	
			/*   check to see if both parts have non=zero length  */
		if(strlen(part1) && strlen(part2))
			rtn = 2;
		else
			rtn = 1;
	}
	
		/*  return                                      */
	return(rtn);

}
