#include "xgen.h"


/***************************************************************
 * This function takes an incoming string and expands and $
 * variables it finds by attempting a getenv.
 * If a variable doesn't exist it will simply remove it from the
 * string, e.g. if the string " echo $PREFIX/junk " is passed in
 * and PREFIX is NULL or undefined, then the expanded string would
 * be " echo /junk ".
 * It will only expand to max characters.
 *
 * Returns:
 *			0 on success,
 *			1 if a variable doesn't exist
 *		   -1 if max is exceeded
 **************************************************************/

ExpandString(string, max)
	char *string;	/* incoming string to expand */
	int max;		/* max characters to process */
{
	char save[1024],	/* for a copy of the incoming string */
		 env[128],		/* room for environment variables */
		 obj[128],		/* room for object values */
		 tableinfo[128],/* room for table row,col specs */
		 *savePtr, 		/* ptr to copy string */
		 *expandPtr, 	/* ptr to incoming string */
		 *envPtr,		/* ptr to the env string */
		 *objPtr,		/* ptr to the obj string */
		 *tablePtr,		/* ptr to the tableinfo string */
		 *tmp,			/* temporary for getenv result */
		 *getenv();	/* allusion to getenv function */
	int	 i,				/* used for length of result from getenv */
		 atchar,		/* counter of characters processed so far */
		 status;		/* status variable */
	InterfaceObject *object;

/***************************************************************
 * make a copy of the incoming string
 **************************************************************/
	strncpy(save, string, max);

/***************************************************************
 * set pointers to beginning of each string
 **************************************************************/
	savePtr = save;
	expandPtr = string;

/***************************************************************
 * init char counter 
 **************************************************************/
	atchar = 0;
	status = 0;


/***************************************************************
 * while not at the end of the saved string
 **************************************************************/
	while(*savePtr != NULL) {
	/***************************************************************
	 * if we've used more than max chars return -1
	 **************************************************************/
		if (atchar++ > max) 
			return(-1);
	/***************************************************************
	 * if we don't see a '$', or '[' character, copy into the expand string
	 **************************************************************/
		if (*savePtr != '$' && *savePtr != '[' ) 
			*expandPtr++ = *savePtr++;
	/***************************************************************
	 * else, 
	 **************************************************************/
		else if (*savePtr == '[')  {
		/***************************************************************
		 * reset objPtr to point to beginning of the obj string
	 	 **************************************************************/
			objPtr = obj;
		/***************************************************************
		 * advance savePtr past the '['.
		 **************************************************************/
			savePtr++;
		/***************************************************************
		 * Advance past whitespace.
		 **************************************************************/
			while (isspace(*savePtr) ) savePtr++;
		/***************************************************************
		 * Then set objPtr to the object name and NULL terminate, 
		 * while advancing savePtr past the object name.
		 * Pick up info in ('s if it exists.
		 * Then advance past the ']' character.
	 	 **************************************************************/
			while(isalpha(*savePtr) || isdigit(*savePtr) || (*savePtr == '_'))
				*objPtr++ = *savePtr++;
			*objPtr = NULL;

		/***************************************************************
		 * Advance past whitespace.
		 **************************************************************/
			while (isspace(*savePtr) ) savePtr++;
		/***************************************************************
		 * If we have a '(' save all info until the ')'
		 **************************************************************/
			tableinfo[0] = NULL;
			if ( *savePtr == '(' ) {
				savePtr++;
				tablePtr = tableinfo;
				while( *savePtr != ')' ) 
					*tablePtr++ = *savePtr++;
				*tablePtr = NULL;
				savePtr++;
			}
		/***************************************************************
		 * Advance past whitespace.
		 **************************************************************/
			while (isspace(*savePtr) ) savePtr++;
		/***************************************************************
		 * savePtr should point to the ']' character advance past it.
		 * If not then we have trouble. 
		 **************************************************************/
			tmp = NULL;
			if ( *savePtr == ']' ) {
				savePtr++;
			/***************************************************************
		 	 * try to get the value of the object in question
	 	 	 **************************************************************/
				if ( NULL != (object = IndexObjectByName(obj))) {
					if ( object->type == TABLE &&  tableinfo[0] != NULL )
					    tmp = GetObjectValue(object,tableinfo);
					else if ( object->type != TABLE &&  tableinfo[0] != NULL )
						XgenFatalError("expanding object",
							"found table spcification in a non-table");
					else
					    tmp = GetObjectValue(object,NULL);
			    } 
			} 
		/***************************************************************
		 * if NULL set status to 1 and continue expanding
	 	 **************************************************************/
			if(NULL == tmp) 
				status = 1;
		/***************************************************************
		 * else copy it into the obj string
	 	 **************************************************************/
			else 
				strcpy(obj, tmp);

		/***************************************************************
		 * get the length of the obj string
	 	 **************************************************************/
			i = strlen(obj);
		/***************************************************************
		 * add to the char counter top the length of the obj string and
		 * check for over flow
	 	 **************************************************************/
			atchar += i;
			if (atchar >= max) 
				return(-1);
		/***************************************************************
		 * reset objPtr to point to beginning of the obj string
	 	 **************************************************************/
			objPtr = obj;
		/***************************************************************
		 * now, until we're done, copy the obj string into the expanded
		 * string.
	 	 **************************************************************/
			if ( tmp != NULL ) 
				while(i--)
					*expandPtr++ = *objPtr++;
		} else {
		/***************************************************************
		 * reset envPtr to point to beginning of the env string
	 	 **************************************************************/
			envPtr = env;
		/***************************************************************
		 * advance savePtr past the '$', and then set envPtr to the
		 * environment variable name and NULL terminate, while advancing
		 * savePtr past the environment variable name.
	 	 **************************************************************/
			savePtr++;
			while(isalpha(*savePtr) || isdigit(*savePtr) || (*savePtr == '_'))
				*envPtr++ = *savePtr++;
			*envPtr = NULL;
		/***************************************************************
		 * try to get the value of the environment variable
	 	 **************************************************************/
			tmp = getenv(env);
		/***************************************************************
		 * if NULL set status to 1 and continue expanding
	 	 **************************************************************/
			if(NULL == tmp) 
				status = 1;
		/***************************************************************
		 * else copy it into the env string
	 	 **************************************************************/
			else {
				strcpy(env, tmp);

			/***************************************************************
		 	* get the length of the env string
	 	 	**************************************************************/
				i = strlen(env);
			/***************************************************************
		 	* add to the char counter top the length of the env string and
		 	* check for over flow
	 	 	**************************************************************/
				atchar += i;
				if (atchar >= max) 
					return(-1);
			/***************************************************************
		 	* reset envPtr to point to beginning of the env string
	 	 	**************************************************************/
				envPtr = env;
			/***************************************************************
		 	* now, until we're done, copy the env string into the expanded
		 	* string.
	 	 	**************************************************************/
				while(i--)
					*expandPtr++ = *envPtr++;
			}
		}
		/***************************************************************
		 * NULL terminate just in case we're done...
	 	 **************************************************************/
		*expandPtr = NULL;
	} /* end of while */

	/***************************************************************
	 * we're done, return status
 	 **************************************************************/
	return(status);
}
