/**********************************************************************
   expand.c     - expand (variables and object references) in value strings
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#include "xgen.h"


/***************************************************************
 * This function takes an incoming string and expands and $
 * variables it finds by attempting a getenv.
 * Variables that should be passed to the shell and not expanded
 * are denoted using two $'s ( e.g., $$VAR ).
 * Quoted double quotes '\"' are expanded to '"'.
 * Quoted square brackets '\[' and '\]' are expanded to '[' and ']',
 * Quoted n's and t's are expanded to newlines and tabs...
 * and no attempt is made to replace an object value.
 * If a variable doesn't exist it will simply remove it from the
 * string, e.g. if the string " echo $PREFIX/junk " is passed in
 * and PREFIX is NULL or undefined, then the expanded string would
 * be " echo /junk ".
 * It will only expand to max characters, then return truncation error
 * code.
 *
 * Returns:
 *            0 on success,
 *            1 if a variable doesn't exist
 *           -1 if max is exceeded
 **************************************************************/

int
ExpandString(string, max)
    char                           *string;     /* incoming string to expand */
    int                             max;        /* max characters to process */
{
    char                           *save,       /* for a copy of the incoming
                                                 * string */
                                    env[8192],   /* room for environment
                                                 * variables */
                                    obj[256],   /* room for object values */
                                    tableinfo[256],     /* room for table
                                                         * row,col specs */
                                   *savePtr,    /* ptr to copy string */
                                   *expandPtr,  /* ptr to incoming string */
                                   *envPtr,     /* ptr to the env string */
                                   *objPtr,     /* ptr to the obj string */
                                   *tablePtr,   /* ptr to the tableinfo
                                                 * string */
                                   *tmp,        /* temporary for getenv
                                                 * result */
                                   *getenv();   /* allusion to getenv
                                                 * function */
    int                             i,  /* used for length of result from
                                         * getenv */
                                    atchar,     /* counter of characters
                                                 * processed so far */
                                    status;     /* status variable */
    InterfaceObject                *object = NULL;

    /***************************************************************
     * make a copy of the incoming string
     **************************************************************/
    save = XtMalloc(max);
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
    while (*savePtr != NULL) {
        /***************************************************************
         * if we've used more than max chars return 1
         **************************************************************/
        if (atchar++ > max) {
            free(save);
            return (1);
        }
        /***************************************************************
         * if we  see a '\', check for a '"', in which advance  to '"'
         * if we  see a '\', check for a '[', in which advance past '['
         * if we  see a '\', check for a 'n','t', or 'b', in which advance 
	 * past them and insert newline, tab, or backspace
         * Gee, I realize that this is not real portable, but....
         **************************************************************/
        if (*savePtr == '\\' && *(savePtr + 1) == '\"')
            savePtr++;
        if (*savePtr == '\\' && (*(savePtr + 1) == '[' ||
                                   *(savePtr + 1) == ']')) {
            savePtr++;
            *expandPtr++ = *savePtr++;
        }
        if (*savePtr == '\\' && *(savePtr + 1) == 'n') {
	    savePtr++;
	    savePtr++;
	    *expandPtr++ = '\n';
	}
        if (*savePtr == '\\' && *(savePtr + 1) == 't') {
	    savePtr++;
	    savePtr++;
	    *expandPtr++ = '\t';
	}
        if (*savePtr == '\\' && *(savePtr + 1) == 'b') {
	    savePtr++;
	    savePtr++;
	    *expandPtr++ = '\b';
	}
        /***************************************************************
         * if we don't see a '$', or '[' character, copy into the expand string
         **************************************************************/
        if (*savePtr != '$' && *savePtr != '[') {
            *expandPtr++ = *savePtr++;
            /***************************************************************
             * else,
             **************************************************************/
        } else if (*savePtr == '[') {
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
            while (isspace(*savePtr))
                savePtr++;
            /***************************************************************
             * Then set objPtr to the object name and NULL terminate,
             * while advancing savePtr past the object name.
             * Pick up info in ('s if it exists.
             * Then advance past the ']' character.
              **************************************************************/
            while (isalpha(*savePtr) || isdigit(*savePtr) || 
		   (*savePtr == ':') || (*savePtr == '_'))
                *objPtr++ = *savePtr++;
            *objPtr = NULL;

            /***************************************************************
             * Advance past whitespace.
             **************************************************************/
            while (isspace(*savePtr))
                savePtr++;
            /***************************************************************
             * If we have a '(' save all info until the ')'
             **************************************************************/
            tableinfo[0] = NULL;
            if (*savePtr == '(') {
                savePtr++;
                tablePtr = tableinfo;
                while (*savePtr != ')')
                    *tablePtr++ = *savePtr++;
                *tablePtr = NULL;
                savePtr++;
            }
            /***************************************************************
             * Advance past whitespace.
             **************************************************************/
            while (isspace(*savePtr))
                savePtr++;
            /***************************************************************
             * savePtr should point to the ']' character advance past it.
             * If not then we have trouble.
             **************************************************************/
            tmp = NULL;
            if (*savePtr == ']') {
                savePtr++;
                /***************************************************************
                  * try to get the value of the object in question
                   **************************************************************/
		if ( UniqueObjectName(obj) &&  strchr(obj,':') == NULL ) {
		    object = IndexObjectByName(obj);
		} else {
		    char **tok;
		    Shell *shell;

		    if ( strchr(obj,':') == NULL ) {
			Boolean save = verbose;

			verbose = True;
			sprintf(errorbuf,"non-unique object [%s] requires shell:object specification", obj);
			XgenWarning("expand string",errorbuf);
			verbose = save;
		    } else {
			tok = (char **)Tokenize(obj,":");
			shell = (Shell *)IndexShell(tok[0]);
			if ( shell != NULL )
			    object = IndexObjectByNameInShell(shell, tok[1]);
		    }
                }
                if (NULL != object) {
                    if (object->type == TABLE && tableinfo[0] != NULL)
                        tmp = GetObjectValue(object, tableinfo);
                    else if (object->type != TABLE && tableinfo[0] != NULL)
                        XgenFatalError("expanding object",
                                 "found table specification in a non-table");
                    else
                        tmp = GetObjectValue(object, NULL);
                }
            }
            /***************************************************************
             * if NULL set status to 1 and continue expanding
              **************************************************************/
            if (NULL == tmp)
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
            if (atchar >= max) {
                free(save);
                return (1);
            }
            /***************************************************************
             * reset objPtr to point to beginning of the obj string
              **************************************************************/
            objPtr = obj;
            /***************************************************************
             * now, until we're done, copy the obj string into the expanded
             * string.
              **************************************************************/
            if (tmp != NULL)
                while (i--) {
                    *expandPtr++ = *objPtr++;
                }
        } else {
            /***************************************************************
             * check for $$, if found (and it's not $$ by itself) remove one
             * $ and store the rest. Pass $$ on. If we see ${ deal with the
	     * variable that should be there...
             **************************************************************/
            if (*(savePtr + 1) == '$') {        /* found $$ */
                if (isspace(*(savePtr + 2))) {  /* found $$ by itself */
                    /***************************************************************
                     * save both $'s in the expand string
                      **************************************************************/
                    *expandPtr++ = *savePtr++;
                    *expandPtr++ = *savePtr++;
                } else {
                    /***************************************************************
                     * advance past the first $ and save the other one.
                      **************************************************************/
                    savePtr++;
                    *expandPtr++ = *savePtr++;
                }
            } else if ( *(savePtr + 1) == '{' ){
		envPtr = env;
		savePtr++;
		savePtr++;
		while ((isalpha(*savePtr) || 
			isdigit(*savePtr) || 
			*savePtr == '_') && *savePtr != '}' )
		    *envPtr++ = *savePtr++;
		if ( *savePtr != '}' ) status = -1;
                *envPtr = NULL;
		savePtr++;
                tmp = getenv(env);
                if (NULL == tmp)
                    status = -1;
                else {
                    strcpy(env, tmp);
                    i = strlen(env);
                    atchar += i;
                    if (atchar >= max) {
                        free(save);
                        return (1);
                    }
                    envPtr = env;
                    while (i--) {
                        *expandPtr++ = *envPtr++;
                    }
                }

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
                while (isalpha(*savePtr) || isdigit(*savePtr) || (*savePtr == '_'))
                    *envPtr++ = *savePtr++;
                *envPtr = NULL;
                /***************************************************************
                 * try to get the value of the environment variable
                  **************************************************************/
                tmp = getenv(env);
                /***************************************************************
                 * if NULL set status to -1 and continue expanding
                  **************************************************************/
                if (NULL == tmp)
                    status = -1;
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
                    if (atchar >= max) {
                        free(save);
                        return (1);
                    }
                    /***************************************************************
                     * reset envPtr to point to beginning of the env string
                      **************************************************************/
                    envPtr = env;
                    /***************************************************************
                     * now, until we're done, copy the env string into the expanded
                     * string.
                      **************************************************************/
                    while (i--) {
                        *expandPtr++ = *envPtr++;
                    }
                }
            }
        }
        /***************************************************************
         * NULL terminate just in case we're done...
          **************************************************************/
        *expandPtr = NULL;
    }                           /* end of while */

    /***************************************************************
     * we're done, return status
      **************************************************************/
    free(save);
    return (status);
}
