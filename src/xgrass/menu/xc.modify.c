#include "xc.xclip.h"

extern XclipGlobalData *zzGlobal;

long
StrTol(input,string)
char *input;
char *string;
{
    long value;
    char *remPtr;
    extern int zzlineno;

    value = strtol(input,&remPtr,10);
    if ( value == 0 && errno == EDOM ) {
        sprintf(errorbuf,"at line %d:\n",zzlineno);
        XCError("parsing modifier string",errorbuf, zzGlobal);
        sprintf(errorbuf,"\t%s value [%s]\n",
            string,input);
        perror(errorbuf);
	if (zzGlobal->standAlone)
            exit(1);
    }
    if ( remPtr == NULL ) {
        sprintf(errorbuf,"at line %d: extra characters [%s] ignored\n",
            zzlineno, remPtr);
        XCWarning("parsing modifier string",errorbuf);
    }
    return value;
}

XcModifierParse(modifier,type,s)
XCTypeModifier *modifier;
int type;
char *s;
{
    extern int zzlineno;
    char **token;
    int numTokens;
    int i;

    modifier->type = type;

    switch(type) {
    case XC_TYPE_ENUMERATE:
	token = _XgTokenize(s,",");
	numTokens = _XgNumberOfTokens(token);
        modifier->enumStrings = (char **)_XgCalloc(numTokens, sizeof(char *));
        modifier->enumKeys = (char **)_XgCalloc(numTokens, sizeof(char *));
	for ( i = 0; i < numTokens; i++) {
	    char **subTokens;
	    subTokens = _XgTokenize(token[i],":");
	    modifier->enumStrings[i] = _XgStrDup(subTokens[0]);
	    if ( subTokens[1] != NULL ) {
		modifier->enumKeys[i] = _XgStrDup(subTokens[1]);
	    }
	    _XgFreeTokens(subTokens);
	}
        modifier->enumNItems = numTokens;
	modifier->enumTable = (XmStringTable)_XgStringArray2XmStringTable(
	    modifier->enumStrings,modifier->enumNItems);
	break;
    case XC_TYPE_INTEGER:
        token = _XgTokenize(s,":");
        if ( _XgNumberOfTokens(token) != 3 ) {
            sprintf(errorbuf,"at line %d:\n",zzlineno);
            sprintf(errorbuf,"%s\tmissing element in modifier string [%s]\n",
                errorbuf,s);
            sprintf(errorbuf,"%s\tmust be in the form \"min:max:start\"\n",
                errorbuf);
            XCFatalError("parsing modifier string",errorbuf);
        }
        if ( token[0] == NULL )
            modifier->min= 0;
        else 
            modifier->min = StrTol(token[0],"minimum");

        if ( token[1] == NULL )
            modifier->max= 1;
        else 
            modifier->max = StrTol(token[1],"maximum");

        if ( token[2] == NULL )
            modifier->start = 0;
        else 
            modifier->start = StrTol(token[2],"start value");

        break;
    case XC_TYPE_DOUBLE:
        token = _XgTokenize(s,":");
        if ( _XgNumberOfTokens(token) != 4 ) {
            sprintf(errorbuf,"at line %d:\n",zzlineno);
            sprintf(errorbuf,"%s\tmissing element in modifier string [%s]\n",
                errorbuf,s);
            sprintf(errorbuf,"%s\tmust be in the form \"min:max:start:decimals\"\n",
                errorbuf);
            XCFatalError("parsing modifier string",errorbuf);
        }
        if ( token[0] == NULL )
            modifier->min= 0;
        else 
            modifier->min = StrTol(token[0],"minimum");

        if ( token[1] == NULL )
            modifier->max= 100;
        else 
            modifier->max = StrTol(token[1],"maximum");

        if ( token[2] == NULL )
            modifier->start = 0;
        else 
            modifier->start = StrTol(token[2],"start value");

        if ( token[3] == NULL )
            modifier->decimalPoints = 2;
        else 
            modifier->decimalPoints = StrTol(token[3],"decimal points");

        break;
    case XC_TYPE_LOGICAL:
        if ( G_index(s,':') == NULL ) {
            sprintf(errorbuf,"at line %d:\n",zzlineno);
            sprintf(errorbuf,"%s\tmissing ':' in modifier string [%s]\n",
                errorbuf,s);
            XCFatalError("parsing modifier string",errorbuf);
        }
        token = _XgTokenize(s,":");
        if ( _XgNumberOfTokens(token) != 2 || !strcmp(token[0],"")) {
            sprintf(errorbuf,"at line %d:\n",zzlineno);
            sprintf(errorbuf,"%s\tmissing element in modifier string [%s]\n",
                errorbuf,s);
            sprintf(errorbuf,"%s\tmust be in the form \"onString:offString\"\n",
                errorbuf);
            XCFatalError("parsing modifier string",errorbuf);
        }
        modifier->onString = token[0];
        modifier->offString = token[1];
        break;
    case XC_TYPE_FILENAME:
        if ( strcmp(s,"edit") ) {
            sprintf(errorbuf,"at line %d:\n",zzlineno);
            sprintf(errorbuf,"%s\tunknown in modifier string [%s]\n",
                errorbuf,s);
            sprintf(errorbuf,"%s\texpecting the keyword \"edit\" \n",
                errorbuf);
            XCFatalError("parsing modifier string",errorbuf);
        }
        modifier->editFile = True;
        break;
    }
}
