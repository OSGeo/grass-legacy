/*
**                               
** Filename: misc.c
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

#include "symtab.h"
struct symtab *index_to_att();
struct symtab *index_to_val();


struct symtab *
index_to_att(ptr,type,init)
struct symtab *ptr;
int type;
int init;
{
    struct symtab *tmpptr;
    static struct symtab *lastfound;

    if ( ! init ) lastfound = (struct symtab *)0;
    tmpptr = ptr;
    while(tmpptr != (struct symtab *)0 ) {
        if(tmpptr->s_type == AttributeSymbol ) {
            if ( tmpptr->element.att->a_type == type && tmpptr != lastfound ) {
                lastfound = tmpptr;
                return(tmpptr);
            }
        }
        tmpptr = tmpptr->next;
    }
    return((struct symtab *)0);
}

struct symtab *
index_to_val(ptr,init)
struct symtab *ptr;
int init;
{
    struct symtab *tmpptr;
    static struct symtab *lastfound;

    if ( ! init ) lastfound = (struct symtab *)0;
    tmpptr = ptr;
    while(tmpptr != (struct symtab *)0 ) {
        if(tmpptr->s_type == AttributeSymbol ) {
            return((struct symtab *)0);
        }
        if(tmpptr->s_type == ValueSymbol ) {
            if ( tmpptr != lastfound ) {
                lastfound = tmpptr;
                return(tmpptr);
            }
        }
        tmpptr = tmpptr->next;
    }
    return((struct symtab *)0);
}

extern char *G_program_name();

nonfatal(s)
char *s;
{
    fprintf(stderr,"%s: %s",G_program_name(), s);
}

fatal(s)
char *s;
{
    fprintf(stderr,"%s: %s",G_program_name(), s);
    exit(0);
}

char *
strip_quotes(s)
char *s;
{
    static char temp[80];
    int len = strlen(s);
    int i;


    for ( i= 1 ; i < len - 1; i++ )
        temp[i - 1] = s[i];

    temp[i - 1] = '\0';
    return(temp);
}
