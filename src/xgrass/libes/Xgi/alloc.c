static char rcsid[] = "@(#)XGRASS $Id: alloc.c,v 0.0 1992/05/05 14:56:08 sink Exp sink $";
/*
 * File: alloc.c
 *
 * Desc: error checking malloc, calloc, and free routines
 *
 * Auth: Kurt Buehler
 *
 * Date: Tue Nov  5 16:21:47 CST 1991
 *
 * Modification History:
 *
 *
 */
#include "xgrass_lib.h"

#ifdef _NO_PROTO
char *
_XgMalloc(s)
int s;
#else
char *
_XgMalloc(int s)
#endif
{
    char *ret;

    if ( s ) {
        ret = malloc(s);
        if ( ret == NULL ) {
            fprintf(stderr,"can not perform malloc\n");
            exit(1);
        }
        return ret;
    }
    return NULL;
}


#ifdef _NO_PROTO
char *
_XgCalloc(num, s)
int num;
int s;
#else
char *
_XgCalloc(int num, int s)
#endif
{
    char *ret;

    if ( s && num ) {
        ret = (char *) calloc(num, s);
        if ( ret == NULL ) {
            fprintf(stderr,"can not perform calloc\n");
            exit(1);
        }
        return ret;
    }
    return NULL;
}

#ifdef _NO_PROTO
char *
_XgRealloc(ptr, size)
char *ptr;
int size;
#else
char *
_XgRealloc(char *ptr, int size)
#endif
{
    char *ret;

    if ( ptr == NULL ) return _XgMalloc(size);
    ret = realloc(ptr, size);
    if ( ret == NULL ) {
        fprintf(stderr,"can not perform realloc\n");
        exit(1);
    }
    return ret;

}

#ifdef _NO_PROTO
void
_XgFree(ptr)
char *ptr;
#else
void
_XgFree(char *ptr)
#endif
{
    if ( ptr == NULL ) return;
    free(ptr);
}
