#include <string.h>
#include <stdlib.h>
#include "xdr.h"

db__send_string_array(a, count)
    dbString *a;
    int count;
{
    int i;
    int stat;

    stat = db__send_int (count);
    for (i = 0; stat==DB_OK && i < count; i++)
	stat = db__send_string (&a[i]);
    
    return stat;
}

/* note: dbString *a; ...(...,&a...) */

db__recv_string_array (a, n)
    dbString **a;
    int *n;
{
    int i,count;
    int stat;
    dbString *b;

    *n = 0;
    *a = NULL;
    stat = db__recv_int (&count);
    if (stat != DB_OK)
	return stat;
    if (count < 0)
    {
	db_protocol_error();
	return DB_PROTOCOL_ERR;
    }
    b = db_alloc_string_array(count);
    if (b == NULL)
	return DB_MEMORY_ERR;

    for (i = 0; i < count; i++)
    {
	stat = db__recv_string (&b[i]);
	if (stat != DB_OK)
	{
	    db_free_string_array(b, count);
	    return stat;
	}
    }
    *n = count;
    *a = b;
    return DB_OK;
}

db__send_string(x)
    dbString *x;
{
    XDR xdrs;
    int len;
    int stat;
    char *s;


    stat = DB_OK;

    s = db_get_string (x);
    if (s == NULL) s = "";  /* can't send a NULL string */
    len = strlen(s)+1;

    xdr_begin_send (&xdrs);
    if(!xdr_int (&xdrs, &len))
	stat = DB_PROTOCOL_ERR;
    else if(!xdr_string (&xdrs, &s, len))
	stat = DB_PROTOCOL_ERR;
    xdr_end_send (&xdrs);

    if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();
    return stat;
}

/*
 * db__recv_string (dbString *x)
 *  reads a string from transport
 *
 *  returns DB_OK, DB_MEMORY_ERR, or DB_PROTOCOL_ERR
 *    x.s will be NULL if error
 *
 * NOTE: caller MUST initialize x by calling db_init_string()
 */
db__recv_string(x)
    dbString *x;
{
    XDR xdrs;
    int len;
    int stat;
    char *s;

    stat = DB_OK;
    xdr_begin_recv (&xdrs);
    if(!xdr_int (&xdrs, &len) || len <= 0)  /* len will include the null byte */
    {
	stat = DB_PROTOCOL_ERR;
    }
    else
    {
	stat = db_enlarge_string (x, len);
    }
    s = db_get_string(x);
    if(stat == DB_OK && !xdr_string (&xdrs, &s, len))
	stat = DB_PROTOCOL_ERR;

    xdr_end_recv (&xdrs);
    if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();
    return stat;
}

db__send_Cstring(s)
    char *s;
{
    dbString x;

    db_init_string (&x);
    db_set_string_no_copy (&x, s);
    return db__send_string (&x);
}
