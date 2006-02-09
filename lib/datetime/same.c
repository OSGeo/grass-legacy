/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#include <grass/datetime.h>


static 
int isequal (char *src, char *dst, int n)
{
    while (n-- > 0)
	if(*dst++ != *src++)
	    return(0);
    return(1);
}


/*!
 * \brief 
 *
 * Returns:
 * 1 if 'src' is exactly the same as 'dst'
 * 0 if they differ   
 *
 *  \param src
 *  \param dst
 *  \return int
 */

 int 
datetime_is_same (DateTime *src, DateTime *dst)
{
    return( isequal ((char *)src, (char *)dst, sizeof(DateTime)) );
}
