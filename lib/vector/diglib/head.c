/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes.
*               Update to GRASS 5.1 Radim Blazek.
*
* PURPOSE:      Lower level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <string.h>
#include "gis.h"
#include "Vect.h"

int
dig__write_head ( struct Map_info *Map )
{
    unsigned char buf[GRASS_V_DIG_HEAD_LENGTH];	
    int length;

    G_debug ( 1, "dig__write_head()");
    
    dig_set_cur_port (&(Map->head.port));
    fseek (Map->dig_fp, 0L, 0);

    
    buf[0] = GRASS_V_VERSION_MAJOR;
    buf[1] = GRASS_V_VERSION_MINOR;
    buf[2] = GRASS_V_EARLIEST_MAJOR;
    buf[3] = GRASS_V_EARLIEST_MINOR;

    buf[4] = Map->head.port.byte_order;
    buf[5] = Map->head.with_z;           
    
    /* bytes 1 - 6 */
    if (0 >= dig__fwrite_port_C ( buf, 6, Map->dig_fp)) 
	return (0);

    /* bytes 7 - 10 */
    G_debug ( 1, "write coor size (%ld) to head", Map->head.size);
    if (0 >= dig__fwrite_port_L ( &(Map->head.size), 1, Map->dig_fp)) 
	return (0);

    memset ( buf, 0, GRASS_V_DIG_HEAD_LENGTH );
    length = GRASS_V_DIG_HEAD_LENGTH - 10;
    
    if (0 >= dig__fwrite_port_C ( buf, length, Map->dig_fp))
        return (0);

    return (1);    
}


int
dig__read_head ( struct Map_info *Map )
{
    unsigned char buf[GRASS_V_DIG_HEAD_LENGTH];
    struct Port_info port;
    int length;

    fseek (Map->dig_fp, 0L, 0);
   
    if (0 >= dig__fread_port_C ( buf, 6, Map->dig_fp))
        return (0);

    Map->head.Version_Major = buf[0];
    Map->head.Version_Minor = buf[1];
    Map->head.Back_Major    = buf[2];
    Map->head.Back_Minor    = buf[3];

    Map->head.port.byte_order    = buf[4];
    Map->head.with_z        = buf[5];

    dig_init_portable ( &port, Map->head.port.byte_order);
    dig_set_cur_port (&port);

    if (0 >= dig__fread_port_L ( &(Map->head.size), 1, Map->dig_fp))
        return (0);
    
    length = GRASS_V_DIG_HEAD_LENGTH - 10;
    if (0 >= dig__fread_port_C ( buf, length, Map->dig_fp))
        return (0);
    
    return (1);
}



