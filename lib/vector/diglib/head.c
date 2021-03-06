/*
 ****************************************************************************
 *
 * MODULE:       Vector library 
 *              
 * AUTHOR(S):    Original author CERL, probably Dave Gerdes.
 *               Update to GRASS 5.7 Radim Blazek.
 *
 * PURPOSE:      Lower level functions for reading/writing/manipulating vectors.
 *
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *              License (>=v2). Read the file COPYING that comes with GRASS
 *              for details.
 *
 *****************************************************************************/
#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>

int dig__write_head(struct Map_info *Map)
{
    unsigned char buf[10];
    long length = GV_COOR_HEAD_SIZE;

    G_debug(1, "dig__write_head()");

    dig_set_cur_port(&(Map->head.port));
    dig_fseek(&(Map->dig_fp), 0L, 0);

    /* bytes 1 - 5 */
    buf[0] = Map->head.Version_Major;
    buf[1] = Map->head.Version_Minor;
    buf[2] = Map->head.Back_Major;
    buf[3] = Map->head.Back_Minor;

    buf[4] = Map->head.port.byte_order;
    if (0 >= dig__fwrite_port_C(buf, 5, &(Map->dig_fp)))
	return (0);

    /* bytes 6 - 9 : header size */
    if (0 >= dig__fwrite_port_L(&length, 1, &(Map->dig_fp)))
	return (0);

    /* byte 10 : dimension 2D or 3D */
    buf[0] = Map->head.with_z;
    if (0 >= dig__fwrite_port_C(buf, 1, &(Map->dig_fp)))
	return (0);

    /* bytes 11 - 14 : size of coordinate file */
    G_debug(1, "write coor size (%ld) to head", Map->head.size);
    if (0 >= dig__fwrite_port_L(&(Map->head.size), 1, &(Map->dig_fp)))
	return (0);

    G_debug(2, "coor body offset %ld", dig_ftell(&(Map->dig_fp)));
    return (1);
}


int dig__read_head(struct Map_info *Map)
{
    unsigned char buf[10];
    struct Port_info port;

    dig_fseek(&(Map->dig_fp), 0L, 0);

    /* bytes 1 - 5 */
    if (0 >= dig__fread_port_C(buf, 5, &(Map->dig_fp)))
	return (0);
    Map->head.Version_Major = buf[0];
    Map->head.Version_Minor = buf[1];
    Map->head.Back_Major = buf[2];
    Map->head.Back_Minor = buf[3];
    Map->head.port.byte_order = buf[4];

    G_debug(2,
	    "Coor header: file version %d.%d , supported from GRASS version %d.%d",
	    Map->head.Version_Major, Map->head.Version_Minor,
	    Map->head.Back_Major, Map->head.Back_Minor);

    G_debug(2, "  byte order %d", Map->head.port.byte_order);

    /* check version numbers */
    if (Map->head.Version_Major > GV_COOR_VER_MAJOR ||
	Map->head.Version_Minor > GV_COOR_VER_MINOR) {
	/* The file was created by GRASS library with higher version than this one */

	if (Map->head.Back_Major > GV_COOR_VER_MAJOR ||
	    Map->head.Back_Minor > GV_COOR_VER_MINOR) {
	    /* This version of GRASS lib is lower than the oldest which can read this format */
	    G_fatal_error
		("Vector 'coor' format version %d.%d is not supported by this version of GRASS. "
		 "Update your GRASS.", Map->head.Version_Major,
		 Map->head.Version_Minor);
	    return (-1);
	}

	G_warning
	    ("Your GRASS version does not fully support vector format %d.%d."
	     " Consider to upgrade GRASS.", Map->head.Version_Major,
	     Map->head.Version_Minor);
    }

    dig_init_portable(&port, Map->head.port.byte_order);
    dig_set_cur_port(&port);

    /* bytes 6 - 9 : header size */
    if (0 >= dig__fread_port_L(&(Map->head.head_size), 1, &(Map->dig_fp)))
	return (0);
    G_debug(2, "  header size %ld", Map->head.head_size);

    /* byte 10 : dimension 2D or 3D */
    if (0 >= dig__fread_port_C(buf, 1, &(Map->dig_fp)))
	return (0);
    Map->head.with_z = buf[0];
    G_debug(2, "  with_z %d", Map->head.with_z);

    /* bytes 11 - 14 : size of coordinate file */
    if (0 >= dig__fread_port_L(&(Map->head.size), 1, &(Map->dig_fp)))
	return (0);
    G_debug(2, "  coor size %ld", Map->head.size);

    /* Go to end of header, file may be written by new version of GRASS with longer header */

    dig_fseek(&(Map->dig_fp), Map->head.head_size, SEEK_SET);

    return (1);
}
