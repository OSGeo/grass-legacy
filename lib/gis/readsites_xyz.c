/* ========================================================================== *
 * G_readsites_xyz(): New implementation of the readsites() library           *
 * function to read an array of xyz and cat values.                           *
 * ========================================================================== *
 * Copyright (c) 2000 Eric G. Miller <egm2@jps.net>                           *
 * -------------------------------------------------------------------------- *
 * This program is free software; you can redistribute it and/or modify       *
 * it under the terms of the GNU General Public License as published by       *
 * the Free Software Foundation; either version 2 of the License, or          *
 * (at your option) any later version.                                        *
 *                                                                            *
 * This program is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *
 * GNU General Public License for more details.                               *
 *                                                                            *
 * You should have received a copy of the GNU General Public License          *
 * along with this program; if not, write to the Free Software                *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 * -------------------------------------------------------------------------- *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "gis.h"
#include "glocale.h"
#include "site.h"


/*!
 * \brief 
 *
 * Allocate an array of <tt>SITE_XYZ</tt> with size <tt>num</tt>.
 *
 *  \param num
 *  \return SITE_XYZ * 
 */

SITE_XYZ *G_alloc_site_xyz(size_t num)
{
	SITE_XYZ *xyz;

	xyz = (SITE_XYZ *) G_malloc(sizeof(SITE_XYZ) * num);
	if (xyz == NULL)
		return NULL;
	memset(xyz, 0, sizeof(SITE_XYZ) * num);
	return xyz;
}


/*!
 * \brief 
 *
 * Free a previously allocated array of <tt>SITE_XYZ</tt>.
 *
 *  \param xyz
 *  \return void
 */

void G_free_site_xyz(SITE_XYZ *theSites)
{
	G_free(theSites);
}


/*!
 * \brief 
 *
 * Read a chunk of a site file into a 
 * <tt>SITE_XYZ</tt> array setting the <b>series Z </b> dimension
 * from the specified attribute. The <tt>fdsite</tt> parameter is
 * the <tt>FILE *</tt> for the sites file; <tt>type</tt> is the
 * attribute type to use for the <tt>z</tt> variable value; the
 * index is the 1--based index value for the attribute; the
 * <tt>size</tt> is the size of the <tt>SITE_XYZ</tt> array passed
 * to the function; the <tt>region</tt> is a pointer to a 
 * <tt>struct Cell_head</tt> for the current region or <tt>NULL</tt>;
 * and, finally, <tt>xyz</tt> is a pointer to an array of 
 * <tt>SITE_XYZ</tt> which will be populated. The return value is
 * the number of records read or <tt>EOF</tt>.
 *
 *  \param fdsite
 *  \param type
 *  \param index
 *  \param size
 *  \param region
 *  \param xyz
 *  \return int
 */

int G_readsites_xyz( 
	FILE * fdsite,   /* The FILE stream to the sites file               */
	int    type,     /* Attribute type: SITE_COL_DIM, etc...            */
	int    index,    /* The field index (1 based) for the attribute     */
	int    size,     /* Size of the array                               */
	struct Cell_head *region,   /* Respect region if not NULL */
	SITE_XYZ *xyz    /* The site array of size 'size'                   */
	)
{
	int i, strs, dims, dbls;
	RASTER_MAP_TYPE map_type;
	Site *s;
	long fdsave;
	char *end_ptr;

	/* If fdsite is EOF or NULL return EOF */
	if (fdsite == NULL || feof(fdsite)) {
		return EOF;
	}

	/* Save file position for seek after G_site_describe */
	fdsave = ftell(fdsite);
	rewind(fdsite);
	
	/* note: G_site_describe only reads first record to guess format */
	if (G_site_describe (fdsite, &dims, &map_type, &strs, &dbls) !=0) {
		G_fatal_error(_("Unable to guess site format!"));
	}
	s = G_site_new_struct (map_type, dims, strs, dbls);
	dims = dims - 2;

	/* Restore file position */
	fseek(fdsite, fdsave, SEEK_SET);
	
	/* Check 'type' and 'index' */
	index -= 1;
	
	switch (type) {
		case SITE_COL_DIM: /* Use n-dimensions */
			if (dims == 0) {
				G_fatal_error(_("No n-dims in site_list"));
			}
			else if (index >= dims) {
				G_fatal_error(_("Dimension index out of range"));
			}
			break;
		case SITE_COL_DBL: /* Use double attribute */
			if (dbls == 0) {
				G_fatal_error(_("No double attributes in site_list"));
			}
			else if (index >= dbls) {
				G_fatal_error(_("Double attribute index out of range"));
			}
			break;
		case SITE_COL_STR: /* Use string attribute */
			if (strs == 0) {
				G_fatal_error(_("No string attributes in site_list"));
			}
			else if (index >= strs) {
				G_fatal_error(_("String attribute index out of range"));
			}
			break;
		case SITE_COL_NUL: /* Doesn't want a z-dim */
			break;
		default:
			/* Die miserable death due to bad call */
			G_fatal_error(_("Unknown attribute type in call to "
				"G_readsites_xyz()!\n"));
	}

	for (i = 0; i < size; ++i){
		/* Read next site */
		if (G_site_get (fdsite, s) != 0) {
			if (i == 0) {
				G_site_free_struct(s);
				return EOF;
			}
			else {
				G_site_free_struct(s);
				return i;
			}
		}
		/* Check if in region */
		if(region && !G_site_in_region(s, region)) {
			i--;
			continue;
		}

		/* Do 'z' based on 'type' and 'index' */
		switch(type) {
			case SITE_COL_DIM: /* Z-dim */
				xyz[i].z = s->dim[index]; break;
			case SITE_COL_DBL: /* Dbl attribute */
				xyz[i].z = s->dbl_att[index]; break;
			case SITE_COL_STR: /* String Attribute */
                                errno = 0;
				xyz[i].z = strtod(s->str_att[index], &end_ptr);
				if (end_ptr == s->str_att[index] || errno == ERANGE) {
					G_fatal_error(_("Failed to convert string attribute."));
				}
				break;
			case SITE_COL_NUL: /* No z-dim requested */
				break;
			default: /* Programming error, die miserably */
				G_fatal_error("G_readsites_xyz(): fatal programmer error!\n");
		}
	      	xyz[i].x = s->east;
	      	xyz[i].y = s->north;
		xyz[i].cattype = s->cattype;
		switch(s->cattype) {
			case CELL_TYPE:
				xyz[i].cat.c = s->ccat; break;
			case FCELL_TYPE:
				xyz[i].cat.f = s->fcat; break;
			case DCELL_TYPE:
				xyz[i].cat.d = s->dcat; break;
			default: /* No cat */
				break;
		}
	   
	}

	G_site_free_struct(s);

	return i;
}


