/* LIBDGL -- a Directed Graph Library implementation
 * Copyright (C) 2002 Roberto Micarelli
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/*
 * best view tabstop=4
 */

#ifndef _GN_HELPERS_H_
#define _GN_HELPERS_H_

#include "tree.h"

extern unsigned char * gngrp_mempush( unsigned char * pstack , long * istack , long size , void * pv );
extern unsigned char * gngrp_mempop( unsigned char * pstack , long * istack , long size );
extern void            gngrp_swapInt32Bytes( gnInt32_t * pn );
extern int gngrp_node_free( gnTreeNode_s * pnode , void * pv );
extern gnTreeNode_s * gngrp__node( gnTreeNode_s * ptree , gnInt32_t nodeid );

__END_DECLS
#endif
