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

/* best view tabstop=4
 */

#ifndef _GN_TREE_H_
#define _GN_TREE_H_

__BEGIN_DECLS

typedef union _gnTreeData
{
	void * 			pv;
	int				n;
	unsigned int 	un;
	long			l;
	unsigned long 	ul;

} gnTreeData_u;

typedef struct _gnTreeNode
{
	long 					key;
	gnTreeData_u 			data , data2;
} gnTreeNode_s;

extern void 			gnTreeInitNode			(
												gnTreeNode_s * pnode , long key , gnTreeData_u data , gnTreeData_u data2
												);

/*
extern void 			gnTreeSetNode			(
												gnTreeNode_s * pnode , 
												long key , 
												gnTreeData_u data 
												);
*/

extern gnTreeNode_s * 	gnTreeNewNode			(
												long key ,
												gnTreeData_u data ,
												gnTreeData_u data2
												);

extern void * 			gnTreeCreate			(
												int (* pfnNodeCancel )( gnTreeNode_s * , void * ) ,
												void * pvcancel
												);

extern void				gnTreeDestroy			(
												void * pvtree
												);

extern gnTreeNode_s * 	gnTreeSearch 			(
												void * pvtree ,
												long key
												);

/*
extern int 				gnTreeInsert			(
												void * pvtree ,
												gnTreeNode_s *pnode
												);
*/
extern gnTreeNode_s *	gnTreeInsert			(
												void * pvtree ,
												gnTreeNode_s *pnode
												);

extern gnTreeNode_s * 	gnTreeCancel			(
												void * pvtree ,
												gnTreeNode_s * pnode
												);

__END_DECLS

#endif
