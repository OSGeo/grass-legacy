/******************************************************************************
 * basename.h [v.in.shape]
 * Find a file's basename without the extension
 * 

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 26th. Feb. 2002
 * Last updated 26th. Feb. 2002
 *

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/

#ifndef BASENAME_H_
#define BASENAME_H_

typedef char extension_name_block[11];

typedef struct extension_name_controller_ {

  extension_name_block *extensions;
  int n_extensions;
  int alloc_extensions;
} ext_ctrl;


/* Prototypes */
int get_file_basename(char *, char *, char *);
int get_file_pathdir(char *, char *);
ext_ctrl *ext_ctrl_init(void);
void ext_ctrl_destroy(ext_ctrl *);

#endif  /* BASENAME_H_  */
