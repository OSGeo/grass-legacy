/******************************************************************************
 * category.h (declaration of category structure)
 * 

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 19th. Feb. 2002
 * Last updated 19th. Feb. 2002
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

#ifndef CATEGORY_STRUCT_H_
#define CATEGORY_STRUCT_H_

#define Get_Cat_Offset( C_STRUCT, A , B_PTR) (B_PTR) = (C_STRUCT)->cats[(C_STRUCT)->cat_osets[(A)]]

#define CATS_AS_ASCII 0
#define CATS_AS_BINARY 1
#define CATS_INITIALISED 1
#define CATS_WITH_VALUES 2

#define INTEGER_CAT 0
#define REAL_CAT 1
#define COMPLEX_CAT 2
#define CHAR_CAT 3
#define DATE_CAT 4
#define MISC_CAT 5
#define BOOL_CAT 6

/* struct category_struct_; */

typedef char fieldname[64];

typedef struct category_struct_  {

  char struct_status;

  int n_cats; /* This will always be the precise value */
  int as_ascii;

  fieldname *cat_names;
  char *cat_types;
  int *cat_osets;
  char *cats;
} category_ctrl;


/* Prototypes */

category_ctrl *category_ctrl_init(void);
void category_ctrl_destroy(category_ctrl *);

#endif  /* CATEGORY_STRUCT_H_  */

