/******************************************************************************
 * basename.c [v.in.shape]
 * Find a file's basename without the extension
 * 

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 26th. Feb. 2002
 * Last updated 27th. Feb. 2002
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "basename.h"


int get_file_basename(char *retstr, char *instr, char *exts0) {

  /* Dimension for up to 40 extensions, of up to 10 characters each */


  ext_ctrl *ec1;
  char exts1[64] = "";
  int n_exts = 1, no_dot = 0, with_ext = 0, with_path = 0;
  char *chptr1, *chptr2, *chptr3, teststr[512];
  char test_ext[11] = "";

  /* loop */
  int ia;


  ec1 = ext_ctrl_init();
  if(ec1 == NULL) return(-1);

  /* Tokenise */

  strncpy(exts1, exts0, 63);

  chptr1 = strtok(exts1, ":");
  strncpy(ec1->extensions[0], chptr1, 10);

  while( chptr1 = strtok(NULL, ":") ) {
    strncpy(ec1->extensions[n_exts++], chptr1, 10);
  }

  ec1->n_extensions = n_exts;

  /* Try and extract an extension (just 1) */

  strcpy(teststr, instr);

  /* Find the right-most '.' */

  chptr2 = strrchr(teststr, '.');

  if(chptr2 == NULL) {
    /* There was no dot */
    no_dot = 1;
  }

  if(!no_dot) {

    /* Check that there is no '/' after the dot, if there is again we have no dot */
    chptr3 = chptr2;
    
    while(*(++chptr3) != '\0' ) {
      if(*chptr3 == '/' || *chptr3 == '\\') {
	no_dot = 1;
	/* DON'T - break; - go to the end to find the null terminator */
      }
    }
  }

  /* So there is an extension - possibly */

  if(!no_dot) {
    strncpy(test_ext, chptr2 + 1, 10);

    /* Check this string - case insensitive - against each provided extension */
    for(ia = 0; ia < ec1->n_extensions; ia++) {
      if(!strncasecmp(test_ext, ec1->extensions[ia], 10)) {
	/* Found a matching extension - so we will remove it */
	with_ext = 1;
	*chptr2 = '\0'; /* Null terminate on the extension dot */
	break;
      }

      /* else do not change the string */
      chptr2 = chptr3;
    }
  }

  /* Step back from the null terminator to find the first '/'
     or the start of the string
  */

  chptr1 = &teststr[0];
  chptr3 = chptr2;

  while(chptr3 > chptr1) {
    if(*(--chptr3) == '/') {
      strcpy(retstr, chptr3 + 1);
      with_path = 1;
      break;
    }
  }

  if(!with_path) {
    strcpy(retstr, chptr1);
  }

  ext_ctrl_destroy(ec1);

  return(0);
  
}


int get_file_pathdir(char *retstr, char *instr) {

  char *chptr1, workstr[512];

  strncpy(workstr, instr, 511);

  chptr1 = strrchr(workstr, '/');
  if(chptr1) {
    *(chptr1 + 1) = '\0';
    strcpy (retstr, workstr);
    return 0;
  }

  chptr1 = strrchr(workstr, '\\');
  if(chptr1) {
    *(chptr1 + 1) = '\0';
    strcpy (retstr, workstr);
    return 0;
  }

  strcpy(retstr, workstr);
  return 1; /* No path, so the string is just copied */
}



ext_ctrl *ext_ctrl_init( void ) {

  ext_ctrl *ect1;

  ect1 = (ext_ctrl *) malloc( sizeof(ext_ctrl) );
  if(ect1 == NULL) return (NULL);

  ect1->extensions = (extension_name_block *) malloc( 40 * sizeof(extension_name_block) );
  if(ect1->extensions == NULL) {
    free(ect1);
    return(NULL);
  }

  ect1->n_extensions = 0;
  ect1->alloc_extensions = 40;

  return ect1;

}


void ext_ctrl_destroy(ext_ctrl *ec0) {
  free(ec0->extensions);
  free(ec0);
}

