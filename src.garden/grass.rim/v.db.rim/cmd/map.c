#define MAP

#include "gis.h"
#include "globals.h"
#include "parse.h"
#include "rim.h"

/* This function adds a vector map name to the table of reference maps.
   The input format is: .map id# map_name  */
map(inp_buf)
     char *inp_buf;
{
  char buffer[INP_SIZ], *p;
  int pos, delete;
  int id_num, old_id;
  char *map_mapset, map_name[MAP_NAME_LENGTH+1];
  char old_name[MAP_NAME_LENGTH+1], old_mapset[MAP_NAME_LENGTH+1];
  char textbuf[100], tempstr[100];

  strcpy(buffer, inp_buf);
  G_squeeze(buffer);

  /* if it is just .map then list the maps */
  if (!strcmp(buffer, ".map")) {
    maps();
    return;
  }

  /* can only do options if not read only data base */
  if(parser(P_INST_RET_MODE,"")==TRUE) {
    fprintf(Outfile,
    "Warning: .map options not available with a read-only data base.\n");
    return;
  }

  pos = strcspn(buffer, " ");
  p = buffer+pos+1;

  if (pos==strlen(buffer)) {
    G_warning("No reference map information provided.  No action taken.");
#ifdef DBVECT
    SLEEP3;
#endif
    return(0);
  }

  /* check for the -d option */
  if (*p == '-' && *(p+1) == 'd') {
    delete = 1;
    p+=3;
  }
  else delete = 0;

  /* get the id number */
  if (sscanf(p, "%d", &id_num)!=1) {
    G_warning("No id number for vector map provided.  No action taken.");
#ifdef DBVECT
    SLEEP3;
#endif
    return(0);
  }

  if (!delete) { /* to note delete (to add) need map name and mapset */
    /* get the vector map name */
    p += strcspn(p, " ");
    if (*p != '\0') {
      pos = strcspn(p+1," ");
      if (pos > MAP_NAME_LENGTH) {
        G_warning("Map name too long.  No action taken.");
#ifdef DBVECT
        SLEEP3;
#endif
        return(0);
      }
      strncpy(map_name, p+1, pos);
      map_name[pos]='\0';
    }
    else {
      G_warning("No vector map name was provided.  No action taken.");
#ifdef DBVECT
      SLEEP3;
#endif
      return (0);
    }

    /* get mapset for this map */
    map_mapset = G_find_vector(map_name, "");
    if (map_mapset == NULL) {
      sprintf(tempstr, "Unable to find any vector map with the name: %s.  \nNo action taken.", map_name);
      G_warning(tempstr);
#ifdef DBVECT
      SLEEP3;
#endif
      return(0);
    }
  }

  if (delete) { /* delete the reference map from the table */
    sprintf(textbuf, "select from referencemaps where map_id = %d", id_num);
    if (crim(MAP_TABLE, textbuf) == RIM_EOT) {
      sprintf(tempstr, "Map id number (%d) does not exist in the database.  Not deleted.", id_num);
      G_warning(tempstr);
#ifdef DBVECT
      SLEEP3;
#endif
      return(0);
    }
    else { /* found the ref map to delete */
      crimdm_w_err(MAP_TABLE, GET, Rim_buffer);
      ret_m_table(Rim_buffer, &old_id, old_name, old_mapset);
      fprintf(Outfile, "Deleting Reference Map: %-10d %20s %20s\n", old_id,
              old_name, old_mapset);
      if (!my_G_yes(Infile, Outfile,
                    "Do you really want to delete this Reference Map", 0))
        {
          G_warning("Reference Map NOT deleted...");
#ifdef DBVECT
          SLEEP3;
#endif
          return(0);
        }
      crimdm_w_err(MAP_TABLE, DEL, Rim_buffer);
    }
  }
  else {
    /* check to see if a ref map with this number already exists */
    sprintf(textbuf, "select from referencemaps where map_id = %d", id_num);
    if (crim(MAP_TABLE, textbuf) != RIM_EOT) {
      sprintf(tempstr, "Map id number (%d) duplicated one in the database.", id_num);
      G_warning(tempstr);
      crimdm_w_err(MAP_TABLE, GET, Rim_buffer);
      ret_m_table(Rim_buffer, &old_id, old_name, old_mapset);
      fprintf(Outfile, "Replacing Reference Map: %-10d %20s %20s\n", old_id,
              old_name, old_mapset);
      fprintf(Outfile, "With New Reference Map:  %-10d %20s %20s\n", id_num,
              map_name, map_mapset);
      if (!my_G_yes(Infile, Outfile,
                    "Do you really want to replace this Reference Map", 0))
        {
          G_warning("Reference Map NOT replaced...");
#ifdef DBVECT
          SLEEP3;
#endif
          return(0);
        }
    }

    /* add this entry to the referencemaps table */
    crim_w_err(MAP_TABLE, "load referencemaps");
    fil_m_table(Rim_buffer, id_num, map_name, map_mapset);
    crimdm_w_err(MAP_TABLE, LOAD, Rim_buffer);
  }
}
