#include "gis.h"
#include "kappa.h"

static
die()
{
  unlink (stats_file);
  G_fatal_error ("WARNING: - problem reading r.stats output\n");
}

stats()
{
  char buf[1024], msg[100];
  char mname[GNAME_MAX], rname[GMAPSET_MAX];
  char *mmapset, *rmapset;
  int i,nl,ns;
  FILE *fd;
  char **tokens, **G_tokenize();

  strcpy (mname, maps[0]);
  mmapset = G_find_cell2 (mname, "");
  if (mmapset == NULL){
    sprintf (msg, "%s: <%s> raster map not found\n", G_program_name(), maps[0]);
    G_fatal_error (msg);
    exit(1);
  }
  strcpy (rname, maps[1]);
  rmapset = G_find_cell2 (rname, "");
  if (rmapset == NULL){
    sprintf (msg, "%s: <%s> reference map not found\n", G_program_name(), maps[1]);
    G_fatal_error (msg);
    exit(1);
  }

  stats_file = G_tempfile();
  strcpy (buf, "r.stats -cin");
  if (!verbose) strcat(buf, "q");
  strcat (buf, " fs=:");
  strcat (buf, " input=");
  strcat (buf, G_fully_qualified_name(maps[0], mmapset));
  strcat (buf, ",");
  strcat (buf, G_fully_qualified_name(maps[1], rmapset));
  strcat (buf, " > ");
  strcat (buf, stats_file);

  if (system(buf)) {
    unlink (stats_file);
    exit(1);
  }

  fd = fopen (stats_file, "r");
  if (fd == NULL) {
    unlink (stats_file);
    sprintf(buf,"%s: unable to open result file <%s>\n",G_program_name(),stats_file);
  }
    
  while (G_getl(buf, sizeof buf, fd)){
    tokens = G_tokenize (buf, ":");
    i = 0;
    ns=nstats++;
    Gstats = (GSTATS *) G_realloc (Gstats, nstats * sizeof(GSTATS));
    Gstats[ns].cats = (long *) G_calloc (nlayers, sizeof(long));
    for (nl=0; nl<nlayers; nl++) {
      if (sscanf (tokens[i++], "%ld", &Gstats[ns].cats[nl]) != 1)
        die();
    }
    if (sscanf (tokens[i++], "%ld", &Gstats[ns].count) != 1)
      die();
    G_free_tokens (tokens);
  }
  fclose (fd);
  unlink (stats_file);
}
