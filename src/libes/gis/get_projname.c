#include  "gis.h"

int G_ask_proj_name(proj_id, proj_name)
 char *proj_id;
 char *proj_name;
     {
	char path[1024], buff[1024], answer[50], *a;
        struct Key_Value *in_proj_keys;
	char  *Tmp_file;
        FILE  *Tmp_fd = NULL;
	FILE  *pj;
	int   in_stat, i, npr;

        sprintf(path,"%s/etc/projections",G_gisbase());
        while (access(path,0) !=0)
        { 
          sprintf(buff,"%s not found",path);
          G_fatal_error(buff);
        }
        in_proj_keys = G_read_key_value_file(path,&in_stat);
        if (in_stat != 0)
        {
          sprintf(buff,"ERROR in reading %s",path);
          G_fatal_error(buff);
        }
        npr = in_proj_keys->nitems;
        Tmp_file = G_tempfile ();
        if (NULL == (Tmp_fd = fopen (Tmp_file, "w"))) {
	  G_fatal_error("Cannot open temp file") ;
        }
        for (i=0; i<npr; i++) {
          fprintf(Tmp_fd,"%s -- %s\n",in_proj_keys->key[i],in_proj_keys->value[i]);
        }
        fclose(Tmp_fd);

        for(;;) {

	  do {
	      fprintf(stderr,"\n\nPlease specify projection name\n");
	      fprintf(stderr,"Enter 'list' for the list of available projections\n");
	      fprintf (stderr, "Hit RETURN to cancel request\n");
	      fprintf(stderr,">");
          } while(!G_gets(answer));

          G_strip(answer); 
          if(strlen(answer)==0) return -1;
          if (strcmp(answer,"list") == 0) {
            if (isatty(1)) {
	      sprintf(buff,"more %s",Tmp_file);
            }
            else
	      sprintf(buff,"cat %s",Tmp_file);
            system(buff);
          }
          else {
            a = G_find_key_value(answer,in_proj_keys);
            if (a==NULL) 
	    {
	      fprintf(stderr,"\ninvalid projection\n");
	    }
            else break;
          }
        }

        sprintf(proj_id,"%s",answer);
        sprintf(proj_name,"%s",a);
        unlink(Tmp_file);
        return 1;
      }

