#include <string.h>
#include <unistd.h>
#include "gis.h"
#include "transects.h"

#define METER		1
#define FOOT            2

int main (int argc, char *argv[])
{
	struct GModule *module;
    struct {
        struct Option *vect, *title, *input, *length, *units, *decl, *fs;
    } parm;
    struct {
	struct Flag *a;
    } flag;
    double length, convert, decl;
    double bk, fw, rt, lf;
    int line, n; 
    int unit_type;
    int area;
    int fn,i;
    FILE *fd;
    char buf[1024];
    char fmt[128];
    char label[512];
    long cat;
    double x[5],y[5],az, xmid, ymid;
    double xx[4],yy[4],xtmp[4],ytmp[4];
    double dsin(), dcos();

    convert = 1.0;
    bk = 0.0; /* bk, fw, rt and lf represent extensions of transects */
    fw = 0.0; /* in backward, forward, rightside, and leftside, respectively. */
    rt = 0.0;
    lf = 0.0;
    unit_type = 0;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Import transect data to a GRASS vector map.";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "transect information file";

    parm.fs = G_define_option();
    parm.fs->key = "fs";
    parm.fs->key_desc = "\"character\"";
    parm.fs->type = TYPE_STRING;
    parm.fs->required = NO;
    parm.fs->description = "input data field separator";
    parm.fs->answer = "space";

    parm.length = G_define_option();
    parm.length->key = "length";
    parm.length->type = TYPE_DOUBLE;
    parm.length->required = YES;
    parm.length->description = "transect length";

    parm.units = G_define_option();
    parm.units->key = "units";
    parm.units->type = TYPE_STRING;
    parm.units->required = NO;
    parm.units->description = "units of transect length: m(eters),f(eet)";
    parm.units->answer = "meters";

    parm.decl = G_define_option();
    parm.decl->key = "decl";
    parm.decl->type = TYPE_STRING;
    parm.decl->required = NO;
    parm.decl->description = "declination - angle (in degrees) to be added to input azimuth angles";
    parm.decl->answer = "0";

    parm.vect = G_define_option();
    parm.vect->key = "vect";
    parm.vect->type = TYPE_STRING;
    parm.vect->gisprompt = "new,dig,vector";
    parm.vect->required = YES;
    parm.vect->description = "Vector map to be created";

    parm.title = G_define_option();
    parm.title->key = "title";
    parm.title->key_desc = "\"phrase\"";
    parm.title->type = TYPE_STRING;
    parm.title->required = NO;
    parm.title->description = "Title for resultant vector map";
    parm.title->answer="Transect map";

    flag.a = G_define_flag();
    flag.a->key = 'a';
    flag.a->description = "area";

    if(G_parser (argc,argv))
        exit(1);


    if ((parm.fs->answer == NULL)||(strcmp(parm.fs->answer, "space")==0))
	parm.fs->answer = " ";

    if (parm.decl->answer && !scan_declination(parm.decl->answer, &decl))
    {
        fprintf(stderr, "Invalid declination(%s)\n", parm.decl->answer);
	G_usage();
	exit(1);
    }

    if (sscanf (parm.length->answer, "%lf", &length) !=1 || length <= 0.0)
    {
        fprintf(stderr, "Invalid transect length(%s)\n",
            parm.length->answer);
        G_usage();
        exit(1);
    }

    if (parm.units->answer) {
        unit_type = parse_units (parm.units->answer);
	switch (unit_type){
	  case METER: 
	    convert = 1.0;
	    break;
	  case FOOT: 
	    convert = 0.3048;
	    break;
	  default: {
	    G_usage();
            exit(1);
	  }
	}
    }

    if ((fd = fopen (parm.input->answer, "r")) == NULL)
    {
        perror(parm.input->answer);
        G_usage();
        exit(1);
    }

    area = flag.a->answer;

    openvect(parm.vect->answer, parm.title->answer);

    if (area)
      sprintf (fmt,"%%ld%s%%lf%s%%lf%s%%lf%s%%lf%s%%lf%s%%lf%s%%lf%s%%[^\n]",
	parm.fs->answer, parm.fs->answer, parm.fs->answer, parm.fs->answer,
	parm.fs->answer, parm.fs->answer, parm.fs->answer, parm.fs->answer);
    else
      sprintf (fmt,"%%ld%s%%lf%s%%lf%s%%lf%s%%[^\n]",
	parm.fs->answer, parm.fs->answer, parm.fs->answer, parm.fs->answer);
    for (line = 1; G_getl(buf, sizeof buf, fd); line++)
    {
        *label = 0;
	if (area) {
	  n = 4;	/* area transects */
	  fn = sscanf (buf, fmt, &cat, x, y, &az, &bk, &fw, &rt, &lf, label);
	  switch(fn)
	  {
	  case 9: break;
	  case 8: if (*label == 0) break;
		/*FALLTHROUGH*/
	  default:
		fprintf (stderr, "ERROR line %d:<%s>\n", line, buf);
		continue;
	  }
	  xmid = x[0] + 0.5 * convert * length * dsin(az+decl);
	  ymid = y[0] + 0.5 * convert * length * dcos(az+decl);
	  xx[0] = -bk * convert;
	  yy[0] = -rt * convert;
	  xx[1] = xx[0];
	  yy[1] = lf * convert;
	  xx[2] = (length+fw)*convert;
	  yy[2] = yy[1];
	  xx[3] = xx[2];
	  yy[3] = yy[0];
	  for (i=0; i<n; i++) {
	    xtmp[i] = x[0] + xx[i]*dsin(az+decl) - yy[i]*dcos(az+decl);
	    ytmp[i] = y[0] + xx[i]*dcos(az+decl) + yy[i]*dsin(az+decl);
	  }        
          for (i=0; i<n; i++) {
	    x[i] = xtmp[i];
	    y[i] = ytmp[i];
	  }
          x[4] = x[0];
          y[4] = y[0];
	  n = 5; /* just for writeline, close polygon */
	}
	else {
	  n = 2;	/* line transects */
	  fn = sscanf (buf, fmt, &cat, x, y, &az, label);
	  switch(fn)
	  {
	  case 5: break;
	  case 4: if (*label == 0) break;
		/*FALLTHROUGH*/
	  default:
		fprintf (stderr, "ERROR line %d:<%s>\n", line, buf);
		continue;
	  }
        x[1] = x[0] + convert * length * dsin (az + decl);
        y[1] = y[0] + convert * length * dcos (az + decl);
	xmid = 0.5 * (x[0] + x[1]);
	ymid = 0.5 * (y[0] + y[1]);
	}
      writeline (x, y, n, xmid, ymid, (CELL)cat, label);
    }
    fclose (fd);
    closevect();
    exit(0);
}

int parse_units (char *s)
{
  int type;

  if (match(s, "meters",1) == 0)
    type = METER;
  else if (match (s, "feet",1) == 0)
    type = FOOT;
  else {
    G_usage();
    exit(1);
  }
  return type;
}

int match (char *s, char *key, int min)
{
        int len;

        len = strlen (s);
        if (len < min) return -1;
        return strncmp (s, key, len);
}
