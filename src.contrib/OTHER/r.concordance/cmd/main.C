#include <utility.H>
#include "scan.H"
#include "MCA.H"

extern "C" {
#include <gis.h>
extern int  G_parser();
extern int  G_usage();
extern int  G_gisinit();
extern int  G_fatal_error(const char*);
extern int  G_warning(const char*);
extern int  G_open_cell_old(const char*, const char*);
extern int  G_open_cell_new(const char*);
extern int  G_window_rows();
extern int  G_window_cols();
extern void G_put_map_row(int, CELL*);
extern void G_get_map_row(int, CELL*, int);
extern void G_close_cell(int);
extern int  G_remove(const char*, const char*);
};

class scendef;

#include <stdio.h>
#include <stdlib.h>

#define TEMP_LAYER "_rconc_tmp"

int parse_definitions(const char*);
void describe_def(ClassDef&, TArray&);

void define_vars(const char*, ClassDef&, TArray& t);
void define_scenario(ClassDef&, TArray&);
void define_input(ClassDef&, TArray& t);
void define_output(ClassDef&, TArray& t);
void read_rows(int);
void run_concordance();
CELL eval_var(TArray&, CELL);
void scenario_definition(scendef&, TToken_string&);
void write_layers();
void print_overall_scores();
int  define_infile(const char*);

CELL Fill;
CELL Scale = 10;

int Nrows, Ncols;
int Verbose;
int Discord;
int Overall;

TAssoc_array *Vars;
TArray* Scenarios, *In_files, *Planes, *Out_files;

class ruledef : public TObject
{
public:
   ClassDef _cldef;
   double   _value;
};

class vardef : public TObject
{
public:
  TArray       _rules;
  int          _fileID;

  vardef() : TObject() {}
  virtual ~vardef() {}
};

class filedef : public TObject
{
public:
  int       _ID;
  CELL*     _cell;
  TString   _name;

  filedef() : TObject() {}
  virtual ~filedef() {}
};

class planedef : public TObject
{
public:
  TToken_string  _desc;
  double*        _scores;
  long           _count;

  planedef(int n) : TObject(), _desc(64)
    { _scores = new double[n]; }
  virtual ~planedef() 
    { delete _scores; }
};

class scendef : public TObject
{
public:
  TString _name;
  TArray  _vars;
  CELL    _category;
  
  scendef(const char* n) : TObject(), _name(n), _vars(64), _category(0) {}
  virtual ~scendef() {}
};

main(int argc, const char* argv[]) 
{
  struct Option *file;
  struct Flag*  verbose, *overall, *discord;

  CELL *cell;
  char *mapset;
  int row,col;

  Vars      = new TAssoc_array;

  Out_files = new TArray(16);
  Scenarios = new TArray(16);
  In_files  = new TArray(16);
  Planes    = new TArray(128);
  
  /* Define the different options */
  
  file = G_define_option();
  file->key        = "definition";
  file->type       = TYPE_STRING;
  file->required   = YES;
  file->description= "Name of an existing analysis definition file" ;
  
  verbose = G_define_flag();
  verbose->key = 'v';
  verbose->description = "Enable verbose computation";

  overall = G_define_flag();
  overall->key = 'o';
  overall->description = "Print overall scores";

  discord = G_define_flag();
  discord->key = 'd';
  discord->description = "Run discordance analysis";
  
  G_gisinit (argv[0]);
  
  if (G_parser(argc, argv))
      exit (-1);
      
  Nrows = G_window_rows();
  Ncols = G_window_cols();

  Verbose = verbose->answer;
  Overall = overall->answer;
  Discord = discord->answer;
  const char* name = file->answer;
  parse_definitions(name);

  // run analysis
  run_concordance();

  // clean up
  delete Vars;
  delete Scenarios;
  delete In_files;
  delete Out_files;
}

int parse_definitions(const char* file)
{
  if (Verbose)
    fprintf(stderr, "Reading definitions from %s...", file);

  RC_Scanner scan(file);
  ClassDef cld; 
  TArray vtemp(36);
  TString curarg;

  if (scan.error())
    {
      G_fatal_error("Analysis file not found"); 
      exit(1);
    }

  int ret = scan.next();

  for (;;)
    {
      if (ret == LEX_ERROR)
	{
	  TString rr(80);
	  rr.format("%s: syntax error around line %d",
		    file, scan.line());
	  G_fatal_error(rr);
	  exit(1);
	  break;
	}
      else if (ret == LEX_ENDFILE)
	{
	  break;
	}
      else if (ret == LEX_VARDEF)
	{
	  curarg = scan.s_val();
	  while ((ret = scan.read_var_definition(cld,vtemp)) 
		 == LEX_DEFINITION)
	      define_vars(curarg, cld, vtemp);
	}
      else if (ret == LEX_SCENARIO)
	{
	  while ((ret = scan.read_var_definition(cld,vtemp)) 
		 == LEX_DEFINITION)
	    define_scenario(cld, vtemp);
	}
      else if (ret == LEX_INPUT)
	{
	  while ((ret = scan.read_var_definition(cld,vtemp)) 
		 == LEX_DEFINITION)
	    ;
	}
      else if (ret == LEX_OUTPUT)
	{
	  while ((ret = scan.read_var_definition(cld,vtemp)) 
		 == LEX_DEFINITION)
	    define_output(cld, vtemp);
	}
      else if (ret == LEX_FILL)
	{
	  Fill = scan.i_val();
	  ret = scan.next();
	}
      else if (ret == LEX_SCALE)
	{
	  Scale = scan.i_val();
	  ret = scan.next();
	}
      else
	{
	  ret = scan.next();
	}
    }

  if (Verbose)
    fprintf(stderr, " done.\nRead %d variables, %d scenarios"
	    ", %d input layers\n",
	    Vars->items(),
	    Scenarios->items(),
	    In_files->items());

}

void describe_def(ClassDef& cl, TArray& t)
{
  cout << "-------------------------------------------------\n";
  cout << "Class type:\t" << cl._cltype << '\n';
  cout << "Val 1:\t" << cl._val << '\n';
  cout << "Val 2:\t" << cl._val2 << '\n';
  cout << "Name:\t" << cl._name << '\n';

  for (int i = 0; i < t.items(); i++)
    {
      TToken_string& tt = (TToken_string&)t[i];
      cout << "\tVar:\t" <<  tt.get(0) << '\n';
      cout << "\tVal:\t" <<  tt.get(1) << '\n';
    }
  cout << "-------------------------------------------------\n";
}


void define_vars(const char* file, ClassDef& cl, TArray& t)
{
  int fileID = define_infile(file);
  
  for (int i = 0; i < t.items(); i++)
    {
      vardef* vd; 
      TToken_string& tt = (TToken_string&)t[i];

      if (Vars->is_key(tt.get(0)))
	  vd = (vardef*)Vars->objptr(tt.get(0));
      else
	{
	  vd = new vardef;
	  vd->_fileID = fileID;
	  Vars->add(tt.get(0),vd);
	}
      if (fileID != vd->_fileID)
	G_fatal_error("Variables must refer to one layer only");

      ClassDef* cdp = cl.dup();
      if (tt.items() > 1) 
	cdp->_retval = atof(tt.get(1));
      vd->_rules.add(cdp);
    }
}

void define_scenario(ClassDef& cl, TArray& t)
{
  if (cl._cltype != name)
    {
      G_fatal_error("Invalid scenario definition");
      exit(1);
    }

  scendef* sd = new scendef(cl._name);
  sd->_vars   = t;
  Scenarios->add(sd);
}

void define_output(ClassDef& cl, TArray& t)
{
  if (cl._cltype != name)
    {
      G_fatal_error("Invalid output definition");
      exit(1);
    }

  TToken_string* tt = new TToken_string(36);
  tt->add(cl._name);
  TToken_string& tt2 = (TToken_string&)t[0];
  tt->add(tt2.get(0));

  Out_files->add(tt);
}

void scenario_definition(scendef& s, TToken_string& tt)
{
  // puts in tt string with weights in Vars ordering
  Vars->restart(); tt = "";
  for (int v = 0; v < Vars->items(); v++)
    {
      THash_object* ho = Vars->get_hashobj();
      
      for (int i = 0; i < s._vars.items(); i++)
	{
	  TToken_string& vd = (TToken_string&)s._vars[i];

	  if (ho->key() == vd.get(0))
	    {
	      tt.add(vd.get(1));
	      break;
	    }
	}
    }
}

int define_infile(const char* f)
{
  // add file to table
  // open file, alloc buffer
  // return ID number (index in table)
  
  for (int i = 0; i < In_files->items(); i++)
    {
      filedef& fd = (filedef&)(*In_files)[i];
      if (fd._name == f)
	return i;
    }
  
  const char* mapset;
  
  if ((mapset = G_find_cell(f,"")) == NULL)
    {
      TString80 err;
      err.format("Input file not found: %s", f);
      G_fatal_error(err);
    }
  
  filedef* fd = new filedef;

  fd->_ID     = G_open_cell_old(f, mapset);
  fd->_name   = f;
  fd->_cell   = G_allocate_cell_buf();
  
  In_files->add(fd);
  return In_files->items() - 1; 
}


void run_concordance()
{
  // main routine
  MCA mca; 
  TToken_string tmp(4*Vars->items());
  TArray atmp(Planes->items());
 
  // create temp map
  int tmpid    = G_open_cell_new(TEMP_LAYER);
  CELL* tmprow = G_allocate_cell_buf();

  if (Verbose) fprintf(stderr, "Now computing planes...\n"); 

  // create planes
  for (int r = 0; r < Nrows; r++)
    {
      // read row r in each file
      read_rows(r);
      for (int c = 0; c < Ncols; c++)
	{
	  // for each var determine value in each point
	  Vars->restart();
	  tmp = "";
	  for (int v = 0; v < Vars->items(); v++)
	    {
	      vardef* vd = (vardef*)Vars->get();
	      // compute value 
	      // make string
	      filedef& ff = (filedef&)(*In_files)[vd->_fileID]; 
	      CELL* row   = ff._cell;
	      tmp.add(eval_var(vd->_rules, row[c]));
	    }
	  // add to Planes table and put ID in temp map
	  for (int pl = 0; pl < Planes->items(); pl++)
	    {
	      planedef& pld = (planedef&)(*Planes)[pl];
	      if (tmp == pld._desc)
		{ pld._count++; break; }
	    }
	  if (pl == Planes->items())
	    {
	      planedef* pld = new planedef(Scenarios->items() == 0 ?
					   1 : Scenarios->items());
	      pld->_desc  = tmp;
	      pld->_count = 1l;
	      Planes->add(pld);
	      if (Verbose) fprintf(stderr, "\rNumber of planes: %d", pl); 
	    }
	  // index of plane goes into temp file
	  tmprow[c] = pl;
	}
      G_put_map_row(tmpid, tmprow);
    }
  if (Verbose)
    fprintf(stderr, "\nClosing temporary file\n");

  G_close_cell(tmpid);

  // planes are defined, tmp file contains ID of plane for each cell
  // go for analysis
  
  if (Verbose)
    fprintf(stderr, "Computing... ");

  // 1) foreach var: mca.define_criterion();
  Vars->restart();

  for (int v = 0; v < Vars->items(); v++)
    {
      THash_object* ho = Vars->get_hashobj();
      mca.define_criterion(ho->key());
    }

  // 2) foreach plane: mca.add_plane_values();
  for (int p = 0; p < Planes->items(); p++)
    {
      planedef& pd = (planedef&)(*Planes)[p];
      mca.add_plane_values(format("%d",p), pd._desc);
    }

  // 3) foreach scenario: create definition, mca.add_scenario_values
  for (int s = 0; s < Scenarios->items(); s++)
    {
      scendef& ssc = (scendef&)(*Scenarios)[s];
      scenario_definition(ssc, tmp);
      mca.add_scenario_values(ssc._name, tmp);
    }

  // 4) compute concordances;
  mca.compute();

  if (Verbose)
    fprintf(stderr, "done.\n");

  // 5) get results
  for (s = 0; s < Scenarios->items(); s++)
    {
      scendef& ssc = (scendef&)(*Scenarios)[s];
      mca.get_ranks(ssc._name, atmp, Discord ? discordance : concordance);

      for (p = 0; p < atmp.items(); p++)
	{
	  TToken_string& tt = (TToken_string&)atmp[p];
	  planedef& pd  = (planedef&)(*Planes)[tt.get_int(0)];
	  pd._scores[s] = atof(tt.get(1));
	}
    }

  // 6) write output layers
  write_layers();

  // 7) if requested, print out overall scores
  if (Overall)
    print_overall_scores();

  // 8) clean up (deallocate rows in all files, close files)
  for (p = 0; p < In_files->items(); p++)
    {
      filedef& fd = (filedef&)(*In_files)[p];
      G_close_cell(fd._ID);
      free(fd._cell);
    }
}

void read_rows(int r)
{
  for (int i = 0; i < In_files->items(); i++)
    {
      filedef& fd = (filedef&)(*In_files)[i];
      G_get_map_row(fd._ID, fd._cell, r);
    }
}

CELL eval_var(TArray& rules, CELL val)
{
  static char buf[36];
  CELL ret;

  // stops at first matching value
  
  for (int i = 0; i < rules.items(); i++)
    {
      bool match = FALSE;
      ClassDef& cld = (ClassDef&)rules[i];
      switch(cld._cltype)
	{
	case name:
	  G_fatal_error("NAME rules make no sense in variable definitions!");
	  exit(1);
	  break;
	case value:
	  if (val == (CELL)cld._val)
	    {
	      ret = (CELL)cld._retval;
	      match = TRUE;
	    }
	  break;
	case range:
	  if (val >= (CELL)cld._val && val <= (CELL)cld._val2)
	    {
	      ret = (CELL)cld._retval;
	      match = TRUE;
	    }
	  break;
	case gt:
	  if (val > (CELL)cld._val)
	    {
	      ret = (CELL)cld._retval;
	      match = TRUE;
	    }
	  break;
	case lt:
	  if (val < (CELL)cld._val)
	    {
	      ret = (CELL)cld._retval;
	      match = TRUE;
	    }
	  break;
	case ge:
	  if (val >= (CELL)cld._val)
	    {
	      ret = (CELL)cld._retval;
	      match = TRUE;
	    }
	  break;
	case le:
	  if (val <= (CELL)cld._val)
	    {
	      ret = (CELL)cld._retval;
	      match = TRUE;
	    }
	  break;
	case as_is:
	  ret = val;
	  match = TRUE;
	  break;
	}
      if (match) break;
    }
  
  if (i == rules.items())
    ret = Fill;

  return ret;
}


void write_layers()
{

  const char* mapset;
  
  if ((mapset = G_find_cell(TEMP_LAYER,"")) == NULL)
    {
      G_fatal_error("Temporary file removed!");
      exit(1);
    }
  
  int tmpid = G_open_cell_old(TEMP_LAYER, mapset);
  TString outf(16);

  CELL* row_in  = G_allocate_cell_buf();
  CELL* row_out = G_allocate_cell_buf();

  for (int i = 0; i < Out_files->items(); i++)
    {
      TToken_string& tt = (TToken_string&)(*Out_files)[i];
      
      for (int s = 0; s < Scenarios->items(); s++)
	{
	  scendef& sc = (scendef&)(*Scenarios)[s];
	  if (sc._name == tt.get(0))
	    {
	      outf = tt.get(1);
	      break;
	    }
	}
      if (s == Scenarios->items())
	{
	  G_warning("mixed scenarios still unimplemented -- sorry");
	  continue;
	}

      // write output file
      int od = G_open_cell_new(outf);

      if (Verbose)
	fprintf(stderr, "Writing %s... ", (const char*)outf);

      for (int r = 0; r < Nrows; r++)
	{
	  G_get_map_row(tmpid, row_in, r);
	  for (int c = 0; c < Ncols; c++)
	    {
	      planedef& pd = (planedef&)(*Planes)[row_in[c]];
	      row_out[c] = (CELL)((double)Scale*pd._scores[s]);
	    }
	  G_put_map_row(od, row_out);
	}
      G_close_cell(od);

      if (Verbose)
	fprintf(stderr, "done\n");
    }
  G_close_cell(tmpid);
  free(row_in);
  free(row_out);
  // remove temp file
  G_remove("cell",TEMP_LAYER);
}


void print_overall_scores()
{
  const char* d = Discord ? "DISCORDANCE" : "CONCORDANCE";
  cout << "\n" << d << " SCORES\n\n";

  for (int s = 0; s < Scenarios->items(); s++)
    {
	  scendef& sc = (scendef&)(*Scenarios)[s];
	  double score = 0.0;

	  for (int p = 0; p < Planes->items(); p++)
	    {
	      planedef& pd = (planedef&)(*Planes)[p];
	      score += pd._scores[s] * (double)(pd._count);
	    }
	  score /= ((double)Nrows*(double)Ncols);

	  cout << "\t" << sc._name << ":\t\t" << score << '\n';
    }
}

