#ifndef __MCA_H
#define __MCA_H

#include <assoc.H>
#include <matrix.H>

#ifdef USE_TCL
#include <tclobj.H>
#endif

enum analysis { concordance, discordance };
enum crtype   { real, rank, percentage, qualitative };

class MCA : public TObject
{
  TAssoc_array   _scenarios;
  TAssoc_array   _planes;
  TArray         _criteria;
  
  bool           _dirty;
  unsigned short _error;
  analysis       _cur_analysis;
  
  int            _last_scenario;
  int            _last_criterion;
  int            _last_plane;
  
  // result matrices
  Matrix2D*      _cnet;
  Matrix2D*      _dwnet;

public:

  // settings
  // add one or more value to scenario (pass token_string)
  int add_scenario_values(const char* name, const char* values,
			  bool cost = FALSE);
  int add_plane_values(const char* name, const char* values,
		       bool cost = FALSE);
  int define_criterion(const char* name, crtype type = real, 
		       int index = -1);

  void enable_criterion(const char* name, bool mode = TRUE);
  void disable_criterion(const char* name) 
    { enable_criterion(name, FALSE); }

  void compute();

  void get_scenarios(TToken_string& t);
  void get_criteria(TToken_string& t);
  void get_scenario_values(const char* name, TToken_string& t);
  void get_plane_values(const char* name, TToken_string& t);
  void get_planes(TToken_string& t);

  // fills array with (newly allocated) Tokenstrings
  // with "plane_name|rank"
  void get_ranks(const char* scenario, TArray& vals, 
		 analysis what = concordance);

  // checking
  bool needs_compute() { return _dirty;         }
  int n_scenarios() { return _last_scenario;    }
  int n_planes()   { return _last_plane;       }
  int n_criteria() { return _last_criterion;  }

  int  status();

  MCA() :  TObject(), _cur_analysis(concordance), _dirty(FALSE), _error(0),
           _last_scenario(0), _last_criterion(0), _last_plane(0),
           _cnet(NULL), _dwnet(NULL)
  {}
  virtual ~MCA();
};

#ifdef USE_TCL

class TclMCA : public TclObject
{
  MCA _mca;
  virtual void do_method(methodID ID, int argc, char** argv, 
			 int optc, char** opt);
public:
  
  TclMCA(int argc = 0, char** argv = NULL, int optc = 0, 
	 char** optv = NULL, char* name = NULL); 
  virtual ~TclMCA() {}
};

#endif
#endif

