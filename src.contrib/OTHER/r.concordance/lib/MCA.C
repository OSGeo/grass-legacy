#include "MCA.H"
#include <utility.H>
#include <stdio.h>
#include <math.h>

#ifdef TESTING
#include <stream.h>
#endif

extern "C" {
extern double atof(const char *);
}

#define MAX(x,y) (x>y?x:y) 

// **************************************************************
// 3 assoc con nomi associati a indici per Scenari, Piani, Criteri
//
// Piano    = nome, lista di valori per ciascun criterio
// Scenario = nome, lista di pesi per ciascun criterio 
// Ciascuno di questi e' in una lista indicizzata per nome 
// prima del calcolo viene infilato in una matrice assecondando
// abilitazioni e disabilitazioni
// **************************************************************

class MCA_Object : public TObject
{
  TToken_string _vals;
  bool          _active;
  int           _id;
public:
  int id()                      { return _id;  }
  TToken_string& val()          { return _vals; }
  bool is_active()              { return _active; }
  void activate(bool on = TRUE) { _active = on;   }

  MCA_Object(int id) : _id(id), _active(TRUE), _vals(36)
    {}
  virtual ~MCA_Object() {}
};

int MCA::add_scenario_values(const char* name, const char* values,
			bool cost)
{
  MCA_Object* tt;
  if (_scenarios.is_key(name))
    tt = (MCA_Object*)_scenarios.objptr(name);
  else {
    tt = new MCA_Object(_last_scenario++);
    _scenarios.add(name, tt);
  }

  TToken_string vls(values);
  for (int i = 0; i < vls.items(); i++)
    {
      double d = atof(vls.get(i));
      if (cost)  // cost values are negative
	{
	  d = -d;
	  vls.add(format("%g",d), i);
	}      
    }
  tt->val().add(vls);
  _dirty = TRUE;
  return _last_scenario; 
}

int MCA::add_plane_values(const char* name, const char* values, bool cost)
{
  
  MCA_Object* tt;
  if (_planes.is_key(name))
    tt = (MCA_Object*)_planes.objptr(name);
  else {
    tt = new MCA_Object(_last_plane++);
    _planes.add(name,tt);
  }

  TToken_string vls(values);
  for (int i = 0; i < vls.items(); i++)
    {
      double d = atof(vls.get(i));
      if (cost)  // cost values are negative
	{
	  d = -d;
	  vls.add(format("%g",d), i);
	}      
    }
  tt->val().add(vls);
}

int MCA::define_criterion(const char* name, crtype how, int index)
{
  MCA_Object* tt = new MCA_Object(_last_criterion++);
  tt->val() = name;

  _criteria.add(tt, index);
  _dirty = TRUE;
  return _criteria.items();
}	
		
void MCA::enable_criterion(const char* name, bool mode)
{
  for (int i = 0; i < _criteria.items(); i++)
    {
      MCA_Object& tt = (MCA_Object&)_criteria[i];
      if (tt.val() == name)
	{
	  if (tt.is_active() != mode)
	    {
	      _dirty = TRUE;
	      tt.activate(mode);
	    }
	  break;
	}
    }
}
			
void MCA::compute()
{
  int ncri = 0;
  int nsce = _last_scenario;
  int npia = _last_plane;

  // compute number of active criteria
  for (int i = 0; i < _criteria.items(); i++)
    {
      MCA_Object& tt = (MCA_Object&)_criteria[i];
      if (tt.is_active()) ncri++;
    }

  // create matrices (looking at active criteria)
  Matrix2D wnat(ncri,nsce);
  Matrix2D pnat(ncri,npia);

  for (i = 0; i < _scenarios.items(); i++)
    {
      // get scenario
      MCA_Object* sc = (MCA_Object*)_scenarios.get();
      // add values if criterion is enabled
      int nxtcr = 0;
      for (int j = 0; j < sc->val().items(); j++)
	{
	  // get criterion
	  MCA_Object& cr = (MCA_Object&)_criteria[j];
	  if (cr.is_active())
	    {
	      wnat(nxtcr++, sc->id()) = atof(sc->val().get(j));
	    }
	}
    }
  for (i = 0; i < _planes.items(); i++)
    {
      // get plane
      MCA_Object* pl = (MCA_Object*)_planes.get();
      // add values if criterion is enabled
      int nxtcr = 0;
      for (int j = 0; j < pl->val().items(); j++)
	{
	  // get criterion
	  MCA_Object& cr = (MCA_Object&)_criteria[j];
	  if (cr.is_active())
	    {
	      pnat(nxtcr++, pl->id()) = atof(pl->val().get(j));
	    }
	}
    }
  _scenarios.restart();
  _planes.restart();

  // normalize everything
  Matrix1D SW(nsce);
  Matrix1D PW(ncri);

  // row sums of scenarios
  for (i=0; i < ncri; i++)
    for (int j=0; j < nsce; j++)
      SW(j) += wnat(i,j);
  
  // maxima of planes
  for (i=0; i < ncri; i++)
    for (int j=0; j < npia; j++)
      if (fabs(PW(i)) < fabs(pnat(i,j))) PW(i) = fabs(pnat(i,j));

  // normalize scenarios
  for (i=0; i < ncri; i++)
    for (int j=0; j < nsce; j++)
      wnat(i,j) /= SW(j);

  // normalize planes
  for (i=0; i < ncri; i++)
    for (int j=0; j < npia; j++)
      if (pnat(i,j) >= 0.0) pnat(i,j) /= PW(i);
      else pnat(i,j) = (PW(i) - fabs(pnat(i,j)))/fabs(PW(i));

  Matrix3D cset(npia,npia,ncri);
  Matrix3D dset(npia,npia,ncri);
  Matrix3D C(npia,npia,nsce);
  Matrix3D DW(npia,npia,nsce);
  Matrix2D cdom(npia, nsce);
  Matrix2D cstar(npia,nsce);
  Matrix2D dwdom(npia,nsce);
  Matrix2D dwstar(npia,nsce);
  Matrix1D dwmax(nsce);

  if (_cnet  != NULL) delete _cnet;
  if (_dwnet != NULL) delete _dwnet;

  _cnet  = new Matrix2D(npia,nsce);
  _dwnet = new Matrix2D(npia,nsce);

  // compute CSET and DSET
  for(int j=0; j < ncri; j++)
    for(int i=0; i < npia; i++)
      for(int ii=0; ii < npia; ii++)
	{
	  if (pnat(j,i) > pnat(j,ii))
	    cset(i,ii,j) = (double)j;
	  else dset(i,ii,j) = (double)j;
	}

  // analyze concordance/discordance
  for (int k=0; k < nsce; k++)
    {
      for(int j=0; j < ncri; j++)
	for(int i=0; i < npia; i++)
	  for(int ii=0; ii < npia; ii++)
	  {
	    if (cset(i,ii,j) != 0.0 && i != ii)
	      C(i,ii,k) += wnat((int)cset(i,ii,j),k);
	    if (dset(i,ii,j) != 0.0)
	      {
		int jj = (int)dset(i,ii,j);
		dwmax(k) = MAX(dwmax(k),fabs(pnat(jj,i)-pnat(jj,ii)));
	      }  
	  }
      for(j=0; j < ncri; j++)
	for(int i=0; i < npia; i++)
	  for(int ii=0; ii < npia; ii++)
	    {
	      if (dset(i,ii,j) != 0.0 && i != ii)
		{
		  int jj = (int)dset(i,ii,j);
		  DW(i,ii,k) += wnat(jj,k)*
		    fabs(pnat(jj,i)-pnat(jj,ii))/dwmax(k);
		}
	    }
      for(int i=0; i < npia; i++)
	for(int ii=0; ii < npia; ii++)
	  {
	    cdom(i,k)  += C(i,ii,k);
	    cstar(i,k) += C(ii,i,k);
	    dwdom(i,k) += DW(i,ii,k);
	    dwstar(i,k)+= DW(ii,i,k);
	  }
      for(i=0; i < npia; i++)
	{
	  (*_cnet)(i,k)  = cdom(i,k)-cstar(i,k);
	  (*_dwnet)(i,k) = dwdom(i,k)-dwstar(i,k);
	}
    }

  // normalize results 

  Matrix1D mxp(nsce);
  Matrix1D mnp(nsce);

  for(i = 0; i < nsce; i++)
    {
      mxp(i) = (*_cnet)(0,i);
      mnp(i) = (*_cnet)(0,i);
      for(j = 1; j < npia; j++)
	{
	  if ((*_cnet)(j,i) > mxp(i)) mxp(i) = (*_cnet)(j,i); 
	  if ((*_cnet)(j,i) < mnp(i)) mnp(i) = (*_cnet)(j,i); 
	}
      for (j = 0; j < npia; j++)
	{
	  (*_cnet)(j,i) = ((*_cnet)(j,i) - mnp(i))/(mxp(i) - mnp(i));
	}
    }

  for(i = 0; i < nsce; i++)
    {
      mxp(i) = (*_dwnet)(0,i);
      mnp(i) = (*_dwnet)(0,i);
      for(j = 1; j < npia; j++)
	{
	  if ((*_dwnet)(j,i) > mxp(i)) mxp(i) = (*_dwnet)(j,i); 
	  if ((*_dwnet)(j,i) < mnp(i)) mnp(i) = (*_dwnet)(j,i); 
	}
      for (j = 0; j < npia; j++)
	{
	  (*_dwnet)(j,i) = ((*_dwnet)(j,i) - mnp(i))/(mxp(i) - mnp(i));
	}
    }


#ifdef TESTING
  cout << "CNET: \n";
  for(i = 0; i < nsce; i++)
    for(j = 0; j < npia; j++)
      {
	cout << (*_cnet)(j,i) << ((j == npia-1) ? '\n' : '\t');
      }
  cout << "DWNET: \n";
  for(i = 0; i < nsce; i++)
    for(j = 0; j < npia; j++)
      {
	cout << (*_dwnet)(j,i) << ((j == npia-1) ? '\n' : '\t');
      }
#endif

  _dirty = FALSE;
}


void MCA::get_scenarios(TToken_string& t)
{
  _scenarios.restart();
  t = "";
  THash_object* m;
  while((m = _scenarios.get_hashobj()) != NULL)
    t.add(m->key());
}

void MCA::get_criteria(TToken_string& t)
{
  t = "";
  for (int i = 0; i < _criteria.items(); i++)
    {
      MCA_Object& m = (MCA_Object&)_criteria[i];
      t.add(m.val().get());
      m.val().restart();
    }
}

void MCA::get_scenario_values(const char* name, TToken_string& t)
{
  MCA_Object& m = (MCA_Object&)_scenarios[name];
  t = m.val();
}

void MCA::get_plane_values(const char* name, TToken_string& t)
{
  MCA_Object& m = (MCA_Object&)_planes[name];
  t = m.val();
}

void MCA::get_planes(TToken_string& t)
{
  _planes.restart();
  t = "";
  THash_object* m;
  while((m = _planes.get_hashobj()) != NULL)
    t.add(m->key());
}

void MCA::get_ranks(const char* scenario, TArray& vals, 
		 analysis what)
{
  Matrix2D* mat = what == concordance ? _cnet : _dwnet;
  vals.destroy();
  static char buf[36];

  MCA_Object& sc = (MCA_Object&)_scenarios[scenario];
  int sid        = sc.id();

  THash_object* m;
  while((m = _planes.get_hashobj()) != NULL)
    {
      TToken_string* t = new TToken_string(36);
      t->add(m->key());
      MCA_Object& mc = (MCA_Object&)m->obj();
      sprintf(buf,"%lf", (*mat)(mc.id(),sid));
      t->add(buf);
      vals.add(t);      
    }
}

MCA::~MCA()
{ 
  if (_cnet != NULL)  delete _cnet;
  if (_dwnet != NULL) delete _dwnet;
}

#ifdef USE_TCL

// ---------------------------------------------------------------
// TCL interface
// ---------------------------------------------------------------

#define M_GETPIA 1
#define M_GETSCE 2
#define M_GETCRI 3
#define M_GETPIV 4
#define M_GETSCV 5
#define M_GETRNK 6
#define M_COMPUT 7
#define M_NUMSCE 8
#define M_NUMPIA 9
#define M_NUMCRI 10
#define M_ENBCRI 11
#define M_DISCRI 12
#define M_DEFCRI 13
#define M_DEFSCE 14
#define M_DEFPIA 15
#define M_ERROR  16
#define M_DIRTY  17

TclMCA::TclMCA(int argc, char** argv, int optc, char** optv, char* name) : 
   TclObject(argc, argv, optc, optv, name)
{
  add_method("getPlanes", M_GETPIA);
  add_method("getScenarios", M_GETSCE);
  add_method("getCriteria", M_GETCRI);
  add_method("getPlaneValues", M_GETPIV);
  add_method("getScenarioValues", M_GETSCV);
  add_method("getRankings", M_GETRNK);
  add_method("compute", M_COMPUT);
  add_method("nScenarios", M_NUMSCE);
  add_method("nPlanes", M_NUMPIA);
  add_method("nCriteria", M_NUMCRI);
  add_method("enableCriterion", M_ENBCRI);
  add_method("disableCriterion", M_DISCRI);
  add_method("defineCriterion", M_DEFCRI);
  add_method("defineScenario", M_DEFSCE);
  add_method("definePlane", M_DEFPIA);
  add_method("isError", M_ERROR);
  add_method("isComputed", M_DIRTY);
}

void TclMCA::do_method(methodID ID, int argc, char** argv, 
		       int optc, char** optv)
{
  reset_result();
  TToken_string dio(256);
  TString scenario(16);
  TArray res(36);

  switch(ID)
    {
    case M_GETPIA:
      _mca.get_planes(dio);
      set_result(dio);
      break;
    case M_GETSCE:
      _mca.get_scenarios(dio);
      set_result(dio);
      break;
    case M_GETCRI:
      _mca.get_criteria(dio);
      set_result(dio);
      break;
    case M_GETPIV:
      _mca.get_plane_values(argv[0],dio);
      set_result(dio);
      break;
    case M_GETSCV:
      _mca.get_scenario_values(argv[0],dio);
      set_result(dio);
      break;
    case M_GETRNK:
      analysis a = concordance;
      while (optc--) 
	{
	  if (strcmp(optv[optc],"discordance") == 0)
	    a = discordance;
	  else if(strncmp(optv[optc],"scenario",(long)8) == 0)
	    {
	      TToken_string opt(optv[optc],'=');
	      scenario = opt.get(1);
	    }
	}
      if (scenario.empty()) 
	error("usage: <MCA> getRankings -scenario=<scenario>"
	      " [-concordance|-discordance]");
      else
	{
	  _mca.get_ranks(scenario,res,a);
	  TString tt(36);
	  for(int i = 0; i < res.items(); i++)
	    {
	      TToken_string& t = (TToken_string&)res[i];
	      tt = "";
	      tt << t.get(0) << " " << t.get(1);
	      add_result(t);
	    }
	}
      break;
    case M_COMPUT:
      _mca.compute();
      break;
    case M_NUMSCE:
      add_result(_mca.n_scenarios());
      break;
    case M_NUMPIA:
      add_result(_mca.n_planes());
      break;
    case M_NUMCRI:
      add_result(_mca.n_criteria());
      break;
    case M_ENBCRI:
      _mca.enable_criterion(argv[0]);
      break;
    case M_DISCRI:
      _mca.disable_criterion(argv[0]);
      break;
    case M_DEFCRI:
      _mca.define_criterion(argv[0]);
      break;
    case M_DEFSCE:
      dio = "";
      for (int i = 1; i < argc; i++)
	dio.add(argv[i]);
      _mca.add_scenario_values(argv[0],dio);
      break;
    case M_DEFPIA:
      dio = "";
      bool cost = FALSE;
      for (i = 1; i < argc; i++)
	dio.add(argv[i]);
      if (optc > 0)
	if (strcmp(optv[0],"cost") == 0)
	  cost = TRUE;
      _mca.add_plane_values(argv[0],dio,cost);
      break;
    case M_ERROR:
      break;
    case M_DIRTY:
      break;   
    }
}

#endif
