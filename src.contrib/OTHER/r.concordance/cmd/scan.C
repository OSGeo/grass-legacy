#include "scan.H"
#include <utility.H>

ClassDef* ClassDef::dup()
{
  ClassDef* c = new ClassDef;
  c->_cltype = _cltype;
  c->_val    = _val;
  c->_val2   = _val2;
  strcpy(c->_name, _name);
  return c;
}


int RC_Scanner::postprocess_token(int val, char* token)
{
  int err = 0, ret, itok;
  int fl = 0;
  TString t(36);
  if (val == '[')
    {
      err |= !expect(LEX_KEYWORD);
      
      switch (ret = i_val())
	{
	case LEX_VARDEF:
	  err |= !expect(LEX_IDENTIFIER);
	  t   = s_val();
	  fl  = 1;
	  break;
	case LEX_FILL:
	  err |= !expect(LEX_INTEGER);
	  itok = i_val();
	  fl   = 2;
	  break;
	case LEX_SCALE:
	  err |= !expect(LEX_INTEGER);
	  itok = i_val();
	  fl   = 2;
	  break;
	case LEX_EOF:
	  ret = LEX_ENDFILE;
	case LEX_SCENARIO:
	case LEX_INPUT:
	case LEX_OUTPUT:
	  break;
	default:
	  ret = LEX_IGNORE;
	}
      err |= !expect(']');

      if (fl == 1) set_result((const char*)t); 
      else if (fl == 2) set_result(itok);

      return err ? LEX_ERROR : ret;
    }
  else return val;
}

int RC_Scanner::read_var_definition(ClassDef& cls, TArray& varvals)
{
  int err = 0;
  double val1, val2;
  int ret;

  // init 
  cls._val = cls._val2 = 0.0;
  *(cls._name) = '\0';

  varvals.destroy();

  ret = next();
  switch (ret)
    {
    case LEX_IDENTIFIER:
      strcpy(cls._name, s_val());
      cls._cltype = strcmp(cls._name, "VALUE") == 0 ? as_is : name;
      err |= !expect(':');
      break;
    case LEX_INTEGER:
    case LEX_FLOAT:
      cls._val = ret == LEX_INTEGER ? (double)i_val() : d_val();
      if ((ret = next()) == ':')
	{
	  cls._cltype = value;
	}
      else if (ret == LEX_KEYWORD && i_val() == LEX_TO)
	{
	  cls._cltype = range;
	  ret = next();
	  if (ret != LEX_INTEGER && ret != LEX_FLOAT)
	    { 
	      err++;
	      flush(':');
	    }
	  else 

	    {
	      cls._val2 = ret == LEX_INTEGER ? (double)i_val() : d_val();
	      err |= !expect(':');
	    }
	}
      else 
	{
	  flush(':');
	  err++;
	};
	break;
    case '>':
    case '<':
      val2 = next();
      if (val2 == '=')
	cls._cltype = (ret == '<') ? le : ge;
      else 
	{
	  cls._cltype = (ret == '<') ? lt : gt;
	}
      if (val2 != LEX_INTEGER && val2 != LEX_FLOAT)
	{
	  flush(':');
	  err++;
	}
      else
	{
	  cls._val = val2 == LEX_INTEGER ? (double)i_val() : d_val(); 
	  err |= !expect(':');
	}
      break;
    default: 
      // valore non standard
      // non era una dichiarazione, continua a scorrere il file
      return ret;
    }
  
  // read variables
  TToken_string s(36);

  for (;;)
    {
      if ((ret = next()) == ';') break;

      if (ret == ',') 
	ret = next();
      
      if (ret != LEX_IDENTIFIER)
	{
	  flush(';'); 
	  err ++ ;
	  break;
	}
      else 
	{
	  s = "";
	  s.add(s_val());
	  ret = next();
	  if (ret == '=')
	    {
	      ret = next();
	      if (ret == LEX_INTEGER)
		s.add(i_val());
	      else if (ret == LEX_FLOAT)
		s.add(format("%g",d_val()));
	      else if (ret == LEX_IDENTIFIER)
		s.add(s_val());
	      else 
		{
		  err++; flush(';');
		}
	    }
	  varvals.add(new TToken_string(s));
	  if (ret == ';') break;
	}
    }

  return err ? LEX_ERROR : LEX_DEFINITION;
  
}
