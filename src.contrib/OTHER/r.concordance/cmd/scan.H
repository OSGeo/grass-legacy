#include <scanner.H>

#define LEX_VARDEF     -101
#define LEX_SCENARIO   -102
#define LEX_OUTPUT     -103
#define LEX_INPUT      -104
#define LEX_FILL       -105
#define LEX_EOF        -106
#define LEX_DEFINITION -107
#define LEX_TO         -108
#define LEX_SCALE      -109

// ----------------------------------------------------
// se ritorna una delle def., ha letto una sezione 
// corretta; nel val appropriato ha messo il parametro
// se previsto; si posiziona oltre la parentesi chiusa
// LEX_DEFINITION e' ritornato solo da read_var_definition
// ----------------------------------------------------

enum classtype { name, value, range, gt, lt, ge, le, as_is }; 

class ClassDef : public TObject
{
  // passed to read_val_definition to host definition lh side
public:
  classtype _cltype;
  double    _val;
  double    _val2;
  double    _retval;    // filled by define_vars not by scanner
  char      _name[36];
  
  
  virtual ClassDef* dup();

  ClassDef() : TObject() { _name[0] = '\0'; }
  virtual ~ClassDef() {}
};

class RC_Scanner : public Scanner
{

protected:
  virtual int postprocess_token(int val, char* token);
public:

  // -------------------------------------------------------------
  // legge una definizione di questo tipo:
  // <range|name|value>: <var=value>[, <var=value>[, ...]];
  // ritorna: in classdef tutta la parte prima dei due punti
  // adeguatamente parsata e decodificata
  // in varvals mette delle ttoken_strings con nome e valore var
  // ritorna LEX_DEFINITION o quello che segue (di solito una sezione)
  // -------------------------------------------------------------
  int read_var_definition(ClassDef& classdef, TArray& varvals);
  
  RC_Scanner(const char* f) : Scanner(f)
    {
      define_keyword("VARDEF",   LEX_VARDEF);
      define_keyword("SCENARIO", LEX_SCENARIO);
      define_keyword("OUTPUT",   LEX_OUTPUT);
      define_keyword("INPUT",    LEX_INPUT);
      define_keyword("FILL",     LEX_FILL);
      define_keyword("EOF",      LEX_EOF);
      define_keyword("SCALE",    LEX_SCALE);
      define_keyword("to",       LEX_TO);
    }
  virtual ~RC_Scanner() {}
};


