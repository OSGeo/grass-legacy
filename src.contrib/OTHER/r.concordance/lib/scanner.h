#ifndef _SCANNER_H

#ifndef __ASSOC_H
#include <assoc.H>
#endif

#include <fstream.h>
#define LEX_ERROR       -8
#define LEX_IGNORE      -7
#define LEX_UNKNOWN     -6
#define LEX_INTEGER     -5
#define LEX_FLOAT       -4
#define LEX_IDENTIFIER  -3
#define LEX_KEYWORD     -2
#define LEX_ENDFILE     -1

// -------------------------------------------------------------------------
// Un simpatico scanner che capisce identificatori, parole chiave,
// interi, float e separatori
// Gestisce n. righe e commenti stile shell (#-->fine riga) e C (/* ... */)
// Si configura derivando e definendo postprocess_token
// -------------------------------------------------------------------------
// Amabile retaggio della vecchia Alice
// -------------------------------------------------------------------------

class Scanner : public TObject
{
  TAssoc_array _keywords;
  char         _separators[32];
  bool         _error;
  
  int  skip_whitespace();
  bool is_integer(char*);
  bool is_float(char*);

  int    _yyline;
  char   _yytxt[512];
  char   _yytxt2[3];
  int    _yyval;
  int    _yyalt;
  int    _errflag;
  int    _itok;
  double _ftok;
  const char EOS = 0x00;
  ifstream _lexin;

  char nextc();

protected:

  void check_newline();
  int  yylex();

  // ritornera' LEX_IGNORE se non si vuole che faccia nulla
  virtual int  postprocess_token(int val, char* token)
    { return val; }

  void set_result(const char* r) { strcpy(_yytxt,r); }
  void set_result(int n)         { _itok = n;        }
  void set_result(double d)      { _ftok = d;        }

public:


  bool error() { return _error; }
  // define keywords
  void define_keyword(const char* k, int value);

  int  next();
  bool expect(int ID) { return next() == ID; }
  
  // eat until ID or EOF
  void flush(int ID);

  const char* s_val()  { return (const char*)_yytxt;     }
  int         i_val()  { return _itok;                   }
  double      d_val()  { return _ftok;                   }
  
  int         line()   { return _yyline;                 }


  // default is  ",:;[]{}()= \t\n"
  void  set_separators(const char* s)
    { strcpy(_separators, s); }
 
  Scanner(const char* file);
  virtual ~Scanner() {}
};

#endif

