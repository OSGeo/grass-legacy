#include <ctype.h>
#include <scanner.H>
#include <stdio.h>

extern "C" {
double atof(char*);
long atol(char*);
int atoi(char*);
};

char Scanner::nextc() 
{
  char c;
  c = _lexin.get();
  return c;
}

bool Scanner::is_integer (char *s)
{
   int c, i = 0;
   if (s[0] == '-') i++;
   while (c = s[i++])
      if (!(isdigit(c))) return 0;
   return 1;
}

bool Scanner::is_float (char *s)
{
   int c, i = 0, dotflag = 0;
   if (s[0] == '-') i++;
   while (c = s[i++])
      {
      if ( c == '.')
         {
         if (dotflag) return 0;
         else dotflag++;
         continue;
         }
      if (!(isdigit(c))) return 0;
      }
   return 1;
}

void Scanner::define_keyword(const char* k, int value)
{
  TString* t = new TString(16);
  (*t) << value;
  _keywords.add(k,t,TRUE);
}


/*------------------------ check_newline ------------------------------------*/

void Scanner::check_newline ()
{
  char c;
  
  _yyline++;
  
  for(;;)
    {
      c = nextc();
      
      if ( c == '\n' )
	{
	  _yyline++;
	  continue;
	}
      
      if ( c != ' ' && c != '\t' )
	{
	  _lexin.putback(c);
	  return;
	}
    }  
}

/*---------------------- Skip whitespace routine ----------------------------*/
// Aggingere gestione errore

int Scanner::skip_whitespace()
{
  char c;
  register inside;
  
  c = nextc();
  
  for(;;)
    {
      switch (c)
	{
	case '#':         // commento fine riga
	  while ((c = nextc()) != '\n')
	    {
	      if (_lexin.eof())
		{
                  _errflag++;
                  break;
		}
	    }
	  check_newline();
	  break;
	case '/':
	  c = nextc();
	  if (c != '*')
	    {
	      _lexin.putback(c);
	      return '/';
	    }
	  c = nextc();
	  inside = 1;
	  while (inside)
	    {
	      if (c == '*')
		{
                  while (c == '*')
		    c = nextc();
                  if (c == '/')
		    {
		      inside = 0;
		      c = nextc();
		    }
		}
	      else if ( c == '\n' )
		{
                  _yyline++;
                  c = nextc();
		}
	      else if (_lexin.eof() )
		// ERROR : commento non terminato
		_errflag++;
	      else
		c = nextc();
	    }
	  break;
	case '\n':
	  check_newline();
	case ' ':
	case '\t':
	case '\f':
	case '\r':
	case '\b':
	  c = nextc();
	  break;
	default:
	  return c;
	}
    }
}



/*-------------------------- Lexical analyzer -------------------------------*/

int Scanner::yylex()
{
   register c;
   int ret = LEX_UNKNOWN;
   c = skip_whitespace();

   if (_lexin.eof() ) return LEX_ENDFILE;

   if (strchr(_separators,c) != NULL )  // terminatore
      {
      _yytxt2[0] = c;
      _yytxt2[1] = EOS;
      return c;
      }
   int i = 0;

   for(;;)
     {
       _yytxt[i++] = c;
       c = nextc();
       if (isspace(c))    // check for whitespace
         {
	   _lexin.putback(c);
	   break;
         }
       if (strchr(_separators,c) != NULL)
         {
	   _lexin.putback(c);
	   break;
         }
     }
   _yytxt[i] = '\0';
   
   if (is_integer(_yytxt))
     {
       _itok = atol(_yytxt);
       ret = LEX_INTEGER;
     }
   else if (is_float(_yytxt))
     {
       _ftok = atof(_yytxt);
       ret = LEX_FLOAT;
     }
   else if (_keywords.is_key(_yytxt))
     {
       TString& t = (TString&)_keywords[_yytxt];
       _itok = atoi(t);
       ret =  LEX_KEYWORD;
     }
   // controllo numero o identificatore
   else if (isalpha(_yytxt[0]) || _yytxt[0] == '_')
     ret = LEX_IDENTIFIER;
   
   return ret;
 }


int Scanner::next()
{
  do {
    _yyval = yylex();
  }
  while((_yyval = postprocess_token(_yyval, _yytxt)) == LEX_IGNORE);
  return _yyval;
}


void Scanner::flush(int ID)
{
  int r;
  while ((r = next()) != ID)
    if (r == LEX_ENDFILE) break;
}

Scanner::Scanner(const char* fname) : TObject(), _lexin(fname), _yyline(0),
                                      _error(FALSE)
{
  if (!_lexin.is_open()) _error = TRUE;
  strcpy(_separators, ",:;[]{}()=<>\"\':q
");
}
