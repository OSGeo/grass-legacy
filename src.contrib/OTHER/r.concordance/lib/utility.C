#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <strings.H>

#define __UTILITY_CPP
#include <utility.H>


#include <unistd.h>
int remove(const char* path)
{ return unlink(path); }


bool fcopy(const char* orig, const char* dest)
{
  const char* const rflag = "r";
  const char* const wflag = "w";

  FILE* i = fopen(orig, rflag);
  if (!i) return error_box("Impossibile leggere il file %s", orig);

  FILE* o = fopen(dest, wflag);
  CHECKS(o, "Impossibile scrivere il file ", dest);

  const word size = 16*1024;
  TString buffer(size);

  while (TRUE)
  {
    const word letti = fread((char*)(const char*)buffer, 1, size, i);
    fwrite((char*)(const char*)buffer, 1, letti, o);
    if (letti < size) break;
  }

  fclose(o);
  fclose(i);

  return TRUE;
}


bool fexist(const char* file)
{
  return access(file, F_OK) == 0;
}


// Best function of the year
// Certified 99%
char* format(const char* fmt, ...)
{
  va_list pars;

  va_start(pars, fmt);
  const int tot = vsprintf(__tmp_string, fmt, pars);
  va_end(pars);

  CHECK(tot >= 0 && tot < sizeof(__tmp_string)-1, "Ue'! Ma quanto scrivi?");
  return(__tmp_string);
}

const char* cmd2name(const char* argv0, const char* argv1)
{
  TFilename app(argv0); app.ext(""); app = app.name();
  if (argv1 != NULL && *argv1 != '\0') app << ' ' << argv1;
  else app << " -0";
  app.lower();

  const int par = app.find(" -");
  if (par > 0)
  {
    int num = atoi(app.mid(par+2)) + 1;
    char c = (num > 9) ? ('a'+num-10) : ('0'+num);
    app.cut(par);
    app << c << "00";
  }

  return strcpy(__tmp_string, app);
}


///////////////////////////////////////////////////////////
// Conversione in cifre romane
///////////////////////////////////////////////////////////

HIDDEN const char * cifre_romane = "IVXLCDM@";
HIDDEN const int valori_cifre [] = { 1, 5, 10, 50, 100, 500, 1000, -1 };

HIDDEN int ctoi(char c)
{
  if (c == '\0') return 0;

  c = toupper(c);
  for (int i = 0; cifre_romane[i]; i++)
    if (cifre_romane[i] == c) return valori_cifre[i];

  return -1;
}

int rtoi(const char * val)
{
  if (val == NULL) return 0;

  int tot = 0;
  int value = ctoi (val[0]);
  for (int i = 1; value > 0; i++)
  {
    const int next_val = ctoi(val[i]);
    if (value < next_val) tot -= value;
    else                  tot += value;
    value = next_val;
  }

  return (value == 0) ? tot : -1;
}

const char* itor(int num)
{
  HIDDEN char roman_string[16];
  int cifra = 0;

  for (int pos = 7; pos--;)
  {
    int val = valori_cifre[pos];
    int quanti = num / val;
    if (quanti < 4)
    {
      if ((pos & 1) && quanti == 1 && (num/valori_cifre[pos-1]) == 9)
      {
        roman_string[cifra++] = cifre_romane[pos-1];
        roman_string[cifra++] = cifre_romane[pos+1];
        val = valori_cifre[pos-1];
        quanti = 9;
      }
      else for (int i = 0; i < quanti; i++)
        roman_string[cifra++] = cifre_romane[pos];
    }
    else
    {
      roman_string[cifra++] = cifre_romane[pos];
      roman_string[cifra++] = cifre_romane[pos+1];
    }

    num -= quanti * val;
  }

  roman_string[cifra] = '\0';
  return roman_string;
}

const char *esc(const char* s)

{
  const char    *s1 = s;
  char  *s2 = __tmp_string;
  int           base;

  while (*s1)
  {
    if (*s1 == '\\')
    {
      s1++;
      switch (tolower(*s1))
      {
      case 'b' : *s2++ = '\b'; break;
               case 'e' : *s2++ = '\033'; break;
               case 'f' : *s2++ = '\f'; break;
               case 'n' : *s2++ = '\n'; break;
               case 'r' : *s2++ = '\r'; break;
               case 't' : *s2++ = '\t'; break;
                 default  :
               {
                 if (isdigit(*s1))
                 {
                   if (*s1 == '0')
                   {
                     s1++;
                     if (tolower(*s1) == 'x') 
                     {
                       s1++;
                       base = 16;
                     }
                     else base = 8;
                   }
                   else base = 10;
                   *s2 = 0;
                   char c = tolower(*s1);
                   while (isdigit(c) || (base == 16 && c >= 'a' && c <= 'f'))
                   {
                     *s2 *= base;
                     if (isdigit(*s1)) *s2 += (*s1 - 48);
                     else *s2 += (*s1 - 'a' + 10) & 0x0F;
                     s1++;
                     c = tolower(*s1);
                   }
                   s2++; s1--;
                 }
                 else *s2++ = *s1;
               }
               }
    }
    else
      if (*s1 == '^')
      {
        s1++;
        *s2++ = (tolower(*s1) - 'a' + 1);
      }
      else *s2++ = *s1 ;
    s1++;
  }
  *s2 = '\0';
  return(__tmp_string);
}

HIDDEN const char * const key = "QSECOFR-";

const char * encode( const char * data)
{
  const int len = strlen(data);

  for (int i = 0; i < len; i++)
    __tmp_string[i] = data[i] + (i < 8 ? key[i] : data[i - 8]);
  __tmp_string[i] = '\0';
  return __tmp_string; 
}

const char * decode( const char * data)
{
  const int len = strlen(data);

  for (int i = 0; i < len; i++)
    __tmp_string[i] = data[i] - (i < 8 ? key[i] : __tmp_string[i - 8]);
  __tmp_string[i] = '\0';
  return __tmp_string; 
}


int stricmp(const char* s1, const char* s2)
{
  while (toupper(*s1) == toupper(*s2))
    if (*s1++ == '\0' && *s2++ == '\0') 
      return 0;
  return *s1 - *s2;
}

