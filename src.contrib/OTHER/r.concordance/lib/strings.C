#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include <strings.H>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <utility.H>

HIDDEN const int MAXSTR = 301;
HIDDEN char __spark[MAXSTR];                                            // Utility buffer
TFixed_string spark(__spark, MAXSTR);

// Dinamically resizes a string
// Certified 99%
// It doesn't work for static strings and negative values of size
void TString::resize(int size, bool cpy)
{
  char* s = new char[size+1];
  if (cpy && _str) strcpy(s, _str);
  else *s = '\0';

  if (_str)
    delete _str;

  _str  = s;
  _size = size;
}

// Set the value for the string
// Certified 99% (uses resize)
TString& TString::set(const char* s)
{
  if (s == NULL) s = "";
  const int sz = *s ? strlen(s) : 7;
  if (sz > size()) resize(sz, FALSE);
  strcpy(_str, s);

  return *this;
}

// Eventually expands the string for s more chars
int TString::make_room(int s)
{
  const int lun = len();
  const int spare = size() - lun;
  if (spare < s)
    resize(size() + 2*s, TRUE);     // Melius abundare ...
  return lun;
}

TString::TString(const char* s) : _str(NULL), _size(0)
{ set(s); }

TString::TString(const TString& s) : _str(NULL), _size(0)
{ set(s); }

TString::TString(int size) : _str(NULL), _size(0)
{ resize(size, FALSE); }

TString::TString() : _str(NULL), _size(0)
{ resize(7, FALSE); }

TString::~TString()
{
  if (_str)
    delete _str;
}

TString& TString::operator <<(const char* s)
{
  if (s && *s)
  {
    const int pos = make_room(strlen(s));
    strcpy(&_str[pos], s);
  }
  return *this;
}

TString& TString::operator <<(char c)
{
  int pos = make_room(1);
  _str[pos++] = c;
  _str[pos] = '\0';
  return *this;
}


TString& TString::operator <<(int n)
{                          
  char s[16];
  sprintf(s, "%d", n);
  return operator <<(s);
}


TString& TString::operator <<(long n)
{
  char s[16];
  sprintf(s, "%ld", n);
  return operator <<(s);
}

TString& TString::operator <<(double n)
{
  char s[32];
  sprintf(s, "%lf", n);
  return operator <<(s);
}


TString& TString::operator <<(const TString& str)
{ return operator <<(str._str); }


TString& TString::strip(const char* k)
{
  int j = 0;
  for (int i = 0; _str[i]; i++)
  {
    char c = _str[i];
    if (strchr(k, c) == NULL) _str[j++] = c;
  }
  return cut(j);
}


TString& TString::strip_spaces()
{
  char instring = '\0';
  int j = 0;
  for (int i = 0; _str[i]; i++)
  {
    char c = _str[i];
    if (isspace(c) && !instring) continue;
    if (c == '"' || c == '\'')
    {
      if (instring == c) instring = '\0';
      else
        if (instring == '\0') instring = c;
    }
    _str[j++] = c;
  }
  _str[j] = '\0';
  return *this;
}

// Certified 100%
const char* TString::class_name() const
{ return "String"; }

// Certified 100%
word TString::class_id() const
{ return CLASS_STRING; }

TObject* TString::dup() const
{
  TString* s = new TString(size());
  s->set(_str);
  return s;
}


void TString::read_from(istream& in)
{
  in >> __tmp_string;
  set(__tmp_string);
}


// Certified 100%
void TString::print_on(ostream& out) const
{ out << _str; }


// Certified 100%
int TString::find(char c, int from) const
{
  CHECKD(from <= len(), "Trying to find past end of string", from);
  const char* p = strchr(_str + from, c);
  return p ? int(p - _str) : -1;
}

// Certified 50%
bool TString::match(const char* s) const

{   
  if (strchr(s, '?') == NULL) return operator ==(s);

  for (const char* me = _str; *s && *me; s++, me++)
    if (*s != '?' && *s != *me) break;
  return *s == '\0' && *me == '\0';
}

HIDDEN char* strstr(const char* string1, const char* string2)
{
  const int len = strlen(string2);
  while (*string1)
  {
    if (strncmp(string1, string2, len) == 0) return (char*) string1;
    string1++;
  }
  return NULL;
}


// Certified 100%
int TString::find(const char* s, int from) const
{
  CHECKD(from <= len(), "Trying to find past end of string", from);
  const char* p = strstr(_str + from, s);
  return p ? int(p - _str) : -1;
}


// Certified 99%
const TString& TString::left(int count) const
{
  spark.strncpy(_str, count);
  return spark;
}

// Certified 99%
const TString& TString::right(int count) const
{
  int from = len()-count;
  if (from < 0) from = 0;
  spark = _str + from;
  return spark;
}


// Certified 100%
const TString& TString::mid(int from, int count) const
{
  const int l = len();

#ifdef DBG
  if (from < 0)
  {
    yesnofatal_box("Ivalid MID parameter: from = %d", from);
    from = 0;
  }
#endif

  if (from > l) from = l;
  if (count < 0) count = l-from;
  
  spark.strncpy(&_str[from],count);
  return spark;
}


// Certified 100%
const TString&  TString::sub(int from, int to) const
{
  const int count = to-from;
  return mid(from, count);
}

// Certified 100%
TString& TString::cut(int n)
{
  _str[n] = '\0';
  return *this;
}


TString& TString::ltrim(int count)
{
  const char* s;

  if (count > 0)
  {
    if (count >= len()) return cut(0);
    s = &_str[count];
  }
  else for (s = _str; *s && isspace(*s); s++);

  if (s != _str) strcpy(_str, s);
  return *this;
}

TString& TString::rtrim(int count)
{
  if (count > 0)
  {
    int i = len() - count;
    if (i < 0) i = 0;
    cut(i);
  }
  else
  {
    char* good = _str-1;
    for (char* s = _str; *s; s++) if (!isspace(*s)) good = s;
    *(good+1) = '\0';
  }
  return *this;
}

TString& TString::trim()
{
  char* last = _str;

  // Salta spazi iniziali
  for (const char* s = _str; *s && isspace(*s); s++);

  // Copia stringa
  if (s > _str)
  {
    for(char* c = _str; *s; s++)
    {
      *c++ = *s;
      if (!isspace(*s)) last = c;
    }
    // Elimina spazi finali
    *last = '\0';
  }
  else rtrim();

  return *this;
}

// Certified 100%
TString& TString::fill(char c, int n)
{
  if (n < 0) n = size(); else
    if (n > size()) resize(n, FALSE);
  memset(_str, c, n);
  _str[n] = '\0';
  return *this;
}

// Certified 100%
TString& TString::right_just(int n, char c)
{
  if (n < 0) n = size();
  trim();
  spark = _str;
  fill(c, n);
  overwrite(spark, n-spark.len());

  return *this;
}

// Certified 100%
TString& TString::center_just(int n, char c)
{
  if (n < 0) n = size();
  trim();
  spark = _str;
  fill(c, n);
  const int p = (n-spark.len()) >> 1;
  overwrite(spark, p);

  return *this;
}

// Certified 100%
TString& TString::left_just(int n, char c)
{
  if (n < 0) n = size();
  trim();
  spark = _str;
  fill(c, n);
  overwrite(spark, 0);

  return *this;
}

TString& TString::picture(const char* pic, const char* s)
{
  if (pic == NULL || *pic == '\0')
    return set(s);

  set(pic);

  int l = strlen(s)-1;  // Prossimo carattere da sostituire a #

  for (int i = len()-1; i >= 0; i--)
  {
    const char k = pic[i];
    if (k == '#') _str[i] = (l >= 0) ? s[l--] : ' ';
    else if (k == '~') { _str[i] = ' '; l--; }
  }

  return *this;
}

// Certified 90% (__spark size limited)
TString& TString::format(const char* fmt, ...)
{
  va_list pars;
  va_start(pars, fmt);
  const int tot = vsprintf(__spark, fmt, pars);
  va_end(pars);

  CHECK(tot >= 0 && tot < spark.size(), "Ue'! Quanto scrivi?");
  if (tot > size()) resize(tot, FALSE);
  strcpy(_str, __spark);

  return *this;
}

// Certified 100%
TString& TString::upper()
{
  for (char* s = _str; *s; s++) *s = toupper(*s);
  return *this;
}

// Certified 100%
TString& TString::lower()
{
  for (char* s = _str; *s; s++) *s = tolower(*s);
  return *this;
}


// Certified 90%
TString& TString::overwrite(const char* s, int pos)
{
  const int l = len();
  if (pos < 0) pos = l;
  const int max = pos+strlen(s);
  if (max > size()) resize(max, TRUE);          // resize needed?

  const bool over = max > l;                    // beyond end of string?
  for (int i = l; i < pos; i++) _str[i] = ' ';  // space padding
  for (; *s; s++) _str[pos++] = *s;             // write
  if (over) _str[pos] = '\0';                   // end of string

  return *this;
}


// Certified 90%
TString& TString::insert(const char* s, int pos)
{
  if (s && *s)
  {
    const int l = strlen(s);
    make_room(l);
    mid(pos);                     // Scrivi in spark la stringa da pos in poi
    overwrite(s, pos);            // Aggiungi s
    strcpy(&_str[pos+l], spark);  // Aggiungi spark
  }
  return *this;
}

// Villa's megasmart hash function
// Certified 90%
word TString::hash() const
{
  word h = 0x0000;
  for (int i = 0; _str[i]; i++)
    h ^= (i & 0x1) ? (_str[i] << 8) : _str[i];
  return h;
}

///////////////////////////////////////////////////////////
// TFixed_string
///////////////////////////////////////////////////////////

// Certified 100%
TFixed_string::TFixed_string(const char* str, int size)
: TString((char*)str, (size < 1) ? strlen(str) : size-1)
{
  if (size > 0 && memchr(str, '\0', size) == NULL)
    cut(0);
}


// Certified 100%
TFixed_string::~TFixed_string()
{ _str = NULL; }        // Impedisce la deallocazione


// Certified 100%
void TFixed_string::resize(int size, bool)
{
#ifdef DBG
  fatal_box("Impossibile ridimensionare una stringa fissa da %d a %d caratteri:\n'%s'", 
            _size, size, _str);
#endif
}


// Certified 99% (s != NULL)
void TFixed_string::strncpy(const char* s, int n)
{
  CHECKD(n <= _size, "Fixed string can't be strncpyed: lenght ", n);
  for (int i = 0; *s && i < n; i++) _str[i] = *s++;
  _str[i] = '\0';
}

// Certified 99%
// More efficient than TString::format, it does not use spark
TString& TFixed_string::format(const char* fmt, ...)
{
  va_list pars;
  va_start(pars, fmt);
  const int tot = vsprintf(_str, fmt, pars);
  va_end(pars);
  CHECK(tot >= 0 && tot < size(), "Ue'! Quanto scrivi con 'sta format?");
  return *this;
}



///////////////////////////////////////////////////////////
// Filename
///////////////////////////////////////////////////////////

inline bool is_not_slash(char s)
{  return s != '\\' && s != '/'; }

// Certified 90%
const char* TFilename::ext() const
{
  const char* d = strrchr(name(), '.');
  if (d && is_not_slash(*(++d))) return d;
  return "";
}

// Certified 90%
void TFilename::ext(const char* e)
{
  char* d = strrchr(name(), '.');
  if (d && is_not_slash(*(d+1))) *d = '\0';

  if (*e && *e != '.') *this << ".";
  *this << e;
}


// Certified 90%
const char* TFilename::name() const
{
  const char* d = strrchr(_str, '/');
  if (d == NULL) d = strrchr(_str, '\\');
  if (d == NULL) d = strchr(_str, ':');
  if (d == NULL) d = _str-1;
  return d+1;
}

// Certified 90%
const char* TFilename::path() const
{
  const char* d = strrchr(_str, '/');
  if (d == NULL) d = strrchr(_str, '\\');
  if (d == NULL)
  {
    d = strchr(_str, ':');
    if (d != NULL) d++;
  }
  if (d == NULL) spark.cut(0);
  else spark.strncpy(_str, d - _str);
  return spark;
}

// Certified 70%
const TFilename& TFilename::tempdir()
{
  const char* dirpref = getenv("TEMP");
  if (dirpref == NULL) dirpref = getenv("TMP");
  if (dirpref == NULL) dirpref = "/tmp";
  set(dirpref);
  
  const int last = len()-1;
  if (!is_not_slash(_str[last])) 
    cut(last);

  int res = 0;
  
  if (!fexist(_str))
  {  
    res =
      mkdir(_str, 0777);
  }
  
  if (res != 0)
    fatal_box("Impossibile creare la directory '%s' per i file temporanei", _str);

  return *this;
}


// Certified 50%
const TFilename& TFilename::temp(const char* prefix)
{
  const TFilename dirpref(tempdir());
  char* t = NULL;
  
  if (prefix)
  {
    set(prefix);      // Copia prefisso e ...
    strip("$#");      // ... toglie caratteri jolly

    const TFixed_string f(prefix);
    if (f.find("$$") != -1) *this << getpid();
    if (f.find("##") != -1) *this << getuid();

    t = tempnam((char*)(const char*)dirpref, (char*)_str);
  }
  else
    t = tempnam((char*)(const char*)dirpref, NULL);

  set(t);

#ifdef DBG
  if (fexist(_str)) 
    fatal_box("Il file '%s' esiste gia'", _str);
#endif  
  if (t) free(t);

  return *this;
}


///////////////////////////////////////////////////////////
// Token string
///////////////////////////////////////////////////////////

// Certified 100%
TToken_string::TToken_string(const char* s, char separator)
: TString(s), _separator(separator)
{
  restart();
}

// Certified 100%
TToken_string::TToken_string(int n, char separator)
: TString(n), _separator(separator), _last(0)
{}

// Certified 100%
TToken_string::TToken_string(const TToken_string& s)
: TString(s), _separator(s._separator), _last(s._last)
{}


// Certified 100%
TObject* TToken_string::dup() const
{
  return new TToken_string(_str, _separator);
}

// Certified 90%
const char* TToken_string::get()
{
  if (_last < 0) return NULL;

  const int start = _last;

  if (_str[start] == '\0')
  {
    _last = -1;
    return NULL;
  }
  else
    if (_str[start] == _separator)
    {
      _last = start+1;
    }
    else
    {
      const int k = find(_separator, start);
      _last = (k >= 0) ? k+1 : -1;
      return sub(start, k);
    }
  return "";
}

// Certified 50%
const char* TToken_string::get(int n)
{
  if (n < 0)
  {
    if (n == -2)
    {
      const char* sep = strrchr(_str, _separator);
      _last = -1;
      return sep ? sep+1 : _str;
    }
    else return get();
  }
  int sep = 0;
  for (const char* s = _str; sep < n && *s; s++)
    if (*s == _separator) sep++;

  if (sep >= n)
  {
    char* p = strchr(s, _separator);
    if (p == NULL)
    {
      spark = s;
      _last = -1;
    }
    else
    {
      *p = '\0';
      spark = s;
      *p = _separator;
      _last = (int)((const char*)p - _str) + 1;
    }
  }
  else
  {
    _last = -1;
    return NULL;
  }

  return spark;
}


// Certified 99%
char TToken_string::get_char(int n)
{
  const char* const car = get(n);
  return car ? *car : '\0';
}

// Certified 99%
int TToken_string::get_int(int n)
{
  const char* const num = get(n);
  return num ? atoi(num) : 0;
}


// Certified 99%
long TToken_string::get_long(int n)
{
  const char* const num = get(n);
  return num ? atol(num) : 0L;
}


// Certified 70%
bool TToken_string::set_item(const char* v, int n)
{
  int sep = 0;
  for (int i = 0; sep < n && _str[i]; i++)
    if (_str[i] == _separator) sep++;

  if (sep < n)  // Aggiunge items mancanti prima della posizione n
  {
    for (;sep < n; sep++) *this << _separator;
    *this << v;
    return FALSE;
  }

  int e = find(_separator, i);
  if (e < 0) e = len();

  spark = &_str[e];           // Salva items seguenti
  cut(i);                     // Considera solo items precedenti
  *this << v << spark;        // Aggiunge item desiderato e seguenti
  return TRUE;
}


// Certified 80%
int TToken_string::get_pos(const char* s)
{
  const char* item;

  restart();
  for (int i = 0; (item = get()) != NULL; i++)
    if (strcmp(item, s) == 0) return i;

  return -1;
}


// Certified 90%
bool TToken_string::empty_items() const
{
  for (const char* c = _str; *c; c++)
    if (!isspace(*c) && *c != _separator) return FALSE;
  return TRUE;
}


// Certified 80%
int TToken_string::items() const
{
  int t = 0;
  if (not_empty())
  {
    t++;
    for (const char* s = _str; *s; s++) 
      if (*s == _separator) t++;
  }  
  return t;
}

// Adds an item to the token string
// Certified 99%
void TToken_string::add(const char* s, int pos)
{
  if (s == NULL || *s == '\0') s = " ";
  if (pos < 0)
  {
    if (not_empty()) *this << _separator;
    *this << s;
  }
  else
    set_item(s, pos);
  if (_last < 0) _last = 0;
}

// Certified 0%
void TToken_string::add(char c, int pos)
{
  const char s[2] = { c, '\0' };
  add(s, pos);
}

// Adds an integer value to the token string
// Certified 100%
void TToken_string::add(long n, int pos)
{
  char s[16];
  sprintf(s, "%ld", n);
  add(s, pos);
}

void TToken_string::add(int n, int pos)
{
  char s[16];
  sprintf(s, "%d", n);
  add(s, pos);
}

// Certified 50%
void TToken_string::destroy(int n)
{
  if (_last == -2) return ;

  if (n < 0)
  {
    char* s = strrchr(_str, _separator);
    if (s != NULL) *s = '\0';
  }
  else
  {
    int sep = 0;
    for (char* s = _str; sep < n && *s; s++)
      if (*s == _separator) sep++;

    if (sep >= n)
    {
      const char* p = strchr(s, _separator);
      *s = '\0';
      if (p != NULL) strcat(s,  p+1);
    }
  }
  restart();  
}

///////////////////////////////////////////////////////////
// Paragraph string
///////////////////////////////////////////////////////////

TParagraph_string::TParagraph_string(const char* s, int width)
: TToken_string(s, '|'), _width(width)
{ tokenize(); }

const TString& TParagraph_string::operator =(const char* s)
{
  TToken_string::operator=(s);
  tokenize();
  return *this;
}

void TParagraph_string::tokenize()
{
  int start = 0;
  for (int end = start+_width; end < len(); end = start+_width)
  {
    for (int i = end; i >= start; i--)
      if (isspace(_str[i])) break;

    if (i < start)
    {
      insert("|", end);
      start = end+1;
    }
    else
    {
      _str[i] = '|';
      start = i+1;
    }
  }
}
