#ifndef __STRINGS_H
#define __STRINGS_H

#ifndef __STRING_H
#include <string.h>
#endif

#ifndef __OBJECT_H
#include <object.H>
#endif

// @C
// Classe TString : public TObject
// @END

class TString : public TObject
{
protected:
  // @DPROT
  char* _str;   // Puntatore alla stringa
  int _size;    // Lunghezza
  // @END

  // @FPROT
  int make_room(int size);   // Cerca spazio per altri size caratteri
  TString& set(const char*); // Inizializza con la stringa puntata da char* di lunghezza size

  TString(char* str, int size) : _str(str), _size(size) {}

public:
  // @FPUB
  virtual void resize(int size, bool cpy); // Ri/alloca la stringa

  TString();
  TString(int size);         // Default constructor for a string of given size
  TString(const char* s);    // Costruttore a partire da una stringa s
  TString(const TString& s); // Costruttore da un oggetto TString s
  virtual ~TString();        // Deallocates the string

  ///////////////////////////////////////////////////////////
    // @DES Methods derived from TObject
    ///////////////////////////////////////////////////////////

      // @FPUB
      virtual const char* class_name() const;       // Ritorna il nome della classe
  virtual word class_id() const;                // Ritorna l'identificatore della classe
  virtual bool ok() const { return _str != NULL; }
  virtual TObject* dup() const;
  virtual void print_on(ostream& out) const;
  virtual void read_from(istream& in);
  virtual word hash() const; // Return hash value

  ///////////////////////////////////////////////////////////
    // @DES Query methods
    ///////////////////////////////////////////////////////////

      // @FPUB
      operator const char*() const { return (const char*)_str; }  // *(TString) -> _str
  char& operator[](int i)                                     // TString[i] -> _str[i]
  { 
    CHECKD(i >= 0 && i <= _size, "Bad string subscript: ", i);
    return _str[i]; 
  }                 
  char operator[](int i) const                                // TString[i] -> _str[i]
  { 
    CHECKD(i >= 0 && i <= _size, "Bad string subscript: ", i);
    return _str[i]; 
  }  

  int size() const { return _size; }
  int len() const { return strlen(_str); }
  bool empty() const { return *_str == '\0'; }
  bool not_empty() const { return *_str != '\0'; }

  int find(char, int from = 0) const;   // Ritorna la posizione del carattere char nell'oggetto TString
  int find(const char* s, int from = 0) const; // Ritorna la posizione della stringa s nell'oggetto TString

  const TString& left(int count) const; // Ritorna l'oggetto TString composto dai count caratteri da sinistra
  const TString& mid(int from, int count = -1) const; // Ritorna l'oggetto TString composto dai count caratteri a partire da from
  const TString& sub(int from, int to = -1) const; // Ritorna la stringa da FROM a TO (escluso)
  const TString& right(int count) const;        // Ritorna l'oggetto TString composto dai count caratteri da destra

  ///////////////////////////////////////////////////////////
    // @DES Modifying methods
    ///////////////////////////////////////////////////////////

      // @FPUB
      TString& fill(char c, int n = -1);         // Riempie con n caratteri c
  TString& spaces(int n = -1) { return fill(' ', n); }
  TString& overwrite(const char* s, int pos = 0); // Sovrascrive la stringa s dalla posizione pos
  TString& insert(const char* s, int pos = 0);    // Inserisce la stringa s dalla posizione pos

  TString& strip(const char* k);  // Elimina tutti i caratteri contenuti in k
  TString& strip_spaces();   // Elimina tutti gli spazi non contenuti tra apici singoli o doppi
  TString& ltrim(int n = 0); // Elimina gli spazi da sinistra se n=0 altrimenti elimina i primi n caratteri (da sinistra).
  TString& rtrim(int n = 0); // Elimina gli spazi da destra se n=0 altrimenti elimina i primi n caratteri (da destra).
  TString& trim();                                       // ltrim e rtrim

  TString& right_just(int n = -1, char c = ' ');  // Giustifica a destra
  TString& center_just(int n = -1, char c = ' '); // Centra
  TString& left_just(int n = -1, char c = ' ');   // Giustifica a sinistra

  TString& picture(const char* pic, const char* s);
  virtual TString& format(const char* fmt, ...);

  TString& cut(int n);  // Inserisce un '\0' alla posizione n-esima.

  TString& upper();     // Mette la stringa in maiuscolo
  TString& lower();     // Mette la stringa in minuscolo

  ///////////////////////////////////////////////////////////
    // @DES Standard operators
    ///////////////////////////////////////////////////////////

      // @FPUB
      const TString& operator =(const TString& s) { return set(s._str); }
  const TString& operator =(const char* s) { return set(s); }

  TString& operator <<(const char*);
  TString& operator <<(char);
  TString& operator <<(int);
  TString& operator <<(long);
  TString& operator <<(double);
  TString& operator <<(const TString& str);     // For efficiency only

  bool operator ==(const char* s) const { return strcmp(_str, s) == 0; }
  bool operator ==(char* s) const { return strcmp(_str, s) == 0; }
  bool operator ==(const TString& s) const { return strcmp(_str, s._str) == 0; }
  bool operator !=(const char* s) const { return strcmp(_str, s) != 0; }
  bool operator !=(char* s) const { return strcmp(_str, s) != 0; }
  bool operator !=(const TString& s) const { return strcmp(_str, s._str) != 0; }
  bool operator  <(const char* s) const { return strcmp(_str, s)  < 0; }
  bool operator  >(const char* s) const { return strcmp(_str, s)  > 0; }
  bool operator >=(const char* s) const { return strcmp(_str, s) >= 0; }
  bool operator <=(const char* s) const { return strcmp(_str, s) <= 0; }
  bool match(const char* s) const; 
  bool compare(const char* s, int max = -1, bool ignorecase = FALSE) const;
};

// @C
// Classe TFixed_string : public TString
// @END

class TFixed_string : public TString
{
protected:
  virtual void resize(int size, bool cpy);      // Causa un errore fatale!

public:
  TFixed_string(const char* str, int size = -1);
  virtual ~TFixed_string();

  virtual TString& format(const char* fmt, ...);

  const TString& operator =(const TString& s) { return set((const char*)s); }
  const TString& operator=(const char* str) { return set(str); }
  void strncpy(const char* s, int n);
};

class TString16 : public TFixed_string
{
  char _str16[17];

public:
  TString16(const char* str = "") : TFixed_string(_str16, 17) { set(str); }
  TString16(const TString& s) : TFixed_string(_str16, 17) { set(s); }
  const TString& operator =(const TString& s) { return set((const char*)s); }
  const TString& operator =(const char* str) { return set(str); }
};

class TString80 : public TFixed_string
{
  char _str80[81];

public:
  TString80(const char* str = "") : TFixed_string(_str80, 81) { set(str); }
  TString80(const TString& s) : TFixed_string(_str80, 81) { set(s); }
  const TString& operator =(const char* str) { return set(str); }
  const TString& operator =(const TString& s) { return set((const char*)s); }
};

class TString256 : public TFixed_string
{
  char _str256[257];

public:
  TString256(const char* str = "") : TFixed_string(_str256, 257) { set(str); }
  TString256(const TString& s) : TFixed_string(_str256, 257) { set(s); }
  const TString& operator =(const char* str) { return set(str); }
  const TString& operator =(const TString& s) { return set((const char*)s); }
};



// @C
// Classe TFilename : public TString80 (256 su Windows'95)
// @END

class TFilename : public TString80
{
public:
  // @FPUB

  TFilename(const char* n = "") : TString80(n) {}
  TFilename(const TString& n) : TString80(n) {}

  const TString& operator =(const char* s) { return set(s); }
  const TString& operator =(const TString& s) { return set((const char*)s); }
  // assegnazione tra TFile e stringa

  const char* ext() const;      // Ritorna l'estensione
  void ext(const char*);        // Imposta come estensione la stringa puntata da char*

  const char* name() const;     // Ritorna il nome del file
  const char* path() const;     // Ritorna il nome del direttorio
  const TFilename& temp(const char* prefix = NULL);     // Genera il nome di un file temporaneo
  const TFilename& tempdir();   // Genera il nome della directory temporanea
};

// @C
// Classe TToken_string : public TString
// @END

class TToken_string : public TString
{
  // @DPRIV
  char _separator;      // Carattere separatore
  int _last;            // Puntatore all'ultimo

protected:
  // @FPROT

  virtual TObject* dup() const; // Crea un duplicato della token string
  bool set_item(const char* v, int n);
  
public:
  // @FPUB
  TToken_string(const char* = "", char separator = '|');
  TToken_string(int n, char separator = '|');
  TToken_string(const TToken_string& s);

  void separator(char s) { _separator = s; }    // Setta il separatore a s

  void restart() { _last = empty() ? -1 : 0; } // Rimette all'inizio il puntatore
  const TString& operator =(const char* s) { set(s);restart();return *this; }
  const TString& operator =(const TString& s) { set(s);restart();return *this; }

  void add(const char* s, int n = -1); // Aggiunge una stringa
  void add(char c, int pos = -1);      // Aggiunge un char
  void add(long n, int pos = -1);      // Aggiunge un intero
  void add(int n, int pos = -1);       // Aggiunge un intero
  void destroy(int pos);        // Toglie la stringa pos
  const char* get();            // Ritorna il prossimo token
  const char* get(int n);       // Ritorna un token (-1 = prossimo; -2 = ultimo; n = ennesimo)
  char get_char(int n = -1);    // Ritorna un carattere
  int get_int(int n = -1);      // Ritorna un intero
  long get_long(int n = -1);    // Ritorna un intero esteso
  int get_pos(const char* s);   // Ritorna la posizione dell'item s
  int items() const;            // Ritorna il numero di token presenti
  bool empty_items() const;     // Controlla se tutti i token sono nulli
};

///////////////////////////////////////////////////////////
// @DES Paragraph
///////////////////////////////////////////////////////////

class TParagraph_string : public TToken_string
{
  int _width;
  bool _fixed;

protected:
  void tokenize();

public:
  TParagraph_string(const char* s, int width);
  const TString& operator =(const char* s);
  void set_width(int width) { _width = width; }
};

#endif



