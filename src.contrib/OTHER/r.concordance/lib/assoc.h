#ifndef __ASSOC_H
#define __ASSOC_H

#ifndef __ARRAY_H
#include <array.H>
#endif

#ifndef __STRINGS_H
#include <strings.H>
#endif

class THash_object : public TObject
{
  friend class TAssoc_array;

  TString  _key;
  TObject* _obj;
public:
  
  TString& key() { return _key; }
  TObject& obj() { return *_obj; }

  THash_object(const char* k)
  { _key = k; }
  ~THash_object() { if (_obj != NULL) delete _obj; }
}; 


class TAssoc_array : public TObject
{
  enum           { HASH_SIZE = 113 };
  word           _cnt;
  word           _row;
  word           _col;
  TArray         _data[HASH_SIZE];

  THash_object* _lookup(const char* k, bool& isnew, bool insert = FALSE);

public:

  int items() const { return _cnt; }
  
  void destroy();

  // aggiunge oggetto; Se c'era gia' la chiave guarda force:
  // se force = TRUE  lo sostituisce e ritorna TRUE
  // se force = FALSE non sostituisce e ritorna TRUE
  // altrimenti ritorna FALSE
  bool add(const char* key, TObject* obj = NULL, bool force = FALSE);

  // aggiunge copia oggetto (deve avere dup()). Vedi l'altra
  // per i parametri
  bool add(const char* key, const TObject& obj, bool force = FALSE);

  // elimina oggetto; ritorna FALSE se non c'era
  bool remove(const char* key);

  // trova oggetto indicizzato; check se non c'e'
  // normalmente si usa operator[key]
  // se l'oggetto aggiunto era NULL ritorna error object
  TObject& find(const char* key);

  // ritorna puntatore o NULL
  TObject* objptr(const char* key);

  // TRUE se la chiave c'e', FALSE altrimenti
  bool     is_key(const char* key);

  // l'indice e' un po' strano ma si usera' questa poiche'
  //   1) e' intuitivo
  //   2) fa molto figo
  TObject& operator[] (const char* key) { return find(key); }
  
  // iterazione come TToken_string
  // si puo' adoperare get() e get_hashobj() intercambiabilmente ma
  // non sono indipendenti (entrambe avanzano gli stessi contatori)

  TObject*      get();           // ritorna solo l'object
  THash_object* get_hashobj();   // se serve anche la chiave
  void          restart() { _row = 0; _col = 0; }

  TAssoc_array()  : _cnt(0), _row(0), _col(0) {}
  virtual ~TAssoc_array() { destroy(); }  
};



#endif



