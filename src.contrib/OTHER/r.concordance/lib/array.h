#ifndef __ARRAY_H
#define __ARRAY_H

#ifndef __OBJECT_H
#include <object.H>
#endif

// @M
#ifndef NULL
#define NULL 0L
#endif
// @END

class TArray;

typedef int (*COMPARE_FUNCTION)(const TObject**, const TObject**);

class TArray : public TObject
{
  TObject** _data;      // Array of pointers to objects
  int _size;            // Size of the array
  int _items;           // Number of items in the array

protected:
  // @FPROT
  void resize(int newdim);            // Estende l'array

public:
  // @FPUB
  TArray(int arraysize);      // Crea un array (chiama resize)
  TArray();                   // Crea un array (non chiama resize)
  TArray(const TArray&);      // copia tutto e duplica gli elementi 
  
  // (casino se non hanno dup() definita)
  virtual ~TArray() ;
  virtual const char* class_name() const ;    // Ritorna il nome della classe
  virtual word class_id() const ;             // Ritorna il class-id
  virtual void print_on(ostream& out) const ; // Stampa un array
  virtual bool ok() const ;                   // Ok se l'array non e' vuoto

  int size() const { return _size; }          // Ritorna grandezza dell'array
  int items() const { return _items; }          // Ritorna numero di oggetti nell'array
  int last() const;                                                                                                               // Ritorna l'indice dell'ultimo oggetto

  TObject& operator[] (int index) const ;     // [] ritorna l'oggetto puntato da index
  TObject* objptr(int index) const ;          // Ritorna l'oggetto di posto [index]
  TArray& operator= (const TArray& a); 

  virtual bool destroy(int index = -1, bool pack = FALSE); // Rimuove uno o tutti gli elementi (default)
  virtual int add(TObject* obj, int index = -1) ; // Aggiunge un oggetto ad un array.
  virtual int insert(TObject* obj, int index = 0);

  int add(const TObject& object, int index = -1) ; // Aggiunge un oggetto all'array. L'oggetto viene duplicato
  int insert(const TObject& object, int index = 0);
  TObject* remove(int index, bool pack = FALSE);
  void swap(int i1, int i2);
  void pack();      // Rende contigui tutti gli elementi non nulli
  void sort(COMPARE_FUNCTION = NULL);  // Ordina i TObject (TSortable per default)
};

// @FIN
inline TObject* TArray::objptr(int index) const
{
  return (index < _size && index >= 0) ? _data[index] : NULL;
}

// @FIN
inline TObject& TArray::operator[] (int index) const
{
  TObject* o = objptr(index);
#ifdef DBG
  if (o == NULL)
    fatal_box("Can't access NULL array item: %d of %d", index, size());
#endif
  return *o;
}

class TBit_array : public TObject
{
  word _size;
  byte* _bit;

protected:
  virtual bool ok() const;
  virtual void print_on(ostream& out) const;

  void resize(word size);
  void copy(const TBit_array& ba);
  word index(long n) const { return word(n >> 3); }
  byte mask(long n) const { return 1 << (n & 0x7); }

public:
  TBit_array(long size = 0);
  TBit_array(const TBit_array& ba);
  ~TBit_array();

  TBit_array& operator=(const TBit_array& ba);
  bool operator[] (long n) const;
  TBit_array& operator |=(const TBit_array& b); 

  long first_one() const;
  long last_one() const;
  long ones() const;

  void set(long n);
  void reset(long n);
  void not(long n);

  void set(long n, bool on) { on ? set(n) : reset(n); }
  void set();
  void reset();
  void set(const char* numbers);
};


#endif
