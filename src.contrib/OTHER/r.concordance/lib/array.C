#include <ctype.h>
#include <stdlib.h>
#include <array.H>
#include <strings.H>


void TArray::resize(int arraysize)

{
  CHECK(arraysize > size(), "Can't reduce array size.");

  TObject** newdata = new TObject* [arraysize];

  int i = 0;
  if (_data != NULL)
    for (i = 0; i < size(); i++) newdata[i] = _data[i];

  while (i < arraysize) newdata[i++] = NULL;

  if (_data != NULL) delete _data;

  _size = arraysize;
  _data = newdata;
}


bool TArray::destroy(int index, bool pack)
{
  const int old = _items;

  if (index < 0)
  {
    for (int i = size(); i--;) if (_data[i] != NULL)
    {
      delete _data[i];
      _data[i] = NULL;
    }
    _items = 0;
  }
  else
  {
    TObject* o = remove(index, pack);
    if (o) delete o;
  }

  return _items < old;
}


TArray::TArray(int arraysize)   : _size(0), _items(0), _data(NULL)

{
  if (arraysize) resize(arraysize);
}

TArray::TArray()   : _size(0), _items(0), _data(NULL)
{
}

TArray::TArray(const TArray& a) : _size(0), _items(0), _data(NULL)
{
  (*this) = a;
}              

TArray& TArray::operator= (const TArray& a)
{
  if (_items > 0) destroy();
  if (_size < a.size()) resize(a.size());
  for (_items = 0; _items < a.items(); _items++) 
    _data[_items] = a[_items].dup();
  return *this;
}              

TArray::~TArray()

{
  if (ok())
  {
    destroy();
    delete _data;
  }
}


const char* TArray::class_name() const

{
  return "Array";
}


word TArray::class_id() const

{
  return CLASS_ARRAY;
}


void TArray::print_on(ostream& out) const

{
  for (int i = 0; i < size(); i++)
  {
    out.width(4);
    out << i << ' ';
    out.width();
    if (_data[i] != NULL) out << *_data[i];
    else out << "(null)";
    out << endl;
  }
}


bool TArray::ok() const

{
  return(size() != 0 && (_data != NULL));
}


int TArray::add(TObject* object, int index)
{
  if (index < 0)        for (index = 0; index < size() && _data[index]; index++);
  if (index >= size()) resize(3*index/2 + 1);

  if (_data[index] != NULL)
  {
#ifdef DBG
    if (object == _data[index])
    {
      error_box("Iu ar traing tu overrait en arrei obgiect: %d", index);
      return index;
    }
#endif
    delete _data[index];
    _items--;
  }

  _data[index] = object;
  if (object != NULL) _items++;

  return index;
}


int TArray::insert(TObject* object, int index)
{
  if (objptr(index))
  {
    if (items() == size()) add(NULL);
    for (int i = size()-1; i >= index; i--)
      _data[i] = _data[i-1];
    _data[index] = NULL;
  }
  return add(object, index);
}

int TArray::add(const TObject& object, int index)
{
  TObject* objptr = object.dup();
  return add(objptr, index);
}

int TArray::insert(const TObject& object, int index)
{
  TObject* objptr = object.dup();
  return insert(objptr, index);
}

TObject* TArray::remove(int index, bool dopack)
{
  TObject* o = objptr(index);
  if (o)
  {
    _data[index] = NULL;
    _items--;
  }
  if (dopack) pack();
  return o;
}

void TArray::swap(int i1, int i2)
{
  TObject* o1 = remove(i1, FALSE);
  TObject* o2 = remove(i2, FALSE);
  if (o1) add(o1, i2);
  if (o2) add(o2, i1);
}

int TArray::last() const
{
  for (int last = size(); --last >= 0; )
    if (_data[last]) break;
  return last;
}


void TArray::pack()
{
  int next = 0;
  for (int i = 0; i < size(); i++)
  {
    if (_data[i] != NULL)
    {
      if (next < i)
      {
        _data[next] = _data[i];
        _data[i] = NULL;
      }
      next++;
    }
  }
}


HIDDEN int sortable_compare(const TObject** o1, const TObject** o2)
{
  const TSortable* s1 = (const TSortable*)*o1;
  const TSortable* s2 = (const TSortable*)*o2;
  return s1->compare(*s2);
}

void TArray::sort(COMPARE_FUNCTION compare)
{
  typedef int (*qsortfunc)(const void*, const void*);
  if (compare == NULL) compare = sortable_compare;

  pack();
  qsort(_data, items(), sizeof(TObject*), (qsortfunc)compare);
}

///////////////////////////////////////////////////////////
// TBit_array
///////////////////////////////////////////////////////////

TBit_array::TBit_array(long size) : _bit(NULL), _size(0)
{
  if (size)     resize(index(size));
}

void TBit_array::copy(const TBit_array& ba)
{
  if (_bit)
  {
    delete _bit;
    _bit = NULL;
    _size = 0;
  }
  resize(ba._size-1);
  memcpy(_bit, ba._bit, _size);
}

TBit_array::TBit_array(const TBit_array& ba) : _bit(NULL), _size(0)
{       copy(ba); }

TBit_array& TBit_array::operator=(const TBit_array& ba)
{
  copy(ba);
  return *this;
}


TBit_array::~TBit_array()
{
  if (_bit)     delete _bit;
}

// Certified 100%
void TBit_array::set()
{
  if (_bit) memset(_bit, 0xFF, _size);
}

// Certified 100%
void TBit_array::reset()
{
  if (_bit) memset(_bit, 0x0, _size);
}

// Certified 99%
void TBit_array::resize(word size)
{
  word oldsize = _size;
  byte* oldbit = _bit;

  _size = size+1;
  _bit = new byte[_size];
  reset();

  if (oldsize)
  {
    memcpy(_bit, oldbit, oldsize);
    delete oldbit;
  }
}


bool TBit_array::operator[] (long n) const
{
  const word i = index(n);
  if (i >= _size) return FALSE;
  return (_bit[i] & mask(n)) != 0;
}

// Certified 99%
TBit_array& TBit_array::operator|= (const TBit_array& ba)
{
  CHECK(_size >= ba._size, "TBit_array |=: right operand too big");

  for (word i = 0; i < _size; i++)
    _bit[i] |= ba._bit[i];

  return *this;
}

// Certified 99%
void TBit_array::set(long n)
{
  const word i = index(n);
  if (i >= _size)       resize(i);
  _bit[i] |= mask(n);
}

// Certified 99%
void TBit_array::reset(long n)
{
  const word i = index(n);
  if (i < _size)
    _bit[i] &= ~mask(n);
}

// Certified 99%
void TBit_array::not(long n)
{
  const word i = index(n);
  if (i >= _size) resize(i);
  _bit[i] ^= mask(n);
}

// Certified 50%
long TBit_array::ones() const
{
  long one = 0;
  for (word i = 0; i < _size; i++)
  {
    const byte b = _bit[i];
    if (b)
    {
      for (byte m = 1; m; m <<= 1)
        if (b & m) one++;
    }
  }
  return one;
}

// Certified 90%
long TBit_array::last_one() const
{
  for (word i = _size; i--;)
  {
    const byte b = _bit[i];
    if (b)
    {
      for (byte j = 8; j--;)
        if ((1<<j) & b) return (long(i)<<3) + j;
    }
  }
  return -1;
}

// Certified 90%
long TBit_array::first_one() const
{
  for (word i = 0; i < _size; i++)
  {
    const byte b = _bit[i];
    if (b)
    {
      for (byte j = 0; j < 8; j++)
        if ((1<<j) & b) return (long(i)<<3)+j;
    }
  }
  return -1;
}

bool TBit_array::ok() const
{
  return _bit != NULL && _size > 0;
}


void TBit_array::set(const char* numbers)
{
  TToken_string s(numbers, ' ');
  for (const char* n = s.get(0); n; n = s.get())
    if (isdigit(*n)) set(atol(n));
}


void TBit_array::print_on(ostream& out) const
{
  const long last = _size<<3;
  for (long i = 1; i < last; i++)
    if (operator[](i)) out << ' ' << i;
}


