#ifndef __OBJECT_H
#define __OBJECT_H

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

#ifndef __CLASSES_H
#include <classes.H>
#endif

#ifndef __STDTYPES_H
#include <stdtypes.H>
#endif

#ifndef __CHECKS_H
#include <checks.H>
#endif
// @C
// Classe Object 
// @END

// @N
// Base class for all object hierarchy
// @END

//////////////////////////////////////////////////////////////////////////////
// Object
//////////////////////////////////////////////////////////////////////////////
class TObject
{
public:
  // @FPUB
  virtual ~TObject() {}
  virtual const char* class_name() const;
  virtual word class_id() const;
  virtual bool ok() const;
  virtual TObject* dup() const;
  virtual void print_on(ostream& out) const;
  virtual void read_from(istream&) {}
  virtual word hash() const { return 0; }
};


// @C
// Classe TErrorObject
// @END

//////////////////////////////////////////////////////////////////////////////
// Error Object
//////////////////////////////////////////////////////////////////////////////
class TError_Object : public TObject
{
public:
  // @FPUB
  virtual const char* class_name() const;
  virtual word class_id() const;
  virtual bool ok() const;
};


// @C
// Classe TSortable
// @END

//////////////////////////////////////////////////////////////////////////////
// Error Object
//////////////////////////////////////////////////////////////////////////////
class TSortable : public TObject
{
public:
  // @FPUB
  virtual int compare(const TSortable& s) const pure;
  virtual const char* class_name() const;
  virtual word class_id() const;
};


//////////////////////////////////////////////////////////////////////////////
// inline functions
//////////////////////////////////////////////////////////////////////////////

// @FIN 
inline ostream& operator <<(ostream& out, const TObject& obj)
{
  obj.print_on(out);
  return out;
}

// @FIN 
inline istream& operator >>(istream& in, TObject& obj)
{
  obj.read_from(in);
  CHECK(obj.ok(), "Can't read an Object from a stream");
  return in;
}

inline bool operator ==(const TSortable& a, const TSortable& b)

{
  int res = a.compare(b);
  return res == 0 || res == UNDEFINED;
}

inline bool operator >(const TSortable& a, const TSortable& b)

{
  int res = a.compare(b);
  return res > 0 || res == UNDEFINED;
}

inline bool operator <(const TSortable& a, const TSortable& b)

{
  int res = a.compare(b);
  return res < 0 || res == UNDEFINED;
}

inline bool operator >=(const TSortable& a, const TSortable& b)

{
  int res = a.compare(b);
  return res >= 0 || res == UNDEFINED;
}

inline bool operator <=(const TSortable& a, const TSortable& b)

{
  int res = a.compare(b);
  return res <= 0 || res == UNDEFINED;
}

inline bool operator !=(const TSortable& a, const TSortable& b)

{
  int res = a.compare(b);
  return res != 0 && res != UNDEFINED;
}

#ifdef __OBJECT_CPP
#define extern
#endif

// @DPUB
extern TError_Object error;
// @END

#undef extern

#endif // __OBJECT_H
