#define __OBJECT_CPP

#include <object.H>

///////////////////////////////////////////////////////////
// Object
///////////////////////////////////////////////////////////

const char* TObject::class_name() const

{
  return "Object";
}


word TObject::class_id() const

{
  return CLASS_OBJECT;
}


bool TObject::ok() const

{
  return TRUE;
}


TObject* TObject::dup() const

{
  CHECK(FALSE, "Can't duplicate an Object");
  return 0L;
}


void TObject::print_on(ostream& out) const

{
  out << class_name();
}


///////////////////////////////////////////////////////////
// Error Object
///////////////////////////////////////////////////////////

const char* TError_Object::class_name() const

{
  return "Error_Object";
}


word TError_Object::class_id() const

{
  return CLASS_ERROR;
}


bool TError_Object::ok() const

{
  return FALSE;
}

///////////////////////////////////////////////////////////
// Sortable
///////////////////////////////////////////////////////////

const char* TSortable::class_name() const

{
  return "Sortable";
}


word TSortable::class_id() const

{
  return CLASS_SORTABLE;
}

