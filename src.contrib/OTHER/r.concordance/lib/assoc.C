#include <assoc.H>

THash_object* TAssoc_array::_lookup(const char* k, bool& isnew, bool insert)
{
  const TFixed_string key(k);
  const word hv = key.hash() % HASH_SIZE;
  TArray& arr = _data[hv];
  THash_object* o = NULL;
  isnew = FALSE;

  for (int i = 0; i < arr.items(); i++)
  {
    THash_object* ob = (THash_object*)&arr[i];
    if (ob->_key == key)
    { o = ob; break; }
    if (ob->_key > key)
      break;
  }

  if (o == NULL && insert) 
  { 
    o = new THash_object(key);
    arr.insert(o,i); 
    isnew = TRUE;
    _cnt++;
  }
  
  return o;
}

void TAssoc_array::destroy()
{
  for (int i = 0; i < HASH_SIZE; i++)
    _data[i].destroy();
  _cnt = _row = _col = 0;
}

bool TAssoc_array::add(const char* key, TObject* obj,
                       bool force)
{
  bool isnew = FALSE;

  THash_object* o = _lookup(key,isnew,TRUE);

  if (!isnew)
  {
    if (force) { o->_obj = obj; }
    return TRUE;
  }
  o->_obj = obj;
  return FALSE;
}

bool TAssoc_array::add(const char* key, const TObject& obj, bool force)
{
  return  add(key,obj.dup(),force);
}

bool TAssoc_array::remove(const char* k)
{
  const TFixed_string key(k);
  const word hv = key.hash() % HASH_SIZE;
  TArray& arr = _data[hv];
  THash_object* o = NULL;

  for (int i = 0; i < arr.items(); i++)
  {
    THash_object* ob = (THash_object*)&arr[i];
    if (ob->_key == key)
    { o = ob; break; }
    if (ob->_key > key)
      break;
  }
  if (o != NULL) 
  { arr.destroy(i,TRUE); _cnt--; return TRUE; }
  return FALSE;
}

TObject& TAssoc_array::find(const char* key)
{
  bool isnew = FALSE;
  THash_object* o = _lookup(key, isnew);
  if (o == NULL) error_box("INTERNAL (HASH): Unref key");
  if (o->_obj == NULL) return error;
  else return *(o->_obj);
}

TObject* TAssoc_array::objptr(const char* key)
{
  bool isnew; 
  THash_object* o = NULL;
  if ((o = _lookup(key,isnew)) != NULL)
    return &(o->obj());
  return NULL;
}

bool TAssoc_array::is_key(const char* key)
{
  bool isnew = FALSE;
  const THash_object* o = _lookup(key, isnew);
  if (o == NULL) return FALSE;
  return TRUE;
}

TObject* TAssoc_array::get()
{
  const TArray* arr = &_data[_row];
  
  for(;_row < HASH_SIZE;)
  {
    if ((int)_col < arr->items()) 
      break;
    arr = &_data[++_row];
    _col = 0;
  }
  if (_row == HASH_SIZE)
  { _row = 0; return NULL; }
  
  THash_object* o = (THash_object*)arr->objptr(_col++);
  return (o == NULL || o->_obj == NULL) ? &error : o->_obj;
}

THash_object* TAssoc_array::get_hashobj()
{
  const TArray* arr = &_data[_row];
  
  for(;_row < HASH_SIZE;)
  {
    if ((int)_col < arr->items()) 
      break;
    arr = &_data[++_row];
    _col = 0;
  }
  if (_row == HASH_SIZE)
  { _row = 0; return NULL; }
  
  return (THash_object*)arr->objptr(_col++);
}

