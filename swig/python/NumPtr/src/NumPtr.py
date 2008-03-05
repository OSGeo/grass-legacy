# This file was created automatically by SWIG.
# Don't modify this file, modify the SWIG interface instead.
# This file is compatible with both classic and new-style classes.

import _NumPtr

def _swig_setattr(self,class_type,name,value):
    if (name == "this"):
        if isinstance(value, class_type):
            self.__dict__[name] = value.this
            if hasattr(value,"thisown"): self.__dict__["thisown"] = value.thisown
            del value.thisown
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    self.__dict__[name] = value

def _swig_getattr(self,class_type,name):
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError,name

import types
try:
    _object = types.ObjectType
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0
del types



getpointer1 = _NumPtr.getpointer1

getpointer2 = _NumPtr.getpointer2

getpointer3 = _NumPtr.getpointer3

getdpointer1 = _NumPtr.getdpointer1

getdpointer2 = _NumPtr.getdpointer2

getdpointer3 = _NumPtr.getdpointer3

getfpointer1 = _NumPtr.getfpointer1

getfpointer2 = _NumPtr.getfpointer2

getfpointer3 = _NumPtr.getfpointer3

getipointer1 = _NumPtr.getipointer1

getipointer2 = _NumPtr.getipointer2

getipointer3 = _NumPtr.getipointer3

test1 = _NumPtr.test1

test2 = _NumPtr.test2

test3 = _NumPtr.test3

testd1 = _NumPtr.testd1

testd2 = _NumPtr.testd2

testd3 = _NumPtr.testd3

testf1 = _NumPtr.testf1

testf2 = _NumPtr.testf2

testf3 = _NumPtr.testf3

testi1 = _NumPtr.testi1

testi2 = _NumPtr.testi2

testi3 = _NumPtr.testi3

