%include typemaps.i
%typemap(python,in) string_allows_none {
	if($input==Py_None) {
		$1=NULL;
	} else {
		$1=PyString_AsString($input);
		if(!$1) {
			return NULL;
		}
	}
}

%inline %{
typedef char * string_allows_none;
%}

%typemap(python,out) char ** {
	int len=0,i;
	PyObject * stringobject;

	if(!$1) {
		$result=PyList_New(0);
	} else {

		for(len=0;$1[len];len++);

		$result=PyList_New(len);
		if(!$result){
			// G_free_list($1);
			 return NULL;
		}
			
		for(i=0;i<len;i++) {
			stringobject=PyString_FromString($1[i]);
			if(!stringobject) return NULL;
			PyList_SetItem($1,i,stringobject);
		}
//		G_free_list($1);
	}
}
%typemap(python,in,numinputs=0) return_string (char * temp) {
	temp=NULL;
	$1=&temp;
}


%typemap(python,argout) CELL * {
		int len=0,i;
		len=G_window_cols();
		$result=PyList_New(len);
		for(i=0;i<len;i++)
		{
			PyList_SetItem($result,i,PyInt_FromLong($1[i]));
		}
}
%typemap(python,in)CELL  * {
		int len=0,i=0; CELL *tmp;
		PyObject obj;
		len=G_window_cols();
		$1=G_allocate_cell_buf();
}

%typemap(python,argout) return_string (char * temp) {
	if($1 && *$1) {
		$result=t_output_helper($result,PyString_FromString(*$1));
		G_free(*$1);
	} else {
		$result=t_output_helper($result,Py_None);
		Py_INCREF(Py_None);
	}
	
}

%inline %{
typedef char ** return_string;
%}

