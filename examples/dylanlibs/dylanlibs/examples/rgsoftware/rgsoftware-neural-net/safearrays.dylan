Module:    rgsoftware-neural-net-internal
Synopsis:  Support for SafeArrays in Dylan
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Methods for converting Dylan arrays to the safe arrays that the NN50
// library uses. Dylan does have safe array support, unfortunately there's a
// bug in getting safe array results from external DLL's in FD 2.0.1 that
// requires us to do this wrap/unwrap for now.
define function array-to-safe-array(array :: <array>) => (result :: <lpsafearray>)
  let dim = array.dimensions;
  let bounds = make(<LPSAFEARRAYBOUND>, element-count: dim.size);
  for(n from 0 below dim.size)
    let b = pointer-value-address(bounds, index: n);
    b.lLBound-value := 1;
    b.cElements-value := dim[dim.size - n - 1];
  end for;
  
  let safe-array = SafeArrayCreate($VT-R8, dim.size, bounds);
  let indexes = #f;
  let store = #f;
  block()
    indexes := make(<c-long*>, element-count: 2);
    store := make(<c-double*>);
    for(i from 0 below dim[0])
      for(j from 0 below dim[1])
        let e = array[i, j];
        pointer-value(indexes, index: 0) := j + 1;
        pointer-value(indexes, index: 1) := i + 1;
        store.pointer-value := e;
        when(negative?(SafeArrayPutElement(safe-array, indexes, store)))
          error("SafeArrayPutElement returned an error code.");
        end when;
      end for;
    end for;
  cleanup
    when(indexes) destroy(indexes) end when;
    when(store) destroy(store) end when;
  end block;
  safe-array;
end function array-to-safe-array;

define function vector-to-safe-array(array :: <vector>) => (result :: <lpsafearray>)
  let bounds = make(<LPSAFEARRAYBOUND>);
  bounds.lLBound-value := 1;
  bounds.cElements-value := array.size;
  
  let safe-array = SafeArrayCreate($VT-R8, 1, bounds);
  let indexes = #f;
  let store = #f;
  block()
    indexes := make(<c-long*>);
    store := make(<c-double*>);
    for(i from 0 below array.size)
      indexes.pointer-value := i + 1;
      store.pointer-value := array[i];
      when(negative?(SafeArrayPutElement(safe-array, indexes, store)))
        error("SafeArrayPutElement returned an error code.");
      end when;
    end for;
  cleanup
    when(indexes) destroy(indexes) end when;
    when(store) destroy(store) end when;
  end block;
  safe-array;
end function vector-to-safe-array;

define function safe-array-to-array(safe-array :: <LPSAFEARRAY>) => (array :: <array>)
  let dim = SafeArrayGetDim(safe-array);
  let array-ubound = make(<vector>, size: dim);
  let array-lbound = make(<vector>, size: dim);
  let array-count = make(<vector>, size: dim);
  for(n from 0 below dim)
    let (ubound-result, ubound) = SafeArrayGetUBound(safe-array, dim - n);
    let (lbound-result, lbound) = SafeArrayGetLBound(safe-array, dim - n);
    array-ubound[n] := ubound;
    array-lbound[n] := lbound;
    array-count[n] := ubound - lbound + 1;    
  end for;

  let array = make(<array>, fill: 0d0, dimensions: array-count);
  let store = #f;
  let indexes = #f;
  block()
    indexes := make(<c-long*>, element-count: 2);
    store := make(<c-double*>);
    for(i from 0 below array-count[0])
      for(j from 0 below array-count[1])
        pointer-value(indexes, index: 0) := j + array-lbound[0];
        pointer-value(indexes, index: 1) := i + array-lbound[1];
        when(negative?(SafeArrayGetElement(safe-array, indexes, store)))
          error("SafeArrayGetElement returned an error code.");
        end when;
        array[i,j] := store.pointer-value;
      end for;
    end for;
  cleanup
    when(indexes) destroy(indexes) end;
    when(store) destroy(store) end;
  end block;
  array;
end function safe-array-to-array;

define function safe-array-to-vector(safe-array :: <LPSAFEARRAY>) => (array :: <vector>)
  let (ubound-result, ubound) = SafeArrayGetUBound(safe-array, 1);
  let (lbound-result, lbound) = SafeArrayGetLBound(safe-array, 1);
  let count = ubound - lbound + 1;    
  let array = make(<vector>, fill: 0d0, size: count);

  let store = #f;
  let indexes = #f;
  block()
    indexes := make(<c-long*>);
    store := make(<c-double*>);
    for(i from 0 below count)
      indexes.pointer-value := i + lbound;
        when(negative?(SafeArrayGetElement(safe-array, indexes, store)))
          error("SafeArrayGetElement returned an error code.");
        end when;
      array[i] := store.pointer-value;
    end for;
  cleanup
    when(indexes) destroy(indexes) end;
    when(store) destroy(store) end;
  end block;
  array;
end function safe-array-to-vector;

// Create a type that is used in C-FFI definitions to convert
// automatically from a safe array to an array and vice versa.
define c-mapped-subtype <nn50-safe-array> (<LPSAFEARRAY>)
  map <array>,
    export-function: method(x :: <array>) pointer-cast(<nn50-safe-array>, array-to-safe-array(x))  end,
    import-function: safe-array-to-array;
end c-mapped-subtype <nn50-safe-array>;

define C-pointer-type <nn50-safe-array*> => <nn50-safe-array>;

define c-mapped-subtype <nn50-safe-vector> (<LPSAFEARRAY>)
  map <array>,
    export-function: method(x :: <vector>) pointer-cast(<nn50-safe-vector>, vector-to-safe-array(x))  end,
    import-function: safe-array-to-vector;
end c-mapped-subtype <nn50-safe-vector>;

define C-pointer-type <nn50-safe-vector*> => <nn50-safe-vector>;

