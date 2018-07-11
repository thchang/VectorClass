# Fortran 2003 VECTOR types.

This package contains a Fortran 2003 implementation of the C++ Vector
class.
The VECTOR type is a dynamically resizing two-dimensional array with
both REAL and INTEGER options (REALVECTOR and INTVECTOR types).
The first dimension of a VECTOR object VEC is fixed upon initialization.
The second dimension self-resizes based on the following rules:
 - If VEC%LENGTH == 0, then no memory is allocated.
 - If VEC%LENGTH overflows VEC%MAXLEN, then double the size
    of VEC%MAXLEN (unless VEC%MAXLEN == 0, then initialize
    to VEC%MAXLEN = 8).
 - If VEC%LENGTH underflows VEC%MAXLEN, occupying less than
    half the total storage, reclaim half the size allocated
    to VEC.

## Contents

 - Vector.f90 contains the VECTOR\_MOD module containing the VECTOR
   interface functions, subroutines, and data types.
 - main.f90 contains driver code in the form of a command line testing
   interface.
 - Makefile is a simple makefile for this project.

## Usage

To use the REALVECTOR and INTVECTOR data types, include the following
module:
``
USE VECTOR_MOD
``
The two types provided are TYPE(REALVECTOR) and TYPE(INTVECTOR).
The REALVECTOR type uses the R8 data type for 64-bit real arithmetic,
provided in the VECTOR\_TYPE module.
Both types have 4 fields:
 - VEC%DAT(:,:) contains the dynamically allocated data.
 - VEC%LENGTH contains the length of the vector.
 - VEC%MAXLEN contains the current length of the second dimension that
   has been allocated for (though not necessarilly used).
It is highly recommended that VEC%LENGTH and VEC%MAXLEN never be directly
accessed.
Subroutines are provided for interfacing safely with the memory.

All subroutines related to the Vector class use an error flag IERR to
report successful operations.
The scheme is as follows:
 - IERR = 0 : SUCCESS
 - IERR < 0 : ILLEGAL INPUT AT ARG : (-IERR)
 - IERR > 0 : MEMORY ALLOCATION/DEALLOCATION ERROR

### User Interface

The following 2 subroutines are used to safely allocate and deallocate
VECTOR objects:
 - NEWVECTOR(VEC, IERR, DIM) allocates VEC as a new VECTOR object.
   The DIM argument is optional. If provided, it specifies the length
   of the first dimension. If omitted, the first dimension is assumed
   to have length 1 (note, that even if it has length 1, the first
   dimension must still be treated as a vector, not a scalar).
 - VECTORFREE(VEC, IERR) frees any memory currently allocated for VEC.

The following 4 subroutines are used to manage memory. Note, that the
argument ITEM must match the first dimension and type of VEC.
 - VECTORPUSH(VEC, ITEM, IERR) pushes ITEM to the
   end of VEC, resizing if necessarry.
 - VECTORPOP(VEC, ITEM, IERR) pops the top item off
   the back of VEC, resizing if necessarry and returns
   the value in ITEM.
 - VECTORINS(VEC, ITEM, IND, IERR) inserts ITEM at
   location IND.
 - VECTORDEL(VEC, IND, IERR) deletes the item at
   location IND.

The following 2 functions are provided for data access:
 - VECTORITEM(VEC, IND, IERR) returns the one dimensional array
   located at index IND.
 - VECTORLENGTH(VEC) returns the integer valued length of VEC
   (VEC%LENGTH).

### Installation

To install, simply pull this repo then use the Makefile to build
the contents in your local directory.
``
make -B
``

### Test cases

After installing, try running the provided command line interface
to test the VECTOR class.
``
./main
``
For more usage examples, consider referencing the code in main.f90.

## Author

* **Tyler H. Chang** - *Primary author*

## Footnotes

This code does not represent the most efficient means for dynamic memory
allocation in Fortran 2003 for most real-world problems.
However, it is a general solution that will work for a wide range of
use cases.

