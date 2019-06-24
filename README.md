# Fortran 2003 VECTOR types.

This package contains a Fortran 2003 dynamic memory module.
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

 - Vector.f90 contains the VECTOR\_MOD module containing the complete
   VECTOR class (interfaces, functions, subroutines, and data types).
 - main.f90 contains driver code in the form of a command line testing
   interface.
 - Makefile is a simple makefile for this project.

## Usage

To use the REALVECTOR and INTVECTOR data types, include the following
module:
``
USE VECTOR_CLASS
``
The two types provided are TYPE(REALVECTOR) and TYPE(INTVECTOR).
The REALVECTOR type uses the R8 data type for 64-bit real arithmetic,
provided in the same module. All internal memory for both VECTOR types
is private, and the VECTOR must be interfaced using the provided
functions and subroutines.

All subroutines related to the Vector class use an error flag ISTAT to
report successful operations.
The scheme is as follows:
 - IERR = 0 : SUCCESS
 - IERR < 0 : ILLEGAL INPUT AT ARG : (-IERR)
 - IERR > 0 : MEMORY ALLOCATION/DEALLOCATION ERROR

### User Interface

The following 2 subroutines are used to safely allocate and deallocate
VECTOR objects:
 - VEC = [REAL|INT]VECTOR([DIM, ISTAT]) allocates VEC as a new VECTOR
   object. The optional argument DIM, if provided, specifies the length
   of the first (static) dimension. If omitted, the first dimension is
   assumed to have length 1 (note, that even if it has length 1, the
   first dimension must still be treated as a vector, not a scalar).
 - VEC%FREE([ISTAT]) frees any memory currently allocated for VEC.

The following 5 subroutines are used to manage memory. Note, that the
argument ITEM must match the first dimension and type of VEC.
 - VEC%PUSH(ITEM[, ISTAT]) pushes ITEM(:) to the
   end of VEC, resizing if necessarry.
 - VEC%POP(ITEM[, ISTAT]) pops the top item off
   the back of VEC, resizing if necessarry and returns
   the value in ITEM(:).
 - VEC%INSERT(ITEM, IND[, ISTAT]) inserts ITEM at
   location IND.
 - VEC%DELETE(IND[, ISTAT]) deletes the item at
   location IND.
 - VEC%SET(IND, ITEM[, ISTAT]) sets the value of VEC
   location IND to ITEM(:).

The following 4 functions are provided for data access:
 - VEC%ITEM(IND[, IERR]) returns the one-dimensional array
   located at index IND.
 - VEC%DAT() returns the entire two-dimensional data set
   currently stored in VEC (first dimension fixed, second
   dynamic).
 - VEC%DIM() returns the integer valued size of the first
   (static) dimension of VEC.
 - VEC%LENGTH() returns the integer valued size of the
   second (dynamic) dimension of VEC.

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

