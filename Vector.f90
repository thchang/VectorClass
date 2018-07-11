! FILE : Vector.f90
! AUTHOR: Tyler H. Chang
! LAST UPDATE: October, 2017
! ISO FORTRAN 2003 STANDARD
! 
! ABSTRACT:
!
! Module for FORTRAN vector (dynamic array) operations.
! A Vector is a 2-Dimensional Array where the 1st
! dimension is static and the 2nd dimension is dynamic.
! Options for both 32-bit INTEGER (IntVector) and 64-bit
! REAL (RealVector) types.
! Also defines KIND=R8 for ~64-bit arithmetic on most
! standard machines.
! 
! DESCRIPTION:
!
! The INTVECTOR and REALVECTOR types serve as
! 2-dimensional arrays for INTEGER and REAL(KIND=R8)
! types respectively. 
! The R8 kind is defined herein and gives ~64-bit 
! arithmetic on all known machines.
! From hereon, WLOG, I will reference both types as
! 'VECTOR'.
!
! The data in a VECTOR is accessed via a call to
! VECITEM(VEC,I,IERR), where VEC is the VECTOR object
! to querry and I is the index of the item to access.
! The length of the VECTOR is accessed via the call
! VECSIZE(VEC).
!
! In special cases, the data in VEC at index I could
! also be directly accessed using VEC%DAT(:,I).
! All other fields store meta data pertaining to
! the dynamic memory allocation scheme and SHOULD NOT 
! be referenced other than for read-only purposes.
! They are:
!      D - the size of the first dimension
! LENGTH - the length of the vector
! MAXLEN - the total number of elements this vector
!	   can store before it must resize itself.
!
! All subroutines related to the Vector class use
! an error flag : IERR to report successful 
! operations.
! The scheme is as follows:
! IERR = 0 : SUCCESS
! IERR < 0 : ILLEGAL INPUT AT ARG : (-ierr)
! IERR > 0 : MEMORY ALLOCATION/DEALLOCATION ERROR
!
! A VECTOR VEC is initialize by:
! NEWVECTOR(VEC, IERR, [DIM=D]) where D specifies
! the size of the first dimension.
! If D is ommitted, a 1-dimensional VECTOR will be
! initialized instead.
!
! A VECTOR VEC is destroyed by:
! VECTORFREE(VEC, IERR)
!
! The following 4 subroutines are used to insert and
! delete items. 
! Note the argument ITEM must always match the size 
! and shape of the first dimension of the VECTOR.
! (I.E., if VEC is a REALVECTOR with first dimension
! of size D, then item must have type R8 and be
! a 1-dimensional array of length D.)
! The argument IND is always of type integer and
! specifies an index in the VECTOR.
!
! VECTORPUSH(VEC, ITEM, IERR) pushes ITEM to the
! end of VEC, resizing if necessarry.
!
! VECTORPOP(VEC, ITEM, IERR) pops the top item off
! the back of VEC, resizing if necessarry and returns
! the value in ITEM.
!
! VECTORINS(VEC, ITEM, IND, IERR) inserts ITEM at
! location IND.
!
! VECTORDEL(VEC, IND, IERR) deletes the item at
! location IND.
!
! The dynamic memory allocation scheme is as follows.
! If VEC%LENGTH == 0, then no memory is allocated.
! If VEC%LENGTH overflows VEC%MAXLEN, then double the size
! of VEC%MAXLEN (unless VEC%MAXLEN == 0, then initialize
! to VEC%MAXLEN = 8).
! If VEC%LENGTH underflows VEC%MAXLEN, occupying less than
! half the total storage, reclaim half the size allocated
! to VEC.

! Module for vector types.
! Included in VECTOR module.
MODULE VECTOR_TYPES

! Hompack type for 64-bit real numbers
INTEGER, PARAMETER :: R8 = SELECTED_REAL_KIND(13)

! Integer Vector type (dynamic array)
TYPE INTVECTOR
   ! meta data
   INTEGER :: D, LENGTH, MAXLEN
   ! data
   INTEGER, ALLOCATABLE :: DAT(:,:)
END TYPE INTVECTOR

! Real 64-bit Vector type (dynamic array)
TYPE REALVECTOR
   ! meta data
   INTEGER :: D, LENGTH, MAXLEN
   ! data
   REAL(KIND=R8), ALLOCATABLE :: DAT(:,:)
END TYPE REALVECTOR

END MODULE VECTOR_TYPES

! Main vector module.
MODULE VECTOR_MOD
USE VECTOR_TYPES

! Constructor interface.
INTERFACE NEWVECTOR
   ! integer case interface
   SUBROUTINE NEWINTVECTOR(VEC, IERR, DIM)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(INOUT) :: VEC
      INTEGER, OPTIONAL, INTENT(OUT) :: IERR
      INTEGER, OPTIONAL, INTENT(IN) :: DIM
   END SUBROUTINE NEWINTVECTOR
   ! real case interface
   SUBROUTINE NEWREALVECTOR(VEC, IERR, DIM)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(INOUT) :: VEC
      INTEGER, OPTIONAL, INTENT(OUT) :: IERR
      INTEGER, OPTIONAL, INTENT(IN) :: DIM
   END SUBROUTINE NEWREALVECTOR
END INTERFACE NEWVECTOR

! Push interface.
INTERFACE VECTORPUSH
   ! integer case interface
   SUBROUTINE INTVECTORPUSH(VEC, ITEM, IERR)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(INOUT) :: VEC
      INTEGER, INTENT(IN) :: ITEM(:)
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE INTVECTORPUSH
   ! real case interface
   SUBROUTINE REALVECTORPUSH(VEC, ITEM, IERR)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(INOUT) :: VEC
      REAL(KIND=R8), INTENT(IN) :: ITEM(:)
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE REALVECTORPUSH
END INTERFACE VECTORPUSH

! Pop interface.
INTERFACE VECTORPOP
   ! integer case interface
   SUBROUTINE INTVECTORPOP(VEC, ITEM, IERR)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(INOUT) :: VEC
      INTEGER, INTENT(OUT) :: ITEM(:)
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE INTVECTORPOP
   ! real case interface
   SUBROUTINE REALVECTORPOP(VEC, ITEM, IERR)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(INOUT) :: VEC
      REAL(KIND=R8), INTENT(OUT) :: ITEM(:)
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE REALVECTORPOP
END INTERFACE VECTORPOP

! Insert interface.
INTERFACE VECTORINS
   ! integer case interface
   SUBROUTINE INTVECTORINS(VEC, ITEM, IND, IERR)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(INOUT) :: VEC
      INTEGER, INTENT(IN) :: ITEM(:), IND
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE INTVECTORINS
   ! real case interface
   SUBROUTINE REALVECTORINS(VEC, ITEM, IND, IERR)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(INOUT) :: VEC
      REAL(KIND=R8), INTENT(IN) :: ITEM(:)
      INTEGER, INTENT(IN) :: IND
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE REALVECTORINS
END INTERFACE VECTORINS

! Delete interface.
INTERFACE VECTORDEL
   ! integer case interface
   SUBROUTINE INTVECTORDEL(VEC, IND, IERR)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(INOUT) :: VEC
      INTEGER, INTENT(IN) :: IND
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE INTVECTORDEL
   ! real case interface
   SUBROUTINE REALVECTORDEL(VEC, IND, IERR)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(INOUT) :: VEC
      INTEGER, INTENT(IN) :: IND
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE REALVECTORDEL
END INTERFACE VECTORDEL

! Get item interface.
INTERFACE VECTORITEM
   ! integer case interface
   FUNCTION INTVECTORITEM(VEC, IND, IERR) RESULT(ITEM)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(IN) :: VEC
      INTEGER, INTENT(IN) :: IND
      INTEGER, INTENT(OUT) :: IERR
      INTEGER :: ITEM(VEC%D)
   END FUNCTION INTVECTORITEM
   ! real case interface
   FUNCTION REALVECTORITEM(VEC, IND, IERR) RESULT(ITEM)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(IN) :: VEC
      INTEGER, INTENT(IN) :: IND
      INTEGER, INTENT(OUT) :: IERR
      REAL(KIND=R8) :: ITEM(VEC%D)
   END FUNCTION REALVECTORITEM
END INTERFACE VECTORITEM

! Get size interface.
INTERFACE VECTORSIZE
   ! integer case interface
   FUNCTION INTVECTORSIZE(VEC) RESULT(SIZE)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(IN) :: VEC
      INTEGER :: SIZE
   END FUNCTION INTVECTORSIZE
   ! real case interface
   FUNCTION REALVECTORSIZE(VEC) RESULT(SIZE)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(IN) :: VEC
      INTEGER :: SIZE
   END FUNCTION REALVECTORSIZE
END INTERFACE VECTORSIZE

! Free memory interface.
INTERFACE VECTORFREE
   ! integer case interface
   SUBROUTINE INTVECTORFREE(VEC, IERR)
      USE VECTOR_TYPES
      TYPE(INTVECTOR), INTENT(INOUT) :: VEC
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE INTVECTORFREE
   ! real case interface
   SUBROUTINE REALVECTORFREE(VEC, IERR)
      USE VECTOR_TYPES
      TYPE(REALVECTOR), INTENT(INOUT) :: VEC
      INTEGER, INTENT(OUT) :: IERR
   END SUBROUTINE REALVECTORFREE
END INTERFACE VECTORFREE

END MODULE VECTOR_MOD

! --- Functions for IntVector type --- !

! constructor
SUBROUTINE NEWINTVECTOR(VEC, IERR, DIM)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(INOUT) :: VEC
INTEGER, OPTIONAL, INTENT(OUT) :: IERR
INTEGER, OPTIONAL, INTENT(IN) :: DIM
! set information
VEC%LENGTH = 0
VEC%MAXLEN = 0
! check for optionals
IF(PRESENT(DIM)) THEN
   IF (DIM < 1) THEN
      IERR = -3
      RETURN
   END IF
   IERR = 0
   VEC%D = DIM
ELSE
   IERR = 0
   VEC%D = 1
END IF
RETURN
END SUBROUTINE NEWINTVECTOR

! push function
SUBROUTINE INTVECTORPUSH(VEC, ITEM, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(IN) :: ITEM(:)
INTEGER, INTENT(OUT) :: IERR
! local data
INTEGER :: TMP(VEC%D,VEC%LENGTH)
! check for illegal size
IF(VEC%D .NE. SIZE(ITEM)) THEN
   IERR = -2
   RETURN
END IF
IERR = 0
! no resizing required
IF(VEC%LENGTH < VEC%MAXLEN) THEN
   ! increment counter and add
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,VEC%LENGTH) = ITEM(:)
! needs to be initialized
ELSE IF(VEC%MAXLEN .EQ. 0) THEN
   ! allocate
   ALLOCATE(VEC%DAT(VEC%D,8), STAT=IERR)
   IF(IERR .NE. 0) RETURN
   VEC%MAXLEN = 8
   ! increment counter and add
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,VEC%LENGTH) = ITEM(:)
! resize data
ELSE
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! reallocate
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF(IERR .NE. 0) RETURN
   ALLOCATE(VEC%DAT(VEC%D,VEC%MAXLEN*2), &
      STAT=IERR)
   IF(IERR .NE. 0) RETURN
   VEC%MAXLEN = VEC%MAXLEN*2
   ! restore current data
   VEC%DAT(:,1:VEC%LENGTH) = TMP(:,:)
   ! add new data
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,VEC%LENGTH) = ITEM(:)
END IF
RETURN
END SUBROUTINE INTVECTORPUSH

! pop function
SUBROUTINE INTVECTORPOP(VEC, ITEM, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(OUT) :: ITEM(:), IERR
! local variables
INTEGER :: TMP(VEC%D, VEC%LENGTH-1)
! check for illegal size
IF(VEC%D .NE. SIZE(ITEM)) THEN
   IERR = -2
   RETURN
END IF
! check for illegal length
IF(VEC%LENGTH .EQ. 0) THEN
   IERR = 0
   ITEM = 0
   RETURN
END IF
IERR = 0
! get item
ITEM(:) = VEC%DAT(:,VEC%LENGTH)
VEC%LENGTH = VEC%LENGTH - 1
! reclaim partial memory
IF (VEC%LENGTH < VEC%MAXLEN / 2 .AND. &
  VEC%LENGTH > 6) THEN
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! free
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! reallocate
   ALLOCATE(VEC%DAT(VEC%D, VEC%MAXLEN / 2), STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = VEC%MAXLEN / 2
   ! copy back
   VEC%DAT(:,1:VEC%LENGTH) = TMP(:,:)
! reclaim all memory
ELSE IF (VEC%LENGTH .EQ. 0) THEN
   ! FREE
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = 0
END IF
RETURN
END SUBROUTINE INTVECTORPOP

! insert function
SUBROUTINE INTVECTORINS(VEC, ITEM, IND, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(IN) :: ITEM(:), IND
INTEGER, INTENT(OUT) :: IERR
! local data
INTEGER :: TMP(VEC%D,VEC%LENGTH)
! check for illegal size
IF(VEC%D .NE. SIZE(ITEM)) THEN
   IERR = -2
   RETURN
END IF
! check for bad index
IF(IND < 1 .OR. IND > VEC%LENGTH) THEN
   IERR = -3
   RETURN
END IF
IERR = 0
! no resizing required
IF(VEC%LENGTH < VEC%MAXLEN) THEN
   ! increment counter and insert
   VEC%DAT(:,IND+1:VEC%LENGTH+1) = &
      VEC%DAT(:,IND:VEC%LENGTH)
   VEC%DAT(:,IND) = ITEM(:)
   VEC%LENGTH = VEC%LENGTH + 1
! resize data
ELSE
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! reallocate
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF(IERR .NE. 0) RETURN
   ALLOCATE(VEC%DAT(VEC%D,VEC%MAXLEN*2), STAT=IERR)
   IF(IERR .NE. 0) RETURN
   VEC%MAXLEN=VEC%MAXLEN*2
   ! restore current data
   VEC%DAT(:,1:IND-1) = TMP(:,1:IND-1)
   VEC%DAT(:,IND+1:VEC%LENGTH+1) = TMP(:,IND:VEC%LENGTH)
   ! insert new data
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,IND) = ITEM(:)
END IF
RETURN
END SUBROUTINE INTVECTORINS

! delete function
SUBROUTINE INTVECTORDEL(VEC, IND, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(IN) :: IND
INTEGER, INTENT(OUT) :: IERR
! local data
INTEGER :: TMP(VEC%D,VEC%LENGTH-1)
! check for bad input
IF (IND < 1 .OR. IND > VEC%LENGTH) THEN
   IERR = -2
   RETURN
END IF
IERR = 0
! shift other elements down
VEC%DAT(:,IND:VEC%LENGTH-1) = VEC%DAT(:,IND+1:VEC%LENGTH)
VEC%LENGTH = VEC%LENGTH - 1
! reclaim partial memory
IF (VEC%LENGTH < VEC%MAXLEN / 2 .AND. &
  VEC%LENGTH > 6) THEN
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! free
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! reallocate
   ALLOCATE(VEC%DAT(VEC%D, VEC%MAXLEN / 2), STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = VEC%MAXLEN / 2
   ! copy back
   VEC%DAT(:,1:VEC%LENGTH) = TMP(:,:)
! reclaim all memory
ELSE IF (VEC%LENGTH .EQ. 0) THEN
   ! free
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = 0
END IF
RETURN
END SUBROUTINE INTVECTORDEL

! memory access function
FUNCTION INTVECTORITEM(VEC, IND, IERR) RESULT(ITEM)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(IN) :: VEC
INTEGER, INTENT(IN) :: IND
INTEGER, INTENT(OUT) :: IERR
INTEGER :: ITEM(VEC%D)
! check for bad input
IF (IND < 1 .OR. IND > VEC%LENGTH) THEN
   IERR = -2
   RETURN
END IF
! get item
IERR = 0
ITEM = VEC%DAT(:,IND)
RETURN
END FUNCTION INTVECTORITEM

! check vector length
FUNCTION INTVECTORSIZE(VEC) RESULT(SIZE)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(IN) :: VEC
INTEGER :: SIZE
! get size
SIZE = VEC%LENGTH
RETURN
END FUNCTION INTVECTORSIZE

! free memory
SUBROUTINE INTVECTORFREE(VEC, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(INTVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(OUT) :: IERR
! free data
IERR = 0
IF (ALLOCATED(VEC%DAT)) THEN
   DEALLOCATE(VEC%DAT, STAT=IERR)
END IF
RETURN
END SUBROUTINE INTVECTORFREE

! --- Functions for RealVector type --- !

! constructor
SUBROUTINE NEWREALVECTOR(VEC, IERR, DIM)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(OUT) :: IERR
INTEGER, OPTIONAL, INTENT(IN) :: DIM
! set information
VEC%LENGTH = 0
VEC%MAXLEN = 0
! check for optionals
IF(PRESENT(DIM)) THEN
   IF (DIM < 1) THEN
      IERR = -3
      RETURN
   END IF
   IERR = 0
   VEC%D = DIM
ELSE
   IERR = 0
   VEC%D = 1
END IF
RETURN
END SUBROUTINE NEWREALVECTOR

! push function
SUBROUTINE REALVECTORPUSH(VEC, ITEM, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(INOUT) :: VEC
REAL(KIND=R8), INTENT(IN) :: ITEM(:)
INTEGER, INTENT(OUT) :: IERR
! local data
REAL(KIND=R8) :: TMP(VEC%D,VEC%LENGTH)
! check for illegal size
IF(VEC%D .NE. SIZE(ITEM)) THEN
   IERR = -2
   RETURN
END IF
IERR = 0
! no resizing required
IF(VEC%LENGTH < VEC%MAXLEN) THEN
   ! increment counter and add
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,VEC%LENGTH) = ITEM(:)
! needs to be initialized
ELSE IF(VEC%MAXLEN .EQ. 0) THEN
   ! allocate
   ALLOCATE(VEC%DAT(VEC%D,8), STAT=IERR)
   IF(IERR .NE. 0) RETURN
   VEC%MAXLEN = 8
   ! increment counter and add
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,VEC%LENGTH) = ITEM(:)
! resize data
ELSE
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! reallocate
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF(IERR .NE. 0) RETURN
   ALLOCATE(VEC%DAT(VEC%D,VEC%MAXLEN*2), &
      STAT=IERR)
   IF(IERR .NE. 0) RETURN
   VEC%MAXLEN=VEC%MAXLEN*2
   ! restore current data
   VEC%DAT(:,1:VEC%LENGTH) = TMP(:,:)
   ! add new data
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,VEC%LENGTH) = ITEM(:)
END IF
RETURN
END SUBROUTINE REALVECTORPUSH

! pop function
SUBROUTINE REALVECTORPOP(VEC, ITEM, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(INOUT) :: VEC
REAL(KIND=R8), INTENT(OUT) :: ITEM(:)
INTEGER, INTENT(OUT) :: IERR
! local variables
REAL(KIND=R8) :: TMP(VEC%D, VEC%LENGTH-1)
! check for illegal size
IF(VEC%D .NE. SIZE(ITEM)) THEN
   IERR = -2
   RETURN
END IF
! check for illegal length
IF(VEC%LENGTH .EQ. 0) THEN
   IERR = 0
   ITEM = 0
   RETURN
END IF
! get item
IERR = 0
ITEM(:) = VEC%DAT(:,VEC%LENGTH)
VEC%LENGTH = VEC%LENGTH - 1
! reclaim partial memory
IF (VEC%LENGTH < VEC%MAXLEN / 2 .AND. &
  VEC%LENGTH > 6) THEN
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! free
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! reallocate
   ALLOCATE(VEC%DAT(VEC%D, VEC%MAXLEN / 2), STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = VEC%MAXLEN / 2
   ! copy back
   VEC%DAT(:,1:VEC%LENGTH) = TMP(:,:)
! reclaim all memory
ELSE IF (VEC%LENGTH .EQ. 0) THEN
   ! free
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = 0
END IF
RETURN
END SUBROUTINE REALVECTORPOP

! insert function
SUBROUTINE REALVECTORINS(VEC, ITEM, IND, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(INOUT) :: VEC
REAL(KIND=R8), INTENT(IN) :: ITEM(:)
INTEGER, INTENT(IN) :: IND
INTEGER, INTENT(OUT) :: IERR
! local data
REAL(KIND=R8) :: TMP(VEC%D,VEC%LENGTH)
! check for illegal size
IF(VEC%D .NE. SIZE(ITEM)) THEN
   IERR = -2
   RETURN
END IF
! check for bad index
IF(IND < 1 .OR. IND > VEC%LENGTH) THEN
   IERR = -3
   RETURN
END IF
IERR = 0
! no resizing required
IF(VEC%LENGTH < VEC%MAXLEN) THEN
   ! increment counter and insert
   VEC%DAT(:,IND+1:VEC%LENGTH+1) = &
      VEC%DAT(:,IND:VEC%LENGTH)
   VEC%DAT(:,IND) = ITEM(:)
   VEC%LENGTH = VEC%LENGTH + 1
! resize data
ELSE
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! reallocate
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF(IERR .NE. 0) RETURN
   ALLOCATE(VEC%DAT(VEC%D,VEC%MAXLEN*2), STAT=IERR)
   IF(IERR .NE. 0) RETURN
   VEC%MAXLEN=VEC%MAXLEN*2
   ! restore current data
   VEC%DAT(:,1:IND-1) = TMP(:,1:IND-1)
   VEC%DAT(:,IND+1:VEC%LENGTH+1) = TMP(:,IND:VEC%LENGTH)
   ! insert new data
   VEC%LENGTH = VEC%LENGTH + 1
   VEC%DAT(:,IND) = ITEM(:)
END IF
RETURN
END SUBROUTINE RealVectorIns

! delete function
SUBROUTINE REALVECTORDEL(VEC, IND, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(IN) :: IND
INTEGER, INTENT(OUT) :: IERR
! local data
REAL(KIND=R8) :: TMP(VEC%D,VEC%LENGTH-1)
! check for bad input
IF (IND < 1 .OR. IND > VEC%LENGTH) THEN
   IERR = -2
   RETURN
END IF
IERR = 0
! shift other elements down
VEC%DAT(:,IND:VEC%LENGTH-1) = VEC%DAT(:,IND+1:VEC%LENGTH)
VEC%LENGTH = VEC%LENGTH - 1
! reclaim partial memory
IF (VEC%LENGTH < VEC%MAXLEN / 2 .AND. &
  VEC%LENGTH > 6) THEN
   ! save current data
   TMP(:,:) = VEC%DAT(:,1:VEC%LENGTH)
   ! free
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! reallocate
   ALLOCATE(VEC%DAT(VEC%D, VEC%MAXLEN / 2), STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = VEC%MAXLEN / 2
   ! copy back
   VEC%DAT(:,1:VEC%LENGTH) = TMP(:,:)
! reclaim all memory
ELSE IF (VEC%LENGTH .EQ. 0) THEN
   ! free
   DEALLOCATE(VEC%DAT, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   VEC%MAXLEN = 0
END IF
RETURN
END SUBROUTINE REALVECTORDEL

! memory access function
FUNCTION REALVECTORITEM(VEC, IND, IERR) RESULT(ITEM)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(IN) :: VEC
INTEGER, INTENT(IN) :: IND
INTEGER, INTENT(OUT) :: IERR
REAL(KIND=R8) :: ITEM(VEC%D)
! check for bad input
IF (IND < 1 .OR. IND > VEC%LENGTH) THEN
   IERR = -2
   RETURN
END IF 
! get item
IERR = 0
ITEM = VEC%DAT(:,IND)
RETURN
END FUNCTION REALVECTORITEM

! check vector length
FUNCTION REALVECTORSIZE(VEC) RESULT(SIZE)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(IN) :: VEC
INTEGER :: SIZE
! get size
SIZE = VEC%LENGTH
RETURN
END FUNCTION REALVECTORSIZE

! free memory
SUBROUTINE REALVECTORFREE(VEC, IERR)
USE VECTOR_TYPES
IMPLICIT NONE
! parameters
TYPE(REALVECTOR), INTENT(INOUT) :: VEC
INTEGER, INTENT(OUT) :: IERR
! free data
IERR = 0
IF (ALLOCATED(VEC%DAT)) THEN
   DEALLOCATE(VEC%DAT, STAT=IERR)
END IF
RETURN
END SUBROUTINE REALVECTORFREE

