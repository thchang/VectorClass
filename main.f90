PROGRAM CMDLINE
! Author: Tyler H. Chang
! Last Update: June, 2018
! Driver code for testing the VECTOR classes.
USE VECTOR_MOD
IMPLICIT NONE

! Declare local data.
INTEGER :: IERR, I
REAL(KIND=R8) :: RITEM(1)
INTEGER :: IITEM(1)
CHARACTER :: INPUT = 'R'
TYPE(REALVECTOR) :: RVEC
TYPE(INTVECTOR) :: IVEC

! Prompt user for VECTOR type (real or integer).
DO WHILE (.TRUE.)

! Get INPUT from the user.
PRINT *, 'Test RealVector or IntVector?'
WRITE (*,'(a)',ADVANCE='no') 'Select (r)eal or (i)nt: '
READ (*,*) INPUT

! Check the input against the available options.
IF (INPUT .EQ. 'r' .OR. INPUT .EQ. 'R') THEN
   PRINT *, 'Allocating RealVector ... '
   EXIT
ELSE IF (INPUT .EQ. 'i' .OR. INPUT .EQ. 'I') THEN
   PRINT *, 'Allocating IntVector ... '
   EXIT
ELSE IF (INPUT .EQ. 'q' .OR. INPUT .EQ. 'Q') THEN
   RETURN
ELSE
   PRINT *, 'Illegal Option. Try again or type q to quit.'
END IF

END DO

! REALVECTOR test case.
IF (INPUT .EQ. 'r' .OR. INPUT .EQ. 'R') THEN

! Create a new REALVECTOR of fixed dimension 1
CALL NEWVECTOR(RVEC, IERR)
IF (IERR .NE. 0) THEN
   PRINT *, 'An error occurred while initializing vector'
   RETURN
END IF

! Loop until user prompts 'q' or 'Q' for 'quit'.
DO WHILE (.TRUE.)

! Print the current vector.
WRITE (*,'(a)',ADVANCE='no') 'Current VECTOR: <'
DO I=1, VECTORSIZE(RVEC)
   WRITE (*,'(1f8.2)',ADVANCE='no') VECTORITEM(RVEC,I,IERR)
END DO
WRITE (*,*) '>'
PRINT *, 'Total memory used = ', VECTORSIZE(RVEC)
PRINT *, 'Total memory allocated = ', RVEC%MAXLEN

! Read the next INPUT from user.
PRINT *, '-----------------------------------'
PRINT *, 'Enter an option: '
PRINT *, '(i) insert'
PRINT *, '(d) delete'
PRINT *, '(p) push'
PRINT *, '(v) pop'
PRINT *, '(q) quit'
READ (*,*) INPUT
PRINT *, '-----------------------------------'

! Check for the exit condition.
IF(INPUT .EQ. 'q' .OR. INPUT .EQ. 'Q') THEN
   EXIT
! Perform an insert.
ELSE IF(INPUT .EQ. 'i' .OR. INPUT.EQ.'I') THEN
   WRITE (*,'(a)',ADVANCE='no'), &
      'Enter item to insert then index: (Item, Index) '
   READ (*,*) RITEM, I
   CALL VECTORINS(RVEC, RITEM, I, IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, &
         'Error occurred while inserting ITEM at INDEX'
   END IF
! Perform a deletion.
ELSE IF(INPUT .EQ. 'd' .OR. INPUT.EQ.'D') THEN
    WRITE (*,'(a)',ADVANCE='no'), 'Enter index to delete: '
    READ (*,*) I
   CALL VECTORDEL(RVEC,I,IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while deleting at INDEX'
   END IF
! Push an item.
ELSE IF(INPUT .EQ. 'p' .OR. INPUT.EQ.'P') THEN
   WRITE (*,'(a)',ADVANCE='no'), 'Enter item to push: '
   READ (*,*) RITEM
   CALL VECTORPUSH(RVEC,RITEM,IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while pushing ITEM'
   END IF
! Pop an item.
ELSE IF(INPUT .EQ. 'v' .OR. INPUT.EQ.'V') THEN
   CALL VECTORPOP(RVEC,RITEM,IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while popping'
   END IF
   PRINT *, 'Top ITEM was: ', ritem
END IF
END DO
! Free the heap.
CALL VECTORFREE(RVEC, IERR)
IF(IERR .NE. 0) THEN
   PRINT *, 'Error occurred while freeing memory'
END IF

! INTVECTOR test case.
ELSE

! Create a new INTVECTOR of fixed dimension 1.
CALL NEWVECTOR(IVEC, IERR)
IF (IERR .NE. 0) THEN
   PRINT *, 'An error occurred while initializing vector'
   RETURN
END IF

! Loop until until exit condition.
DO WHILE (.TRUE.)

! Print the VECTOR.
WRITE (*,'(a)',ADVANCE='no') 'Current VECTOR: <'
DO I=1, VECTORSIZE(IVEC)
   WRITE (*,'(1i8)',ADVANCE='no') VECTORITEM(IVEC,I,IERR)
END DO
WRITE (*,*) '>'
PRINT *, 'Total memory used = ', VECTORSIZE(IVEC)
PRINT *, 'Total memory allocated = ', IVEC%MAXLEN

! Read the next user INPUT.
PRINT *, '-----------------------------------'
PRINT *, 'Enter an option: '
PRINT *, '(i) insert'
PRINT *, '(d) delete'
PRINT *, '(p) push'
PRINT *, '(v) pop'
PRINT *, '(q) quit'
READ (*,*) INPUT
PRINT *, '-----------------------------------'

! Check for the exit condition.
IF(INPUT .EQ. 'q' .OR. INPUT .EQ. 'Q') THEN
   EXIT
! Perform an insert.
ELSE IF(INPUT .EQ. 'i' .OR. INPUT.EQ.'I') THEN
   WRITE (*,'(a)',ADVANCE='no'), &
      'Enter item to insert then index: (Item, Index) '
   READ (*,*) IITEM, I
   CALL VECTORINS(IVEC, IITEM, I, IERR)
   IF(ierr .NE. 0) THEN
      PRINT *, &
         'Error occurred while inserting ITEM at INDEX'
   END IF
! Perform a deletion.
ELSE IF(INPUT .EQ. 'd' .OR. INPUT.EQ.'D') THEN
    WRITE (*,'(a)',ADVANCE='no'), 'Enter index to delete: '
    READ (*,*) i
   CALL VECTORDEL(IVEC,I,IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while deleting at INDEX'
   END IF
! Push an item.
ELSE IF(INPUT .EQ. 'p' .OR. INPUT.EQ.'P') THEN
   WRITE (*,'(a)',ADVANCE='no'), 'Enter item to push: '
   READ (*,*) IITEM
   CALL VECTORPUSH(IVEC,IITEM,IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while pushing ITEM'
   END IF
! Pop an item.
ELSE IF(INPUT .EQ. 'v' .OR. INPUT.EQ.'V') THEN
   CALL VECTORPOP(IVEC,IITEM,IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while popping'
   END IF
   PRINT *, 'Top ITEM was: ', IITEM
END IF
END DO
! Free the heap memory.
CALL VECTORFREE(IVEC, IERR)
IF(IERR .NE. 0) THEN
   PRINT *, 'Error occurred while freeing memory'
END IF

END IF
RETURN
END PROGRAM CMDLINE
