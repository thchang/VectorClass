PROGRAM CMDLINE
! Author: Tyler H. Chang
! Last Update: June, 2018
! Driver code for testing the VECTOR classes.
USE VECTOR_CLASS
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
RVEC = REALVECTOR(DIM=1, ISTAT=IERR)
IF (IERR .NE. 0) THEN
   PRINT *, 'An error occurred while initializing vector'
   RETURN
END IF

! Loop until user prompts 'q' or 'Q' for 'quit'.
DO WHILE (.TRUE.)

! Print the current vector.
WRITE (*,'(a)',ADVANCE='no') 'Current VECTOR: <'
DO I=1, RVEC%LENGTH()
   WRITE (*,'(1f8.2)',ADVANCE='no') RVEC%ITEM(I,IERR)
END DO
WRITE (*,*) '>'

! Read the next INPUT from user.
PRINT *, '-----------------------------------'
PRINT *, 'Enter an option: '
PRINT *, '(i) insert'
PRINT *, '(d) delete'
PRINT *, '(p) push'
PRINT *, '(v) pop'
PRINT *, '(s) set'
PRINT *, '(q) quit'
READ (*,*) INPUT
PRINT *, '-----------------------------------'

! Check for the exit condition.
IF(INPUT .EQ. 'q' .OR. INPUT .EQ. 'Q') THEN
   EXIT
! Perform an insert.
ELSE IF(INPUT .EQ. 'i' .OR. INPUT.EQ.'I') THEN
   WRITE (*,'(a)',ADVANCE='no') &
      'Enter item to insert then index: (Item, Index) '
   READ (*,*) RITEM, I
   CALL RVEC%INSERT(RITEM, I, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, &
         'Error occurred while inserting ITEM at INDEX'
   END IF
! Perform a deletion.
ELSE IF(INPUT .EQ. 'd' .OR. INPUT.EQ.'D') THEN
    WRITE (*,'(a)',ADVANCE='no') 'Enter index to delete: '
    READ (*,*) I
   CALL RVEC%DELETE(I,IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while deleting at INDEX'
   END IF
! Push an item.
ELSE IF(INPUT .EQ. 'p' .OR. INPUT.EQ.'P') THEN
   WRITE (*,'(a)',ADVANCE='no') 'Enter item to push: '
   READ (*,*) RITEM
   CALL RVEC%PUSH(RITEM, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while pushing ITEM'
   END IF
! Pop an item.
ELSE IF(INPUT .EQ. 'v' .OR. INPUT.EQ.'V') THEN
   CALL RVEC%POP(RITEM, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while popping'
   END IF
   PRINT *, 'Top ITEM was: ', ritem
! Set an item.
ELSE IF(INPUT .EQ. 's' .OR. INPUT.EQ.'S') THEN
   WRITE (*,'(a)',ADVANCE='no') &
      'Enter index then value to set: (Index, Item) '
   READ (*,*) I, RITEM
   CALL RVEC%SET(I, RITEM, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while setting'
   END IF
END IF
END DO
! Free the heap.
CALL RVEC%FREE(ISTAT=IERR)
IF(IERR .NE. 0) THEN
   PRINT *, 'Error occurred while freeing memory'
END IF

! INTVECTOR test case.
ELSE

! Create a new INTVECTOR of fixed dimension 1.
IVEC = INTVECTOR(DIM=1, ISTAT=IERR)
IF (IERR .NE. 0) THEN
   PRINT *, 'An error occurred while initializing vector'
   RETURN
END IF

! Loop until until exit condition.
DO WHILE (.TRUE.)

! Print the VECTOR.
WRITE (*,'(a)',ADVANCE='no') 'Current VECTOR: <'
DO I=1, IVEC%LENGTH()
   WRITE (*,'(1i8)',ADVANCE='no') IVEC%ITEM(I, ISTAT=IERR)
END DO
WRITE (*,*) '>'

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
   WRITE (*,'(a)',ADVANCE='no') &
      'Enter item to insert then index: (Item, Index) '
   READ (*,*) IITEM, I
   CALL IVEC%INSERT(IITEM, I, ISTAT=IERR)
   IF(ierr .NE. 0) THEN
      PRINT *, &
         'Error occurred while inserting ITEM at INDEX'
   END IF
! Perform a deletion.
ELSE IF(INPUT .EQ. 'd' .OR. INPUT.EQ.'D') THEN
    WRITE (*,'(a)',ADVANCE='no') 'Enter index to delete: '
    READ (*,*) i
   CALL IVEC%DELETE(I, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while deleting at INDEX'
   END IF
! Push an item.
ELSE IF(INPUT .EQ. 'p' .OR. INPUT.EQ.'P') THEN
   WRITE (*,'(a)',ADVANCE='no') 'Enter item to push: '
   READ (*,*) IITEM
   CALL IVEC%PUSH(IITEM, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while pushing ITEM'
   END IF
! Pop an item.
ELSE IF(INPUT .EQ. 'v' .OR. INPUT.EQ.'V') THEN
   CALL IVEC%POP(IITEM, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while popping'
   END IF
   PRINT *, 'Top ITEM was: ', IITEM
! Set an item.
ELSE IF(INPUT .EQ. 's' .OR. INPUT.EQ.'S') THEN
   WRITE (*,'(a)',ADVANCE='no') &
      'Enter index then value to set: (Index, Item) '
   READ (*,*) I, IITEM
   CALL IVEC%SET(I, IITEM, ISTAT=IERR)
   IF(IERR .NE. 0) THEN
      PRINT *, 'Error occurred while setting'
   END IF
END IF
END DO
! Free the heap memory.
CALL IVEC%FREE(ISTAT=IERR)
IF(IERR .NE. 0) THEN
   PRINT *, 'Error occurred while freeing memory'
END IF

END IF
RETURN
END PROGRAM CMDLINE
