! model_ex.f90
!
! (C) 2012, 2013 Athanasios Migdalas @ MSCC
!
! $Author: samig $
!
! The lp_ex code is free to copy, modify and distribute.
!
!---------------------------------------------------------------------------!
! PURPOSE/DESCRIPTION:                                                      !
! The code demostrates the read in of Gnu MPL model, creation and solution  !
! of the problem, and the export in glpk MPS format.                        !
!                                                                           !
! Code based on Example 1 (pp. 94--95) of section 3.2.1 of the              !
! GNU Linear Programming Kit Reference Manual by Andrew Makhorin.           !
!---------------------------------------------------------------------------!
!                                                                           
! $Revision: 1.2 $
! Last modified: $Date: 2013/09/29 18:57:16 $
! Last compiled and tested successfully with:
!      gfortran 4.8.1 20130411                         Date: 26/09/2013
!

PROGRAM model_ex

  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_NULL_CHAR

  USE glpk_f

  IMPLICIT NONE

  INTEGER(C_INT) :: prob, stat, tran
  TYPE(glp_smcp) :: prob_param
  
  CHARACTER(C_CHAR), POINTER :: null_ptr => NULL() ! A null character pointer

  TYPE(glp_mpscp), POINTER :: null_mpscp => NULL() ! A null struct pointer

  INTEGER(C_INT), PARAMETER :: skip = 0  ! 0 = don't skip data section; 1 = skip data section

  ! Step 1: Create the problem and allocate memory space for it

  prob = glp_create_prob()

  tran = glp_mpl_alloc_wksp()

  ! Step 2: Read in the problem from the model file

  stat = glp_mpl_read_model(tran, "assign.mod"//CHAR(0), skip)

  IF (stat /= 0 ) THEN
     PRINT *, " ERROR while reading in and translating the model: ", stat
  ENDIF

  ! Step 3: Read in the proble data from a file.

   !  stat = glp_mpl_read_data(tran, "assign.dat"//CHAR(0))

   ! IF ( stat /= 0 ) THEN
   !    PRINT *, " ERROR while reading in the problem data: ", stat
   !    CALL glp_mpl_free_wksp(tran)
   !    GOTO 100
   ! ENDIF

  ! Step 4: Generate the model

  stat = glp_mpl_generate(tran, null_ptr)

  IF ( stat /= 0 ) THEN
     PRINT *, " ERROR while generating the model: ", stat
     GOTO 100
  ENDIF

  ! Step 5: Build the problem and may export it in free MPS form

  CALL glp_mpl_build_prob(tran, prob)

  stat = glp_write_mps(prob, GLP_MPS_FILE, null_mpscp, "assign.mps")

  IF ( stat /= 0 ) THEN
     PRINT *, " ERROR while writing to the file the glpk MPS format: ", stat
     GOTO 100
  ENDIF

  ! Step 6: Initialize the solver control parameters, here the simplex method.

  CALL glp_init_smcp(prob_param)

  prob_param%msg_lev = GLP_MSG_ALL
  prob_param%meth = GLP_PRIMAL
  

  ! Step 7: Apply the solver, here the simplex method, to solve the problem

  stat = glp_simplex(prob, prob_param)

  ! Step 8: Free allocated workspace and delete the problem

  100 CONTINUE

  CALL glp_mpl_free_wksp(tran)
  CALL glp_delete_prob(prob)

END PROGRAM model_ex
