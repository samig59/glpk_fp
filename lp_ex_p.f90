! lp_ex_p.f90
!
! (C) 2010, 2011, 2012, 2013 Athanasios Migdalas @ MSCC
!
! $Author: samig $
!
! The lp_ex_p code is free to copy, modify and distribute.
!
!----------------------------------------------------------------!
! PURPOSE/DESCRIPTION:                                           !
! Example of calling glpk from Fortran in order to solve         !
! a linear programming problem. The problem is from Chapter 1    !
! of the GNU Linear Programming Kit Reference Manual by          !
! Andrew Makhorin. The Fortran code follows almost line by line  !
! the C-code given there.                                        !
!----------------------------------------------------------------!
!
! $Revision: 1.3 $
! Last edited: $Date: 2013/10/01 20:34:15 $
! Last compiled and successfully with: 
!      gfortran 4.6.3                                  Date: 29/09/2013
!      sunf90 Sun Fortran 95 8.6 Linux_i386 2011/11/1  Date: 29/09/2013
!      ifort 12.0.0 20101116                           Date: 24/09/2013
!      gfortran 4.8.1 20130411                         Date: 26/09/2013
!
!----------------------------------------------------------------!
!

PROGRAM lp_ex_p
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_NULL_CHAR, C_PTR
  USE glpk_fp
  IMPLICIT NONE

  ! Pointer to problem structure ... ONLY DIFFERENCE FROM lp_ex.f90

  TYPE(C_PTR) :: prob_ptr

  ! Simplex control parameters for the problem

  TYPE(glp_smcp) :: prob_lp_param
  
  ! Number of constraints and decision variables in the problem

  INTEGER(C_INT), PARAMETER :: M = 3, N = 3 

  ! Number of nonzeros in the problem constraint matrix

  INTEGER(C_INT), PARAMETER :: NZ = M * N ! Full matrix here

  ! Structure of tripplets to hold the matrix in sparse form

  REAL(C_DOUBLE) :: ar(0:NZ)           ! OBS!!! Must start at lbd 0 
  INTEGER(C_INT) :: ia(0:NZ), ja(0:NZ)  ! and ubd must be at least NZ

  ! Optimal solution components

  REAL(C_DOUBLE) :: xopt(N), zopt

  ! Work variables

  INTEGER(C_INT) :: stat, i

  ! Step 1: create and name the problem

  prob_ptr = glp_create_prob()
  
  CALL glp_set_prob_name(prob_ptr, "My First LP Example"//C_NULL_CHAR)

  ! Step 2: define the problem data and load it

  CALL glp_set_obj_dir(prob_ptr, GLP_MAX) ! It's a maximiztion problem

  CALL glp_set_obj_name(prob_ptr, "Objective"//C_NULL_CHAR)
  
  stat = glp_add_rows(prob_ptr, M)          ! It has M (=3) row

  stat = glp_add_cols(prob_ptr, N)          ! It has N (=3) columns (decision variables)

  ! If the default integer is 4 then the designation _4 is not needed.
  ! This is true for gfortran and ifort but not for g95 (which may have integer 8 as default)

  CALL glp_set_row_name(prob_ptr, 1_4, "First Row"//C_NULL_CHAR)   ! Name first row
   
  CALL glp_set_row_bnds(prob_ptr, 1_4, GLP_UP, 0.0D0, 100.0D0) ! Give its bounds

  CALL glp_set_row_name(prob_ptr, 2_4, "Second Row"//C_NULL_CHAR)
  
  CALL glp_set_row_bnds(prob_ptr, 2_4, GLP_UP, 0.0D0, 600.0D0)

  CALL glp_set_row_name(prob_ptr, 3_4, "Third Row"//C_NULL_CHAR)
  
  CALL glp_set_row_bnds(prob_ptr, 3_4, GLP_UP, 0.0D0, 300.0D0)

  CALL glp_set_col_name(prob_ptr, 1_4, "X1"//C_NULL_CHAR)   ! Name first column (decision variable)

  CALL glp_set_col_bnds(prob_ptr, 1_4, GLP_LO, 0.0D0, 0.0D0) ! Give its bounds

  CALL glp_set_obj_coef(prob_ptr, 1_4, 10.0D0) ! Give its objective coefficient

  CALL glp_set_col_name(prob_ptr, 2_4, "X2"//C_NULL_CHAR)   

  CALL glp_set_col_bnds(prob_ptr, 2_4, GLP_LO, 0.0D0, 0.0D0) 

  CALL glp_set_obj_coef(prob_ptr, 2_4, 6.0D0) 

  CALL glp_set_col_name(prob_ptr, 3_4, "X3"//C_NULL_CHAR)   

  CALL glp_set_col_bnds(prob_ptr, 3_4, GLP_LO, 0.0D0, 0.0D0) 

  CALL glp_set_obj_coef(prob_ptr, 3_4, 4.0D0) 

  ! Create the constraints matrix in the form of triplets (like in sparse matrices)

  ! First matrix row

  ia(1) = 1; ja(1) = 1; ar(1) = 1.0D0
  ia(2) = 1; ja(2) = 2; ar(2) = 1.0D0
  ia(3) = 1; ja(3) = 3; ar(3) = 1.0D0

  ! Second matrix row

  ia(4) = 2; ja(4) = 1; ar(4) = 10.0D0
  ia(5) = 2; ja(5) = 2; ar(5) =  4.0D0
  ia(6) = 2; ja(6) = 3; ar(6) =  5.0D0
  
  ! Third matrix row

  ia(7) = 3; ja(7) = 1; ar(7) =  2.0D0
  ia(8) = 3; ja(8) = 2; ar(8) =  2.0D0
  ia(9) = 3; ja(9) = 3; ar(9) =  6.0D0

  ! Load the matrix

  CALL glp_load_matrix(prob_ptr, M*N, ia, ja, ar)

  ! Step 3: Solve the problem using the simplex method

  ! Initialize and if necessary specify control parameters

  CALL glp_init_smcp(prob_lp_param)

  prob_lp_param%msg_lev = GLP_MSG_ALL
  prob_lp_param%meth = GLP_PRIMAL
  prob_lp_param%pricing = GLP_PT_PSE
  prob_lp_param%r_test = GLP_RT_HAR

  stat = glp_simplex(prob_ptr, prob_lp_param)

  ! Step 4: Extract the solution and print it

  zopt = glp_get_obj_val(prob_ptr)

  DO i = 1, N
     xopt(i) = glp_get_col_prim(prob_ptr, i)
  ENDDO

  ! Not really needed.

  PRINT *
  PRINT *, "====================================="
  PRINT *, "||            LP SOLUTION          ||"
  PRINT *, "====================================="
  PRINT *
  PRINT *, " OBJECTIVE VALUE: ", zopt
  PRINT *, " VALUES OF DECISION VARIABLES: "
  PRINT *, (xopt(i), i = 1, N)
  PRINT *

  ! Step 5: Delete problem from memory

  CALL glp_delete_prob(prob_ptr)

END PROGRAM lp_ex_p

!
!
! The output of this program should be similar to the following:
!
!
! GLPK Simplex Optimizer, v4.45
! 3 rows, 3 columns, 9 non-zeros
! *     0: obj =   0.000000000e+00  infeas =  0.000e+00 (0)
! *     2: obj =   7.333333333e+02  infeas =  0.000e+00 (0)
! OPTIMAL SOLUTION FOUND

!  =====================================
!  ||            LP SOLUTION          ||
!  =====================================

!   OBJECTIVE VALUE:    733.33333333333326     
!   VALUES OF DECISION VARIABLES: 
!    33.333333333333336        66.666666666666657        0.0000000000000000    
