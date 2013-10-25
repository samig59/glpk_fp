! bp_ex_p.f90
!
! (C) 2012, 2013 Athanasios Migdalas @ MSCC
!
!  $Author: samig $
!
! The bp_ex code is free to copy, modify and distribute.
!
!------------------------------------------------------------------------------!
! Example of calling glpk from Fortran in order to solve                       !
! a binary problem. The eample is a 10-variable problem (pp.58--65)            !
! from the book "Discrete Optimization" by D. R. Plane and C. McMillan, Jr.,   !
! publish by Prentice-Hall in 1971.                                            !
!------------------------------------------------------------------------------!
! $Revision: 1.1 $                 
! Last edited: $Date: 2013/09/29 11:00:15 $
! Last successfully compiled and tested with:
!      gfortran 4.8.1  20130411 (prerelease)              Date: 01/10/2013
!      ifort 12.0.0 20101116                              Date: 01/10/2013
!      sunf95 8.6 2011/11/16                              Date: 29/09/2013
!      g95 (GCC 4.0.3 (g95 0.94!) Jan 17 2013)            Date: 01/10/2013
!      gfortran (Ubuntu/Linaro 4.6.3-1ubuntu5)            Date: 24/09/2013

PROGRAM bp_ex_p  
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_NULL_CHAR, C_PTR
  USE glpk_fp
  IMPLICIT NONE

  TYPE(C_PTR) :: prob_ptr  

  INTEGER(C_INT), PARAMETER :: M = 7, N = 10, MN = M*N ! Problem size
  INTEGER(C_INT), PARAMETER :: NZ = 41                 ! Nonzero matrix elements
  INTEGER(C_INT) :: stat

  REAL(C_DOUBLE) :: ar(0:NZ)             ! OBS! Must start at lbd 0 and
  INTEGER(C_INT) :: ia(0:NZ), ja(0:NZ)   ! ubd should be at least MN ( exactly NZ )

  TYPE(glp_smcp) :: prob_LPparam
  TYPE(glp_iocp) :: prob_IPparam

  REAL(C_DOUBLE) :: lp_obj, ip_obj
  
  ! Step 1: Create and name the problem

  prob_ptr = glp_create_prob()
  
  CALL glp_set_prob_name(prob_ptr, "My First Binary Example"//C_NULL_CHAR)

  ! Step 2: Define the problem data and load it

  CALL glp_set_obj_dir(prob_ptr, GLP_MIN) ! It's a minimization problem

  CALL glp_set_obj_name(prob_ptr, "Objective"//C_NULL_CHAR)

  stat = glp_add_rows(prob_ptr, M)  ! It has M(=7) rows (linear constraints)
  stat = glp_add_cols(prob_ptr, N)  ! It has N(=10) cols (decision variables)

  ! Give constraint names and bounds

  CALL glp_set_row_name(prob_ptr, 1, "First Row"//C_NULL_CHAR)
  CALL glp_set_row_bnds(prob_ptr, 1, GLP_LO, 2.0D0, 0.0D0)

  CALL glp_set_row_name(prob_ptr, 2, "Second Row"//C_NULL_CHAR)
  CALL glp_set_row_bnds(prob_ptr, 2, GLP_LO, 1.0D0, 0.0D0) 

  CALL glp_set_row_name(prob_ptr, 3, "Third Row"//C_NULL_CHAR)
  CALL glp_set_row_bnds(prob_ptr, 3, GLP_LO, 1.0D0, 0.0D0) 

  CALL glp_set_row_name(prob_ptr, 4, "Fourth Row"//C_NULL_CHAR)
  CALL glp_set_row_bnds(prob_ptr, 4, GLP_LO, -1.0D0, 0.0D0) 

  CALL glp_set_row_name(prob_ptr, 5, "Fifth Row"//C_NULL_CHAR)
  CALL glp_set_row_bnds(prob_ptr, 5, GLP_LO, 3.0D0, 0.0D0) 

  CALL glp_set_row_name(prob_ptr, 6, "Sixth Row"//C_NULL_CHAR)
  CALL glp_set_row_bnds(prob_ptr, 6, GLP_LO, 7.0D0, 0.0D0) 

  CALL glp_set_row_name(prob_ptr, 7, "Seventh Row"//C_NULL_CHAR)
  CALL glp_set_row_bnds(prob_ptr, 7, GLP_LO, 1.0D0, 0.0D0) 

  ! Give decision variable names, bounds and kinds (types)

  CALL glp_set_col_name(prob_ptr, 1, "X1"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 1, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 1, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 2, "X2"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 2, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 2, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 3, "X3"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 3, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 3, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 4, "X4"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 4, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 4, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 5, "X5"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 5, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 5, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 6, "X6"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 6, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 6, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 7, "X7")
  CALL glp_set_col_bnds(prob_ptr, 7, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 7, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 8, "X8"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 8, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 8, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 9, "X9"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 9, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 9, GLP_BV)

  CALL glp_set_col_name(prob_ptr, 10, "X10"//C_NULL_CHAR)
  CALL glp_set_col_bnds(prob_ptr, 10, GLP_DB, 0.0D0, 1.0D0)
  CALL glp_set_col_kind(prob_ptr, 10, GLP_BV)

  ! Give the objective function coefficients

  CALL glp_set_obj_coef(prob_ptr, 1, 10.0D0)
  CALL glp_set_obj_coef(prob_ptr, 2,  7.0D0)
  CALL glp_set_obj_coef(prob_ptr, 3,  1.0D0)
  CALL glp_set_obj_coef(prob_ptr, 4, 12.0D0)
  CALL glp_set_obj_coef(prob_ptr, 5,  2.0D0)
  CALL glp_set_obj_coef(prob_ptr, 6,  8.0D0)
  CALL glp_set_obj_coef(prob_ptr, 7,  3.0D0)
  CALL glp_set_obj_coef(prob_ptr, 8,  1.0D0)
  CALL glp_set_obj_coef(prob_ptr, 9,  5.0D0)
  CALL glp_set_obj_coef(prob_ptr,10,  3.0D0)
  
  ! Give the non-zero constraint matrix elements
  
  ! First row

  ! row index   ! col index ! coefficient value

  ia(1) = 1;      ja(1) = 1;   ar(1) =  -3.0D0
  ia(2) = 1;      ja(2) = 2;   ar(2) =  12.0D0
  ia(3) = 1;      ja(3) = 3;   ar(3) =   8.0D0
  ia(4) = 1;      ja(4) = 9;   ar(4) =   7.0D0
  ia(5) = 1;      ja(5) = 10;  ar(5) =  -2.0D0

  ! Second row

  ia(6) = 2;      ja(6) = 2;   ar(6) =  -1.0D0
  ia(7) = 2;      ja(7) = 3;   ar(7) =  10.0D0
  ia(8) = 2;      ja(8) = 5;   ar(8) =   5.0D0
  ia(9) = 2;      ja(9) = 6;   ar(9) =  -1.0D0
  ia(10)= 2;      ja(10)= 7;   ar(10)=  -7.0D0
  ia(11)= 2;      ja(11)= 8;   ar(11)=  -1.0D0

  ! Third row

  ia(12)= 3;      ja(12)= 1;   ar(12)=  -5.0D0
  ia(13)= 3;      ja(13)= 2;   ar(13)=   3.0D0
  ia(14)= 3;      ja(14)= 3;   ar(14)=   1.0D0
  ia(15)= 3;      ja(15)= 8;   ar(15)=   2.0D0
  ia(16)= 3;      ja(16)=10;   ar(16)=  -1.0D0

  ! Fourth row
  
  ia(17)= 4;      ja(17)= 1;   ar(17)=   5.0D0
  ia(18)= 4;      ja(18)= 2;   ar(18)=  -3.0D0
  ia(19)= 4;      ja(19)= 3;   ar(19)=  -1.0D0
  ia(20)= 4;      ja(20)= 8;   ar(20)=  -2.0D0
  ia(21)= 4;      ja(21)=10;   ar(21)=   1.0D0

  ! Fifth row

  ia(22)= 5;      ja(22)= 3;   ar(22)=   4.0D0
  ia(23)= 5;      ja(23)= 4;   ar(23)=   2.0D0
  ia(24)= 5;      ja(24)= 6;   ar(24)=   5.0D0
  ia(25)= 5;      ja(25)= 7;   ar(25)=  -1.0D0
  ia(26)= 5;      ja(26)= 8;   ar(26)=   9.0D0
  ia(27)= 5;      ja(27)= 9;   ar(27)=   2.0D0

  ! Sixth row

  ia(28)= 6;      ja(28)= 2;   ar(28)=  -9.0D0
  ia(29)= 6;      ja(29)= 4;   ar(29)=  12.0D0
  ia(30)= 6;      ja(30)= 5;   ar(30)=   7.0D0
  ia(31)= 6;      ja(31)= 6;   ar(31)=  -6.0D0
  ia(32)= 6;      ja(32)= 8;   ar(32)=  -2.0D0
  ia(33)= 6;      ja(33)= 9;   ar(33)=  15.0D0
  ia(34)= 6;      ja(34)=10;   ar(34)=  -3.0D0

  ! Seventh row

  ia(35)= 7;      ja(35)= 1;   ar(35)=   8.0D0
  ia(36)= 7;      ja(36)= 2;   ar(36)=  -5.0D0
  ia(37)= 7;      ja(37)= 3;   ar(37)=  -2.0D0
  ia(38)= 7;      ja(38)= 4;   ar(38)=   7.0D0
  ia(39)= 7;      ja(39)= 5;   ar(39)=   1.0D0
  ia(40)= 7;      ja(40)= 7;   ar(40)=   5.0D0
  ia(41)= 7;      ja(41)= 9;   ar(41)=  10.0D0

  ! Load the matrix. Note the exact number of non-zeros are passed.

  CALL glp_load_matrix(prob_ptr, NZ, ia, ja, ar)

  ! Step 3: Initialize the parameters of Simplex solver 

  CALL glp_init_smcp(prob_LPparam)
  prob_LPparam%presolve = GLP_ON

  ! Step 4: Initialize the parameters of the Integer solver

  stat = glp_init_iocp(prob_IPparam)
  prob_IPparam%fp_heur = GLP_ON

  ! If a call-back function is to be used, pass its address ...
  ! prob_IPparam%cb_func = LOC(your_call-back_function_name)

  ! Step 5: Solve the LP relaxation of the proble

  stat   = glp_simplex(prob_ptr, prob_LPparam)
  lp_obj = glp_get_obj_val(prob_ptr)
  stat   = glp_get_status(prob_ptr)

  ! Note: The following is not necessary sinc glpk is indeed very verbal...

  PRINT *
  PRINT *, "================================================="
  PRINT *, " LP Relaxation "
  PRINT *, " LP objective value: ", lp_obj
  PRINT *, " LP status:          ", stat
  PRINT *, "================================================="
  PRINT *

  ! Step 6: Solve the integer problem

  stat   = glp_intopt(prob_ptr, prob_IPparam)
  ip_obj = glp_mip_obj_val(prob_ptr)
  stat   = glp_mip_status(prob_ptr)

  PRINT *
  PRINT *, "================================================="
  PRINT *, " Integer Program "
  PRINT *, " IP objective value: ", ip_obj
  PRINT *, " IP status:          ", stat
  PRINT *, "================================================="
  PRINT *

END PROGRAM bp_ex_p
