! cb_prob_common.f90
!
! (C) 2012, 2013 Athanasios Migdalas @ MSCC
!
!  $Author: samig $
!
! The cb_prob_common code is free to copy, modify and distribute.
!
!-----------------------------------------------------------------------------------------!
! PURPOSE:                                                                                !
! Example of module to communicate problem parameters and other data to a call-back       !
! procedure, such as the cb_proc.f90, for the (mixed) integer solver of the GNU Linear    !
! Programming Kit (glpk)                                                                  !
!                                                                                         !
!-----------------------------------------------------------------------------------------!
! $Revision: 1.3 $                 
! Last edited: $Date: 2013/10/01 22:31:39 $
! Last successfully compiled and tested with:
!      gfortran 4.8.1  20130411 (prerelease)              Date: 29/09/2013
!      ifort 12.0.0 20101116                              Date: 29/09/2013
!      sunf95 8.6 2011/11/16                              Date: 29/09/2013
!      G95 (GCC 4.0.3 (g95 0.94!) Jan 17 2013)            Date: 29/09/2013
!      gfortran 4.9.0 20130319 (experimental)             Date: 27/10/2013
!-----------------------------------------------------------------------------------------!

MODULE cb_prob_common

  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_CHAR, C_PTR

  TYPE(C_PTR) :: prob_ptr         ! To hold the pointer to the problem structure

  INTEGER(C_INT), PARAMETER :: M = 7, N = 10, MN = M*N ! Problem size
  INTEGER(C_INT), PARAMETER :: NZ = 41                 ! Nonzero matrix elements

  ! An initial feasible solution to replace glpk heuristic  (see cb_proc.f90)

  REAL(C_DOUBLE) :: x(0:N)  = [0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0]

END MODULE cb_prob_common
