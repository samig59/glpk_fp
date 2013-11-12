! cb_proc.f90
!
! (C) 2012, 2013 Athanasios Migdalas @ MSCC
!
!  $Author: samig $
!
! The cb_proc code is free to copy, modify and distribute.
!
!-----------------------------------------------------------------------------------------!
! PURPOSE:                                                                                !
! Skeleton and example of call-back procedure for the GLPK integer programming solver.    !
! Consult Chapter 5 of the "GNU Linear Programming Kit" draft by Andrew Makhorin          !
! for more details.                                                                       !
!                                                                                         !
!-----------------------------------------------------------------------------------------!
! $Revision: 1.3 $                 
! Last edited: $Date: 2013/10/01 22:37:24 $
! Last successfully compiled and tested with:
!      gfortran 4.8.1  20130411 (prerelease)              Date: 02/10/2013
!      ifort 12.0.0 20101116                              Date: 29/09/2013
!      sunf95 8.6 2011/11/16                              Date: 29/09/2013
!      G95 (GCC 4.0.3 (g95 0.94!) Jan 17 2013)            Date: 29/09/2013
!      gfortran 4.9.0 20130319 (experimental)             Date: 27/10/2013
!-----------------------------------------------------------------------------------------!

SUBROUTINE cb_proc(tree,info)  BIND(C)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR

  USE glpk_fp

  USE cb_prob_common, ONLY: N, prob_ptr, x ! Module defining problem parameters

  IMPLICIT NONE

  TYPE(C_PTR), VALUE :: tree
  INTEGER(C_INT), VALUE :: info 

  INTEGER(C_INT) :: i

  ! Check the reason for which the call-back procedure has been called
  ! by the integer solver and take the appropriate actions.
  
  PRINT *, "Call-back reason: ", glp_ios_reason(tree) ! Just to have some output ;-)

  SELECT CASE( glp_ios_reason(tree) )

     !CASE(GLP_ISELECT)

        ! Here a subroblem may be selected from the active list
        ! and its reference number be passed to the solver using
        ! glp_ios_select_node. 
        !
        ! The default behavior of the solver is otherwise selected 
        ! by the glp_iocp%bt_tech parameter.
        !
        ! glp_ios_next_node and glp_ios_prev_node may be used to
        ! explore the active subproblem list.

     !CASE(GLP_IPREPRO)

        ! The LP relaxation of the currently selected subroblem 
        ! has not been solved yet. Thus some kind of preprocessing
        ! may be applied.

     !CASE(GLP_IROWGEN)

        ! The LP relaxation of the currently selected subproblem
        ! has just been solved to optimality and its objective
        ! value is the best so far. Thus, additional constraints
        ! that cut the current optimal solution may be added and
        ! the LP relaxtion re-optimized.
        !
        ! This addition can be done using glp_add_rows, glp_set_row_name,
        ! glp_set_row_bnds, and glp_set_mat_row.
        !
        ! Solution information about the current LP relaxation can
        ! be obtained with glp_get_obj_val, glp_get_row_prim,
        ! glp_get_row_dual, glp_get_col_prim, and glp_get_col_dual.

     CASE(GLP_IHEUR)

        ! The LP relaxation has produced an objective value that is
        ! better than the best known integer bound but the its
        ! solution is fractional.
        !
        ! Then it may be worthwile to find and then supply an integer solution
        ! by calling glp_ios_heur_sol.
        !
        ! Here if the use of the glp_ios_curr_node idicates that  there
        ! is only one subproblem in the tree, then, essentially we are
        ! replacing the glpk presolver.

        IF ( glp_ios_curr_node(tree) == 1 ) CALL glp_ios_heur_sol(tree,x)
           
     !CASE(GLP_ICUTGEN)

        ! The LP relaxation of the current subproblem has just been solved
        ! and a fractional optimal solution was obtained though the 
        ! produced objective value is better than the best integer bound.
        !
        ! Thus cutting plane constraints may be added before applying 
        ! branching. These can be added either as in the case of 
        ! GLP_IROWGEN or by adding them to the cutting pool of the solver.

     !CASE(GLP_IBRANCH)

        ! The LP relaxation of the current subproblem has just been solved
        ! and a a fractional optimal solution was obtained though the 
        ! produced objective value is better than the best integer bound.
        !
        ! Then a suitable variable may be chosen here for branching
        ! and its ordinal number be passed to the solver with
        ! glp_ios_branch_upon.
        !
        ! Otherwise, the default behavior of the solver is defined 
        ! through the glp_iocp%br_tech parameter.

     CASE(GLP_IBINGO)

        ! The LP relaxation of the current subproblem has just been solved
        ! abd its solution is integer with an objective value that is better
        ! than the best integer bound so far. Components of such a solution
        ! can be obtained the same way as in the case of GLP_IROWGEN.
        !
        ! Components of the new (mixed) integer solution can be obtained
        ! with glp_mip_obj_val, glp_mip_row_val, and glp_mip_col_val.
        !
        ! No further actions are allowed.

        DO i = 1, N
           x(i) = glp_mip_col_val(prob_ptr,i)
        ENDDO

  END SELECT

END SUBROUTINE cb_proc
