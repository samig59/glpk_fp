! glpk_fp.f90
! based on glpk.h (version as given by the VERSION constants below)
!
!---------------------------------------------------------------------------------!
! glpk_fp.f90 and glpk_f.f90 are differentiated only with respect of the treatment!
! of the types glp_tree, glp_tran and glp_prob (see below)                        !
!---------------------------------------------------------------------------------!
!
! $Author: samig $
!
!**********************************************************************************
!  
!  Copyright (C) 2013 Athanasios Migdalas, MSCC
!  E-mail: <samig59@gmail.com>.
!
!  The glpk_fp module is free: you can redistribute it and/or modify it
!  under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!
!  The glpk_fp module is distributed in the hope that it will be useful, but WITHOUT
!  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
!  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
!  License for more details.
!
!  For further details, see <http://www.gnu.org/licenses/>.
!**********************************************************************************
! $Revision: 1.7 $
! Last modified: $Date: 2013/10/26 16:13:10 $
! Last compiled with: 
!      gfortran 4.6.3                                   (Date: 27/09/2013)
!      ifort 12.0.0 20101116                            (Date: 01/10/2013)
!      gfortran 4.8.1 20130411                          (Date: 01/10/2013)
!      G95 (GCC 4.0.3 (g95 0.94!) Jan 17 2013)          (Date: 01/10/2013)
!      sunf90 Sun Fortran 95 8.6 Linux_i386 2011/11/16  (Date: 01/10/2013)
!

MODULE glpk_fp

  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_CHAR, C_FUNPTR
  
  IMPLICIT NONE

!----------------------------------------------------------------------------------!
!                            PARAMETERS                                            !
!           Defined as macro constants in glpk.h                                   !
!----------------------------------------------------------------------------------!

! /* library version numbers: */

  INTEGER(C_INT), PARAMETER :: GLP_MAJOR_VERSION =  4
  INTEGER(C_INT), PARAMETER :: GLP_MINOR_VERSION = 45

! /* optimization direction flag: */

  INTEGER(C_INT), PARAMETER :: GLP_MIN  = 1  !/* minimization */
  INTEGER(C_INT), PARAMETER :: GLP_MAX  = 2  !/* maximization */

! /* type of auxiliary/structural variable: */

  INTEGER(C_INT), PARAMETER :: GLP_FR = 1  !/* free variable */
  INTEGER(C_INT), PARAMETER :: GLP_LO = 2  !/* variable with lower bound */
  INTEGER(C_INT), PARAMETER :: GLP_UP = 3  !/* variable with upper bound */
  INTEGER(C_INT), PARAMETER :: GLP_DB = 4  !/* double-bounded variable */
  INTEGER(C_INT), PARAMETER :: GLP_FX = 5  !/* fixed variable */

! /* solution status: */

  INTEGER(C_INT), PARAMETER :: GLP_UNDEF = 1  !/* solution is undefined */
  INTEGER(C_INT), PARAMETER :: GLP_FEAS  = 2  !/* solution is feasible */
  INTEGER(C_INT), PARAMETER :: GLP_INFEAS= 3  !/* solution is infeasible */
  INTEGER(C_INT), PARAMETER :: GLP_NOFEAS= 4  !/* no feasible solution exists */
  INTEGER(C_INT), PARAMETER :: GLP_OPT   = 5  !/* solution is optimal */
  INTEGER(C_INT), PARAMETER :: GLP_UNBND = 6  !/* solution is unbounded */

! /* kind of structural variable: */

  INTEGER(C_INT), PARAMETER :: GLP_CV = 1  !/* continuous variable */
  INTEGER(C_INT), PARAMETER :: GLP_IV = 2  !/* integer variable */
  INTEGER(C_INT), PARAMETER :: GLP_BV = 3  !/* binary variable */

! /* status of auxiliary/structural variable: */

  INTEGER(C_INT), PARAMETER :: GLP_BS = 1  !/* basic variable */
  INTEGER(C_INT), PARAMETER :: GLP_NL = 2  !/* non-basic variable on lower bound */
  INTEGER(C_INT), PARAMETER :: GLP_NU = 3  !/* non-basic variable on upper bound */
  INTEGER(C_INT), PARAMETER :: GLP_NF = 4  !/* non-basic free variable */
  INTEGER(C_INT), PARAMETER :: GLP_NS = 5  !/* non-basic fixed variable */

! /* solution indicator: */

  INTEGER(C_INT), PARAMETER :: GLP_SOL = 1  !/* basic solution */
  INTEGER(C_INT), PARAMETER :: GLP_IPT = 2  !/* interior-point solution */
  INTEGER(C_INT), PARAMETER :: GLP_MIP = 3  !/* mixed integer solution */

! /* scaling options: */

  INTEGER(C_INT), PARAMETER :: GLP_SF_GM = Z'01' ! 0x01  /* perform geometric mean scaling */
  INTEGER(C_INT), PARAMETER :: GLP_SF_EQ = Z'10' ! 0x10  /* perform equilibration scaling */
  INTEGER(C_INT), PARAMETER :: GLP_SF_2N = Z'20' ! 0x20  /* round scale factors to power of two */
  INTEGER(C_INT), PARAMETER :: GLP_SF_SKIP = Z'40' ! 0x40  /* skip if problem is well scaled */
  INTEGER(C_INT), PARAMETER :: GLP_SF_AUTO = Z'80' ! 0x80  /* choose scaling options automatically */

! /* reason codes: */

  INTEGER(C_INT), PARAMETER :: GLP_IROWGEN = Z'01' ! 0x01  /* request for row generation */
  INTEGER(C_INT), PARAMETER :: GLP_IBINGO  = Z'02' ! 0x02  /* better integer solution found */
  INTEGER(C_INT), PARAMETER :: GLP_IHEUR   = Z'03' ! 0x03  /* request for heuristic solution */
  INTEGER(C_INT), PARAMETER :: GLP_ICUTGEN = Z'04' ! 0x04  /* request for cut generation */
  INTEGER(C_INT), PARAMETER :: GLP_IBRANCH = Z'05' ! 0x05  /* request for branching */
  INTEGER(C_INT), PARAMETER :: GLP_ISELECT = Z'06' ! 0x06  /* request for subproblem selection */
  INTEGER(C_INT), PARAMETER :: GLP_IPREPRO = Z'07' ! 0x07  /* request for preprocessing */

! /* factorization type */

  INTEGER(C_INT), PARAMETER :: GLP_BF_FT = 1 ! /* LUF + Forrest-Tomlin */
  INTEGER(C_INT), PARAMETER :: GLP_BF_BG = 2 ! /* LUF + Schur compl. + Bartels-Golub */
  INTEGER(C_INT), PARAMETER :: GLP_BF_GR = 3 ! /* LUF + Schur compl. + Givens rotation */

! /* START: simplex method control parameters */

  INTEGER(C_INT), PARAMETER :: GLP_MSG_OFF = 0  ! /* no output */
  INTEGER(C_INT), PARAMETER :: GLP_MSG_ERR = 1  ! /* warning and error messages only */
  INTEGER(C_INT), PARAMETER :: GLP_MSG_ON  = 2  ! /* normal output */
  INTEGER(C_INT), PARAMETER :: GLP_MSG_ALL = 3  ! /* full output */
  INTEGER(C_INT), PARAMETER :: GLP_MSG_DBG = 4  !/* debug output */

! /* simplex method option: */

  INTEGER(C_INT), PARAMETER :: GLP_PRIMAL = 1  !/* USE, INTRINSIC :: primal simplex */
  INTEGER(C_INT), PARAMETER :: GLP_DUALP  = 2  !/* USE, INTRINSIC :: dual; if it fails, USE, INTRINSIC :: primal */
  INTEGER(C_INT), PARAMETER :: GLP_DUAL   = 3  !/* USE, INTRINSIC :: dual simplex */

! /* pricing technique: */

  INTEGER(C_INT), PARAMETER :: GLP_PT_STD = Z'11' !  0x11  /* standard (Dantzig rule) */
  INTEGER(C_INT), PARAMETER :: GLP_PT_PSE = Z'22' !  0x22  /* projected steepest edge *

! /* ratio test technique: */

  INTEGER(C_INT), PARAMETER :: GLP_RT_STD = Z'11' !  0x11  /* standard (textbook) */
  INTEGER(C_INT), PARAMETER :: GLP_RT_HAR = Z'22' !  0x22  /* two-pass Harris' ratio test */

! /* END: simplex method control parameters */

! /* ordering algorithm: */

  INTEGER(C_INT), PARAMETER :: GLP_ORD_NONE = 0  ! /* natural (original) ordering */
  INTEGER(C_INT), PARAMETER :: GLP_ORD_QMD  = 1  ! /* quotient minimum degree (QMD) */
  INTEGER(C_INT), PARAMETER :: GLP_ORD_AMD  = 2  ! /* approx. minimum degree (AMD) */
  INTEGER(C_INT), PARAMETER :: GLP_ORD_SYMAMD = 3! /* approx. minimum degree (SYMAMD) */

! /* START: integer optimizer control parameters */

! /* branching technique: */

  INTEGER(C_INT), PARAMETER :: GLP_BR_FFV = 1 ! /* first fractional variable */
  INTEGER(C_INT), PARAMETER :: GLP_BR_LFV = 2 ! /* last fractional variable */
  INTEGER(C_INT), PARAMETER :: GLP_BR_MFV = 3 ! /* most fractional variable */
  INTEGER(C_INT), PARAMETER :: GLP_BR_DTH = 4 ! /* heuristic by Driebeck and Tomlin */
  INTEGER(C_INT), PARAMETER :: GLP_BR_PCH = 5 ! /* hybrid pseudocost heuristic *

!/* backtracking technique: */

  INTEGER(C_INT), PARAMETER :: GLP_BT_DFS = 1 ! /* depth first search */
  INTEGER(C_INT), PARAMETER :: GLP_BT_BFS = 2 ! /* breadth first search */
  INTEGER(C_INT), PARAMETER :: GLP_BT_BLB = 3 ! /* best local bound */
  INTEGER(C_INT), PARAMETER :: GLP_BT_BPH = 4 ! /* best projection heuristic */

!/* preprocessing technique: */

  INTEGER(C_INT), PARAMETER :: GLP_PP_NONE = 0 ! /* disable preprocessing */
  INTEGER(C_INT), PARAMETER :: GLP_PP_ROOT = 1 ! /* preprocessing only on root level */
  INTEGER(C_INT), PARAMETER :: GLP_PP_ALL  = 2 ! /* preprocessing on all levels */

! /* END: integer optimizer control parameters */

! /* START: additional row attributes */

! /* row origin flag: */

  INTEGER(C_INT), PARAMETER :: GLP_RF_REG = 0  ! /* regular constraint */
  INTEGER(C_INT), PARAMETER :: GLP_RF_LAZY= 1  ! /* "lazy" constraint */
  INTEGER(C_INT), PARAMETER :: GLP_RF_CUT = 2  ! /* cutting plane constraint */

! /* row class descriptor: */

  INTEGER(C_INT), PARAMETER :: GLP_RF_GMI = 1  ! /* Gomory's mixed integer cut */
  INTEGER(C_INT), PARAMETER :: GLP_RF_MIR = 2  ! /* mixed integer rounding cut */
  INTEGER(C_INT), PARAMETER :: GLP_RF_COV = 3  ! /* mixed cover cut */
  INTEGER(C_INT), PARAMETER :: GLP_RF_CLQ = 4  ! /* clique cut */

! /* END: additional row attributes */

! /* enable/disable flag: */

  INTEGER(C_INT), PARAMETER :: GLP_ON = 1  ! /* enable something */
  INTEGER(C_INT), PARAMETER :: GLP_OFF= 0  ! /* disable something */

! /* branch selection indicator: */

  INTEGER(C_INT), PARAMETER :: GLP_NO_BRNCH = 0 ! /* select no branch */
  INTEGER(C_INT), PARAMETER :: GLP_DN_BRNCH = 1 ! /* select down-branch */
  INTEGER(C_INT), PARAMETER :: GLP_UP_BRNCH = 2 ! /* select up-branch */

! /* return codes: */

  INTEGER(C_INT), PARAMETER :: GLP_EBADB = Z'01' !  0x01  /* invalid basis */
  INTEGER(C_INT), PARAMETER :: GLP_ESING = Z'02' !  0x02  /* singular matrix */
  INTEGER(C_INT), PARAMETER :: GLP_ECOND = Z'03' !  0x03  /* ill-conditioned matrix */
  INTEGER(C_INT), PARAMETER :: GLP_EBOUND= Z'04' !  0x04  /* invalid bounds */
  INTEGER(C_INT), PARAMETER :: GLP_EFAIL = Z'05' !  0x05  /* solver failed */
  INTEGER(C_INT), PARAMETER :: GLP_EOBJLL= Z'06' !  0x06  /* objective lower limit reached */
  INTEGER(C_INT), PARAMETER :: GLP_EOBJUL= Z'07' !  0x07  /* objective upper limit reached */
  INTEGER(C_INT), PARAMETER :: GLP_EITLIM= Z'08' !  0x08  /* iteration limit exceeded */
  INTEGER(C_INT), PARAMETER :: GLP_ETMLIM= Z'09' !  0x09  /* time limit exceeded */
  INTEGER(C_INT), PARAMETER :: GLP_ENOPFS= Z'0A' !  0x0A  /* no primal feasible solution */
  INTEGER(C_INT), PARAMETER :: GLP_ENODFS= Z'0B' !  0x0B  /* no dual feasible solution */
  INTEGER(C_INT), PARAMETER :: GLP_EROOT = Z'0C' !  0x0C  /* root LP optimum not provided */
  INTEGER(C_INT), PARAMETER :: GLP_ESTOP = Z'0D' !  0x0D  /* search terminated by application */
  INTEGER(C_INT), PARAMETER :: GLP_EMIPGAP=Z'0E' !  0x0E  /* relative mip gap tolerance reached */
  INTEGER(C_INT), PARAMETER :: GLP_ENOFEAS=Z'0F' !  0x0F  /* no primal/dual feasible solution */
  INTEGER(C_INT), PARAMETER :: GLP_ENOCVG =Z'10' !  0x10  /* no convergence */
  INTEGER(C_INT), PARAMETER :: GLP_EINSTAB=Z'11' !  0x11  /* numerical instability */
  INTEGER(C_INT), PARAMETER :: GLP_EDATA  =Z'12' !  0x12  /* invalid data */
  INTEGER(C_INT), PARAMETER :: GLP_ERANGE =Z'13' !  0x13  /* result out of range */

! /* condition indicator: */

  INTEGER(C_INT), PARAMETER :: GLP_KKT_PE = 1 ! /* primal equalities */
  INTEGER(C_INT), PARAMETER :: GLP_KKT_PB = 2 ! /* primal bounds */
  INTEGER(C_INT), PARAMETER :: GLP_KKT_DE = 3 ! /* dual equalities */
  INTEGER(C_INT), PARAMETER :: GLP_KKT_DB = 4 ! /* dual bounds */
  INTEGER(C_INT), PARAMETER :: GLP_KKT_CS = 5 ! /* complementary slackness */

! /* MPS file format: */

  INTEGER(C_INT), PARAMETER :: GLP_MPS_DECK = 1 !  /* fixed (ancient) */
  INTEGER(C_INT), PARAMETER :: GLP_MPS_FILE = 2 !  /* free (modern) */

!----------------------------------------------------------------------------------!
!                                 TYPES                                            !
!                            New structures                                        !
!----------------------------------------------------------------------------------!

!-------------------------! OBS! OBS! OBS! !---------------------------------------!
! The types glp_tree, glp_tran and glp_prob are accessed in C by pointers which,   !
! in the case of glpk_f.f90, are declared as INTEGER(C_INT) in the calling Fortran !
! unit and declared as INTEGER(C_INT), VALUE arguments in the Fortran interface.   !
!                                                                                  !
! ALTERNATIVELY: They can be declared as TYPE(C_PTR) in the calling Fortran unit   !
! and declared as TYPE(C_PTR), VALUE arguments in the Fortran interface.  This is  !
! done here (in glpk_fp.f90).                                                      !
!----------------------------------------------------------------------------------!
!
! #ifndef GLP_TREE_DEFINED
! #define GLP_TREE_DEFINED
! typedef struct { double _opaque_tree[100]; } glp_tree;
! /* branch-and-bound tree */
! #endif

! #ifndef GLP_TRAN_DEFINED
! #define GLP_TRAN_DEFINED
! typedef struct { double _opaque_tran[100]; } glp_tran;
! /* MathProg translator workspace */
! #endif

! #ifndef GLP_PROB_DEFINED
! #define GLP_PROB_DEFINED
! typedef struct { double _opaque_prob[100]; } glp_prob;
! /* LP/MIP problem object */
! #endif
!----------------------------------------------------------------------------------!

! /* basis factorization control parameters */

  TYPE, BIND(C) ::  glp_bfcp
       INTEGER(C_INT) :: msg_lev    ! /* (reserved) */
       INTEGER(C_INT) :: type       ! /* factorization type */
       INTEGER(C_INT) :: lu_size    ! /* luf.sv_size */
       REAL(C_DOUBLE) :: piv_tol    ! /* luf.piv_tol */
       INTEGER(C_INT) :: piv_lim    ! /* luf.piv_lim */
       INTEGER(C_INT) :: suhl       ! /* luf.suhl */
       REAL(C_DOUBLE) :: eps_tol    ! /* luf.eps_tol */
       REAL(C_DOUBLE) :: max_gro    ! /* luf.max_gro */
       INTEGER(C_INT) :: nfs_max    ! /* fhv.hh_max */
       REAL(C_DOUBLE) :: upd_tol    ! /* fhv.upd_tol */
       INTEGER(C_INT) :: nrs_max    ! /* lpf.n_max */
       INTEGER(C_INT) :: rs_size    ! /* lpf.v_size */
       REAL(C_DOUBLE) :: foo_bar(0:37) !    /* (reserved) */
    END type glp_bfcp

! /* simplex method control parameters */

    TYPE, BIND(C) ::  glp_smcp
       INTEGER(C_INT) :: msg_lev    ! /* message level: */
       INTEGER(C_INT) :: meth       ! /* simplex method option */
       INTEGER(C_INT) :: pricing    ! /* pricing technique: */
       INTEGER(C_INT) :: r_test     ! /* ratio test technique: */
       REAL(C_DOUBLE) :: tol_bnd    ! /* spx.tol_bnd */
       REAL(C_DOUBLE) :: tol_dj     ! /* spx.tol_dj */
       REAL(C_DOUBLE) :: tol_piv    ! /* spx.tol_piv */
       REAL(C_DOUBLE) :: obj_ll     ! /* spx.obj_ll */
       REAL(C_DOUBLE) :: obj_ul     ! /* spx.obj_ul */
       INTEGER(C_INT) :: it_lim     ! /* spx.it_lim */
       INTEGER(C_INT) :: tm_lim     ! /* spx.tm_lim (milliseconds) */
       INTEGER(C_INT) :: out_frq    ! /* spx.out_frq */
       INTEGER(C_INT) :: out_dly    ! /* spx.out_dly (milliseconds) */
       INTEGER(C_INT) :: presolve   ! /* enable/disable using LP presolver */
       REAL(C_DOUBLE) :: foo_bar(0:35) !    /* (reserved) */
    END type glp_smcp

! /* interior-point solver control parameters */

    TYPE, BIND(C) ::  glp_iptcp
       INTEGER(C_INT) :: msg_lev    ! /* message level (see glp_smcp) */
       INTEGER(C_INT) :: ord_alg    ! /* ordering algorithm: */
       REAL(C_DOUBLE) :: foo_bar(0:47)  !   /* (reserved) */
    END type glp_iptcp

! /* integer optimizer control parameters */

    TYPE, BIND(C) ::  glp_iocp
       INTEGER(C_INT) :: msg_lev    ! /* message level (see glp_smcp) */
       INTEGER(C_INT) :: br_tech    ! /* branching technique */
       INTEGER(C_INT) :: bt_tech    ! /* backtracking technique */
       REAL(C_DOUBLE) :: tol_int    ! /* mip.tol_int */
       REAL(C_DOUBLE) :: tol_obj    ! /* mip.tol_obj */
       INTEGER(C_INT) :: tm_lim     ! /* mip.tm_lim (milliseconds) */
       INTEGER(C_INT) :: out_frq    ! /* mip.out_frq (milliseconds) */
       INTEGER(C_INT) :: out_dly    ! /* mip.out_dly (milliseconds) */
       !INTEGER(C_INT) :: cb_func   ! /* mip.cb_func *
       TYPE(C_FUNPTR) :: cb_func    ! /* mip.cb_func *
       INTEGER (C_INT) :: cb_info   ! /* mip.cb_info */
       INTEGER (C_INT) :: cb_size   ! /* mip.cb_size */
       INTEGER (C_INT) :: pp_tech   ! /* preprocessing technique */
       REAL (C_DOUBLE) :: mip_gap   ! /* relative MIP gap tolerance */
       INTEGER (C_INT) :: mir_cuts  ! /* MIR cuts       (GLP_ON/GLP_OFF) */
       INTEGER (C_INT) :: gmi_cuts  ! /* Gomory's cuts  (GLP_ON/GLP_OFF) */
       INTEGER (C_INT) :: cov_cuts  ! /* cover cuts     (GLP_ON/GLP_OFF) */
       INTEGER (C_INT) :: clq_cuts  ! /* clique cuts    (GLP_ON/GLP_OFF) */
       INTEGER (C_INT) :: presolve  ! /* enable/disable using MIP presolver */
       INTEGER (C_INT) :: binarize  ! /* try to binarize integer variables */
       INTEGER (C_INT) :: fp_heur   ! /* feasibility pump heuristic */
! #if 1 /* 28/V-2010 */
!      INTEGER(C_INT) :: alien      ! /* USE, INTRINSIC :: alien solver */
! #endif
       REAL (C_DOUBLE) :: foo_bar(0:28) ! /* (reserved) */
    END TYPE glp_iocp

! /* additional row attributes */

    TYPE, BIND(C) :: glp_attr
       INTEGER(C_INT) :: level   ! /* subproblem level at which the row was added */
       INTEGER(C_INT) :: origin  !  /* row origin flag */
       INTEGER(C_INT) :: klass   !  /* row class descriptor */
       REAL(C_DOUBLE) :: foo_bar(0:6) !  /* (reserved) */
    END type glp_attr

! /* MPS format control parameters */
    
    TYPE, BIND(C) :: glp_mpscp
       INTEGER(C_INT) :: blank ! /* character code to replace blanks in symbolic names */
       CHARACTER(C_CHAR) ::obj_name !  *obj_name;  /* objective row name */
       REAL(C_DOUBLE) :: tol_mps !  /* zero tolerance for MPS data */
       REAL(C_DOUBLE) :: foo_bar(0:16) !  /* (reserved for USE, INTRINSIC :: in the future) */
    END type glp_mpscp

! /* CPLEX LP format control parameters */

    TYPE, BIND(C) :: glp_cpxcp
       REAL(C_DOUBLE) :: foo_bar(0:19) ! /* (reserved for USE, INTRINSIC :: in the future) */
    END type glp_cpxcp


    INTERFACE

!----------------------------------------------------------------------------------!
!          Problem creation and modification procedures                            !
!----------------------------------------------------------------------------------!

 
! /* create problem object */

       FUNCTION glp_create_prob() BIND(C, NAME="glp_create_prob")
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
         TYPE(C_PTR) :: glp_create_prob
       END FUNCTION glp_create_prob
 
! /* assign (change) problem name */
 
       SUBROUTINE glp_set_prob_name(problem,name) BIND(C, NAME=" glp_set_prob_name")
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_CHAR
         TYPE(C_PTR), VALUE :: problem
         CHARACTER(C_CHAR)     :: name(*)
       END SUBROUTINE glp_set_prob_name
 
! /* assign (change) objective FUNCTION name */

       SUBROUTINE glp_set_obj_name(problem,name) BIND(C, NAME="glp_set_obj_name")
         USE, INTRINSIC :: ISO_C_BINDING, ONLY:  C_CHAR, C_PTR
         TYPE(C_PTR), VALUE :: problem
         CHARACTER(C_CHAR)     :: name(*)
       END SUBROUTINE glp_set_obj_name

! /* set (change) optimization direction flag */

       SUBROUTINE glp_set_obj_dir(problem,dir) BIND(C, name="glp_set_obj_dir")
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
         TYPE(C_PTR), VALUE :: problem
         INTEGER(C_INT), VALUE :: dir
       END SUBROUTINE glp_set_obj_dir
 
! /* add new rows to problem object */

         FUNCTION glp_add_rows(problem,nrs) BIND(C, NAME="glp_add_rows")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_add_rows
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: nrs
          END FUNCTION glp_add_rows
 
! /* add new columns to problem object */

          FUNCTION glp_add_cols(problem,ncs) BIND(C, NAME="glp_add_cols")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_add_cols
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: ncs
          END FUNCTION glp_add_cols

! /* assign (change) row name */

          SUBROUTINE glp_set_row_name(problem, i, name) BIND(C, NAME="glp_set_row_name")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: i
            CHARACTER(C_CHAR)   :: name(*)
          END SUBROUTINE glp_set_row_name

! /* assign (change) column name */

          SUBROUTINE glp_set_col_name(problem, j, name) BIND(C, NAME="glp_set_col_name")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: j
            CHARACTER(C_CHAR)   :: name(*)
          END SUBROUTINE glp_set_col_name
 
! /* set (change) row bounds */

          SUBROUTINE glp_set_row_bnds(problem,i,type,lb,ub) BIND(C, NAME=" glp_set_row_bnds")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: i
            INTEGER(C_INT), VALUE :: type
            REAL(C_DOUBLE), VALUE :: lb
            REAL(C_DOUBLE), VALUE :: ub
          END SUBROUTINE glp_set_row_bnds
 
! /* set (change) column bounds */

          SUBROUTINE glp_set_col_bnds(problem,j,type,lb,ub) BIND(C, NAME="glp_set_col_bnds")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: j
            INTEGER(C_INT), VALUE :: type
            REAL(C_DOUBLE), VALUE :: lb
            REAL(C_DOUBLE), VALUE :: ub
          END SUBROUTINE glp_set_col_bnds

! /* set (change) obj. coefficient or constant term */

          SUBROUTINE glp_set_obj_coef(problem,j,coef) BIND(C, NAME="glp_set_obj_coef")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: j
            REAL(C_DOUBLE), VALUE :: coef
          END SUBROUTINE glp_set_obj_coef
 
! /* set (replace) row of the constraint matrix */
          
          SUBROUTINE glp_set_mat_row(problem, i, len, ind, val) BIND(C, NAME="glp_set_mat_row")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: i
            INTEGER(C_INT), VALUE :: len
            INTEGER(C_INT) :: ind(*)
            REAL(C_DOUBLE) :: val(*)
          END SUBROUTINE glp_set_mat_row

! /* set (replace) column of the constraint matrix */

          SUBROUTINE glp_set_mat_col(problem, j, len, ind, val) BIND(C, NAME="glp_set_mat_col")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: j
            INTEGER(C_INT), VALUE :: len
            INTEGER(C_INT) :: ind(*)
            REAL(C_DOUBLE) :: val(*)
          END SUBROUTINE glp_set_mat_col

! /* load (replace) the whole constraint matrix */

          SUBROUTINE glp_load_matrix(problem,ne,ia,ja,ar) BIND(C, NAME="glp_load_matrix")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: ne
            INTEGER(C_INT) :: ia(*)
            INTEGER(C_INT) :: ja(*)
            REAL(C_DOUBLE) :: ar(*)
          END SUBROUTINE glp_load_matrix

! /* check for duplicate elements in sparse matrix */
 
          INTEGER(C_INT) FUNCTION glp_check_dup(m,n,ne,ia,ja) BIND(C, NAME="glp_check_dup")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
            INTEGER(C_INT), VALUE :: m
            INTEGER(C_INT), VALUE :: n
            INTEGER(C_INT), VALUE :: ne
            INTEGER(C_INT) :: ia(*)
            INTEGER(C_INT) :: ja(*)
          END FUNCTION glp_check_dup

! /* sort elements of the constraint matrix */

          SUBROUTINE glp_sort_matrix(problem) BIND(C, NAME="glp_sort_matrix")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE :: problem
          END SUBROUTINE glp_sort_matrix

! /* delete specified rows from problem object */

          SUBROUTINE glp_del_rows(problem, nrs, num) BIND(C, NAME="glp_del_rows")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: problem
            INTEGER(C_INT), VALUE :: nrs
            INTEGER(C_INT) :: num(*)
          END SUBROUTINE glp_del_rows

! /* delete specified columns from problem object */

          SUBROUTINE glp_del_cols(problem, ncs, num) BIND(C, NAME="glp_del_cols")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: problem 
            INTEGER(C_INT), VALUE :: ncs
            INTEGER(C_INT) :: num(*)
          END SUBROUTINE glp_del_cols

! /* copy problem object content */

          SUBROUTINE glp_copy_prob(dest, prob, names) BIND(C, NAME="glp_copy_prob")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT), VALUE :: dest
            TYPE(C_PTR), VALUE:: prob
            INTEGER(C_INT), VALUE :: names
          END SUBROUTINE glp_copy_prob


! /* erase problem object content */

          SUBROUTINE glp_erase_prob(prob) BIND(C, NAME="glp_erase_prob")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE :: prob
          END SUBROUTINE glp_erase_prob

! /* delete problem object */
 
        SUBROUTINE glp_delete_prob(prob) BIND(C, NAME="glp_delete_prob")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
          TYPE(C_PTR), VALUE :: prob
        END SUBROUTINE glp_delete_prob

!----------------------------------------------------------------------------------!
!              Problem retrieving procedures                                       !
!----------------------------------------------------------------------------------!

! /* retrieve problem name */

        FUNCTION glp_get_prob_name(problem) BIND(C, NAME="glp_get_prob_name")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
          TYPE(C_PTR) :: glp_get_prob_name
          TYPE(C_PTR), VALUE :: problem
        END FUNCTION glp_get_prob_name
 

! /* retrieve objective function name */

        FUNCTION glp_get_obj_name(problem) BIND(C, NAME="glp_get_obj_name")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
          TYPE(C_PTR) :: glp_get_obj_name
          TYPE(C_PTR), VALUE :: problem
        END FUNCTION glp_get_obj_name


! /* retrieve optimization direction flag */

          FUNCTION glp_get_obj_dir(problem) BIND(C, NAME="glp_get_obj_dir")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_get_obj_dir
            TYPE(C_PTR), VALUE ::  problem
          END FUNCTION glp_get_obj_dir

! /* retrieve number of rows */

          FUNCTION glp_get_num_rows(problem) BIND(C, NAME="glp_get_num_rows")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) ::  glp_get_num_rows
            TYPE(C_PTR), VALUE ::  problem
          END FUNCTION glp_get_num_rows

! /* retrieve number of columns */

          FUNCTION glp_get_num_cols(problem) BIND(C, NAME="glp_get_num_cols")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT)  ::  glp_get_num_cols
            TYPE(C_PTR), VALUE ::  problem
          END FUNCTION glp_get_num_cols

! /* retrieve row name */

        FUNCTION glp_get_row_name(problem, i) BIND(C, NAME="glp_get_row_name")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
          TYPE(C_PTR) :: glp_get_row_name
          INTEGER(C_INT), VALUE :: i
          TYPE(C_PTR), VALUE ::  problem
        END FUNCTION glp_get_row_name

! /* retrieve column name */

        FUNCTION glp_get_col_name(problem, j) BIND(C, NAME="glp_get_col_name")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
          TYPE(C_PTR) :: glp_get_col_name
          INTEGER(C_INT), VALUE :: j
          TYPE(C_PTR), VALUE :: problem
        END FUNCTION glp_get_col_name

! /* retrieve row type */

           FUNCTION glp_get_row_type(problem, i) BIND(C, NAME="glp_get_row_type")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_get_row_type
            TYPE(C_PTR), VALUE ::  problem
            INTEGER(C_INT), VALUE ::  i
          END FUNCTION glp_get_row_type

! /* retrieve row lower bound */

           FUNCTION glp_get_row_lb(problem, i) BIND(C, NAME="glp_get_row_lb")
             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
             REAL(C_DOUBLE) :: glp_get_row_lb
             TYPE(C_PTR), VALUE ::  problem
             INTEGER(C_INT), VALUE ::  i
          END FUNCTION glp_get_row_lb

! /* retrieve row upper bound */

          FUNCTION glp_get_row_ub(problem, i) BIND(C, NAME="glp_get_row_ub")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE)  :: glp_get_row_ub
            TYPE(C_PTR), VALUE ::  problem
            INTEGER(C_INT), VALUE ::  i
          END FUNCTION glp_get_row_ub


! /* retrieve column type */

          FUNCTION glp_get_col_type(problem, j) BIND(C, NAME="glp_get_col_type")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT)  :: glp_get_col_type
            TYPE(C_PTR), VALUE ::  problem
            INTEGER(C_INT), VALUE ::  j
          END FUNCTION glp_get_col_type

! /* retrieve column lower bound */

         FUNCTION glp_get_col_lb(problem, j) BIND(C, NAME="glp_get_col_lb")
           USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
           REAL(C_DOUBLE) :: glp_get_col_lb
           TYPE(C_PTR), VALUE ::  problem
           INTEGER(C_INT), VALUE ::  j
          END FUNCTION glp_get_col_lb

! /* retrieve column upper bound */

          FUNCTION glp_get_col_ub(problem, j) BIND(C, NAME="glp_get_col_ub")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_col_ub
            TYPE(C_PTR), VALUE ::  problem
            INTEGER(C_INT), VALUE ::  j
          END FUNCTION glp_get_col_ub

! /* retrieve obj. coefficient or constant term */

          FUNCTION glp_get_obj_coef(problem, j) BIND(C, NAME="glp_get_obj_coef")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_obj_coef
            TYPE(C_PTR), VALUE ::  problem
            INTEGER(C_INT), VALUE ::  j
          END FUNCTION glp_get_obj_coef

! /* retrieve number of constraint coefficients */

          FUNCTION glp_get_num_nz(problem) BIND(C, NAME="glp_get_num_nz")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) ::  glp_get_num_nz
            TYPE(C_PTR), VALUE ::  problem
          END FUNCTION glp_get_num_nz

! /* retrieve row of the constraint matrix */

          FUNCTION glp_get_mat_row(problem, i, ind, val ) BIND(C, NAME="glp_get_mat_row")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            INTEGER(C_INT) :: glp_get_mat_row
            TYPE(C_PTR), VALUE ::  problem
            INTEGER(C_INT), VALUE :: i
            INTEGER(C_INT) :: ind(*)
            REAL(C_DOUBLE) :: val(*)
          END FUNCTION glp_get_mat_row

! /* retrieve column of the constraint matrix */

          FUNCTION glp_get_mat_col(problem, j, ind, val ) BIND(C, NAME="glp_get_mat_col")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            INTEGER(C_INT) :: glp_get_mat_col
            TYPE(C_PTR), VALUE ::  problem
            INTEGER(C_INT), VALUE :: j
            INTEGER(C_INT) :: ind(*)
            REAL(C_DOUBLE) :: val(*)
          END FUNCTION glp_get_mat_col

!----------------------------------------------------------------------------------!
!              Row and column searching procedures                                 !
!----------------------------------------------------------------------------------!

! /* create the name index */

          SUBROUTINE glp_create_index(probl) BIND(C, NAME="glp_create_index")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE :: probl
          END SUBROUTINE glp_create_index

! /* find row by its name */

          FUNCTION glp_find_row(probl, name) BIND(C, NAME="glp_find_row")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            INTEGER(C_INT) :: glp_find_row
            TYPE(C_PTR), VALUE :: probl
            CHARACTER(C_CHAR) :: name(*)
          END FUNCTION glp_find_row

! /* find column by its name */

          FUNCTION glp_find_col(probl, name) BIND(C, NAME="glp_find_col")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            INTEGER(C_INT) :: glp_find_col
            TYPE(C_PTR), VALUE :: probl
            CHARACTER(C_CHAR) :: name(*)
          END FUNCTION glp_find_col

! /* delete the name index */

         SUBROUTINE glp_delete_index(probl) BIND(C, NAME="glp_delete_index")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE :: probl
          END SUBROUTINE glp_delete_index

!----------------------------------------------------------------------------------!
!                        Problem scaling procedures                                !
!----------------------------------------------------------------------------------!


! /* set (change) row scale factor */

          SUBROUTINE glp_set_rii(prob, i, rii) BIND(C, NAME="glp_set_rii")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            INTEGER(C_INT), VALUE :: i
            REAL(C_DOUBLE), VALUE :: rii
            TYPE(C_PTR), VALUE :: prob
          END SUBROUTINE glp_set_rii

! /* set (change) column scale factor */

          SUBROUTINE glp_set_sjj(prob, j, sjj)  BIND(C, NAME="glp_set_sjj")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            INTEGER(C_INT), VALUE :: j
            REAL(C_DOUBLE), VALUE :: sjj
            TYPE(C_PTR), VALUE :: prob
          END SUBROUTINE glp_set_sjj

! /* retrieve row scale factor */

           FUNCTION glp_get_rii(prob, i) BIND(C, NAME="glp_get_rii")
             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
             REAL(C_DOUBLE) ::  glp_get_rii
             INTEGER(C_INT), VALUE :: i         
             TYPE(C_PTR), VALUE :: prob 
           END FUNCTION glp_get_rii

! /* retrieve column scale factor */

           FUNCTION glp_get_sjj(prob, j) BIND(C, NAME="glp_get_sjj")
             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
             REAL(C_DOUBLE) :: glp_get_sjj
             INTEGER(C_INT), VALUE :: j     
             TYPE(C_PTR), VALUE :: prob     
           END FUNCTION glp_get_sjj

! /* scale problem data */

           SUBROUTINE glp_scale_prob(prob, flags)
             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
             INTEGER(C_INT), VALUE :: flags
             TYPE(C_PTR), VALUE :: prob
           END SUBROUTINE glp_scale_prob


! /* unscale problem data */

           SUBROUTINE glp_unscale_prob(prob) BIND(C, NAME="glp_unscale_prob")
             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
             TYPE(C_PTR), VALUE :: prob
           END SUBROUTINE glp_unscale_prob

!----------------------------------------------------------------------------------!
!         Linear Programming basis constructing procedures                         !
!----------------------------------------------------------------------------------!

! /* set (change) row status */

          SUBROUTINE glp_set_row_stat(prob, i, stat) BIND(C, NAME="glp_set_row_stat")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: i
            INTEGER(C_INT), VALUE :: stat
          END SUBROUTINE glp_set_row_stat

! /* set (change) column status */

          SUBROUTINE glp_set_col_stat(prob, i, stat) BIND(C, NAME="glp_set_col_stat")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: i
            INTEGER(C_INT), VALUE :: stat
          END SUBROUTINE glp_set_col_stat

! /* construct standard initial LP basis */

          SUBROUTINE glp_std_basis(problem) BIND(C, NAME="glp_std_basis")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE :: problem
          END SUBROUTINE glp_std_basis


! /* construct advanced initial LP basis */

            SUBROUTINE glp_adv_basis(problem,flags) BIND(C,NAME="glp_adv_basis")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
                TYPE(C_PTR), VALUE :: problem
                INTEGER(C_INT), VALUE :: flags
            end subroutine

! /* construct Bixby's initial LP basis */

            SUBROUTINE glp_cpx_basis(problem) BIND(C,NAME="glp_cpx_basis")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
              TYPE(C_PTR), VALUE :: problem
            END SUBROUTINE glp_cpx_basis

!----------------------------------------------------------------------------------!
!          Simplex method procedures                                               !
!----------------------------------------------------------------------------------!

 
! /* solve LP problem with the simplex method */

         FUNCTION glp_simplex(prob,parm) BIND(C, NAME="glp_simplex")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            IMPORT :: glp_smcp
            INTEGER(C_INT) :: glp_simplex
            TYPE(C_PTR), VALUE :: prob
            TYPE(glp_smcp) :: parm
          END FUNCTION glp_simplex

! /* solve LP problem in exact arithmetic */

          INTEGER(C_INT) FUNCTION  glp_exact(prob, parm) BIND(C, NAME="glp_exact")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            IMPORT :: glp_smcp
            TYPE(C_PTR), VALUE :: prob
            TYPE(glp_smcp) :: parm
          END FUNCTION glp_exact

! /* initialize simplex method control parameters */
 
       SUBROUTINE glp_init_smcp(parm) BIND(C, NAME="glp_init_smcp")
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
         IMPORT :: glp_smcp
         TYPE(glp_smcp) :: parm
       END SUBROUTINE glp_init_smcp
 
! /* retrieve generic status of basic solution */

          FUNCTION glp_get_status(prob) BIND(C, NAME="glp_get_status")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_get_status
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_get_status

! /* retrieve status of primal basic solution */

          FUNCTION glp_get_prim_stat(prob) BIND(C, NAME="glp_get_prim_stat")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_get_prim_stat
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_get_prim_stat

! /* retrieve status of dual basic solution */

          FUNCTION glp_get_dual_stat(prob) BIND(C, NAME="glp_get_dual_stat")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_get_dual_stat
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_get_dual_stat

! /* retrieve objective value (basic solution) */
 
          FUNCTION glp_get_obj_val(prob) BIND(C, NAME="glp_get_obj_val")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY:  C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_obj_val
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_get_obj_val

! /* retrieve row status */

          FUNCTION glp_get_row_stat(prob, i) BIND(C, NAME="glp_get_row_stat")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_get_row_stat
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: i
          END FUNCTION glp_get_row_stat


! /* retrieve row primal value (basic solution) */

          FUNCTION glp_get_row_prim(prob, i) BIND(C, NAME="glp_get_row_prim")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_row_prim
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: i
          END FUNCTION glp_get_row_prim

! /* retrieve row dual value (basic solution) */

          FUNCTION glp_get_row_dual(prob, i) BIND(C, NAME="glp_get_row_dual")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_row_dual
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: i
          END FUNCTION glp_get_row_dual

! /* retrieve column status */

          FUNCTION glp_get_col_stat(prob, j) BIND(C, NAME="glp_get_col_stat")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_get_col_stat
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: j
          END FUNCTION glp_get_col_stat

! /* retrieve column primal value (basic solution) */
 
          FUNCTION glp_get_col_prim(prob,j) BIND(C, NAME="glp_get_col_prim")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_col_prim
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: j
          END FUNCTION glp_get_col_prim

! /* retrieve column dual value (basic solution) */

          FUNCTION glp_get_col_dual(prob, i) BIND(C, NAME="glp_get_col_dual")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_col_dual
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: i
          END FUNCTION glp_get_col_dual

! /* determine variable causing unboundedness */

          FUNCTION glp_get_unbnd_ray(prob) BIND(C, NAME="glp_get_unbnd_ray")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_get_unbnd_ray
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_get_unbnd_ray

!----------------------------------------------------------------------------------!
!                   Interior-Point Method procedures                               !
!----------------------------------------------------------------------------------!

! /* solve LP problem with the interior-point method */

            FUNCTION glp_interior(problem,parm) BIND(C,NAME="glp_interior")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
              IMPORT :: glp_iptcp
              INTEGER(C_INT) :: glp_interior
              TYPE(C_PTR), VALUE :: problem
              TYPE(glp_iptcp) :: parm
            END FUNCTION

! /* initialize interior-point solver control parameters */

            SUBROUTINE glp_init_iptcp(parm) BIND(C,NAME="glp_init_iptcp")
              IMPORT :: glp_iptcp
              TYPE(glp_iptcp) :: parm
            END SUBROUTINE glp_init_iptcp

! /* retrieve status of interior-point solution */

            FUNCTION glp_ipt_status(problem) BIND(C,NAME="glp_ipt_status")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
              INTEGER(C_INT) :: glp_ipt_status
              TYPE(C_PTR), VALUE :: problem
            END FUNCTION

! /* retrieve row primal value (interior point) */

            FUNCTION glp_ipt_row_prim(problem, i) BIND(C,NAME="glp_ipt_row_prim")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE, C_INT, C_PTR
              REAL(C_DOUBLE) :: glp_ipt_row_prim
              TYPE(C_PTR), value :: problem
              INTEGER(C_INT), value :: i
            END FUNCTION glp_ipt_row_prim


! /* retrieve row dual value (interior point) */

            FUNCTION glp_ipt_row_dual(problem, i) BIND(C,NAME="glp_ipt_row_dual")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE, C_INT, C_PTR
              REAL(C_DOUBLE) :: glp_ipt_row_dual
              TYPE(C_PTR), value :: problem
              INTEGER(C_INT), value :: i
            END FUNCTION glp_ipt_row_dual

! /* retrieve column primal value (interior point) */

            FUNCTION glp_ipt_obj_val(problem) BIND(C,NAME="glp_ipt_obj_val")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE, C_PTR
              REAL(C_DOUBLE) :: glp_ipt_obj_val
              TYPE(C_PTR), value :: problem
            END FUNCTION glp_ipt_obj_val

! /* retrieve column dual value (interior point) */

            FUNCTION glp_ipt_col_dual(problem, j) BIND(C,NAME="glp_ipt_col_dual")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE, C_INT, C_PTR
              REAL(C_DOUBLE) :: glp_ipt_col_dual
              TYPE(C_PTR), value :: problem
              INTEGER(C_INT), value :: j
            END FUNCTION glp_ipt_col_dual

!----------------------------------------------------------------------------------!
!              Mixed Integer Programming procedures                                !
!----------------------------------------------------------------------------------!

! /* set (change) column kind */

         SUBROUTINE glp_set_col_kind(prob,j,kind) BIND(C, NAME="glp_set_col_kind")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: j
            INTEGER(C_INT), VALUE :: kind
          END SUBROUTINE glp_set_col_kind

! /* retrieve column kind */

           FUNCTION glp_get_col_kind(problem, j) BIND(C,NAME="glp_get_col_kind")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
              INTEGER(C_INT) :: glp_get_col_kind
              TYPE(C_PTR), VALUE :: problem
              INTEGER(C_INT), VALUE :: j
            END FUNCTION glp_get_col_kind

! /* retrieve number of integer columns */

            FUNCTION glp_get_num_int(problem) BIND(C,NAME="glp_get_num_int")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
              INTEGER(C_INT) :: glp_get_num_int
              TYPE(C_PTR), VALUE :: problem
            END FUNCTION glp_get_num_int

! /* retrieve number of binary columns */

            FUNCTION glp_get_num_bin(problem) BIND(C,NAME="glp_get_num_bin")
              USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
              INTEGER(C_INT) :: glp_get_num_bin
              TYPE(C_PTR), VALUE :: problem
            END FUNCTION glp_get_num_bin


! /* solve MIP problem with the branch-and-bound method */
 
          FUNCTION glp_intopt(prob,parm) BIND(C, NAME="glp_intopt")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            IMPORT :: glp_iocp
            INTEGER(C_INT) :: glp_intopt
            TYPE(C_PTR), VALUE :: prob
            TYPE(glp_iocp) :: parm
          END FUNCTION glp_intopt

! /* initialize integer optimizer control parameters */
 
        INTEGER(C_INT) FUNCTION glp_init_iocp(parm) BIND(C, NAME="glp_init_iocp")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
            IMPORT :: glp_iocp
            TYPE(glp_iocp) :: parm
          END FUNCTION glp_init_iocp

! /* retrieve status of MIP solution */

          INTEGER(C_INT) FUNCTION glp_mip_status(prob) BIND(C, NAME="glp_mip_status")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_mip_status

 
! /* retrieve objective value (MIP solution) */

          FUNCTION glp_mip_obj_val(prob) BIND(C, NAME="glp_mip_obj_val")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_DOUBLE
            REAL(C_DOUBLE) :: glp_mip_obj_val
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_mip_obj_val

! /* retrieve row value (MIP solution) */

          FUNCTION glp_mip_row_val(prob,i) BIND(C, NAME="glp_mip_row_val")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_mip_row_val
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: i
        END FUNCTION glp_mip_row_val

! /* retrieve column value (MIP solution) */

          FUNCTION glp_mip_col_val(prob,j) BIND(C, NAME="glp_mip_col_val")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_mip_col_val
            TYPE(C_PTR), VALUE :: prob
            INTEGER(C_INT), VALUE :: j
        END FUNCTION glp_mip_col_val

!----------------------------------------------------------------------------------!
!               Problem Solution read and write procedures                         !
!----------------------------------------------------------------------------------!

! /* write basic solution in printable format */

        FUNCTION glp_print_sol(prob, fname) BIND(C, NAME="glp_print_sol")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_print_sol
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_print_sol

! /* read basic solution from text file */

        FUNCTION glp_read_sol(prob, fname) BIND(C, NAME="glp_read_sol")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_read_sol
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_read_sol

! /* write basic solution to text file */

        FUNCTION glp_write_sol(prob, fname) BIND(C, NAME="glp_write_sol")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_write_sol
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_write_sol

! /* write interior-point solution in printable format */

        FUNCTION glp_print_ipt(prob, fname) BIND(C, NAME="glp_print_ipt")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_print_ipt
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_print_ipt

! /* read interior-point solution from text file */

        FUNCTION glp_read_ipt(prob, fname) BIND(C, NAME="glp_read_ipt")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_read_ipt
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_read_ipt

! /* write interior-point solution to text file */

        FUNCTION glp_write_ipt(prob, fname) BIND(C, NAME="glp_write_ipt")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_write_ipt
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_write_ipt

! /* write MIP solution in printable format */

        FUNCTION glp_print_mip(prob, fname) BIND(C, NAME="glp_print_mip")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_print_mip
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_print_mip

! /* read MIP solution from text file */

        FUNCTION glp_read_mip(prob, fname) BIND(C, NAME="glp_read_mip")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_read_mip
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_read_mip

! /* write MIP solution to text file */

        FUNCTION glp_write_mip(prob, fname) BIND(C, NAME="glp_write_mip")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_write_mip
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
        END FUNCTION glp_write_mip

!----------------------------------------------------------------------------------!
!             Post-optimal analysis procedures                                     !
!----------------------------------------------------------------------------------!

! /* print sensitivity analysis report */

        FUNCTION glp_print_ranges(prob, len, list, flags, fname) BIND(C, NAME="glp_print_ranges")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR, C_CHAR
          INTEGER(C_INT) :: glp_print_ranges
          INTEGER(C_INT), VALUE ::  len, flags
          TYPE(C_PTR), VALUE :: prob
          CHARACTER(C_CHAR) :: fname(*)
          INTEGER(C_INT) :: list(*)
        END FUNCTION glp_print_ranges

!----------------------------------------------------------------------------------!
!          Linear Programming basis manipulation procedures                        !
!----------------------------------------------------------------------------------!

! /* check if the basis factorization exists */

        INTEGER(C_INT) FUNCTION glp_bf_exists(prob) BIND(C, NAME="glp_bf_exist")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_bf_exists

! /* compute the basis factorization */

        INTEGER(C_INT) FUNCTION glp_factorize(prob) BIND(C, NAME="glp_factorize")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_factorize

! /* check if the basis factorization has been updated */

        INTEGER(C_INT) FUNCTION glp_bf_updated(prob) BIND(C, NAME="lp_bf_updated")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_bf_updated

! /* retrieve basis factorization control parameters */

        SUBROUTINE  glp_get_bfcp(prob, parm) BIND(C, NAME="glp_get_bfcp")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
          IMPORT :: glp_bfcp
          TYPE(C_PTR), VALUE :: prob
          TYPE(glp_bfcp) :: parm
        END SUBROUTINE glp_get_bfcp

! /* change basis factorization control parameters */

        SUBROUTINE glp_set_bfcp(prob, parm) BIND(C, NAME="glp_set_bfcp")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
          IMPORT :: glp_bfcp
          TYPE(C_PTR), VALUE :: prob
          TYPE(glp_bfcp) :: parm
        END SUBROUTINE glp_set_bfcp

! /* retrieve the basis header information */

        INTEGER(C_INT) FUNCTION glp_get_bhead(prob, k) BIND(C, NAME="glp_get_bhead")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          INTEGER(C_INT), VALUE :: k
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_get_bhead

! /* retrieve row index in the basis header */

        INTEGER(C_INT) FUNCTION glp_get_row_bind(prob, i) BIND(C, NAME="glp_get_row_bind")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          INTEGER(C_INT), VALUE :: i
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_get_row_bind

! /* retrieve column index in the basis header */

        INTEGER(C_INT) FUNCTION glp_get_col_bind(prob, j) BIND(C, NAME="glp_get_col_bind")
           USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
           TYPE(C_PTR), VALUE :: prob
           INTEGER(C_INT), VALUE ::  j
        END FUNCTION glp_get_col_bind
        

! /* perform forward transformation (solve system B*x = b) */

        SUBROUTINE glp_ftran(prob, x) BIND(C, NAME="glp_ftran")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_DOUBLE
          TYPE(C_PTR), VALUE :: prob
          REAL(C_DOUBLE) :: x
        END SUBROUTINE glp_ftran

! /* perform backward transformation (solve system B'*x = b) */

        SUBROUTINE glp_btran(prob, x) BIND(C, NAME="glp_btran")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_DOUBLE
          TYPE(C_PTR), VALUE :: prob
          REAL(C_DOUBLE) :: x
        END SUBROUTINE glp_btran

!----------------------------------------------------------------------------------!
!                Simplex Tableau manipulation procedures                           !
!----------------------------------------------------------------------------------!

! /* "warm up" LP basis */

        INTEGER(C_INT) FUNCTION glp_warm_up(prob) BIND(C, NAME="glp_warm_up")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_warm_up

! /* compute row of the simplex tableau */

        INTEGER(C_INT) FUNCTION glp_eval_tab_row(prob, k, ind, val) &
             BIND(C, NAME="glp_eval_tab_row")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE :: k
          INTEGER(C_INT) :: ind(*)
          REAL(C_DOUBLE) :: val(*)
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_eval_tab_row

! /* compute column of the simplex tableau */

        INTEGER(C_INT) FUNCTION glp_eval_tab_col(prob, k, ind, val) &
             BIND(C, NAME="glp_eval_tab_col")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE ::  k
          INTEGER(C_INT) :: ind(*)
          REAL(C_DOUBLE) :: val(*)
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_eval_tab_col

! /* transform explicitly specified row */

        INTEGER(C_INT) FUNCTION glp_transform_row(prob, len, ind, val) &
             BIND(C, NAME="glp_transform_row")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE :: len
          INTEGER(C_INT) :: ind(*)
          REAL(C_DOUBLE) :: val(*)
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_transform_row

! /* transform explicitly specified column */

        INTEGER(C_INT) FUNCTION glp_transform_col(prob, len, ind, val) &
             BIND(C, NAME="glp_transform_col")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE :: len
          INTEGER(C_INT) :: ind(*)
          REAL(C_DOUBLE) :: val(*)
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_transform_col

! /* perform primal ratio test */

        INTEGER(C_INT) FUNCTION glp_prim_rtest(prob, len, ind, val, dir, eps) &
             BIND(C, NAME="glp_prim_rtest")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE :: len, dir
          REAL(C_DOUBLE), VALUE :: eps
          INTEGER(C_INT) :: ind(*)
          REAL(C_DOUBLE) :: val(*)
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_prim_rtest

! /* perform dual ratio test */

        INTEGER(C_INT) FUNCTION glp_dual_rtest(prob, len, ind, val, dir, eps) &
             BIND(C, NAME="glp_dual_rtest")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE :: len, dir
          REAL(C_DOUBLE), VALUE :: eps
          INTEGER(C_INT) :: ind(*)
          REAL(C_DOUBLE) :: val(*)
          TYPE(C_PTR), VALUE :: prob
        END FUNCTION glp_dual_rtest

!----------------------------------------------------------------------------------!
!          More post-optimal analysis procedures                                   !  
!----------------------------------------------------------------------------------!

! /* analyze active bound of non-basic variable */

        SUBROUTINE glp_analyze_bound(prob, k, value1, var1, value2, var2) &
             BIND(C, NAME="glp_analyze_bound")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE :: k
          INTEGER(C_INT) :: var1, var2
          REAL(C_DOUBLE) :: value1, value2
          TYPE(C_PTR), VALUE :: prob
        END SUBROUTINE glp_analyze_bound

! /* analyze objective coefficient at basic variable */

        SUBROUTINE glp_analyze_coef(prob, k, coef1, var1, value1, coef2, var2, value2) &
             BIND(C, NAME="glp_analyze_coef")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
          INTEGER(C_INT), VALUE :: k
          INTEGER(C_INT) :: var1, var2
          REAL(C_DOUBLE) :: value1, value2, coef1, coef2
          TYPE(C_PTR), VALUE :: prob
        END SUBROUTINE glp_analyze_coef

!----------------------------------------------------------------------------------!
!           Branch and Cut procedures                                              !
!----------------------------------------------------------------------------------!

! /* determine reason for calling the callback routine */

        FUNCTION glp_ios_reason(tree) BIND(C, NAME="glp_ios_reason")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          INTEGER(C_INT) ::  glp_ios_reason
          TYPE(C_PTR), VALUE :: tree
        END FUNCTION glp_ios_reason

! /* access the problem object */

        FUNCTION glp_ios_get_prob(tree) BIND(C, NAME="glp_ios_get_prob")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
          TYPE(C_PTR) :: glp_ios_get_prob
          TYPE(C_PTR), VALUE :: tree
        END FUNCTION glp_ios_get_prob

! /* determine size of the branch-and-bound tree */

        SUBROUTINE glp_ios_tree_size(tree, a_cnt, n_cnt, t_cnt) BIND(C, NAME="glp_ios_tree_size")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: tree
          INTEGER(C_INT) :: a_cnt, n_cnt, t_cnt
        END SUBROUTINE glp_ios_tree_size

! /* determine current active subproblem */

        INTEGER(C_INT) FUNCTION glp_ios_curr_node(tree) BIND(C, NAME="glp_ios_curr_node")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: tree
        END FUNCTION glp_ios_curr_node

! /* determine next active subproblem */

        INTEGER(C_INT) FUNCTION glp_ios_next_node(tree, p) BIND(C, NAME="glp_ios_next_node")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: tree
          INTEGER(C_INT), VALUE :: p
        END FUNCTION glp_ios_next_node

! /* determine previous active subproblem */

        INTEGER(C_INT) FUNCTION glp_ios_prev_node(tree, p) BIND(C, NAME="glp_ios_prev_node")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: tree
          INTEGER(C_INT), VALUE :: p
        END FUNCTION glp_ios_prev_node

! /* determine parent subproblem */

        INTEGER(C_INT) FUNCTION glp_ios_up_node(tree, p) BIND(C, NAME="glp_ios_up_node")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: tree
          INTEGER(C_INT), VALUE :: p
        END FUNCTION glp_ios_up_node

! /* determine subproblem level */

        INTEGER(C_INT) FUNCTION glp_ios_node_level(tree, p) BIND(C, NAME="glp_ios_node_level")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
          TYPE(C_PTR), VALUE :: tree
          INTEGER(C_INT), VALUE :: p
        END FUNCTION glp_ios_node_level

! /* determine subproblem local bound */

          FUNCTION glp_ios_node_bound(tree,p) BIND(C, NAME="glp_ios_node_bound")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_ios_node_bound
            TYPE(C_PTR), VALUE :: tree
            INTEGER(C_INT), VALUE :: p
          END FUNCTION glp_ios_node_bound

! /* find active subproblem with best local bound */

        INTEGER(C_INT) FUNCTION glp_ios_best_node(tree) BIND(C, NAME="glp_ios_best_node")
          USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: tree
        END FUNCTION glp_ios_best_node

! /* compute relative MIP gap */

          FUNCTION glp_ios_mip_gap(tree) BIND(C, NAME="glp_ios_mip_gap")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
            REAL(C_DOUBLE) :: glp_ios_mip_gap
            TYPE(C_PTR), VALUE :: tree
          END FUNCTION glp_ios_mip_gap

! /* access subproblem application-specific data */

          FUNCTION glp_ios_node_data(tree, p) BIND(C, NAME="glp_ios_node_data")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR) :: glp_ios_node_data
            TYPE(C_PTR), VALUE :: tree
            INTEGER(C_INT), VALUE :: p
          END FUNCTION glp_ios_node_data

! /* retrieve additional row attributes */

          SUBROUTINE glp_ios_row_attr(tree, i, attr) BIND(C, NAME="glp_ios_row_attr")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            IMPORT:: glp_attr
            INTEGER(C_INT), VALUE :: i
            TYPE(C_PTR), VALUE :: tree
            TYPE(glp_attr) :: attr
          END SUBROUTINE glp_ios_row_attr

! /* check if can branch upon specified variable */

          SUBROUTINE glp_ios_can_branch(tree, j) BIND(C, NAME="glp_ios_can_branch")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: tree
            INTEGER(C_INT), VALUE :: j
          END SUBROUTINE glp_ios_can_branch

! /* choose variable to branch upon */

          SUBROUTINE glp_ios_branch_upon(tree, j, sel) BIND(C, NAME="glp_ios_branch_upon")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: tree
            INTEGER(C_INT), VALUE :: j, sel
          END SUBROUTINE glp_ios_branch_upon

! /* select subproblem to continue the search */

          SUBROUTINE glp_ios_select_node(tree, p) BIND(C, NAME="glp_ios_select_node")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: tree
            INTEGER(C_INT), VALUE :: p
          END SUBROUTINE glp_ios_select_node

! /* provide solution found by heuristic */

          SUBROUTINE glp_ios_heur_sol(tree, x) BIND(C, NAME="glp_ios_heur_sol")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_DOUBLE
            TYPE(C_PTR), VALUE :: tree
            REAL(C_DOUBLE) :: x(*)
          END SUBROUTINE glp_ios_heur_sol

! /* terminate the solution process */

          SUBROUTINE glp_ios_terminate(tree) BIND(C, NAME="glp_ios_terminate")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE :: tree
          END SUBROUTINE glp_ios_terminate

!----------------------------------------------------------------------------------!
!                   The Cut Pool procedures                                        !
!----------------------------------------------------------------------------------!

! /* determine current size of the cut pool */

          FUNCTION glp_ios_pool_size(tree) BIND(C, NAME="glp_ios_pool_size")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT) :: glp_ios_pool_size
            TYPE(C_PTR), VALUE :: tree
          END FUNCTION glp_ios_pool_size

! /* add row (constraint) to the cut pool */

          FUNCTION glp_ios_add_row(tree, name, klass, flags, len, ind, val, type, rhs) &
               BIND(C, NAME="glp_ios_add_row")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_DOUBLE, C_PTR
            INTEGER(C_INT) :: glp_ios_add_row, ind
            INTEGER(C_INT), VALUE :: klass, flags, len, type
            TYPE(C_PTR), VALUE :: tree
            REAL(C_DOUBLE) :: val(*), rhs
            CHARACTER(C_CHAR) :: name(*)
          END FUNCTION glp_ios_add_row

! /* remove row (constraint) from the cut pool */

          SUBROUTINE glp_ios_del_row(tree, i) BIND(C, NAME="glp_ios_del_row")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: tree
            INTEGER(C_INT), VALUE :: i
          END SUBROUTINE glp_ios_del_row

! /* remove all rows (constraints) from the cut pool */

          SUBROUTINE glp_ios_clear_pool(tree)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE :: tree
          END SUBROUTINE glp_ios_clear_pool

!----------------------------------------------------------------------------------!
!        Problem data reading and writing procedures                               !
!----------------------------------------------------------------------------------!

! /* initialize MPS format control parameters */

          SUBROUTINE glp_init_mpscp(parm) BIND(C, NAME="glp_init_mpscp")
            IMPORT :: glp_mpscp 
            TYPE(glp_mpscp) :: parm
          END SUBROUTINE glp_init_mpscp
 
! /* read problem data in MPS format */

          INTEGER(C_INT) FUNCTION glp_read_mps(prob, fmt, parm, fname) &
               BIND(C, NAME="glp_read_mps")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            IMPORT :: glp_mpscp
            INTEGER(C_INT), VALUE :: fmt
            CHARACTER(C_CHAR) :: fname(*)
            TYPE(glp_mpscp) :: parm
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_read_mps

! /* write problem data in MPS format */

          INTEGER(C_INT) FUNCTION glp_write_mps(prob, fmt, parm, fname) &
               BIND(C, NAME="glp_write_mps")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            IMPORT :: glp_mpscp
            INTEGER(C_INT), VALUE :: fmt
            CHARACTER(C_CHAR) :: fname(*)
            TYPE(glp_mpscp) :: parm
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_write_mps

! /* initialize CPLEX LP format control parameters */

          SUBROUTINE glp_init_cpxcp(parm) BIND(C, NAME="glp_init_cpxcp")
            IMPORT :: glp_cpxcp
            TYPE(glp_cpxcp) :: parm
          END SUBROUTINE glp_init_cpxcp

! /* write problem data in CPLEX LP format */

          FUNCTION glp_write_lp(prob, parm, fname) BIND(C, NAME="glp_write_lp")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            IMPORT :: glp_cpxcp
            INTEGER(C_INT) :: glp_write_lp
            CHARACTER(C_CHAR) :: fname(*)
            TYPE(glp_cpxcp) :: parm
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_write_lp

! /* read problem data in GLPK format */

          INTEGER(C_INT) FUNCTION glp_read_prob(prob, flags, fname) &
               BIND(C, NAME="glp_read_prob")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            INTEGER(C_INT), VALUE :: flags
            CHARACTER(C_CHAR) :: fname(*)
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_read_prob

! /* write problem data in GLPK format */

          INTEGER(C_INT) FUNCTION glp_write_prob(prob, flag, fname) &
               BIND(C, NAME="glp_write_prob")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            INTEGER(C_INT), VALUE :: flag
            CHARACTER(C_CHAR) :: fname(*)
            TYPE(C_PTR), VALUE :: prob
          END FUNCTION glp_write_prob

!----------------------------------------------------------------------------------!
!        Manipulation procedures for models expressed in MathProg                  !
!----------------------------------------------------------------------------------!

! /* allocate the MathProg translator workspace */

          TYPE(C_PTR) FUNCTION glp_mpl_alloc_wksp() BIND(C, NAME="glp_mpl_alloc_wksp")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
          END FUNCTION glp_mpl_alloc_wksp

! /* read and translate model section */

          INTEGER(C_INT) FUNCTION  glp_mpl_read_model(tran, fname, skip) &
               BIND(C, NAME="glp_mpl_read_model")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            INTEGER(C_INT), VALUE:: skip
            TYPE(C_PTR), VALUE:: tran
            CHARACTER(C_CHAR) :: fname(*)
          END FUNCTION glp_mpl_read_model

! /* read and translate data section */

          INTEGER(C_INT) FUNCTION  glp_mpl_read_data(tran, fname) &
               BIND(C, NAME="glp_mpl_read_data")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            TYPE(C_PTR), VALUE:: tran
            CHARACTER(C_CHAR) :: fname(*)
          END FUNCTION glp_mpl_read_data

! /* generate the model */

          INTEGER(C_INT) FUNCTION  glp_mpl_generate(tran, fname) &
               BIND(C, NAME="glp_mpl_generate")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR
            TYPE(C_PTR), VALUE:: tran
            CHARACTER(C_CHAR) :: fname(*)
          END FUNCTION glp_mpl_generate

! /* build LP/MIP problem instance from the model */

          SUBROUTINE glp_mpl_build_prob(tran, prob) BIND(C, NAME="glp_mpl_build_prob")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE:: tran, prob
          END SUBROUTINE glp_mpl_build_prob

! /* postsolve the model */

          INTEGER(C_INT) FUNCTION glp_mpl_postsolve(tran, prob, sol)&
               BIND(C, NAME="glp_mpl_postsolve")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            TYPE(C_PTR), VALUE :: prob, tran
            INTEGER(C_INT), VALUE:: sol
          END FUNCTION glp_mpl_postsolve

! /* free the MathProg translator workspace */

          SUBROUTINE glp_mpl_free_wksp(tran) BIND(C, NAME="glp_mpl_free_wksp")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
            TYPE(C_PTR), VALUE:: tran
          END SUBROUTINE glp_mpl_free_wksp

!----------------------------------------------------------------------------------!
!        Stand-alone Linear Programming and Mixed Integer Programming solver       !
!----------------------------------------------------------------------------------!

! /* stand-alone LP/MIP solver */

        INTEGER(C_INT) FUNCTION glp_main(argc, argv) BIND(C, NAME="glp_main")
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
            INTEGER(C_INT), VALUE :: argc
            TYPE(C_PTR) :: argv(*)
          END FUNCTION glp_main

     END INTERFACE

END MODULE glpk_fp

