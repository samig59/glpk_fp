! glpk_utils.f90
! based on glpk.h (version as given in glpk_fp.f90)
!
!---------------------------------------------------------------------------------!
! glpk_utils.f90 accompanies glpk_fp.f90 and provides interfaces to some of the   !
! utility procedures of glpk.                                                     !
! The separation is intentional since you typically don't need these              !
! when using glpk in its classical setting, i.e. in order to solve LPs and MIPs   !
!---------------------------------------------------------------------------------!
!
! $Author: samig $
!
!**********************************************************************************
!  
!  Copyright (C) 2013 Athanasios Migdalas, MSCC
!  E-mail: <samig59@gmail.com>.
!
!  The glpk_utils module is free: you can redistribute it and/or modify it
!  under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!
!  The glpk_utils module is distributed in the hope that it will be useful, but WITHOUT
!  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
!  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
!  License for more details.
!
!  For further details, see the license that follwed with glpk and
!  visit <http://www.gnu.org/licenses/>.
!**********************************************************************************
! $Revision: 1.1 $
! Last modified: $Date: 2013/10/03 12:57:36 $
! Last compiled with: 
!      gfortran 4.6.3                                   (Date: 03/10/2013)
!      ifort 12.0.0 20101116                            (Date: 03/10/2013)
!      gfortran 4.8.1 20130411                          (Date: 03/10/2013)            
!      G95 (GCC 4.0.3 (g95 0.94!) Jan 17 2013)          (Date: 03/10/2013)
!      sunf90 Sun Fortran 95 8.6 Linux_i386 2011/11/16  (Date: 03/10/2013)
!----------------------------------------------------------------------------------
! OBSERVE:                                                                        !
! (1) Note that the fact that it compile does not mean that it is fully functional!
! (2) Note that Fortran is not directly interoperable with C's variable length    !
!     argument lists. Therefore such glpk utility functions have not been listed  !
!     here. You probably need application specific C wrappers for such C functions!
!---------------------------------------------------------------------------------!

MODULE glpk_utils

  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR

  IMPLICIT NONE

  ! #ifndef GLP_LONG_DEFINED
  ! #define GLP_LONG_DEFINED
  ! typedef struct { int lo, hi; } glp_long;
  ! /* long integer data type */
  ! #endif

  TYPE, BIND(C) :: glp_long
     INTEGER(C_INT) :: lo, hi
  END type glp_long

  INTERFACE

     ! /* initialize GLPK environment */

     INTEGER(C_INT) FUNCTION glp_init_env() BIND(C, NAME="glp_init_env")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
     END FUNCTION glp_init_env

     ! const char *glp_version(void);
     ! /* determine library version */

     FUNCTION glp_version() BIND(C, NAME="glp_version")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
       TYPE(C_PTR) :: glp_version
     END FUNCTION glp_version

     ! /* free GLPK environment */
     
     INTEGER(C_INT) FUNCTION glp_free_env() BIND(C, NAME="glp_free_env")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
     END FUNCTION glp_free_env

     ! /* enable/disable terminal output */

     INTEGER(C_INT) FUNCTION glp_term_out(flag) BIND(C, NAME="glp_term_out")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
       INTEGER(C_INT), VALUE :: flag
     END FUNCTION glp_term_out

     ! /* start copying terminal output to text file */

     INTEGER(C_INT) FUNCTION glp_open_tee(fname) BIND(C, NAME="glp_open_tee")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_INT
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_open_tee

     ! /* stop copying terminal output to text file */

     INTEGER(C_INT) FUNCTION glp_close_tee() BIND(C, NAME="glp_close_tee")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
     END FUNCTION glp_close_tee

     ! /* allocate memory block */

     FUNCTION glp_malloc(size) BIND(C, NAME="glp_malloc")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
       INTEGER(C_INT), VALUE :: size
       TYPE(C_PTR) :: glp_malloc
     END FUNCTION glp_malloc

     ! /* allocate memory block */

     FUNCTION glp_calloc(n, size) BIND(C, NAME="glp_calloc")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
       INTEGER(C_INT), VALUE :: size, n
       TYPE(C_PTR) :: glp_calloc
     END FUNCTION glp_calloc

     ! /* free memory block */

     SUBROUTINE glp_free(ptr) BIND(C, NAME="glp_free")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
       TYPE(C_PTR), VALUE :: ptr
     END SUBROUTINE glp_free

     ! /* set memory usage limit */

     SUBROUTINE glp_mem_limit(limit) BIND(C, NAME="glp_mem_limit")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
       INTEGER(C_INT), VALUE :: limit
     END SUBROUTINE glp_mem_limit

     ! /* get memory usage information */
     
     SUBROUTINE glp_mem_usage(count, cpeak, total, tpeak) BIND(C, NAME="glp_mem_usage")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       IMPORT :: glp_long
       INTEGER(C_INT) :: count, cpeak
       !TYPE(C_PTR), VALUE :: total, tpeak
       TYPE(glp_long) :: total, tpeak
     END SUBROUTINE glp_mem_usage

     ! glp_long glp_time(void);
     ! /* determine current universal time */

     FUNCTION glp_time() BIND(C, NAME="glp_time")
       IMPORT :: glp_long
       TYPE(glp_long) :: glp_time
     END FUNCTION glp_time

     ! double glp_difftime(glp_long t1, glp_long t0);
     ! /* compute difference between two time values */

     REAL(C_DOUBLE) FUNCTION glp_difftime(t1, t0) BIND(C, NAME="glp_difftime")
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
        IMPORT :: glp_long
        TYPE(glp_long), VALUE :: t1, t0
      END FUNCTION glp_difftime

  END INTERFACE

END MODULE glpk_utils
