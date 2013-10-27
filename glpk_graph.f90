! glpk_graph.f90
! based on glpk.h (version as given in glpk_fp.f90)
!
!---------------------------------------------------------------------------------!
! glpk_graph.f90 accompanies glpk_fp.f90 and provides interfaces to the structures!
! and procedures of glpk that concern graphs, networks and flows.                 !
! The separation is intentional since you typically don't need the graph interfaces
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
!  The glpk_graph module is free: you can redistribute it and/or modify it
!  under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!
!  The glpk_graph module is distributed in the hope that it will be useful, but WITHOUT
!  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
!  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
!  License for more details.
!
!  For further details, see the license that follwed with glpk and also
!  visit <http://www.gnu.org/licenses/>.
!**********************************************************************************
! $Revision: 1.4 $
! Last modified: $Date: 2013/10/05 09:05:06 $
! Last compiled with: 
!      gfortran 4.6.3                                   (Date: --/--/----)
!      ifort 12.0.0 20101116                            (Date: 05/10/2013)
!      gfortran 4.8.1 20130411                          (Date: 05/10/2013)           
!      G95 (GCC 4.0.3 (g95 0.94!) Jan 17 2013)          (Date: 05/10/2013)
!      sunf90 Sun Fortran 95 8.6 Linux_i386 2011/11/16  (Date: 05/10/2013)
!----------------------------------------------------------------------------------
! OBSERVE:                                                                        !
! Note that the fact that it compile does not mean that it is fully functional    !
!---------------------------------------------------------------------------------!
!

MODULE glpk_graph

  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
  IMPLICIT NONE

  ! C preprossero #define constants

  ! /* assignment problem formulation: */

  INTEGER(C_INT), PARAMETER :: GLP_ASN_MIN = 1 !/* perfect matching (minimization) */
  INTEGER(C_INT), PARAMETER :: GLP_ASN_MAX = 2 ! /* perfect matching (maximization) */
  INTEGER(C_INT), PARAMETER :: GLP_ASN_MMP = 3 ! /* maximum matching */

  ! Glpk graph structures 

  TYPE, BIND(C) :: glp_vertex         ! glp_vertex & _glp_vertex
     INTEGER(C_INT) :: i
     TYPE(C_PTR) :: name  ! char *name; /* CHARACTER(C_CHAR), POINTER :: name(:) */
     TYPE(C_PTR) :: entry ! void *entry;
     TYPE(C_PTR) :: data  ! void *data;
     TYPE(C_PTR) :: temp  ! void *temp;
     TYPE(C_PTR) :: in    ! glp_arc *in;
     TYPE(C_PTR) :: out   ! glp_arc *out;
  END type glp_vertex

  TYPE, BIND(C) :: glp_arc             ! glp_arc & _glp_arc
     TYPE(C_PTR) :: tail   !glp_vertex *tail;
     TYPE(C_PTR) :: head   !glp_vertex *head;
     TYPE(C_PTR) :: data   ! void *data;
     TYPE(C_PTR) :: temp   ! void *temp;
     TYPE(C_PTR) :: t_prev ! glp_arc *t_prev;
     TYPE(C_PTR) :: t_next ! glp_arc *t_next;
     TYPE(C_PTR) :: h_prev ! glp_arc *h_prev;
     TYPE(C_PTR) :: h_next ! glp_arc *h_next;
  END type glp_arc

  TYPE, BIND(C) :: glp_graph          ! glp_graph & _glp_graph
     TYPE(C_PTR) :: pool  ! void *pool;
     TYPE(C_PTR) :: name  ! char *name; /* CHARACTER(C_CHAR), POINTER :: name(:) */
     INTEGER(C_INT) :: nv_max
     INTEGER(C_INT) :: nv
     INTEGER(C_INT) :: na
     TYPE(C_PTR) :: v     ! glp_vertex **v; /* glp_vertex *v[1+nv_max] */
     INTEGER(C_INT) :: v_size
     INTEGER(C_INT) :: a_size
  END TYPE glp_graph

  INTERFACE

     ! /* create graph */

     FUNCTION glp_create_graph(v_size, a_size) BIND(C, NAME="glp_create_graph")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR) :: glp_create_graph
       INTEGER(C_INT), VALUE :: v_size, a_size
     END FUNCTION glp_create_graph

     ! /* assign (change) graph name */

     SUBROUTINE glp_set_graph_name(graph, name) BIND(C, NAME="glp_set_graph_name")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       CHARACTER(C_CHAR) :: name(*)
     END SUBROUTINE glp_set_graph_name

      ! /* add new arc to graph */

      FUNCTION glp_add_arc(graph, i, j) BIND(C, NAME="glp_add_arc")
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
        TYPE(C_PTR) :: glp_add_arc
        TYPE(C_PTR), VALUE :: graph
        INTEGER(C_INT), VALUE :: i, j
      END FUNCTION glp_add_arc

      ! /* add new vertices to graph */

      INTEGER(C_INT) FUNCTION glp_add_vertices(graph, nadd) BIND(C, NAME="glp_add_vertices")
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
        TYPE(C_PTR), VALUE :: graph
        INTEGER(C_INT), VALUE :: nadd
      END FUNCTION glp_add_vertices

      ! /* assign (change) vertex name */

      SUBROUTINE glp_set_vertex_name(graph, i, name) BIND(C, NAME="glp_set_vertex_name")
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
        TYPE(C_PTR), VALUE :: graph
        INTEGER(C_INT), VALUE :: i
        CHARACTER(C_CHAR) :: name(*)
      END SUBROUTINE glp_set_vertex_name

      ! /* delete vertices from graph */

      SUBROUTINE glp_del_vertices(graph, ndel, num) BIND(C, NAME="glp_del_vertices")
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
        TYPE(C_PTR), VALUE :: graph
        INTEGER(C_INT), VALUE :: ndel
        INTEGER(C_INT) :: num(*)
      END SUBROUTINE glp_del_vertices

      ! /* delete arc from graph */

      SUBROUTINE glp_del_arc(graph, arc) BIND(C, NAME="glp_del_arc")
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
        TYPE(C_PTR), VALUE :: graph, arc
      END SUBROUTINE glp_del_arc

     ! /* erase graph content */
     
     SUBROUTINE glp_erase_graph(graph, v_size, a_size) BIND(C, NAME="glp_erase_graph")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR) :: graph
       INTEGER(C_INT), VALUE :: v_size, a_size
     END SUBROUTINE glp_erase_graph

     ! /* delete graph */

     SUBROUTINE glp_delete_graph(graph) BIND(C, NAME="glp_delete_graph")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
       TYPE(C_PTR), VALUE :: graph
     END SUBROUTINE glp_delete_graph

     ! /* create vertex name index */

     SUBROUTINE glp_create_v_index(graph) BIND(C, NAME="glp_create_v_index")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
       TYPE(C_PTR), VALUE :: graph
     END SUBROUTINE glp_create_v_index

     ! /* find vertex by its name */

     INTEGER(C_INT) FUNCTION glp_find_vertex(graph, name) BIND(C, NAME="glp_find_vertex")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       CHARACTER(C_CHAR) :: name
     END FUNCTION glp_find_vertex
       
     ! /* delete vertex name index */

     SUBROUTINE glp_delete_v_index(graph) BIND(C, NAME="glp_delete_v_index")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
       TYPE(C_PTR), VALUE :: graph
     END SUBROUTINE glp_delete_v_index

     ! /* read graph from plain text file */

     INTEGER(C_INT) FUNCTION glp_read_graph(graph, fname) BIND(C, NAME="glp_read_graph")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       CHARACTER(C_CHAR) :: fname
     END FUNCTION glp_read_graph

     ! /* write graph to plain text file */

     INTEGER(C_INT) FUNCTION glp_write_graph(graph, fname) BIND(C, NAME="glp_write_graph")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       CHARACTER(C_CHAR) :: fname
     END FUNCTION glp_write_graph

     ! /* convert minimum cost flow problem to LP */

     SUBROUTINE glp_mincost_lp(prob, graph, names, v_rhs, a_low, a_cap, a_cost) &
          BIND(C, NAME="glp_mincost_lp")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: prob, graph
       INTEGER(C_INT), VALUE :: names, v_rhs, a_low, a_cap, a_cost
     END SUBROUTINE glp_mincost_lp

     ! /* find minimum-cost flow with out-of-kilter algorithm */

     INTEGER(C_INT) FUNCTION glp_mincost_okalg(graph, v_rhs, a_low, a_cap, a_cost, sol, a_x, v_pi) &
          BIND(C, NAME="glp_mincost_okalg")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_rhs, a_low, a_cap, a_cost, a_x, v_pi
       REAL(C_DOUBLE) :: sol
     END FUNCTION glp_mincost_okalg

     ! /* convert maximum flow problem to LP */

     SUBROUTINE glp_maxflow_lp(prob, graph, names, s, t, a_cap) BIND(C, NAME="glp_maxflow_lp")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: prob, graph
       INTEGER(C_INT), VALUE :: names, s, t, a_cap
     END SUBROUTINE glp_maxflow_lp

     ! /* find maximal flow with Ford-Fulkerson algorithm */

     INTEGER(C_INT) FUNCTION glp_maxflow_ffalg(graph, s, t, a_cap, sol, a_x, v_cut) &
          BIND(C, NAME="glp_maxflow_ffalg")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: s, t, a_cap, a_x, v_cut
       REAL(C_DOUBLE) :: sol
     END FUNCTION glp_maxflow_ffalg

     ! /* check correctness of assignment problem data */

     INTEGER(C_INT) FUNCTION glp_check_asnprob(graph, v_set) BIND(C, NAME="glp_check_asnprob")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_set
     END FUNCTION glp_check_asnprob

     ! /* convert assignment problem to LP */

     INTEGER(C_INT) FUNCTION glp_asnprob_lp(prob, form, graph, names, v_set, a_cost) &
          BIND(C, NAME="glp_asnprob_lp")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: prob, graph
       INTEGER(C_INT), VALUE :: form, names, v_set, a_cost
     END FUNCTION glp_asnprob_lp

     ! /* solve assignment problem with out-of-kilter algorithm */

     INTEGER(C_INT) FUNCTION glp_asnprob_okalg(form, graph, v_set, a_cost, sol, a_x) &
          BIND(C, NAME="glp_asnprob_okalg")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: form, v_set, a_cost, a_x
       REAL(C_DOUBLE) :: sol
     END FUNCTION glp_asnprob_okalg

     ! /* find bipartite matching of maximum cardinality */

     INTEGER(C_INT) FUNCTION glp_asnprob_hall(graph, v_set, a_x) BIND(C, NAME="glp_asnprob_hall")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_set, a_x
     END FUNCTION glp_asnprob_hall

     ! /* solve critical path problem */

     REAL(C_DOUBLE) FUNCTION glp_cpp(graph, v_t, v_es, v_ls) BIND(c, NAME="glp_cpp")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_t, v_es, v_ls
     END FUNCTION glp_cpp

     ! /* read min-cost flow problem data in DIMACS format */

     INTEGER(C_INT) FUNCTION glp_read_mincost(graph, v_rhs, a_low, a_cap, a_cost, fname) &
          BIND(C, NAME="glp_read_mincost")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_rhs, a_low, a_cap, a_cost
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_read_mincost

     ! /* write min-cost flow problem data in DIMACS format */

     INTEGER(C_INT) FUNCTION glp_write_mincost(graph, v_rhs, a_low, a_cap, a_cost, fname) &
          BIND(C, NAME="glp_write_mincost")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_rhs, a_low, a_cap, a_cost
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_write_mincost
     
     ! /* read maximum flow problem data in DIMACS format */

     INTEGER(C_INT) FUNCTION glp_read_maxflow(graph, s, t, a_cap, fname) BIND(C, NAME="glp_read_maxflow")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: a_cap
       INTEGER(C_INT) :: s, t
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_read_maxflow
       
     ! /* write maximum flow problem data in DIMACS format */

     INTEGER(C_INT) FUNCTION glp_write_maxflow(graph, s, t, a_cap, fname) BIND(C, NAME="glp_write_maxflow")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: s, t, a_cap
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_write_maxflow

     ! /* read assignment problem data in DIMACS format */

     INTEGER(C_INT) FUNCTION glp_read_asnprob(graph, v_set, a_cost, fname) BIND(C, NAME="glp_read_asnprob")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph 
       INTEGER(C_INT), VALUE :: v_set, a_cost
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_read_asnprob

     ! /* write assignment problem data in DIMACS format */

     INTEGER(C_INT) FUNCTION glp_write_asnprob(graph, v_set, a_cost, fname) BIND(C, NAME="glp_write_asnprob")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph 
       INTEGER(C_INT), VALUE :: v_set, a_cost
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_write_asnprob

     ! /* read graph in DIMACS clique/coloring format */

     INTEGER(C_INT) FUNCTION glp_read_ccdata(graph, v_wgt, fname) BIND(C, NAME="glp_read_ccdata")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph 
       INTEGER(C_INT), VALUE :: v_wgt
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_read_ccdata
     
     ! /* write graph in DIMACS clique/coloring format */

     INTEGER(C_INT) FUNCTION glp_write_ccdata(graph, v_wgt, fname) BIND(C, NAME="glp_write_ccdata")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_CHAR
       TYPE(C_PTR), VALUE :: graph 
       INTEGER(C_INT), VALUE :: v_wgt
       CHARACTER(C_CHAR) :: fname(*)
     END FUNCTION glp_write_ccdata

     ! /* Klingman's network problem generator */

     INTEGER(C_INT) FUNCTION glp_netgen(graph, v_rhs, a_cap, a_cost, parm) BIND(C, NAME="glp_netgen")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph 
       INTEGER(C_INT), VALUE :: v_rhs, a_cap, a_cost
       INTEGER(C_INT) :: parm(0:15)
     END FUNCTION glp_netgen

     ! /* grid-like network problem generator */

     INTEGER(C_INT) FUNCTION glp_gridgen(graph, v_rhs, a_cap, a_cost, parm) BIND(C, NAME="glp_gridgen")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph 
       INTEGER(C_INT), VALUE :: v_rhs, a_cap, a_cost
       INTEGER(C_INT) :: parm(0:14)
     END FUNCTION glp_gridgen

     ! /* Goldfarb's maximum flow problem generator */

     INTEGER(C_INT) FUNCTION glp_rmfgen(graph, s, t, a_cap, parm) BIND(C, NAME="glp_rmfgen")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph 
       INTEGER(C_INT) :: s, t, parm(0:5)
       INTEGER(C_INT), VALUE :: a_cap
     END FUNCTION glp_rmfgen

     ! /* find all weakly connected components of graph */

     INTEGER(C_INT) FUNCTION glp_weak_comp(graph, v_num) BIND(C, NAME="glp_weak_comp")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_num
     END FUNCTION glp_weak_comp

     ! /* find all strongly connected components of graph */

     INTEGER(C_INT) FUNCTION glp_strong_comp(graph, v_num) BIND(C, NAME="glp_strong_comp")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_num
     END FUNCTION glp_strong_comp

     ! /* topological sorting of acyclic digraph */

     INTEGER(C_INT) FUNCTION glp_top_sort(graph, v_num) BIND(C, NAME="glp_top_sort")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_num
     END FUNCTION glp_top_sort

     ! /* find maximum weight clique with exact algorithm */

     INTEGER(C_INT) FUNCTION glp_wclique_exact(graph, v_wgt, sol, v_set) &
          BIND(C, NAME="glp_wclique_exact")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE
       TYPE(C_PTR), VALUE :: graph
       INTEGER(C_INT), VALUE :: v_wgt, v_set
       REAL(C_DOUBLE) :: sol
     END FUNCTION glp_wclique_exact

  END INTERFACE
  
END MODULE glpk_graph
