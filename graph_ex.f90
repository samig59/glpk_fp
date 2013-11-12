! graph_ex.f90
!
! (C) 2012, 2013 Athanasios Migdalas @ MSCC
!
! $Author: samig $
!
! The graph_ex code is free to copy, modify and distribute.
!
!---------------------------------------------------------------------------!
! PURPOSE/DESCRIPTION:                                                      !
! The code demostrates the dimensioning and creation of a graph and the     !
! search for its weakly connected components.                               !
!---------------------------------------------------------------------------!
!                                                                           
! $Revision: 1.1 $
! Last modified: $Date: 2013/10/27 09:35:57 $
! Last compiled and tested successfully with:
!      gfortran 4.8.1 20130411                         Date: 27/10/2013
!      gfortran 4.6.3 (Ubuntu/Linaro 4.6.3-1ubuntu5)   Date: 27/10/2013
!

PROGRAM graph_ex
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_INT, C_SIZEOF, C_SIZE_T, C_F_POINTER, C_CHAR

  USE glpk_graph

  IMPLICIT NONE

  TYPE(C_PTR) :: gr1, arc
  INTEGER(C_INT) :: i, j, k, s

  ! Graph and its name

  TYPE(glp_graph), POINTER :: gr2
  CHARACTER(C_CHAR), POINTER :: name(:)

  ! Vertex data type

  TYPE, BIND(C) :: vertex
     INTEGER(C_INT) :: num
  END TYPE vertex

  TYPE(vertex) :: v

  ! Step 1: Create a graph of certain size in bytes (OBS!!)
  
  ! Find the needed bytes to represent a vertex (arc)
  ! Obs...C_SIZEOF is INTEGER(8), while C_INT is INTEGER(4)
  ! on 64-bit machine but ... it is OK here.

  s = C_SIZEOF(v)  
  
  PRINT *, "Size of INTEGER(C_INT) is ", s, " bytes"
  PRINT *, "while C_SIZE_T is ", C_SIZE_T, " bytes"

  ! Create the graph

  gr1 = glp_create_graph(s,0)

  PRINT *, "Graph created"

  ! Name the graph 

  CALL glp_set_graph_name(gr1,"My first graph"//CHAR(0))

  PRINT *, "Graph named"

  ! Step 2: Substantiate the graph by actually 
  !         adding nodes and arcs to it ...
  !         or read in a graph with glp_read_graph

  ! Add nodes

  k = glp_add_vertices(gr1, 10)

  PRINT *, "Vertices added"

  ! Add arcs

  k = 0
  DO i = 1, 10, 2
     DO j = 10, 1, -2
        arc = glp_add_arc(gr1, i, j)
        PRINT *, "Adding arc (",i,",",j,")"
        k = k + 1
     ENDDO
  ENDDO

  PRINT *, k, "Arcs added"

  ! Step 3: Store graph in a file

  i = glp_write_graph(gr1, "GrapExamp1.txt"//CHAR(0))

  IF ( i /= 0 ) PRINT *, "Failed to store the graph in external file"

  ! Step 4: Count the number of weakly connected components

  k = glp_weak_comp(gr1, -1)

  PRINT *, "Found ", k, " weak components"

  ! Step 5: Count the number of strongly connected components

  k = glp_strong_comp(gr1, -1)  ! -1 < 0 don't store component info
 
  !k = glp_strong_comp(gr1, 0)    ! 0 = offsetof num in vertex

  PRINT *, "Found ", k, " strong components"

  ! Transform graph pointer from C to Fortran

  CALL C_F_POINTER(gr1,gr2)

  ! Get sizes

  PRINT *," Number of Nodes: ", gr2%nv, "Namber of arcs: ", gr2%na

  ! Get the name

  CALL C_F_POINTER(gr2%name, name, [15])

  PRINT *, " Name of graph: ", name

  ! Final step: Delete graph and free the memory

   CALL glp_delete_graph(gr1)

   PRINT *, "Graph deleted"
  
 END PROGRAM graph_ex
