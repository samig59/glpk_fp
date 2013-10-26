Fortran Interface to Glpk's API
===============================

A Fortran Glpk API  is provided in three Fortran modules:

  * glpk_fp.f90    : provides access to Glpk's LP and (M)IP procedures.

  * glpk_graph.f90 : provides access to (some of) Glpk's graph and
    network procedures.

  * glpk_utils.f90 : provides access to (some of) Glpk's utility
    procedures.

The Fortran API is based on the standrad intrinsic module
ISO_C_BINDING which defines the Fortran and C interoperability.

The split of the API is on purpose. The development is at a very early
stage. However, in the case of LP and (M)IP the functionality is
almost complete.

Examples:

  * lp_ex_p.f90    : Solves an LP problem

  * bp_ex_p.f90    : Solves an IP (actually BP) problem

  * model_ex_p.f90 : demostrates the read in of Gnu MPL model, creation, and 
                     solution of the problem, and the export in glpk MPS format.
                     The Gnum MPL model file "assign.mod" is needed in order
                     to test this example.


How to compile and run the examles:

  1. Compile the glpk_fp.f90 module:
     $gfortran -c glpk_fp.f90

  2. Compile the example, for instance, model_ex_p.f90
     $gfortran -c model_ex_p.f90

  3. Create the executable, for instance, model_ex_p
     $gfortran -o model_ex_p model_ex_p.f90 glpk_fp.o -lglpk -lm

  4. Run the executable, for instance, model_ex_p
     $./model_ex_p
