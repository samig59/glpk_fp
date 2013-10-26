Fortran Interface to Glpk's API
===============================

A Fortran Glpk API  is provided in three Fortran modules:

  * glpk_fp.f90    : provides access to Glpk's LP and (M)IP procedures

  * glpk_graph.f90 : provides access to (some of) Glpk's graph and
    network procedures

  * glpk_utils.f90 : provides access to (some of) Glpk's utility
    procedures


The split of the API is on purpose. The development is at a very early
stage. However, in the case of LP and (M)IP the functionality is
almost complete.

Examples:

  * lp_ex_p.f90 : Solves an LP problem

  * bp_ex_p.f90 : Solves an IP (actually BP) problem

