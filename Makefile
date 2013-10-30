FC = gfortran

.SUFFIXES:
.SUFFIXES: .f90 .o .mod

OBJFP = glpk_fp.o
OBJGR = glpk_graph.o
OBJUT = glpk_utils.o

MODULES := $(wildcard *.mod)

OBJLP = lp_ex_p.o
OBJMD = model_ex.o
OBJBN = bp_ex_p.o
OBJNT = graph_ex.o
OBJCB = bp_callback_ex.o
OBJCC = cb_prob_common.o
OBJCF = cb_proc.o

FDEBUG = -g
FFLAGS = $(FDEBUG) 
LDFLAGS = -g -lglpk -lm

all : linear model binary callback graph
.PHONY : all

linear : $(OBJLP) $(OBJFP)
	$(FC) -o linear  $(OBJLP) $(OBJFP) $(LDFLAGS)

model : $(OBJMD) $(OBJFP)
	$(FC) -o model $(OBJMD) $(OBJFP) $(LDFLAGS)

binary : $(OBJBN) $(OBJFP)
	$(FC) -o binary $(OBJBN) $(OBJFP) $(LDFLAGS)

graph : $(OBJNT) $(OBJGR)
	$(FC) -o graph $(OBJNT) $(OBJGR) $(LDFLAGS)

callback : $(OBJCB) $(OBJCC) $(OBJCF) $(OBJFP)
	$(FC) -o callback  $(OBJCB) $(OBJCC) $(OBJCF) $(OBJFP) $(LDFLAGS)

$(OBJFP) : glpk_fp.f90
	$(FC) -c $*.f90

$(OBJGR) : glpk_graph.f90
	$(FC) -c $*.f90

$(OBJLP) : $(OBJFP) lp_ex_p.f90
	$(FC) -c $*.f90

$(OBJMD) : $(OBJFP) model_ex.f90 
	$(FC) -c $*.f90

$(OBJBN) : $(OBJFP) bp_ex_p.f90
	$(FC) -c $*.f90

$(OBJNT) : $(OBJGR) graph_ex.f90
	$(FC) -c $*.f90

$(OBJCC) : cb_prob_common.f90
	$(FC) -c $*.f90

$(OBJCF) : $(OBJFP) $(OBJCC) cb_proc.f90
	$(FC) -c $*.f90

$(OBJCB) : $(OBJFP) $(OBJCC) $(OBJCF) bp_callback_ex.f90
	$(FC) -c $*.f90

.PHONY: cleanall
cleanall:
	-rm $(wildcard *.o) $(MODULES) linear model binary callback graph
