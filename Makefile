# libraries to plot
DISLINLIB = -I/usr/local/dislin/gf -ldislin
PGPLOTLIB = -L/usr/local/pgplot -L/usr/X11/lib -lpgplot -lX11

#directories
MODULEDIR = ./modules
FUNCTDIR  = ./functions
TESTDIR   = ./test
PLOTDIR   = ./plot

#module files
MODULEFILES = $(MODULEDIR)/module_linear_equations.f90 \
			  $(MODULEDIR)/module_no_linear_equations.f90 \
			  $(MODULEDIR)/module_edo.f90 \
			  $(MODULEDIR)/module_integrate.f90 \
			  $(MODULEDIR)/module_probability_distributions.f90  \
			  $(MODULEDIR)/module_sdo.f90 

#functions files
FUNCTFILES = $(FUNCTDIR)/chemical_reaction_function.f90 \
		     $(FUNCTDIR)/dynamical_systems_function.f90 \
			 $(FUNCTDIR)/lotka_volterra_function.f90 \
			 $(FUNCTDIR)/one_dimension_function.f90 \
			 $(FUNCTDIR)/whatever_function.f90 \
			 $(FUNCTDIR)/stochastic_dynamical_systems_function.f90

#test files
TESTFILES = $(TESTDIR)/linear_sys.f90 \
			$(TESTDIR)/ode.f90 \
			$(TESTDIR)/fix_point.f90 \
			$(TESTDIR)/integrate_one_dimension.f90 \
			$(TESTDIR)/newton_test.f90 \
			$(TESTDIR)/sde.f90 \
			$(TESTDIR)/probability_distribution.f90

#plots files
PLOTSFILES = $(PLOTDIR)/plot_bogdanov_takens_bifurcation.f90 \
			 $(PLOTDIR)/plot_lorenz_attractor.f90 \
			 $(PLOTDIR)/plot_lotka_volterra.f90 \
			 $(PLOTDIR)/plot_lotka_volterra2.f90 \
			 $(PLOTDIR)/plot_ode_fun1.f90 \
			 $(PLOTDIR)/plot_pendulum.f90 \
			 $(PLOTDIR)/plot_transesterification_isoterm.f90 \
			 $(PLOTDIR)/plot_bruinsma.f90

all : compilar exetest clean

exetest :
	$(FC) -o test_newton.exe newton_test.o whatever_function.o module_linear_equations.o module_no_linear_equations.o
	$(FC) -o test_linear.exe linear_sys.o whatever_function.o module_linear_equations.o
	$(FC) -o test_fix_point.exe fix_point.o whatever_function.o module_no_linear_equations.o module_linear_equations.o
	$(FC) -o test_ode.exe ode.o whatever_function.o module_edo.o
	$(FC) -o test_integrate_one.exe integrate_one_dimension.o one_dimension_function.o module_integrate.o
	$(FC) -o test_sde.exe sde.o module_probability_distributions.o stochastic_dynamical_systems_function.o module_sdo.o
	$(FC) -o test_probability_distribution.exe probability_distribution.o module_probability_distributions.o


exeplots :
	$(FC) -o plot_fun1.exe plot_ode_fun1.o $(PGPLOTLIB) whatever_function.o module_edo.o
	$(FC) -o plot_lotka_volterra1.exe plot_lotka_volterra.o $(PGPLOTLIB) lotka_volterra_function.o module_edo.o
	$(FC) -o plot_lotka_volterra2.exe plot_lotka_volterra2.o $(PGPLOTLIB) lotka_volterra_function.o module_edo.o
	$(FC) -o plot_transes_iso.exe plot_transesterification_isoterm.o $(PGPLOTLIB) chemical_reaction_function.o module_edo.o
	$(FC) -o plot_bogdanov_takens.exe plot_bogdanov_takens_bifurcation.o $(PGPLOTLIB) dynamical_systems_function.o module_edo.o
	$(FC) -o plot_pendulum.exe plot_pendulum.o $(PGPLOTLIB) dynamical_systems_function.o module_edo.o
	$(FC) -o plot_lorenz.exe plot_lorenz_attractor.o $(DISLINLIB) dynamical_systems_function.o module_edo.o
	$(FC) -o plot_bruinsma.exe plot_bruinsma.o $(PGPLOTLIB) module_probability_distributions.o stochastic_dynamical_systems_function.o module_sdo.o


compilar : modulos function pruebas

#compile modules
modulos :
	$(FC) -c $(MODULEDIR)/module_linear_equations.f90
	$(FC) -c $(MODULEDIR)/module_no_linear_equations.f90
	$(FC) -c $(MODULEDIR)/module_edo.f90
	$(FC) -c $(MODULEDIR)/module_integrate.f90
	$(FC) -c $(MODULEDIR)/module_probability_distributions.f90
	$(FC) -c $(MODULEDIR)/module_sdo.f90
	$(FC) -c $(MODULEDIR)/dislin.f90

#compile functions
function :
	$(FC) -c $(FUNCTDIR)/chemical_reaction_function.f90
	$(FC) -c $(FUNCTDIR)/dynamical_systems_function.f90
	$(FC) -c $(FUNCTDIR)/lotka_volterra_function.f90
	$(FC) -c $(FUNCTDIR)/one_dimension_function.f90
	$(FC) -c $(FUNCTDIR)/whatever_function.f90
	$(FC) -c $(FUNCTDIR)/stochastic_dynamical_systems_function.f90

#test files
pruebas :
	$(FC) -c $(TESTDIR)/linear_sys.f90 $(DISLINLIB)
	$(FC) -c $(TESTDIR)/ode.f90 $(DISLINLIB)
	$(FC) -c $(TESTDIR)/fix_point.f90 $(DISLINLIB)
	$(FC) -c $(TESTDIR)/integrate_one_dimension.f90 $(DISLINLIB)
	$(FC) -c $(TESTDIR)/newton_test.f90 $(DISLINLIB)
	$(FC) -c $(TESTDIR)/sde.f90 $(DISLINLIB)
	$(FC) -c $(TESTDIR)/probability_distribution.f90 $(DISLINLIB)

# plots :
# 	$(FC) -c $(PLOTSFILES) $(DISLINLIB) $(PGPLOTLIB)

clean:
	rm *.o *.mod

cleanexe:
	rm *.exe
