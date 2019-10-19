# Radial-distribution-function

The guidelines present here are intended for compilation of the fortran codes on UNIX or LINUX system. 
This is fortran coding for RDF of an ionic liquid. You can clone the repository and then execute following commands on your computer

1) Open terminal.
2) Navigate yourself into the directory of cloned repository.
3) Type/Copy 
   ~$ gfortran -c subroutine_center.f95
   ~$ gfortran -c subroutine_species.f95
   ~$ gfortran rdf_calculator.f95
   ~$ ./a.out
4) Check for the text files in the current directory. 
5) Use gnuplot or any program of your choice to plot the text files 

In order to plot multiple text files in gnuplot plot Refer here => https://riptutorial.com/gnuplot/example/27408/plotting-multiple-data-files
