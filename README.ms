Cytomata is an open-source script collection that integrates tools and methods used for mass cytometry (CyTOF) analysis into an automated pipeline.
Author: Lev Petrov


master.sh
Shell script controlling all the important settings. Path setting, module switching and all pipeline preferences are here.
Plotting preferences have to be changed separately in corresponding scripts.

environment_setup.R
Install all packages and dependencies listed here.

cleaning_debarcoding.R
First step in analysis. Loading raw .fcs files, gating doublets and demultiplexing (debarcoding) of samples is done here.


