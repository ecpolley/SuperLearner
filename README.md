# SuperLearner

This is the current version of the SuperLearner R package (version 2.*).

## How to install from Github ##

The folders here are the source documents for creating the SuperLearner R package. Follow these steps to build and install the package on your computer:

1.  Download the entire repository using the **Downloads** link in the upper right.
2.  Unzip the downloaded file. [Of course, if you have git, you could replace steps 1 and 2 with: `git clone git://github.com/ecpolley/SuperLearner.git`]
3.  Run `R CMD build /path/to/folder`
4.  You should now have a file called SuperLearner\_2.0-2.tar.gz
5.  Run `R CMD INSTALL SuperLearner_2.0-2.tar.gz`
6.  Alternative to step 5, in R use the command `install.packages('SuperLearner_2.0-2.tar.gz', repos = NULL, type = 'source')` assuming you are in the correct directory for the *.tar.gz file (or add full path to the command)

The package should now exist in your R library and can be loaded with the command `library(SuperLearner)`.  