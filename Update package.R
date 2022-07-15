#Use this script to update the package documentation when new functions are added
#there's probably a newer way do this, but it's always worked fine for me
#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

library(devtools)
library(roxygen2)

setwd('./PHInfo')

document()

#once the documentation has been updated, upload the new files to the github folder
#then can download the updated package with:
#devtools::install_github('PHinfo','ReverieNB')
