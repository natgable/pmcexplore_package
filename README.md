# pmcexplore_package

pmcexplore is an R package to handle text data from PubMed Central.

I completed this project during my time as a Coding It Forward Civic Digital Fellow at the National Institutes of Health.

To use the R package:

1) Download the file "pmcexplore/"
2) Open R Studio 
3) In an R Studio script or chunk, run: `setwd("path_to/pmcexplore")` where you input the path to the folder you downloaded from GitHub.
4) Then run: `devtools::install("pmcexplore")`. 
5) Make sure you have the right R version (>= 4.0.0) and package dependencies.
6) Now run `library(pmcexplore)`.
6) To learn about the package and how to use it, run `browseVignettes("pmcexplore")`. To see all the functions available, you can run `ls("package:pmcexplore")`. To learn about each of the functions or datasets, run `?` followed by the function or dataset name. 
