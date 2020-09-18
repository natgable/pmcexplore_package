# pmcexplore

pmcexplore is an R package to handle text data from PubMed Central.

I completed this project during my time as a Coding It Forward Civic Digital Fellow at the National Institutes of Health. I worked on a cross-agency team but based within NIDDK.

This repo contains all code for the R package, as well as all code for the Shiny app associated with the package.

To use the R package:

1) Download the file "pmcexplore/"
2) Open R Studio 
3) In an R Studio script or chunk, run: `setwd("path_to/pmcexplore")` where you input the path to the folder you downloaded from GitHub.
4) Then run: `devtools::install("pmcexplore")`. 
5) Make sure you have the right R version (>= 4.0.0) and package dependencies.
6) Now run `library(pmcexplore)`.
6) To learn about the package and how to use it, run `browseVignettes("pmcexplore")`. To see all the functions available, you can run `ls("package:pmcexplore")`. To learn about each of the functions or datasets, run `?` followed by the function or dataset name. 

# About the project

## Context

Good data sharing and stewardship practices are important in the future of biomedical and scientific research. The FAIR (findable, accessible, interoperable, and reusable) data guiding approach allows for better data sharing and reuse, study reproducibility, and collaboration between researchers. The goal of this project was to do an overview analysis of the data sharing landscape of NIH-funded publications hosted on PubMed Central (PMC). Specifically: are researchers sharing data? If so, how?

Data availability statements (DASs) are sections within papers for researchers to explain what data they used, information on how to access those data, and links to repositories or downloads if applicable. PMC stores all papers in a specific XML (Extensible Markup Language) format, and each section of an XML can have a specific tag. Currently, PMC allows for researchers to create an XML tag to specify a DAS, and researchers are encouraged to do so. However, there are no strict rules and regulations around tagging DASs or sharing data in a paper.

The initial goals for this project centered around creating a suite of functions, bundled into an R package called pmcexplore. These functions were then used to look at the data sharing landscape of PubMed Central. 
