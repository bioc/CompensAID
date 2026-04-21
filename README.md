CompensAID: An Automated Detection Tool for Reference Errors 


Overview
--------
CompensAID is an R package developed to automatically assess which marker combinations show skewing of populations due to reference errors. Reference errors occur when single-stained reference controls are insufficiently bright or fail to capture biological and/or technical signatures. CompensAID works for both conventional and spectral flow cytometry data. 


Key Features
--------
- Density-based cutoff detection
- Automated gating
- Positive population segmentation
- Calculation of the Secondary Stain Index (SSI) across each segment and for all marker combinations
- Output of an SSI matrix and a detailed SSI information data frame


Installation
--------
Install the package using the following command in R:

install.packages("devtools")

devtools::install_github("Olsman/CompensAID")


Citation
--------
The CompensAID package incorporates the Secondary Stain Index (SSI) as described in:
Daniels, K., and Gardner, R. Secondary Stain Index. Memorial Sloan Kettering Cancer Center. https://wi.mit.edu/sites/default/files/2021-05/20200504_Post-it_Secondary_Stain_Index_Final.pdf (2020).

CompensAID article:
R. Olsman, S. Bonte, M. Hofmans, et al., “CompensAID: An Automated Detection Tool for Reference Errors,” 
Cytometry Part A109, no. 2 (2026): 124–134, https://doi.org/10.1002/cyto.a.70016.