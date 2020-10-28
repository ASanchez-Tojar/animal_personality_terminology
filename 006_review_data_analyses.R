################################################################################
# Authors: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)
#   Affiliation: Dept. Evolutionary Biology, Bielefeld University, Germany
#   Profile: https://scholar.google.de/citations?user=Sh-Rjq8AAAAJ&hl=de

# Script first created on the 28th of October 2020


################################################################################
# Description of script and Instructions
################################################################################

# This script is to import the combined file that was reviewed and corrected
# outside R and that will be the final file to be used for the analyses of the 
# literature review.


################################################################################
# Packages needed
################################################################################

pacman::p_load(openxlsx,stringr,dplyr,tidyverse)

# Clear memory
rm(list=ls())


################################################################################
# Functions needed
################################################################################

# none


################################################################################
# Import data
################################################################################

# combined and cleaned literature review data
combined.data <- read.xlsx("data/ten_journals/combined/ten_journals_fulltext_screening_and_data_extraction_combined_conflict_resolving.xlsx",
                           colNames=T,sheet = 1)


################################################################################
# Preparing dataset
################################################################################

# first we need to remove duplicates, which consists of those papers that were 
# screened twice and any conflicts discussed and agreed on. For any changes
# made to this dataset, please see the columns conflict_resolution_notes. The 
# excel file highlights those cells that had some changes to make them easier
# to spot. Be aware that contexts can differ between observers for the same
# paper, which is not important for the analyses, but will be kept as such in
# the non-duplicated dataset for the sake of transparency and reproducibility.

