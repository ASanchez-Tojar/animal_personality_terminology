[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5848521.svg)](https://doi.org/10.5281/zenodo.5848521)

# Animal personality terminology

This repository contains all materials used for a project using a self-report questionnaire and a systematic review to understand how animal personality terminology is used and interpreted. Specifically, the repository contains all R code and data used in the following study:

---

Alfredo Sánchez-Tójar, Maria Moiron, Petri T. Niemelä. *Accepted*. **Terminology use in animal personality research: a self-report questionnaire and a systematic review**. *Proceedings of the Royal Society B*. DOI: 10.1098/rspb.2021.2259

* All authors contributed equally to this work

---

For any further information, please contact: [Alfredo Sánchez-Tójar](https://scholar.google.co.uk/citations?hl=en&user=Sh-Rjq8AAAAJ&view_op=list_works&sortby=pubdate), email: alfredo.tojar@gmail.com

## Code:

All R code is available in the main folder and scripts are numbered in order of use from 001 to 009. Scripts titles should be self-explanatory, but each script contains a "Description of script and Instructions" section with further information. The paths provided to import datasets into R assume your location is where the code is, i.e. the general folder `animal_personality_terminology`, make sure to setwd(), if necessary. The R version used throughout was 4.0.4, except for scripts 001-003, for which we provide a copy of the sessionInfo() in [001](https://github.com/ASanchez-Tojar/animal_personality_terminology/blob/main/literature_search/30_most_cited/screening_process_Rpackages_session.txt), and [002-003](https://github.com/ASanchez-Tojar/animal_personality_terminology/blob/main/literature_search/ten_journals/screening_process_Rpackages_session.txt).

## Folders:

[`data`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/data): contains three sub-folders, one for each of the main data components of our study: 
* [`Personality concept`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/data/personality_concept): this sub-folder contains information about the  animal personality definitions extracted from 26 highly cited articles in the field. 
* [`Survey`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/data/survey): this sub-folder contains the data from the self-report questionnaire, and also data to recreate figure 1 (affiliation map). In all, 440 participants took part in our self-report questionnaire.
* [`Ten journals`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/data/ten_journals): this sub-folder contains the data extracted as part of the literature review, which is shown per observer and also, and more importantly, the combined dataset covering all studies reviewed, including among-observer conflict resolution. In all, 88 articles published in 9 journals provided enough data for our review.

[`figures`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/figures): contains all figures (main and supplementary ones) created for this study, all of which were created using the R package [ggplot2 v.3.3.3](https://cran.r-project.org/web/packages/ggplot2/index.html).

[`literature_search`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/literature_search): contains the references found by our searches, which we performed in Web of Science. We performed three searches to identify: (1) the 30 most cited papers in animal personality, (2) all papers in animal personality, and (3) all papers in animal personality published in the 10 journals that published most animal personality research. 

[`screening`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/screening): contains the datasets showing the references screened at the title-and-abstract and full-text screening, including the decisions taken for each of the references. Title-and-abstract screening was performed using [rayyan](https://rayyan.qcri.org/), and files had to be formatted accordingly.

[`survey`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/survey): contains a pdf copy of the google forms survey used to collect data for the self-report questionnaire. 

[`tables`](https://github.com/ASanchez-Tojar/animal_personality_terminology/tree/main/tables): contains all tables (main and supplementary ones) created for this study, all of which were created using the R package [gt v.0.2.2](https://cran.r-project.org/web/packages/gt/index.html).

### Notes:

See details of the licence of this repository in [LICENSE.txt] (https://github.com/ASanchez-Tojar/animal_personality_terminology/blob/main/LICENSE.txt).

If you have troubbles importing some of the datasets, particularly if you are using Windows, it might be because the path names are longer than allowed by Windows. If that is the case, try and copy the entire folder (i.e. animal_personality_terminology) to your Desktop and running the code from there. We did not think of this character limit when naming the files as self-explanatory as possible. Sincere apologies for any inconveniences this may cause.
