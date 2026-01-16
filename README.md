
## Overview
This repository contains python and R scripts for analysing etymological patterns of scientific names across Animalia.
The analyses include temporal trends, phylum-level comparisons, GAM-based modelling of category proportions, and cultural-class trends.

## Data
Species name data were obtained from the Catalogue of Life (CoL) (https://doi.org/10.48580/dgplc).
Due to the size of the dataset, the raw data are not included in this repository.
All analyses were performed on a locally stored snapshot of the CoL.
The access date of the CoL are described in the associated publication.

## Etymology categories
The following six etymology categories are used:
- Abstract Morphology
- Specific Morphology
- Conceptual Morphology
- Geography
- People
- Other
Initially, People-based epithets were classified into People_Male and People_Female using an LLM.
However, epithets classified as People_Female were extremely rare compared to People_Male across the dataset.
For this reason, People_Male and People_Female were merged into a single People category for all analyses.

## Cultural classes
Author's countries are grouped into the following cultural classes:
- African Names
- East Asian Names
- European Names
- Latin American Names
- Middle Eastern Names
- South Asian Names
Country-to-class mapping follows Phonchai et al. (2025).

## How to run
### Python
python scripts/01_Semantic_Clustering_Spider.ipynb
python scripts/02_Semantic_Clustering_Animalia.ipynb
python scripts/03_Category_Labeling.ipynb
python scripts/04_Author_Nationality_Labeling.ipynb
### R
R scripts/05_Statistical_Analysis.R

## Citation
If you use this code, please cite this repository.
Citation information is provided in the "Citation.cff" file.

## License
MIT License