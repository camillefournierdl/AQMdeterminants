# Reproduction material for: Large gaps in monitoring urban air pollution in low- and middle- income countries associated with economic conditions and political institutions

## Authors

- Maja Schoch
- [Camille Fournier de Lauriere](https://github.com/camillefournierdl)
- Thomas Bernauer

Schoch, M., Fournier de Lauriere, C. & Bernauer, T. (2025).
Large gaps in monitoring urban air pollution in low- and middle- income countries associated with economic conditions and political institutions
PLOS ONE. [link](insert link)

## Overview

This repository contains the data, code, and supplementary materials
associated with the above research paper. It enables full reproduction
of the paper’s figure panels, tables, and appendix materials. Multi-panel
figures were assembled using an external software (e.g., Canva).

In addition to the reproducible components of the paper, the repository
includes some exploratory analyses and visualizations not included in the
final publication.

## Datasets used

Datasets used in this analysis are all described in the Methods section of the paper and summarized in Table S3 of the Appendix.

## Repository Structure

```
    .
    ├── data/
    ├── dataRaster/
    ├── revisions/
    │   ├── data/
    │   ├── plots/
    ├── cleanAQMDataPrep_v4.Rmd
    ├── cleanAQMAnalysis_v4.Rmd
    ├── outputData/
    ├── plots/
    ├── plotsExtra/
    ├── plotsMain/
    ├── README.md
    ├── .gitignore
    ├── .gitattributes
    └── LICENSE

```

The cleanAQMDataPrep_v4.Rmd script is used to create data used in the final analysis, for which outputs are stored in the outputData folder. 
It should be ran before the cleanAQMAnalysis_v4.Rmd script.

The three plots folder contain figures used in the main analysis, as well as extra visualisations that were created during the research process.
We include all plots for transparency, even if they were not used in the final publication.

The revision folder contains scripts and data added after submission.

Data folders: - data/ — datasets used at submission -
revisions/dataCensus/ — datasets added after revision

## License

MIT License
© 2025 Camille Fournier de Lauriere
