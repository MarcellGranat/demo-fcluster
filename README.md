# demo-fcluster

### Files of the repository

    ├── utils.R

This file loads the packages and the commonly used functions. Run this script before any other script you want to evaluate.

    ├── data_setup.R

The data were downloaded from the Eurostat database using the {eurostat} package. This file downloades the required data. To ensure reproducibility, the downloaded data was saved using the {pins} package in the version available at the time of modeling to a OneDrive folder, to which we can provide access upon request.

    ├── markdown

        ├── abstract.md
    
        ├── ...
    
This folder contains the markdown files for the text of the manuscript. For ownership rights these files are left empty, so the manuscript.qmd can be still rendered, but without the text.
