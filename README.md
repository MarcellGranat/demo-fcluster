# demo-fcluster

### Files of the repository

    ├── utils.R

This file loads the packages and the commonly used functions. Run this script before any other script you want to evaluate.

    ├── data_setup.R

The data were downloaded from the Eurostat database using the {eurostat} package. This file downloades the required data. To ensure reproducibility, the downloaded data was saved using the {pins} package in the version available at the time of modeling to a OneDrive folder, to which we can provide access upon request.

    ├── f_clust.R
    
This script contains the core modelling of the research. Considering its computation demand, it is put into a separeta file. Its output {clustering_result.RData} is required for rendering the manuscript.

    ├── manuscript.qmd
    
Codes for reproducing the figures and tables of the manuscript. If the markdown folder contains the filled files, the rendered output does not differ from the submitted manuscript.

    ├── **render_manuscript.R**
    
    ├── references.bib
    
    ├── captions.R
    
    ├── fix_tex.R
    
{render_manuscript.R} runs the quarto commands supplemented with {captions.R} and {fix_tex.R} to render {manuscipt.qmd}. This is recommended for a reproducable .tex code with captions and comment for the charts.

    ├── markdown

        ├── abstract.md
    
        ├── ...
    
This folder contains the markdown files for the text of the manuscript. For ownership rights these files are left empty, so the manuscript.qmd can be still rendered, but without the text.
