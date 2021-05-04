# Readme

This repository contains the Infestation Pattern Simulation Supporting PREdisposition DetailS (IPS-SPREADS) as presented in the study Pietzsch et al. (2021, submitted). Each Folder contains the files listed below for each of the three modeling steps: testing (sensitivity analysis), calibrating (calibration) and experiments (application) as described and discussed in the submitted study.
List of contents of each folder:
+ a model file of IPS-SPREADS,
+ model input data (directory input),
+ model output data (directory output),
+ processed results (file results.csv) and,
+ the corresponding R script used for the data procession and visualization.

In the application directory an additional file is provided which contains the coordinates of the used research areas. In the other directories, the coordinates are included directly within the respective R script.

# Reasoning for multiple model files
For each modeling step a separate model file was used, as the simulations were performed on a high performance computation cluster and this enabled a much faster simulation. Furthermore, the differences between the three model are one of the following:
+ achieve random values for the parameters of interest within the respective analysis,
+ clean data processing fragments on some research sites (due to rasterization of vector data) and / or,
+ test different sanitation felling intensities inside and outside the protected area.

The reference model file of IPS-SPREADS is equivalent to the file within the application directory.
