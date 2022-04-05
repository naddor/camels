# CAMELS data set

This repo contains the code used to produce most of the attributes of the **Catchment Attributes and MEteorology for Large-sample Studies** or **CAMELS** data set. These attributes were computed for 671 catchments in the United States and were introduced in the following paper:

*Addor, N., Newman, A. J., Mizukami, N. and Clark, M. P.: The CAMELS data set: catchment attributes and meteorology for large-sample studies, Hydrol. Earth Syst. Sci., 21(10), 5293–5313, [doi:10.5194/hess-21-5293-2017](http://dx.doi.org/10.5194/hess-21-5293-2017), 2017.*

This code was subsequently used to produce a similar data set for Chile including 516 catchments, **CAMELS-CL**:

*Alvarez-Garreton, C., Mendoza, P. A., Boisier, J. P., Addor, N., Galleguillos, M., Zambrano-Bigiarini, M., Lara, A., Puelma, C., Cortes, G., Garreaud, R., McPhee, J. and Ayala, A.: The CAMELS-CL dataset: catchment attributes and meteorology for large sample studies – Chile dataset, Hydrol. Earth Syst. Sci., 22, 5817–5846, [doi:10.5194/hess-22-5817-2018](http://dx.doi.org/doi:10.5194/hess-22-5817-2018), 2018.*

This code is currently being adapted for the production of **CAMELS-CH** for Switzerland.

By using the same code for several data sets, we aim to improve results comparability across countries. This code is open source to improve the transparency and reproducibility of hydrological experiments.

# Getting started

To get started, do not forget to:

* Set your R working environment to the project base directory.
* Copy-paste the .env.example file to .env and edit the paths and variables it contains.
* If you change a variable in the .env file you'll need to restart R

# Repo structure

The repo has the following structure:

* **compute**: functions to compute indices or attributes
* **extract**: functions and scripts to extract data from files
* **produce**: scripts to generate files with the attributes and corresponding maps
* **utils**: utility functions (for example, for plotting)

# Style guide

Please follow the coding style as defined by [the tidyverse style guide](https://style.tidyverse.org/index.html)

