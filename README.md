# wine-consumer-preferences
An application of choice-based conjoint analysis written in R.

## Setup
To ensure consistency and replicability, an environment should be built so that
the same version of a package is installed in everyone's environment.

### On MacOS/Linux

1. Open the current repo as a project in RStudio
2. Run from the terminal `Rscript src/setup.R`
3. Now you should have the same environment as the one specified in the lockfile

### On Windows

1. Open the current repo as a project in RStudio
2. Simply run the `src/setup.R` file as you would with any R script.

## Workflow

Follow these rules when contributing your code:
1. Create your branch and make your changes there
2. Work locally, stage, and commit your changes and push to your remote branch on GH
3. Open a pull request asking to merge your changes into the `main` branch
4. Review your changes and approve merge request on GH.

## Repo structure

```
.
├── data                    # folder containing all data used in the project
│   └── CBC_Wine_data.csv   # dataset
├── README.md               # this document
├── renv.lock
├── src                     # folder containing all scripts
    ├── eda.R               # exploratory data analysis script
    ├── checks.R            # data simulation sanity checks
    └── setup.R             # setup script to be executed at first startup
```
## Analysis plan

1. visualize data
2. check irregularities and determine if some datapoints should be dropped due to irregularities (e.g., fencing,
and other hints that participants might not be taking the survey seriously)
3. start with fitting mixed multinomial models estimating both within and between participants effects
