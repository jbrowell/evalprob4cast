# evalprob4cast
Implementation of Recommended Practices (reference) for renewable power forecast evaluation.

## Installation instructions

### Option 1: Install from GitHub
You can install the package by copying the command below into `R`. You'll need to generate and use a personal `auth_token` as this is still a private repository.
``` r
remotes::install_github(repo="jbrowell/evalprob4cast", dependencies=TRUE, auth_token)
```
Note that the `remotes` package is required for installation. If it is not installed in your system, install it by running:
``` r
install.packages("remotes")
```

### Option 2: Clone and install
Alternatively, you can clone this repository and install the package from your local copy. This might be most convenient for installing specific versions on a branch, for example.
```r
devtools::install(PackagePath)
```
Note that the `devtools` package is required for installation. If it is not installed in your system, install it by running:
```r
install.packages("devtools")
```


### Load the package for use
Now the package can be loaded and used:
``` r
library(evalprob4cast)
```

## Usage

### Examples
The scripts in `demo/` demonstrates basic usage using sample data that is included in the package.

### Data format requirements

The tools inside `evalprob4cast` expects a certain data structure. The data must be organized as a list of 2 members, called exactly `forecasts` and `observations`. The `forecasts` member may then consist of any number of data frames, where one data frame represents the time series generated by one forecast candidate. The first column of a forecast data table must be named `TimeStamp`. If applicable, the second column may hold the timestamps at which the forecasts were issued, which must then be named `BaseTime`. Every other column represents the time series of an ensemble member, and they are not restricted to any specific naming rules.

The `observations` member is just one and only one data frame. The first column contains the observation timestamps and must be named `TimeStamp`; the second column contains the observed values and must be named `obs`.

The package comes sample data in this format and a helper function, `load_forecast_data()`, intended for users that prefer to store their forecasts and observations in individual csv-files.

## Guidance for contributors

Please report bugs and request features by raising [issues](https://github.com/jbrowell/evalprob4cast/issues), and contact the authors if you'd like to be added as a collaborator. Otherwise, fork this repo and submit pull requests.
