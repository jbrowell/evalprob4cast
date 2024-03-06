# RP-RES-forecast-evaluation
Implementation of Recommended Practices (reference) for renewable power forecast evaluation.

## Installation instructions

You can install the package by copying the command below into ```R```.

```
remotes::install_github(repo="jbrowell/RP-RES-forecast-evaluation", dependencies=TRUE)
```

Note that the ```remotes``` package is required for installation. If it is not installed in your system, install it by running:

```
install.packages("remotes")
```

Now the package can be loaded and used:
```
library(IEAwind51)
```

## Guidance for contributors

Contributions should be made to the ```dev``` branch.

## Data format requirements

The tools inside IEAwind51RP expects a certain data structure. The data must be organized as a list of 2 members, called exactly ```forecasts``` and ```observations```. The "forecasts" member may then consist of any number of data frames, where one data frame represents the time series generated by one forecast candidate. The first column of a forecast data table must be named ```TimeStamp```. If applicable, the second column may hold the timestamps at which the forecasts were issued, which must then be named ```BaseTime```. Every other column represents the time series of an ensemble member, and they are not restricted to any specific naming rules. Insert example data.

The ```observations``` member is just one and only one data frame. The first column contains the observation timestamps and must be named ```TimeStamp```; the second column contains the observed values and must be named ```obs```.

The package comes with a helper function, ```load_forecast_data()```, intended for users that prefer to store their forecasts and observations in individual csv-files.
