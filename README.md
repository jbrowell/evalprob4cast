# RP-RES-forecast-evaluation
Implementations of recommended practice for renewable power forecast evaluation.

## Suggestions for functionality:

Notes from IEA Task meeting on 19-May-2023
- Data cleaning/missing values. We need to at least report what the package does, and perhaps print warning.
- Allow user to sup-set evaluation, e.g. by high/med/low wind speed
- Leverage existing R packages, e.g. ROC, scoringRules, other?
- Support for hierarchies, e.g. individual wind farms and total (can aggregate by ensemble member?)
