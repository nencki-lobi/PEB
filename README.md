# PEB

Please cite the corresponding publication when using these materials:

Zaremba, D., Marczak, M., Michałowski, J., Klöckner, C.A., Wierzba, M., & Marchewka A. (2023) The effect of emotional experience on climate action taking. PsyArXiv. [https://doi.org/10.17605/OSF.IO/CDYM2](https://doi.org/10.17605/OSF.IO/CDYM2)

## Obtaining data from the SQL database

Data can be obtained directly from the SQL database by running the following command:

```
psql -U grieg -d grieg --csv --file code/peb.sql
```

## How to run

You can reproduce the analyses described in the manuscript by running the following command in R:

```
source('PEB-main.R')
```

## Requirements

The following R packages are required: `broom`, `car`, `ggplot2`, `gtsummary`, `lavaan`, `psych`, `report`, `Rmisc`, `tidyverse`.