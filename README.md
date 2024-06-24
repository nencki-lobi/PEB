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

The following R packages are required: `broom`, `car`, `ggplot2`, `gtsummary`, `lavaan`, `patchwork`, `psych`, `report`, `Rmisc`, `tidyverse`.

## Funding

<img align="left" src="https://www.norwaygrants.si/wp-content/uploads/2021/12/Norway_grants@4x-913x1024.png" width=10% height=10%> 
<br>The research leading to these results has received funding from the Norwegian Financial Mechanism 2014-2021, no. 2019/34/H/HS6/00677.
