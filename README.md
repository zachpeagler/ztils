# ztils

![License: MIT License](https://img.shields.io/badge/License-MIT-lightgrey) 
![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange)
![year](https://img.shields.io/badge/year-2024-blue)

 Various utilities meant to aid in speeding up common statistical operations, such as:
 - removing outliers
 - generating PDF and CDF plots
 - running multiple Kolmogorov-Smirnov tests at once
 - generating prediction plots

## Outlier and extreme removal
These functions subset a dataframe by removing any rows containing outliers or extremes of a target variable.

### no_outliers(data, var)

This function works by keeping only rows in the dataframe containing variable values within the quartiles +- 1.5 times the interquartile range.

#### Arguments
- **data**: the dataframe to remove rows containing outliers of the target variable
- **var**: the variable to calculate outliers against

```{r}
### This isn't a great example because the iris dataset does not contain any statistical outliers.
no_outliers(iris, Sepal.Length)
```

### no_extremes(data, var)

This function works by keeping only rows in the dataframe containing variable values within the quartiles +- 3.0 times the interquartile range.

#### Arguments
- **data**: the dataframe to remove rows containing outliers of the target variable
- **var**: the variable to calculate outliers against

```{r}
### This isn't a great example because the iris dataset does not contain any statistical outliers.
no_extremes(iris, Sepal.Length)
```

## Probability density function fitting and plotting

### multiPDF_cont(var, seq_length, distributions)

This function gets the probability density functions for selected distributions against **continuous**, **non-negative** numbers. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions).

This function returns a dataframe containing the real density and the probability density function for each selected distribution.

#### Arguments
- **var**: the variable of which to get the PDF
   - no default
- **seq_length**: the length to fit the distribution against
   - default 50
- **distributions**: the distributions to fit **var** against
   - default "all"
``` {r}
multiPDF_cont(iris$Petal.Length)

multiPDF_cont(iris$Sepal.Length, 100, c("normal", "lognormal"))
```

### multiPDF_plot(var, seq_length, distributions, palette, var_name)

This function extends **multiPDF_cont** and gets the probability density functions for selected distributions against **continuous**, **non-negative** numbers. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions). It then plots this using **ggplot2** and a **scico** palette.

**Returns**: A plot showing the PDF of the selected variable against the selected distributions over the selected sequence length.

#### Arguments
- **var**: the variable of which to get the PDF
- **seq_length**: the length to fit the distribution against
- **distributions**: the distributions to fit **var** against
