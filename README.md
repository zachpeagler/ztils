# ztils

![License: MIT License](https://img.shields.io/badge/License-MIT-lightgrey) 
![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange)
![year](https://img.shields.io/badge/year-2024-blue)

 Various utilities meant to aid in speeding up common statistical operations, such as:
 - removing outliers and extremes
 - generating probability density and cumulative distribution graphs with ggplot2
 - running one-sample Kolmogorov-Smirnov tests against multiple distributions at once
 - generating prediction plots with ggplot2
 - scaling data and performing principal component analysis (PCA)
 - plotting PCA with ggplot2

## Installation

Not on CRAN yet, working on that.

To install the development version:


```{r}
remotes::install_github('zachpeagler/ztils')
```

<br>


## no_outliers()

### Description
This function works by keeping only rows in the dataframe containing variable values within the quartiles +- 1.5 times the interquartile range.

### Usage
> This function has no defaults, as it is entirely dependent on the user input.
```{r}
no_outliers(data,
            var
            )
```

### Arguments
- **data**: the dataframe to remove rows containing outliers of the target variable
- **var**: the variable to calculate outliers against

### Returns
Returns the specified dataframe **data** minus the rows containing outliers in the **var** variable.

### Examples:
```{r}
no_outliers(iris, Sepal.Length)
```

>This isn't a great example because the iris dataset does not contain any statistical outliers.



## no_extremes()

### Description
This function works by keeping only rows in the dataframe containing variable values within the quartiles +- 3.0 times the interquartile range.

### Usage
> This function has no defaults, as it is entirely dependent on the user input.
```{r}
no_extremes(data,
            var
            )
```

### Arguments
- **data**: the dataframe to remove rows containing outliers of the target variable
- **var**: the variable to calculate outliers against
  
### Returns
Returns the specified dataframe **data** minus the rows containing extremes in the **var** variable.

### Examples:
```{r}
no_extremes(iris, Sepal.Length)
```

>This isn't a great example because the iris dataset does not contain any statistical outliers.

<br>


## multiPDF_cont()

### Description
This function gets the probability density function (PDF) for selected distributions against **continuous** variables. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions).

> Note that only **non-negative** numbers are supported by the lognormal and gamma distributions. Feeding this function a negative number with those distributions selected will result in an error.

### Usage:
```{r}
multiPDF_cont(var, 
              seq_length = 50, 
              distributions = "all"
              )
```

### Returns
This function returns a dataframe with row number equal to **seq_length** containing the real density and the probability density function of **var** for selected **distributions**.

### Arguments
- **var**: the variable of which to get the PDF
   - no default
- **seq_length**: the length to fit the distribution against
   - default 50
- **distributions**: the distributions to fit **var** against
   - default "all"

### Examples
``` {r}
multiPDF_cont(iris$Petal.Length)

multiPDF_cont(iris$Sepal.Length, 100, c("normal", "lognormal"))
```



## multiPDF_plot()

### Description
This function extends **multiPDF_cont** and gets the probability density functions (PDFs) for selected distributions against **continuous**, **non-negative** numbers. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions). It then plots this using **ggplot2** and a **scico** palette, using **var_name** for the plot labeling, if specified. If not specified, it will use **var** instead.

### Usage
```{r}
multiPDF_plot(var, 
              seq_length = 50,
              distributions = "all", 
              palette = "oslo", 
              var_name = NULL
              )
```

### Returns
A plot showing the PDF of the selected variable against the selected distributions over the selected sequence length.

### Arguments
- **var**: the variable of which to get the PDF
- **seq_length**: the length to fit the distribution against
- **distributions**: the distributions to fit **var** against
- **palette**: A *scico* palette to use on the graph, with each distribution corresponding to a color.
- **var_name**: A name to use in the title and x axis label of the plot.

### Examples
```{r}

multiPDF_plot(iris$Sepal.Length)

multiPDF_plot(iris$Sepal.Length,
              length = 100,
              distributions = c("normal", "lognormal", "gamma"),
              palette = "bilbao",
              var_name = "Sepal Length (cm)"
              )

```

<br>

### multiCDF_cont()

### Description
This function gets the cumulative distribution function (CDF) for selected distributions against **continuous** variables. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions).

> Note that only **non-negative** numbers are supported by the lognormal and gamma distributions. Feeding this function a negative number with those distributions selected will result in an error.

### Usage:
```{r}
multiCDF_cont(var, 
              seq_length = 50, 
              distributions = "all"
              )
```

### Returns
This function returns a dataframe with row number equal to **seq_length** containing the real density and the probability density function of **var** for selected **distributions**.

### Arguments
- **var**: the variable of which to get the PDF
   - no default
- **seq_length**: the length to fit the distribution against
   - default 50
- **distributions**: the distributions to fit **var** against
   - default "all"

### Examples
``` {r}
multiCDF_cont(iris$Petal.Length)

multiCDF_cont(iris$Sepal.Length,
              100, 
              c("normal", "lognormal")
              )
```



## multiCDF_plot()

### Description
This function extends **multiCDF_cont** and gets the cumulative distribution functions (CDFs) for selected distributions against **continuous**, **non-negative** numbers. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions). It then plots this using **ggplot2** and a **scico** palette, using **var_name** for the plot labeling, if specified. If not specified, it will use **var** instead.

### Usage
```{r}
multiCDF_plot(var, 
              seq_length = 50,
              distributions = "all", 
              palette = "oslo", 
              var_name = NULL
              )
```

### Returns
A plot showing the CDF of the selected variable against the selected distributions over the selected sequence length.

### Arguments
- **var**: the variable of which to get the CDF
- **seq_length**: the length to fit the distribution against
- **distributions**: the distributions to fit **var** against
- **palette**: A *scico* palette to use on the graph, with each distribution corresponding to a color.
- **var_name**: A name to use in the title and x axis label of the plot.

### Examples
```{r}

multiCDF_plot(iris$Sepal.Length)

multiCDF_plot(iris$Sepal.Length,
              length = 100,
              distributions = c("normal", "lognormal", "gamma"),
              palette = "bilbao",
              var_name = "Sepal Length (cm)"
              )
```

<br>

## multiKS_cont()

### Description
This function gets the distance and p-value from a one-sample Kolmogorov-Smirnov (KS) test for selected distributions against a continous input variable. Possible distributions include "normal", "lognormal", "gamma", "exponential", and "all".

### Usage
```{r}
multiKS_cont(var,
             distributions = "all"   
             )
```
> Note: If using "lognormal" or "gamma" distributions, the target variable *must* be non-negative.

### Arguments
- **var**: The variable to perform one-sample KS tests on
- **distributions**: The distributions to test against

### Returns
Returns a dataframe with the distance and p-value for each performed KS test. The distance is a relative metric of similarity. A p-value of > 0.05 indicates that the target variable's distribution is *not* significantly different from the specified distribution.

### Examples
```{r}
multiKS_cont(iris$Sepal.Length)

multiKS_cont(iris$Sepal.Length, c("normal", "lognormal"))
```

<br>





## function()

### Description


### Usage
```{r}

```

### Arguments


### Returns


### Examples
```{r}

```

<br>


# Bug reporting
If you find any bugs, please report them at https://github.com/zachpeagler/ztils/issues.