# ztils

![License: MIT License](https://img.shields.io/badge/License-MIT-lightgrey)
[![R-CMD-check](https://github.com/zachpeagler/ztils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zachpeagler/ztils/actions/workflows/R-CMD-check.yaml)
![lifecycle](https://img.shields.io/badge/lifecycle-maturing-green)
![year](https://img.shields.io/badge/year-2024-blue)

 Various utilities meant to aid in speeding up common statistical operations, such as:
 - removing outliers and extremes
 - generating probability density and cumulative distribution graphs with ggplot2
 - running one-sample Kolmogorov-Smirnov tests against multiple distributions at once
 - generating prediction plots with ggplot2
 - scaling data and performing principal component analysis (PCA)
 - plotting PCA with ggplot2

## Installation

To install from CRAN
```{r}
install.packages("ztils")
```
To install the development version:
```{r}
remotes::install_github("zachpeagler/ztils")
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


## multipdf_cont()

### Description
This function gets the probability density function (PDF) for selected distributions against **continuous** variables. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions).

> Note that only **non-negative** numbers are supported by the lognormal and gamma distributions. Feeding this function a negative number with those distributions selected will result in an error.

### Usage:
```{r}
multipdf_cont(var, 
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
multipdf_cont(iris$Petal.Length)

multipdf_cont(iris$Sepal.Length, 100, c("normal", "lognormal"))
```



## multipdf_plot()

### Description
This function extends **multiPDF_cont** and gets the probability density functions (PDFs) for selected distributions against **continuous**, **non-negative** numbers. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions). It then plots this using **ggplot2** and a **scico** palette, using **var_name** for the plot labeling, if specified. If not specified, it will use **var** instead.

### Usage
```{r}
multipdf_plot(var, 
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
- **palette**: A *scico* palette to use on the graph, with each distribution corresponding to a color. For all possible palettes, call **scico_palette_names()**.
- **var_name**: A name to use in the title and x axis label of the plot.

### Examples
```{r}

multipdf_plot(iris$Sepal.Length)

multipdf_plot(iris$Sepal.Length,
              seq_length = 100,
              distributions = c("normal", "lognormal", "gamma"),
              palette = "bilbao",
              var_name = "Sepal Length (cm)"
              )

```

<br>

### multicdf_cont()

### Description
This function gets the cumulative distribution function (CDF) for selected distributions against **continuous** variables. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions).

> Note that only **non-negative** numbers are supported by the lognormal and gamma distributions. Feeding this function a negative number with those distributions selected will result in an error.

### Usage:
```{r}
multicdf_cont(var, 
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
multicdf_cont(iris$Petal.Length)

multicdf_cont(iris$Sepal.Length,
              100, 
              c("normal", "lognormal")
              )
```



## multicdf_plot()

### Description
This function extends **multiCDF_cont** and gets the cumulative distribution functions (CDFs) for selected distributions against **continuous**, **non-negative** numbers. Possible distributions include any combination of "normal", "lognormal", "gamma", "exponential", and "all" (which just uses all of the prior distributions). It then plots this using **ggplot2** and a **scico** palette, using **var_name** for the plot labeling, if specified. If not specified, it will use **var** instead.

### Usage
```{r}
multicdf_plot(var, 
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
- **palette**: A *scico* palette to use on the graph, with each distribution corresponding to a color. For all possible palettes, call **scico_palette_names()**.
- **var_name**: A name to use in the title and x axis label of the plot.

### Examples
```{r}

multicdf_plot(iris$Sepal.Length)

multicdf_plot(iris$Sepal.Length,
              seq_length = 100,
              distributions = c("normal", "lognormal", "gamma"),
              palette = "bilbao",
              var_name = "Sepal Length (cm)"
              )
```

<br>

## multiks_cont()

### Description
This function gets the distance and p-value from a one-sample Kolmogorov-Smirnov (KS) test for selected distributions against a continous input variable. Possible distributions include "normal", "lognormal", "gamma", "exponential", and "all".

### Usage
```{r}
multiks_cont(var,
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
multiks_cont(iris$Sepal.Length)

multiks_cont(iris$Sepal.Length, c("normal", "lognormal"))
```

<br>

## gml_pseudor2

### Description
This function calculates the pseudo R^2 (proportion of variance explained by the model) for a general linear model (glm).  glms don't have real R^2 due to the intrinsic difference between a linear model and a generalized linear model, but we can still calculate an approximiation of the R^2 as (1 - (deviance/null deviance)).

### Usage
```{r}
glm_pseudor2(mod)
```

### Arguments
- **mod**: The glm object to calculate a pseudo-R^2 for.

### Returns
Returns the pseudo R^2 value of the model.

### Examples
```{r}
gmod <- glm(Sepal.Length ~ Petal.Length + Species, data = iris)
glm_pseudor2(gmod)
```

<br>


## pca_plot()

### Description
This function performs a principal component analysis (PCA) for the selected **pcavars** with the option to automatically scale the variables. It then graphs PC1 on the x axis and PC2 on the y-axis using *ggplot2*, coloring the graph with a *scico* palette over the specified **groups**. This is similar to the *biplot* command from the *stats* package, but performs all the steps required in graphing a PCA for you.

### Usage
```{r}
pca_plot(group,
         pcavars,
         scaled = FALSE,
         palette = "oslo
         )
```

### Arguments
- **group**: The group column, used for assigning colors.
- **pcavars**: The variables (columns) to perform a principal component analysis on. Should be *explanatory* variables and not *response* variables.
- **scaled**: A boolean (TRUE or FALSE) indicated if the **pcavars** have already been scaled or if they should be scaled in the function.
- **palette**: A *scico* palette used to color the graph. For all possible palettes, call **scico_palette_names()**. If non-scico palettes are desired, the palette can be overridden with scale_color and scale_fill functions.

### Returns
A ggplot object showing PC1 on the x axis and PC2 on the y axis, colored by group with vectors and labels showing the individual pca variables.

### Examples
```{r}
pca_plot(iris$Species, iris[,c(1:4)])

pca_plot(iris$Species, iris[,c(1:4)], FALSE, "bilbao")
```

<br>


## pca_data()

### Description
This function performs a principal component analysis (PCA) on the specified variables, **pcavars** and attaches the resulting principal components to the specified dataframe, **data**, with optional variable scaling.

### Usage
```{r}
pca_data(data,
         pcavars,
         scaled = FALSE
         )
```

### Arguments
- **data**: The dataframe to attach principal components to.
- **pcavars**: The variables to use in the principal component analysis.
- **scaled**: A logical value (TRUE or FALSE) indicating if **pcavars** have already been scaled or if they should be scaled in the function.

### Returns
Returns a dataframe with principal components as additional columns.

### Examples
```{r}
pca_data(iris, iris[,c(1:4)], FALSE)
```

<br>


## predict_plot()

### Description
This function performs a prediction based on the supplied **model**, then graphs it using *ggplot2*. Options are available for predicting based on the confidence or prediction interval, as well as for applying corrections, such as exponential and logistic.

> I would like to alter this function to reduce the number of required inputs, as all the information *should* be available from the model call, but that's a work in progress.
### Usage
```{r}
predict_plot(mod,
             data,
             rvar,
             pvar,
             group = NULL,
             length = 50,
             interval = "confidence",
             correction = "normal",
             palette = "oslo"
             )
```

### Arguments
- **mod**: A univariate linear model to base predictions on.
- **data**: The dataframe used in the model. Will be used to pull variables for plotting.
- **rvar**: The response variable (y-axis), must be the same as the one in the model
- **pvar**: The predictor variable (x-axis), must be the same as the one in the model.
- **group**: An optional grouping variable. If a group is present, separate predictions will be made for each group.
- **length**: The length to predict over. A longer length will result in more precision.
- **interval**: Tells the function to predict over either the confidence interval or the prediction interval.
  - "confidence" or "prediction"
- **correction**: If you log transform or logit transform the variables in the model, you can choose to apply a correction to the predicted output to reverse that transformation.
  - "normal", "exponential", or "logit"
- **palette**: A *scico* palette used to color the graph. For all possible palettes, call **scico_palette_names()**. If non-scico palettes are desired, the palette can be overridden with scale_color and scale_fill functions.

### Returns
Returns a plot with the observed (real) data plotted as points and the prediction plotted as lines, with a 95% confidence or prediction interval.

> This function has a known issue with the colors on ungrouped predictions being kind of funky, as the function uses the predictor variable (x-axis) for the color, which works for the actual data (points), but doesn't translate well to the predicted lines and ribbon.

### Examples
```{r}
mod1 <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
predict_plot(mod1, iris, Sepal.Length, Petal.Length, Species)
```

<br>


# Bug reporting
If you find any bugs, please report them at https://github.com/zachpeagler/ztils/issues.