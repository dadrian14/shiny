# shiny
This repository contains the code for R Shiny apps I have created, mostly for the purpose of teaching and learning statistics.  Currently, the web apps are only accessible within the GVSU network, so sharing the code is the only way I have of sharing them outside the university.  I have organized them as follows.

## steps
This is a tool for introducing coding your own apps.  It contains step-by-step instructions for making a simple app that plots the normal density curve.  The instructions are an R Markdown file that contains several Shiny apps (code and web apps) at the different stages of development.

## introstats
This folder contains several apps that are designed for an introductory course in applied statistics.  These apps include:

  * `samp_dist_samp_prop` Sampling distribution of the sample proportion
  * `ci_prop` Confidence interval for a population proportion
  * `ht_prop` Hypothesis test for a population proportion
  * `quant_descr` Numerical and graphical summaries for the distribution of a quantitative variable
  * `guess_sd` Given four histograms/boxplots and four standard deviation values, can you match the distribution to the standard deviation? (made by Suchir Gupta)
  * `clt` The sampling distribution of the sample mean and the Central Limit Theorem
  * `std_norm` "Forward" and "Backward" calculations for the standard normal distribution
  * `t_dist` Comparing the t distribution to the standard normal distribution (and t* to z*)
  * `ci_mean` Confidence intervals for a population mean
  * `ht_mean` Hypothesis tests for a population mean
  * `chisq_test` Chi-squared test for a two-way table
  * `slope_intercept` Slope-intercept form of a line
  * `least_squares` The least squares regression line has the smallest sum of squared residuals out of all lines.
  * `two_sample_apps` Simulating independent two-group data; confidence intervals and hypothesis tests for a difference in population means
  * `testing_errors` Type 1 and 2 errors and power illustrated through simulation in the two-sample t test context
  
## regression_doe
This folder contains apps I have used in my courses in Regression and Design of Experiments, both at the undergraduate and graduate levels.  The apps include:

  * `slr_model` Simulate from the simple linear regression model.  The focus is on the difference between the parameters and their estimates and on the differences between the errors and the residuals.
  * `power_curve` Power curve for the two-sample t test, focusing on its dependence on the effect size, standard deviation, sample size, and significance level
  * `norm_quant_plot` Produces normal quantile plots for simulated data from different distributions (normal and non-normal) and explains what is plotted on the x- and y-axes
  * `leverage` Displays how leverage is a measure of the statistical distance of the x-values for an individual from the center of the x-values for the dataset, focusing on the case of two explanatory variables
  * `model_sel` Shows the bias/variance tradeoff involved in model selection.  Specifically, underfitting can lead to bias in regression coefficient estiamtes and overfitting can lead to increased variance.
  * `1wayrandom` Focuses on the difference between the statistical properties of fixed and random effects in one-way models
  * `3wayANOVA` Shows the effects of each term in the three-way factorial model on the cell means
  * `sim2factor` Uses simulation to explore the differences between two-factor models with crossed/nested and fixed/mixed/random factors
  
## misc
Contains Shiny apps about other subjects, including:
  * `spelling_app` Can you choose between correctly and incorrectly spelled versions of 21 difficult words?
  * `trig_unit_circle` Geometric interpretation of the sine and cosine functions using a circle with radius of one


