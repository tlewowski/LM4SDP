# Statistical analysis of the results of the experiments.
# Statistical calculations were conducted using the reproducer R package by Madeyski et al., available from CRAN [1]. We used the probability of superiority $\hat{p}$ (PHat) non-parametric effect size measure (function PHat.test from reproducer) to assess the size of the effect. To assess statistical significance, we used confidence intervals based on the t-distribution as recommended by Brunner and Munzel. For positive effect sizes, we identified the effect size as significantly greater than zero if the lower confidence interval limit was greater than the null hypothesis value, which is 0.5 for $\hat{p}$. For negative effect sizes, we identified the effect size as significantly less than zero if the upper confidence interval limit was less than the null hypothesis value.
# L. Madeyski, B. Kitchenham, and T. Lewowski, reproducer: Reproduce Statistical Analyses and Meta-Analyses, 2023. R package version 0.5.3. https://CRAN.R-project.org/package=reproducer
library(reproducer) 
library(tidyverse)
library(dplyr)

#setwd("/Users/lma/Documents/...")

# Example statistical analysis for AllMetrics vs PyDriller process metrics for the MCC metric
basic_data <- readxl::read_excel("./basic-metrics-full.xlsx")#, sheet = "Out_23_AllMetricsVsPyDrillerPro")
single_data <- readxl::read_excel("./single-metric-results-full.xlsx")#, sheet = "Out_23_AllMetricsVsPyDrillerPro")
minimal_data <- readxl::read_excel("./minimal-results-full.xlsx")#, sheet = "Out_23_AllMetricsVsPyDrillerPro")
autoencoder_data <- readxl::read_excel("./autoencoders.xlsx")

print("Pydriller vs ALL")
pHatTest_mcc <- reproducer::PHat.test(
  x=dplyr::filter(basic_data, metric_set == "pydriller")$real_mcc, 
  y=dplyr::filter(basic_data, metric_set == "all-non-null-numeric")$real_mcc, 
  alternative="greater")
pHatTest_mcc
# Outout:
# # A tibble: 1 × 8
#      phat sqse.phat phat.df phat.tvalue phat.pvalue phat.ci.lower phat.ci.upper phat.sig
#     <dbl>     <dbl>   <dbl>       <dbl>       <dbl>         <dbl>         <dbl> <lgl>   
#   1 0.650  0.000537    512.        6.46    1.22e-10         0.612             1 TRUE   

# Example statistical analysis for AllMetrics vs PyDriller process metrics for the precision
pHatTest_precision <- reproducer::PHat.test(
  y=dplyr::filter(basic_data, metric_set == "all-non-null-numeric")$real_precision,  
  x=dplyr::filter(basic_data, metric_set == "pydriller")$real_precision,
  alternative="greater")
pHatTest_precision
# Outout:
# # A tibble: 1 × 8
#      phat sqse.phat phat.df phat.tvalue phat.pvalue phat.ci.lower phat.ci.upper phat.sig
#     <dbl>     <dbl>   <dbl>       <dbl>       <dbl>         <dbl>         <dbl> <lgl>   
#   1 0.679  0.000525    416.        7.80    2.58e-14         0.641             1 TRUE  


print("PyDriller vs Process")
reproducer::PHat.test(
  x=dplyr::filter(basic_data, metric_set == "process")$real_mcc, 
  y=dplyr::filter(basic_data, metric_set == "pydriller")$real_mcc, 
  alternative="greater")
reproducer::PHat.test(
  x=dplyr::filter(basic_data, metric_set == "process")$real_precision, 
  y=dplyr::filter(basic_data, metric_set == "pydriller")$real_precision, 
  alternative="greater")



print("Product vs code smells")
reproducer::PHat.test(
  x=dplyr::filter(basic_data, metric_set == "none")$real_mcc, 
  y=(dplyr::filter(basic_data, metric_set == "product") %>% dplyr::filter(smell_models == "False"))$real_mcc, 
  alternative="greater")
reproducer::PHat.test(
  x=dplyr::filter(basic_data, metric_set == "none")$real_precision, 
  y=(dplyr::filter(basic_data, metric_set == "product") %>% dplyr::filter(smell_models == "False"))$real_precision, 
  alternative="greater")



print("With or without code smells")
reproducer::PHat.test(
  x=dplyr::filter(basic_data, smell_models == "True")$real_mcc, 
  y=dplyr::filter(basic_data, smell_models == "False")$real_mcc, 
  alternative="greater")
reproducer::PHat.test(
  x=dplyr::filter(basic_data, smell_models == "True")$real_precision, 
  y=dplyr::filter(basic_data, smell_models == "False")$real_precision, 
  alternative="greater")

print("Single metric - code smells use or no")

reproducer::PHat.test(
  x=dplyr::filter(single_data, smell_models == "True")$real_mcc, 
  y=dplyr::filter(single_data, smell_models == "False")$real_mcc, 
  alternative="greater")
reproducer::PHat.test(
  x=dplyr::filter(single_data, smell_models == "True")$real_precision, 
  y=dplyr::filter(single_data, smell_models == "False")$real_precision, 
  alternative="greater")


print("Minimal sets - 7E vs 9B")

reproducer::PHat.test(
  x=dplyr::filter(minimal_data, metric_set == "7E")$real_mcc, 
  y=dplyr::filter(minimal_data, metric_set == "9B")$real_mcc, 
  alternative="greater")
reproducer::PHat.test(
  x=dplyr::filter(minimal_data, metric_set == "7E")$real_precision, 
  y=dplyr::filter(minimal_data, metric_set == "9B")$real_precision, 
  alternative="greater")

getLayerCount <- function(arch) {
  if(startsWith(arch, "single")) {
    1
  } else if(startsWith(arch, "dual")) {
    2
  } else if(startsWith(arch, "triple")) {
    3
  } else if(startsWith(arch, "quadruple")) {
    4
  } else {
    0
  }
}

getSmallestLayer <- function(arch) {
  if (grepl("-1-", arch, fixed = TRUE)) {
    1
  } else if(grepl("-3-", arch, fixed=TRUE)) {
    3
  } else if(grepl("-5-", arch, fixed=TRUE)) {
    5
  } else if(grepl("-10-", arch, fixed=TRUE)) {
    10
  } else {
    0
  }
}


unscaled <- autoencoder_data %>%
  dplyr::mutate(layer_count = sapply(autoencoder_arch, getLayerCount)) %>%
  dplyr::mutate(smallest_layer = sapply(autoencoder_arch, getSmallestLayer))

print("Autoencoders - model type")
print("RF vs DT")
reproducer::PHat.test(
  y=dplyr::filter(unscaled, model_type == "unscaled-randomforest")$real_mcc,
  x=dplyr::filter(unscaled, model_type == "unscaled-decisiontree")$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=dplyr::filter(unscaled, model_type == "unscaled-randomforest")$real_precision,
  x=dplyr::filter(unscaled, model_type == "unscaled-decisiontree")$real_precision,
  alternative="greater")

print("RF vs LR")
reproducer::PHat.test(
  y=dplyr::filter(unscaled, model_type == "unscaled-randomforest")$real_mcc,
  x=dplyr::filter(unscaled, model_type == "scaled-linear-ridge")$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=dplyr::filter(unscaled, model_type == "unscaled-randomforest")$real_precision,
  x=dplyr::filter(unscaled, model_type == "scaled-linear-ridge")$real_precision,
  alternative="greater")

print("DT vs LR")
reproducer::PHat.test(
  y=dplyr::filter(unscaled, model_type == "unscaled-decisiontree")$real_mcc,
  x=dplyr::filter(unscaled, model_type == "scaled-linear-ridge")$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=dplyr::filter(unscaled, model_type == "unscaled-decisiontree")$real_precision,
  x=dplyr::filter(unscaled, model_type == "scaled-linear-ridge")$real_precision,
  alternative="greater")


print("Autoencoders - mode")

unscaled_rf <- unscaled %>% dplyr::filter(model_type == "unscaled-randomforest")

reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, autoencoder_mode == "featureselection")$real_mcc,
  x=dplyr::filter(unscaled_rf, autoencoder_mode == "denoising")$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, autoencoder_mode == "featureselection")$real_precision,
  x=dplyr::filter(unscaled_rf, autoencoder_mode == "denoising")$real_precision,
  alternative="greater")


print("Autoencoders - smallest layer size")
print("1 vs 3")
reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, smallest_layer == 3)$real_mcc,
  x=dplyr::filter(unscaled_rf, smallest_layer == 1)$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, smallest_layer == 3)$real_precision,
  x=dplyr::filter(unscaled_rf, smallest_layer == 1)$real_precision,
  alternative="greater")

print("3 vs 5")
reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, smallest_layer == 5)$real_mcc,
  x=dplyr::filter(unscaled_rf, smallest_layer == 3)$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, smallest_layer == 5)$real_precision,
  x=dplyr::filter(unscaled_rf, smallest_layer == 3)$real_precision,
  alternative="greater")

print("5 vs 10")
reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, smallest_layer == 10)$real_mcc,
  x=dplyr::filter(unscaled_rf, smallest_layer == 5)$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=dplyr::filter(unscaled_rf, smallest_layer == 10)$real_precision,
  x=dplyr::filter(unscaled_rf, smallest_layer == 5)$real_precision,
  alternative="greater")


print("Autoencoders vs regular")
print("10 vs full")
basic_rf <- basic_data %>% dplyr::filter(model_type == 'unscaled-randomforest')
reproducer::PHat.test(
  y=basic_rf$real_mcc,
  x=dplyr::filter(unscaled_rf, smallest_layer == 10)$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=basic_rf$real_precision,
  x=dplyr::filter(unscaled_rf, smallest_layer == 10)$real_precision,
  alternative="greater")

print("10 vs 9B")
minimal_rf_9b <- minimal_data %>% dplyr::filter(metric_set == "9B") %>% dplyr::filter(model_type == 'unscaled-randomforest')

reproducer::PHat.test(
  y=minimal_rf_9b$real_mcc,
  x=dplyr::filter(unscaled_rf, smallest_layer == 10)$real_mcc,
  alternative="greater")

reproducer::PHat.test(
  y=minimal_rf_9b$real_precision,
  x=dplyr::filter(unscaled_rf, smallest_layer == 10)$real_precision,
  alternative="greater")
