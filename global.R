library(tidyverse)
library(formattable)
library(shiny)

periodo = 30
set.seed(1)
normal = rnorm(1e7)

E_p_mayor = function(p,mean = 0, sd = 1){
  mean(normal[normal >= qnorm(p)]*sd) + mean
}
