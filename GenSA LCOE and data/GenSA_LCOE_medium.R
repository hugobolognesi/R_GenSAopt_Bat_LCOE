# Install and load the read excel package if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

# Install and load the solve package if not already installed
if (!requireNamespace("lpSolve", quietly = TRUE)) {
  install.packages("lpSolve")
}

# Install and load the graph and image package if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}
if (!requireNamespace("clipr", quietly = TRUE)) {
  install.packages("clipr")
}

# Install and load the financial package if not already installed
if (!requireNamespace("FinCal", quietly = TRUE)) {
  install.packages("FinCal")
}

# Install and load the GenSA package if not already installed
if (!requireNamespace("GenSA", quietly = TRUE)) {
  install.packages("GenSA")
}

# Install and load the SAP and PSO package if not already installed (other optional optimization tools)
if (!requireNamespace("SAP", quietly = TRUE)) {
  install.packages("SAP")
}
if (!requireNamespace("pso", quietly = TRUE)) {
  install.packages("pso")
}

# Install and load the 'pso' package
#install.packages("pso")
#library(pso)
# Install and load required packages
library(GenSA)
library(lpSolve)
library(readxl)
library(dplyr)
library(ggplot2)
library(FinCal)
library(SAP)
library(pso)
library(reshape2)
library(clipr)

# Clear all variables and prompt
rm(list = ls())

# Import data
data <- read_excel("Dados_medium.xlsx") #Medium-demand company power data


## Start LMO-SLBESS optimization ##
source("LCOE_SLB_Medium.R") #Script with LMO-SLB calculation data
# Defining variables for the minimum objective function using GenSA
calcular_LCOE_SLB_GenSA <- function(Q_pv_, Q_mod_, Dem_TDC_fp_, Dem_TDG_, P_lim_) {
  # Definição das variáveis para otimização
  x0 <- c(Q_pv_, Q_mod_, Dem_TDC_fp_, Dem_TDG_)  # Valores iniciais
  # Define initial values, lower bounds, upper bounds, and integer variables
  lb <- c(0, 1, 0, 0)  # Lower bounds
  ub <- c(1000, 1000, P_lim_, 1000)  # Upper bounds
  #intcon <- rep (TRUE, 2)  # Integer variables
  # Set other SAPSO parameters
  control <- list(
    maxit = 2000,    # maximum number of iterations
    verbose = TRUE,    # messages from the algorithm
    smooth = FALSE       # True if the function is differentiable
  )
  # Perform optimization using GenSA
  set.seed(123)  # Set seed for reproducibility
  result <- GenSA::GenSA(par = x0, fn = LCOE_SLB_Medium, lower = lb, upper = ub, control=control)
  
  # Print the result
  cat("Called function: LCOE_SLB_Medium", result$par, "\n")
  cat("Optimal solution:", result$par, "\n")
  cat("Optimal function value:", result$value, "\n")
  
  return(result$trace.mat)
} 

#Call LCOE minimization function using GenSA
calcular_LCOE_SLB_GenSA (Q_pv, Q_mod, Dem_TDC_fp, Dem_TDG, P_lim)

## End LMO-SLBESS optimization ##

### Start NEW LIB optimization ###

# Objective function
source("LCOE_NLIB_Medium.R")
# Defining variables for the minimum objective function using GenSA
calcular_LCOE_NLIB_GenSA <- function(Q_pv_, Q_mod_, Dem_TDC_fp_, Dem_TDG_, P_lim_) {
  # Definição das variáveis para otimização
  x0 <- c(Q_pv_, Q_mod_, Dem_TDC_fp_, Dem_TDG_)  # Valores iniciais
  # Define initial values, lower bounds, upper bounds, and integer variables
  lb <- c(0, 1, 0, 0)  # Lower bounds
  ub <- c(1000, 1000, P_lim_, 1000)  # Upper bounds
  #intcon <- rep (TRUE, 2)  # Integer variables
  # Set other SAPSO parameters
  control <- list(
    maxit = 2000,    # maximum number of iterations
    verbose = TRUE,    # messages from the algorithm
    smooth = FALSE       # True if the function is differentiable
  )
  # Perform optimization using GenSA
  set.seed(123)  # Set seed for reproducibility
  result <- GenSA::GenSA(par = x0, fn = LCOE_NLIB_Medium, lower = lb, upper = ub, control=control)
  
  # Print the result
  cat("Called function: LCOE_NLIB_Medium", result$par, "\n")
  cat("Optimal solution:", result$par, "\n")
  cat("Optimal function value:", result$value, "\n")
  
  return(result$trace.mat)
} 

# Call LCOE minimization function using GenSA
calcular_LCOE_NLIB_GenSA (Q_pv, Q_mod, Dem_TDC_fp, Dem_TDG, P_lim)

## End NEW LIB optimization ##

### Start BAU optimization ###

# Função objetivo
source("LCOE_BAU_Medium.R")
# Defining variables for the minimum objective function using GenSA
calcular_LCOE_BAU_GenSA <- function(Dem_TDC_fp_, P_lim_) {
  # Definição das variáveis para otimização
  x0 <- c(Dem_TDC_fp_)  # Valores iniciais
  # Define initial values, lower bounds, upper bounds, and integer variables
  lb <- c(0)  # Lower bounds
  ub <- c(P_lim_)  # Upper bounds
  #intcon <- rep (TRUE, 2)  # Integer variables
  # Set other SAPSO parameters
  control <- list(
    maxit = 2000,    # maximum number of iterations
    verbose = TRUE,    # messages from the algorithm
    smooth = FALSE       # True if the function is differentiable
  )
  # Perform optimization using GenSA
  set.seed(123)  # Set seed for reproducibility
  result <- GenSA::GenSA(par = x0, fn = LCOE_BAU_Medium, lower = lb, upper = ub, control=control)
  
  # Print the result
  cat("Called function: LCOE_BAU_Medium", result$par, "\n")
  cat("Optimal solution:", result$par, "\n")
  cat("Optimal function value:", result$value, "\n")
  
  return(result$trace.mat)
} 

# Call LCOE minimization function using GenSA
calcular_LCOE_BAU_GenSA (Dem_TDC_fp, P_lim)

## End BAU optimization ###

## Start 2nd Generation SLB ###
source("LCOE_SLB_LFP_Medium.R")
# Definindo variaveis para funcao objetivo de minimo usando GenSA
calcular_LCOE_SLB_LFP_GenSA <- function(Q_pv_, Q_mod_, Dem_TDC_fp_, Dem_TDG_, P_lim_) {
  # Definição das variáveis para otimização
  x0 <- c(Q_pv_, Q_mod_, Dem_TDC_fp_, Dem_TDG_)  # Valores iniciais
  # Define initial values, lower bounds, upper bounds, and integer variables
  lb <- c(0, 1, 0, 0)  # Lower bounds
  ub <- c(1000, 1000, P_lim_, 1000)  # Upper bounds
  #intcon <- rep (TRUE, 2)  # Integer variables
  # Set other SAPSO parameters
  control <- List(
    maxit = 2000,    # maximum number of iterations
    verbose = TRUE,    # messages from the algorithm
    smooth = FALSE       # True if the function is differentiable
  )
  # Perform optimization using GenSA
  set.seed(123)  # Set seed for reproducibility
  result <- GenSA::GenSA(par = x0, fn = LCOE_SLB_LFP_Medium, lower = lb, upper = ub, control=control)
  
  # Print the result
  cat("Called function: LCOE_SLB_LFP_Medium", "\n")
  cat("Optimal solution:", result$par, "\n")
  cat("Optimal function value:", result$value, "\n")
  
  return(result$trace.mat)
} 

# Chame a função para minimizar LCOE usando GenSA
calcular_LCOE_SLB_LFP_GenSA (Q_pv, Q_mod, Dem_TDC_fp, Dem_TDG, P_lim)

## End of LFP-SLBESS optimization ##
