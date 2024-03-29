% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elasticity_feenstra1994.R
\name{elasticity_feenstra1994}
\alias{elasticity_feenstra1994}
\title{Calculate the elasticity of substitution using the method described in
   Feenstra (1994) New Product Varieties and the Measurement of International
   Prices.}
\usage{
elasticity_feenstra1994(
  df,
  t_name = "date",
  v_name = "Type of crude oil",
  p_name = "CIF price (2) ($/bbl)",
  q_name = "Volume (1000 bbl)",
  freq = c("d", "w", "m", "q", "y"),
  conf_int = 0.95,
  bw_2006 = T,
  bw_sigma_min = 1.05,
  bw_sigma_max = 131.05,
  bw_sigma_step = 1,
  bw_rho_min = 0,
  bw_rho_max = 0.99,
  bw_rho_step = 0.01
)
}
\arguments{
\item{df}{data.frame; trade data from which to estimate elasticities of
substitution for a specific variety (see details).}

\item{t_name}{char; name of time column must be in date-format.}

\item{v_name}{char; name of variety column. Can be countries as in Feenstra
(1994) but also more granular import data can be used.}

\item{p_name}{char; name of price column}

\item{q_name}{char; name of quantity column}

\item{freq}{char; frequency of data. Used to calculate first differences
correctly. One of "d", "w", "m", "q", "y".}

\item{conf_int}{num; confidence level (e.g. 0.95)}

\item{bw_2006}{log; if TRUE (default), then the grid search approach described
in Broda and Weinstein (2006) Globalization and the Gains From Variety is
applied in cases where sigma cannot be found.}

\item{sigma_min}{num; minimum sigma for grid search (> 1). Default 1.05
(as in B-W 2006)}

\item{sigma_max}{num; maximum sigma for grid search. Default 131.5 (as in
B-W 2006)}

\item{sigma_step}{num; step size for sigma grid search. Default 1 (as in
B-W 2006)}

\item{rho_min}{num; minimum rho for grid search (>= 0). Default 0.}

\item{rho_max}{num; maximum rho for grid search (< 1). Default 0.99.}

\item{rho_step}{num; step size for rho grid search. Default 0.01.}
}
\description{
Calculate the elasticity of substitution using the method described in
   Feenstra (1994) New Product Varieties and the Measurement of International
   Prices.
}
\details{
Following Feenstra (1994), the data.frame should contain imports
   of only one specific country (destination) and one product type
   (e.g. TV's).
}
