#' Estimate production differences for nested CES vs standard CES
#'
#' @param x data.frame; data.frame of interest
#' @param x_diff data.frame; data with an added/deleted variety/variety group
#' @param quantity char; colname of quantity
#' @param type char; colname of type
#' @param sigma_ces num; elasticity of substitution for the Feenstra method
#' @param sigma_nces num; within product group elasticity of substitution (nested CES)
#' @param gamma_nces num; across product group elasticity of substitution (nested CES)
#' @export

est_prod_loss <- function(x,
                          x_diff,
                          quantity,
                          type,
                          sigma_ces,
                          sigma_nces,
                          gamma_nces){

  x <- x[ , c(quantity, type)]
  x_diff <- x_diff[ , c(quantity, type)]

  colnames(x) <- colnames(x_diff) <- c("q", "type")

  df_utility_nested <- x %>%
    mutate(u = q^((sigma_nces-1)/sigma_nces)) %>%
    group_by(type) %>%
    summarize(u = sum(u)) %>%
    mutate(u = u^((sigma_nces*(gamma_nces-1))/((sigma_nces-1)*gamma_nces))) %>%
    ungroup() %>%
    summarize(u = sum(u)) %>%
    mutate(u = u^(gamma_nces/(gamma_nces-1))) %>%
    as.numeric()

  df_utility_feenstra <- x %>%
    mutate(u = q^((sigma_ces-1)/sigma_ces)) %>%
    summarize(u = sum(u)) %>%
    mutate(u = u^(sigma_ces/(sigma_ces-1))) %>%
    as.numeric()

  df_utility_nested_diff <- x_diff %>%
    mutate(u = q^((sigma_nces-1)/sigma_nces)) %>%
    group_by(type) %>%
    summarize(u = sum(u)) %>%
    mutate(u = u^((sigma_nces*(gamma_nces-1))/((sigma_nces-1)*gamma_nces))) %>%
    ungroup() %>%
    summarize(u = sum(u)) %>%
    mutate(u = u^(gamma_nces/(gamma_nces-1))) %>%
    as.numeric()

  df_utility_feenstra_diff <- x_diff %>%
    mutate(u = q^((sigma_ces-1)/sigma_ces)) %>%
    summarize(u = sum(u)) %>%
    mutate(u = u^(sigma_ces/(sigma_ces-1))) %>%
    as.numeric()


  return(data.frame("Nested CES [production diff in %]" = round((df_utility_nested_diff / df_utility_nested - 1)*100, 2),
                    "CES [production diff in %]" = round((df_utility_feenstra_diff / df_utility_feenstra - 1)*100, 2),
                    check.names = F)
  )

}
