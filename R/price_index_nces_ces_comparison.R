#' Calculate Nested CES Price Index with optimal shares (see details)
#'
#' @param x data.frame; containing all relevant columns
#' @param time char; col-name of time
#' @param refPeriod type depends on type of time variable; base year of the price
#'    index; in this period the price index is normalized to the conventional
#'    price index and tastes are computed from the consumption expenditures and
#'    prices in this period,
#' @param quantity char; col-name of quantity
#' @param price char; col-name of price
#' @param variety char; col-name of variety
#' @param good char; col-name of type
#' @param sigma char; col-name of within good elasticity of substitution
#' @param gamma char; col-name of across good elasticity of substitution
#' @details
#' In this price index calculation we take into account that companies do not
#' voluntarily change their consumption basket when sanctions are implemented.
#' In CES Price Index literature tastes or quality is often assumed to be
#' constant over two periods. As tastes usually can't be observed one can
#' estimate a price index by taking first differences. However, changes in
#' consumption are assumed to be the result of changing preferences. This,
#' argument does no longer hold when we consider sanctions as this is similar to
#' an exogenous supply shock.
#'
#' To calculate the price index taking deviations from the optimal set into
#' account we do the following: We normalize the price index to the conventional
#' price index in the period where we argue that consumption expenditures were
#' optimal. From this normalization we can compute the taste parameters. Those,
#' are then kept constant over later periods to calculate the nested CES price
#' index taking deviations from the optimal consumption basket into account.
#' @export

# # df_year calculated in nces_price_index_deviation_from_optimal
# x <- df_year %>%
#   filter(repCountry == "Euro Area")#,
#          #!oilType %in% c("medium NA", "heavy sweet"))
#
# time     = "year"
# quantity = "q"
# price    = "p"
# variety  = "crudeoil"
# variety_ces = "sourceCountry"
# good     = "oilType"
# taste    = "taste"
# sigma_ces = "sigma_ces"
# sigma_nces    = "sigma_nces"
# gamma_nces    = "gamma_nces"

price_index_nces_ces_comparison <- function(x,
                                            time,
                                            quantity,
                                            price,
                                            variety,
                                            variety_ces,
                                            good,
                                            taste,
                                            sigma_ces,
                                            sigma_nces,
                                            gamma_nces){

  x <- x[ , c(time, variety, variety_ces, good, quantity, price, taste, sigma_ces, sigma_nces, gamma_nces)]

  colnames(x) <- c("time", "variety", "variety_ces", "good", "q", "p", "taste", "sigma_ces", "sigma_nces", "gamma_nces")

  # drop observations with positive price and no quantity or vice versa
  x <- subset(x, (!(q == 0 & p != 0) & !(q != 0 & p == 0)))

  # ungroup to make sure no grouping is applied to the data.frame
  x <- ungroup(x)

  # calculate CES price index
  df_ces <- x %>%
    group_by(time, variety_ces, sigma_ces) %>%
    summarize(p = sum(p*q/sum(q)),
              q = sum(q)) %>%
    group_by(time, sigma_ces) %>%
    summarize(P = mean((sum(p^(1-sigma_ces)))^(1/(1-sigma_ces)))) %>%
    ungroup() %>%
    mutate(PchgCes = (P/lag(P)-1)*100) %>%
    rename("PCes" = P)

  df_nces <- x %>%
    group_by(time, variety, sigma_nces, gamma_nces, good, taste) %>%
    summarize(p = sum(p*q/sum(q)),
              q = sum(q)) %>%
    group_by(time, good, sigma_nces, gamma_nces, taste) %>%
    summarize(P_nest = mean((sum(p^(1-sigma_nces)))^(1/(1-sigma_nces)))) %>%
    group_by(time, sigma_nces, gamma_nces) %>%
    summarize(P = mean((sum(taste*P_nest^(1-gamma_nces)))^(1/(1-gamma_nces)))) %>%
    ungroup() %>%
    mutate(PchgNces = (P/lag(P)-1)*100) %>%
    rename("PNces" = P)

  out <- merge(df_ces, df_nces)

  return(out)

}
