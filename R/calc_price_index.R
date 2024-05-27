#' Calculate the Feenstra Price Index
#'
#' @param x data.frame; containing all relevant columns
#' @param time char; col-name of time
#' @param quantity char; col-name of quantity
#' @param price char; col-name of price
#' @param variety char; col-name of variety
#' @param good char; col-name of type
#' @param sigma char; col-name of within good elasticity of substitution
#' @param gamma char; col-name of across good elasticity of substitution
#' @export

# x <- df_year %>%
#   filter(repCountry == "Euro Area",
#          !oilType %in% c("medium NA", "heavy sweet"))
#
# time     = "year"
# quantity = "vol"
# price    = "price"
# variety  = "crudeoil"
# good     = "oilType"
# sigma    = "sigma"
# gamma    = "gamma_nces"
#
# # check Euro Area light sour: The price index is way to high. The problem must
# # lie in the code block starting from line 63

calc_price_index <- function(x,
                             time,
                             quantity,
                             price,
                             variety,
                             good,
                             sigma,
                             gamma){

  x <- x[ , c(time, variety, good, quantity, price, sigma, gamma)]

  colnames(x) <- c("time", "variety", "good", "q", "p", "sigma", "gamma")

  # drop observations with positive price and no quantity or vice versa
  x <- subset(x, (!(q == 0 & p != 0) & !(q != 0 & p == 0)))

  # create balanced panel
  min_time <- min(x$time, na.rm = T)
  max_time <- max(x$time, na.rm = T)

  tmp <- x %>%
    select(good, variety) %>%
    unique()

  lengthTmp <- nrow(tmp)

  tmp <- tmp[rep(row.names(tmp), each = max_time - min_time + 1), ]
  tmp <- cbind(time = rep(min_time:max_time, lengthTmp), tmp)

  x <- left_join(tmp, x) %>%
    mutate(q = ifelse(is.na(q), 0, q),
           p = ifelse(is.na(p), 0, p)) %>%
    group_by(good, variety) %>%
    mutate(sigma = max(sigma, na.rm = T),
           gamma = max(gamma, na.rm = T))

  # define goods available in this and last period
  df_price_idx <- x %>%
    group_by(good, variety) %>%
    arrange(time) %>%
    mutate(entry      = ifelse(lag(q) == 0 & q > 0, 1, 0),
           exit       = ifelse(lead(q) == 0 & q > 0, 1, 0),
           entryExp   = entry*p*q,
           exitExp    = exit*p*q,
           lagP       = lag(p),
           lagExit    = lag(exit),
           lagExitExp = lag(exitExp)) %>%
    group_by(time, good) %>%
    mutate(sumExp        = sum(p*q),
           sumEntryExp   = sum(entryExp),
           sumExitExp    = sum(exitExp),
           lagSumExitExp = sum(lagExitExp)) %>%
    group_by(good, variety) %>%
    arrange(time) %>%
    mutate(lagSumExp = lag(sumExp),
           lagExp    = lag(p*q)) %>%
    ungroup() %>%
    mutate(sumExpI_t    = sumExp - sumEntryExp, # expenditure that is not coming from newly added products
           sumExpI_t1   = lagSumExp - lagSumExitExp, # expenditure from the same set of goods (line above) in the previous period
           costShare_t  = ifelse(entry != 1 & sumExpI_t != 0 & q > 0, p*q/sumExpI_t, NA), # cost share of "constant" products in current period
           costShare_t1 = ifelse(lagExit != 1 & sumExpI_t1 != 0 & lagExp > 0, lagExp/sumExpI_t1, NA)) %>% # cost share of "constant" products in previous period
    # create weights
    mutate(weightNum = (costShare_t - costShare_t1)/(log(costShare_t)-log(costShare_t1)),
           weightNum = ifelse(is.nan(weightNum), NA, weightNum)) %>%
    group_by(time, good) %>%
    mutate(weight = weightNum/sum(weightNum, na.rm = T)) %>%
    ungroup() %>%
    # calculate log of weighted price ratio
    mutate(logPriceChgWgt = log((p/lagP)^weight)) %>%
    group_by(time, good) %>%
    mutate(priceIndex = exp(sum(logPriceChgWgt, na.rm = T))) %>%
    ungroup() %>%
    # create lambda ratio
    mutate(lambda_t        = (sumExp - sumEntryExp)/sumExp,
           lambda_t1       = (lagSumExp - lagSumExitExp)/lagSumExp,
           lambdaRatio     = lambda_t/lambda_t1,
           lambdaRatioCorr = lambdaRatio^(1/(sigma-1)),
           exactPriceIndex = priceIndex*lambdaRatioCorr,
           exactPriceIndex = ifelse(time == min_time, 1, exactPriceIndex)) %>%
    # calculate cumulative price index
    group_by(good, variety) %>%
    arrange(time) %>%
    mutate(cumPriceIndex = cumprod(priceIndex),
           cumExactPriceIndex = cumprod(exactPriceIndex)) %>%
    ungroup() %>%
    arrange(variety, good, time)

  # calculate total expenditure share for each good to calculate aggregate price
  # index
  df_expShare_goods <- x %>%
    group_by(time, good) %>%
    summarize(expShare = sum(p*q, na.rm = T)) %>%
    group_by(time) %>%
    mutate(expShare = expShare / sum(expShare),
           countGoods = 1,
           countGoods = sum(countGoods)) %>%
    ungroup()

  # calculate aggregate price index
  df_price_idx <- df_price_idx %>%
    left_join(df_expShare_goods) %>%
    mutate(lnCumExactPriceIndex = log(cumExactPriceIndex),
           lnExpShare = log(expShare)) %>%
    group_by(good) %>%
    arrange(time) %>%
    mutate(chgLnCumExactPriceIndex = lnCumExactPriceIndex - lag(lnCumExactPriceIndex),
           chgLnExpShare = lnExpShare - lag(lnExpShare)) %>%
    # calculate aggregate price index
    group_by(time) %>%
    mutate(# see formula A.65 in the online appendix of Redding and Weinstein (2020) QJE
           contLnChgCumExactPriceIdx = sum(chgLnCumExactPriceIndex/countGoods), # contribution within nest
           contLnChgExpShare = (1/(gamma-1)*sum(chgLnExpShare/countGoods)), # contribution across nests
           # calculate aggregate (nested price index)
           chgLnAggPriceIdx = contLnChgCumExactPriceIdx+contLnChgExpShare,
           # percentage change
           chgPercAggPriceIdx = (exp(chgLnAggPriceIdx)-1)*100,
           contPercChgCumExactPriceIdx = (exp(contLnChgCumExactPriceIdx)-1)*100,
           contPercChgExpShare = (exp(contLnChgExpShare)-1)*100)

  # # prepare final dataset
  # df_out <- df_price_idx %>%
  #   select(time, good, sigma, gamma, lambdaRatio, lambdaRatioCorr, priceIndex,
  #          exactPriceIndex, cumPriceIndex, cumExactPriceIndex,
  #          chgPercAggPriceIdx, contPercChgCumExactPriceIdx,
  #          contPercChgExpShare) %>%
  #   unique()

  return(df_price_idx)

}

