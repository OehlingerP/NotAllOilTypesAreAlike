#' Calculate the elasticity of substitution using the method described in
#'    Feenstra (1994) New Product Varieties and the Measurement of International
#'    Prices.
#' @param df data.frame; trade data from which to estimate elasticities of
#'    substitution for a specific variety (see details).
#' @param t_name char; name of time column must be in date-format.
#' @param v_name char; name of variety column. Can be countries as in Feenstra
#'    (1994) but also more granular import data can be used.
#' @param p_name char; name of price column
#' @param q_name char; name of quantity column
#' @param freq char; frequency of data. Used to calculate first differences
#'    correctly. One of "d", "w", "m", "q", "y".
#' @param conf_int num; confidence level (e.g. 0.95)
#' @param bw_2006 log; if TRUE (default), then the grid search approach described
#'    in Broda and Weinstein (2006) Globalization and the Gains From Variety is
#'    applied in cases where sigma cannot be found.
#' @param sigma_min num; minimum sigma for grid search (> 1). Default 1.05
#'    (as in B-W 2006)
#' @param sigma_max num; maximum sigma for grid search. Default 131.5 (as in
#'    B-W 2006)
#' @param sigma_step num; step size for sigma grid search. Default 1 (as in
#'    B-W 2006)
#' @param rho_min num; minimum rho for grid search (>= 0). Default 0.
#' @param rho_max num; maximum rho for grid search (< 1). Default 0.99.
#' @param rho_step num; step size for rho grid search. Default 0.01.
#' @details Following Feenstra (1994), the data.frame should contain imports
#'    of only one specific country (destination) and one product type
#'    (e.g. TV's).
#' @export

elasticity_feenstra1994 <- function( df,
                                     t_name     = "date",
                                     v_name     = "Type of crude oil",
                                     p_name     = "CIF price (2) ($/bbl)",
                                     q_name     = "Volume (1000 bbl)",
                                     freq       = c( "d", "w", "m", "q", "y" ),
                                     conf_int   = 0.95,
                                     bw_2006    = T,
                                     bw_sigma_min  = 1.05,
                                     bw_sigma_max  = 131.05,
                                     bw_sigma_step = 1,
                                     bw_rho_min    = 0,
                                     bw_rho_max    = 0.99,
                                     bw_rho_step   = 0.01 ){

  freq <- match.arg( freq )

  if( !lubridate::is.Date( df[ , t_name, drop = T  ] ) ) stop( "t_name must be a date variable" )

  # prepare data
  temp <- df[ , c( t_name, v_name, p_name, q_name ) ]
  colnames( temp ) <- c( "t", "v", "p", "q" )

  if( length( temp$v ) < 3 ) stop( "Less than three varieties in data. Stop as estimation would fail due to perfect multicollinearity." )

  if( any( is.na( temp ) ) ) warning( "NA's in data. Will be dropped." )

  temp <- na.omit( temp )

  day_diff <- switch( freq,
                      "d" = 1,
                      "w" = 7,
                      "m" = 31,
                      "q" = 93,
                      "y" = 366 )


  # calculate log differences of shares and log difference of prices
  temp <- temp %>%
    group_by( t ) %>%
    # calculate total expenditure
    mutate( totE = sum( p*q ) ) %>%
    ungroup() %>%
    # calculate shares and log-transform
    mutate( s = p*q/totE,
            ls = log( s ),
            lp = log( p ) ) %>%
    group_by( v ) %>%
    arrange( t ) %>%
    # calculate differences
    mutate( dls = ls - lag( ls ),
            dlp = lp - lag( lp ) ) %>% # what to do with observations that are not regular?
    # remove observations with variety not bought in the previous month
    mutate( dt = t - lag( t ),
            dt = ifelse( dt > day_diff, NA, dt ) ) %>%
    ungroup()

  warning( paste0( nrow( temp ) - nrow( na.omit( temp ) ), " observations dropped due to first differences." ) )

  temp <- na.omit( temp ) %>%
    select( -dt )

  # select reference variety
  tab <- sort( table( temp$v ), decreasing = T )
  if( length( unique( temp$t ) ) > tab[ 1 ] ) warning( "No reference variety available that is observed at each point in time. Next best alternative chosen. However, this will results in additional observations dropped." )
  ref_variety <- names( tab[ 1 ])

  temp_ref_variety <- temp %>%
    filter( v == ref_variety ) %>%
    select( t, dls, dlp ) %>%
    rename( "kdls" = dls, "kdlp" = dlp )

  # merge data and calculate final regression variables: y, x1 and x2
  temp <- merge( temp, temp_ref_variety, by = "t" ) %>%
    # filter( v != ref_variety ) %>% # In my opinion the reference variety should be deleted but I am not sure
    mutate( y = ( dlp - kdlp )^2,
            x1 = ( dls - kdls )^2,
            x2 = ( dls - kdls ) * ( dlp - kdlp ) )

  # IV 1 - STEP 1 (this is nothing else than calculating country means)
  iv1_step1 <- lm( x1~v-1, temp )
  temp$x1hat <- iv1_step1$fitted.values
  temp$x1F <- summary( iv1_step1 )$fstatistic[ 1 ]

  iv1_step1 <- lm( x2~v-1, temp )
  temp$x2hat <- iv1_step1$fitted.values
  temp$x2F <- summary( iv1_step1 )$fstatistic[ 1 ]

  # IV 1 - STEP 2
  iv1_step2 <- lm( y~x1hat+x2hat, temp )
  beta <- iv1_step2$coefficients
  temp$uhat <- temp$y - beta[ 1 ] - beta[ 2 ] * temp$x1 - beta[ 3 ] * temp$x2 # this needs to be checked I don't understand why we use x1 and x2 with the estimated coefficients from a regression with x1hat and x2hat
  temp$uhat2 <- temp$uhat^2

  # IV 2
  # Feenstra (1994) page 165
  # efficient estimates can be obtained by correcting the IV estimator for
  # heteroscedasticity: the identification condition (12) implies that the
  # variance of u_i changes over i
  # IV 2 - STEP 1
  iv2_step1 <- lm( uhat2~v-1, temp )
  temp$uhat2hat <- iv2_step1$fitted.values

  temp <- temp %>%
    mutate( shat = sqrt( uhat2hat ),
            ones = 1,
            ystar = y / shat,
            x1hatstar = x1hat / shat,
            x2hatstar = x2hat / shat,
            onesstar = ones / shat )

  # IV 2 - STEP 2
  model <- lm( ystar~x1hatstar+x2hatstar+onesstar-1, temp )

  theta1_iv <- model$coefficients[ 1 ]
  theta2_iv <- model$coefficients[ 2 ]

  rho <- ifelse( theta2_iv > 0,
                  0.5+sqrt( .25 - 1/(4+theta2_iv^2/theta1_iv)),
                  0.5-sqrt( .25 - 1/(4+theta2_iv^2/theta1_iv)) )

  sigma <- 1+(2*rho-1)/((1-rho)*theta2_iv)

  # calculate standard errors ----------------------------------------

  # test (A2) in Feenstra Appendix
    Z <- model.matrix( ~v-1, as.data.frame( cbind( "v" = temp$v, 1 ) ) )
    X <- as.matrix( cbind( 1, temp %>% ungroup() %>% select( x1, x2 ) ) )

  # replicate (A4) in Appendix of Feenstra 1994
    Xhat <- Z %*% solve( t( Z ) %*% Z ) %*% t( Z ) %*% X
    S <- diag( temp$uhat2hat )
    #theta_star <- solve( t( Xhat ) %*% solve( S ) %*% Xhat ) %*% t( Xhat ) %*% solve( S ) %*% Y

  # now calculate standard errors of coefficients (theta 0, 1, 2 )
    Vconsttheta <- solve( t( Xhat ) %*% solve( S ) %*% Xhat )

    Vtheta <- Vconsttheta[ 2:3, 2:3 ]

    SEconsttheta <- sqrt( diag( Vconsttheta ) )

  # calculate standard errors for sigma and rho (see Appendix B Feenstra 1994)
    M <- matrix( c( (sigma-1)^3*(1-rho),
                    -(sigma-1)^2*(1-rho)^2*(1-2*rho),
                    (sigma-1)^2*(1-rho),
                    2*(sigma-1)*rho*(1-rho)^2 ),
                 ncol = 2,
                 byrow = T )

    Vsigma_rho <- t(M) %*% Vtheta %*% M

    SEsigma_rho <- sqrt( diag( Vsigma_rho ) )

  # calculate confidence ellipse
    confidence_ellipse <- car::confidenceEllipse( lm(ystar~x1hatstar+x2hatstar+onesstar-1, temp),
                                                  vcov.=Vconsttheta,
                                                  dfn = c( 2, length( unique( temp$v ) )-3),
                                                  draw = F, levels = conf_int )

    df_conf_ellipse <- as.data.frame( confidence_ellipse ) %>%
      rename( "theta1" = x, "theta2" = y ) %>%
      mutate( rho = ifelse( theta2 > 0,
                            0.5+sqrt( .25 - 1/(4+theta2^2/theta1)),
                            0.5-sqrt( .25 - 1/(4+theta2^2/theta1)) ),
              sigma = 1+(2*rho-1)/((1-rho)*theta2))

    sigma_lb <- min( df_conf_ellipse$sigma, na.rm = T )
    sigma_ub <- max( df_conf_ellipse$sigma, na.rm = T )


  # Broda and Weinstein (2006) grid search approach
    if( bw_2006 == T ){

      # Broda and Weinstein (2006) Grid Search if necessary
      # (for unreasonable values of sigma1)
      df_grid <- NULL

      if( sigma < 1 | is.na( sigma ) | is.nan( sigma ) ){

        # Here we add B-W Grid search for negative values of theata hat
        # we will specify many rhos and sigmas to generate thetas and using those
        # we create residuals for each observation. Then we square, divide by T,
        # then sum over all. This will create a sum for every combination of rho and
        # sigma. The smallest is the pair of parameters we are looking for.
        # As in standard OLS you look for the estiamtes that minimize the sum of
        # squared residuals.

        # define grid search parameters
        # B-W 2006

        df_grid <- expand.grid( "sigma" = seq( bw_sigma_min, bw_sigma_max, bw_sigma_step ), "rho" = seq( bw_rho_min, bw_rho_max, bw_rho_step ) ) %>%
          # calculate theta 1 and theta 2
          mutate( theta1 = rho/((sigma-1)^2*(1-rho)),
                  theta2 = (2*rho-1)/((sigma-1)*(1-rho)))

        mat_theta <- as.matrix( df_grid[ , c( "theta1", "theta2" ) ] )
        mat <- as.matrix( temp[ , c( "y", "x1hat", "x2hat" ) ] )

        residuals <- t( mat[ , rep( 1, nrow( mat_theta ) ) ] ) - mat_theta %*% t( mat[ , 2:3 ] )

        residuals <- residuals - as.matrix( rowMeans( residuals ) )[ , rep( 1, ncol( residuals ) ) ] # subtract mean as you estimated the model with onestar: lm( Ystar~X1hatstar+X2hatstar+onesstar-1, temp )

        # calculate variety means
        variety_means <- do.call( cbind, lapply( unique( temp$v ), function( VARIETY ){

          idx <- which( temp$v == VARIETY )
          rowMeans( residuals[ , idx, drop = F ] )^2

        }) )

        df_grid$ssr <- rowSums( variety_means )

        idx_min   <- which( df_grid$ssr == min( df_grid$ssr, na.rm = T ) )[ 1 ]
        sigma     <- df_grid[ idx_min, "sigma" ]
        rho       <- df_grid[ idx_min, "rho" ]
        theta1_iv <- df_grid[ idx_min, "theta1" ]
        theta2_iv <- df_grid[ idx_min, "theta2" ]

        # confidence intervals not yet calculated for grid search approach
        SEconsttheta <- rep( NA, 3 )
        SEsigma_rho <- rep( NA, 2 )
        sigma_lb <- NA
        sigma_ub <- NA
      }

    }


  out <- data.frame( "nObs"       = nrow( temp ),
                     "nVarieties" = length( unique( temp$v ) ), # +1 due to reference variety
                     "theta1"     = theta1_iv,
                     "theta2"     = theta2_iv,
                     "rho"        = rho,
                     "sigma"      = sigma,
                     "theta1_se"  = SEconsttheta[ 1 ],
                     "theta2_se"  = SEconsttheta[ 2 ],
                     "rho_se"     = SEsigma_rho[ 2 ],
                     "sigma_se"   = SEsigma_rho[ 1 ],
                     "sigma_lb"   = sigma_lb,
                     "sigma_ub"   = sigma_ub,
                     check.names  = F )

  return( out )

}




