# This code is from Vehtari + Gelman, 2014
# http://www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf
#
colVars <- function(a) {
  n <- dim(a)[[1]]; 
  c <- dim(a)[[2]]; 
  return(.colMeans(((a - matrix(.colMeans(a, n, c), 
                                nrow = n, ncol = c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))
}

## convenience functions
rhat <- function(fit){ summary(fit)$summary[,"Rhat"] }
ess  <- function(fit){ summary(fit)$summary[,"n_eff"] }


# original version from http://www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf
waic <- function(stanfit){
  log_lik <- extract (stanfit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}

# log-sum-exp trick to avoid underflow in exp(log_lik)
waic.logsumexp <- function(stanfit){
  log_lik <- extract (stanfit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)

  ## log-sum-exp trick
  offset <- log_lik[cbind(max.col(abs(t(log_lik))), 1:n)] # column-wise most extreme value
  lpd <- log(1./S)+log(colSums(exp(sweep(log_lik, 2, offset))))+offset

  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}


# arbitrary-precision library to avoid underflow in exp(log_lik)
waic.mpfr <- function(stanfit, prec=120){
  require(Rmpfr)
  log_lik <- extract (stanfit, "log_lik")$log_lik
  log_likp <- mpfr(log_lik, prec)
  
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- as.numeric(log(colMeans(exp(log_likp)))) # run in arbitrary precision
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}
