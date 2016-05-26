#' An arima function
#'
#' This function returns some diagnostics from auto.arima as JSON
#' serializable list.
#' @param x vector of observations
#' @keywords arima
#' @export
#' @examples
#' arima_all(WWWusage)

arima_all <-function(x){
  fit<-auto.arima(x)
  bt<-Box.test(fit$residuals,
               lag=5,
               fitdf=length(fit$model$phi)+length(fit$model$theta))
  return(list("p.value"=bt$p.value,
              "ub"=as.vector(fit$resid+sd(fit$resid)),
              "lb"=as.vector(fit$resid-sd(fit$resid)),
              "resid"=as.vector(fit$resid),
              "phi"=as.vector(fit$model$phi),
              "theta"=as.vector(fit$model$theta),
              "D"=as.vector(fit$model$D)))
  
}
