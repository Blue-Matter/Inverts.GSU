#' Condition an operating model for Green Sea Urchin
#'
#' Uses the RCM model of OpenMSE to fit an operating model to data
#'
#' @param In A list object of class ('In') that includes the operating model parameters (slot 2, class OM) and data (slot 3, class RCMinput) for conditioning
#' @param sims Integer or vector of integers - the number of simulations for the operating model (e.g. 96) or the specific vector of simulations for the operating model (e.g. 13:48)
#' @param max_F Positive real number - the maximum instantaneous mortality rate in any historical time step
#' @param comp_like The likelihood function used for composition data (age / length) c("multinomial", "lognormal", "mvlogistic", "dirmult1", "dirmult2") see ?RCM
#' @param resample Logical - should parameters be drawn from the variance covariance matrix of the MLE fit (T) or just the MLE parameter values (F)
#' @param parallel Logical - should model fitting use multiple cores
#' @param silent Logical - should all RCM messages be repressed (ie not read out onto the console)?
#' @examples
#' cond.GSU("In.GSU.12")
#' @author T. Carruthers
#' @seealso \link{RCM}
#' @export
cond.GSU = function(RCMinput, sims = 12, max_F=3, comp_like="multinomial", resample = T, parallel=F, silent=T){
  if(parallel){
    setup()
    cores=parallel::detectCores()/2
  }
  if(length(sims) == 1) simy = 1:sims
  if(length(sims) > 1) simy = sims
  OM = SubCpars(RCMinput$OM, simy)

  RCMfit = RCM(OM, RCMinput[[3]], selectivity = "logistic_length",
           s_selectivity = RCMinput[[3]]@Misc$s_selectivity,
           start = list(ivul_par = RCMinput[[3]]@Misc$ivul_par),
           map = list(ivul_par = RCMinput[[3]]@Misc$map_ivul_par),
           max_F = max_F, mean_fit = T, condition = "catch",
           cores = cores,  drop_nonconv=T,drop_highF=T,comp_like = comp_like, silent=silent)
  RCMfit@OM@Name = paste0("Green sea urchin Stat. area ",RCMinput[[1]])
  RCMfit

}



