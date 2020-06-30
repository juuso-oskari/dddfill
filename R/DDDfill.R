rm(list=ls())
#' function to fill missing ddd-values from drug purchase data
#'
#' Fills the missing ddd-values in the data according to following logic:
#' by default finds the closest prior vnr-matching purchase (if prior not found, finds closest after).
#' However, if k_mean given to be different from 1, compares the match with the trimmed mean of (up to) k_mean
#' matching purchases both sides to the purchase, and uses that if too big of a difference (reduces possible wrong entry effect).
#' For match, calculates the ddd/kust -ratio, which can be then used to fill the missing ddd-value by:
#' kust of ddd-missing purchase * ddd/kust -ratio (of closest match or mean of kmean matches around the purchase)
#' @importFrom haven read_sas
#' @import data.table
#' @import sas7bdat
#' @param read_path path to data (supported fileforms: .sas7bdat)
#' @param k_mean 1 (default), >1 compares the closest prior match with (up to) k_mean matches on both sides
#' @param tol 10 (default), tolerance of rejecting the closest value when comparing to the mean value
#' @param pvmvar "otpvm" (default), what is the column id for purchase dates
#' @param vnrovar "vnr" (default), what is the column id for purchase code
#' @param kustvar "kust" (default), what is the column id for cost of the purchase
#' @param dddvar "ddd" (default), what is the column id for ddd
#' @param restoreOrder 1 (default), with default restores the ordering to original,
#'  with 0 the returned data.table is ordered by pvmvar and then by vnrovar
#' @return ddd filled data.table
#' @examples
#' # example call with alternative values for some of the default valued parameters:
#' ddd_filled_data.table <- DDDfill("path_to_original_data.sas7bdat", k_mean = 4)


DDDfill <- function(read_path, k_mean = 1, tol = 10, pvmvar = "otpvm", vnrovar = "vnr", kustvar = "kust", dddvar = "ddd", restoreOrder = 1){
  require(haven)
  require(sas7bdat)
  require(data.table)
  dt <- setDT(read_sas(read_path))
  dt[, ddd:=as.numeric(get(dddvar))]
  dt[, vnr:=as.numeric(get(vnrovar))]
  dt[, kust:=as.numeric(get(kustvar))]
  dt[, otpvm:=as.Date(get(pvmvar), format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))]
  dt[otpvm<as.Date("2014-01-01") & (kust-43 > 0), kust:=(kust-43)] # remove dispatching cost from purchase cost
  dt[otpvm>=as.Date("2014-01-01") & (kust-239 > 0), kust:=(kust-239)]
  if(min(dt$kust) < 0) {
    message("Negative values in kust")
  }
  dt$index <- seq.int(nrow(dt)) # to restore the original order at the end of function
  dt <- dt[order(vnr, otpvm)]

  ii <- which(is.na(dt[,ddd]))
  fx <- function(i, kmean=1) {
    nearest <- function(i, dir){
      if( (i+1*dir) == 0 | (i+1*dir) == (nrow(dt) + 1)){
        NA
      }else if( (dt[i+1*dir, vnr]==dt[i, vnr]) ) {
        ifelse(!is.na(dt[i+1*dir,ddd]), dt[i+1*dir,ddd] / dt[i+1*dir,kust], nearest(i+1*dir, dir))
      }else{
        NA
      }
    }
    nearest_prior <- nearest(i, -1)
    res<-ifelse(!is.na(nearest_prior), dt[i, kust] * nearest_prior , dt[i, kust] * nearest(i, 1))
    if(kmean!=1){
      lb <- ifelse((i - ceiling(kmean/2))>0, (i - ceiling(kmean/2)), 1)
      ub <- ifelse((i + floor(kmean/2))<nrow(dt)+1, (i + floor(kmean/2)), nrow(dt))
      d <- dt[lb: ub][vnr==dt[i, vnr],]
      mean_res <- dt[i, kust] * mean(unlist(d[, ddd] / d[, kust]), trim = 0.3, na.rm = TRUE)
      ifelse(!is.na(mean_res) & abs(mean_res-res)>tol, mean_res, res)
    }else{
      res
    }
  }
  res <- lapply(ii, fx, kmean=k_mean)
  res <- unlist(res)
  if(all(is.na(res))) {
    message("All results NA")
  }
  dt$ddd[ii] <- res
  if(restoreOrder == 1)
    dt <- dt[order(index)]
  dt[,index:=NULL]
  return(dt)
}
