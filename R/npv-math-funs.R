#' npv-math-funs.R
#' 

#' Calculate sums of a vector of geometric series
#' 
#' Geometric series are of the form Sn = a + ar + ar^2 + ... + ar^(n-1)
#' The formula for the sum of a finite series is based on integer n, but the formula can be calculated for real values of n.
#' Infinite series converge for -1 < r < 1
#' @param a vector of the inital terms
#' @param r vector of common ratios
#' @param n vector of the number of terms (may be >= 0)
#' @return a numeric vector of the sums. Value is NaN for series that do not converge. Errors on non-numeric input or inputs of unequal (non-scalar) length
#' 
#' @export
#' @examples 
#' sum_of_geometric_series(1, -1/3, 2)
sum_of_geometric_series <- function(a, r, n) {
  
  if (! is.numeric(a) & 
      is.numeric(r) & 
      is.numeric(n)) stop("Sum of geometric series requires terms a, r, n to all be numeric")
  
  if (some(list(a, r), is.infinite)) stop("All values for the first term (a) and common ratio (r) must be finite.")
  
  if (! all(n >= 0)) stop("Geometric series cannot have a negative number of terms")
  
  case_when(
    a == 0 ~ 0,
    n == 0 ~ 0,
    
    r <= -1 & !is.finite(n) ~ NaN,
    r >= 1 & !is.finite(n) ~ Inf,
    abs(r) < 1 & !is.finite(n) ~ (a / (1 - r)), # converging infinite case
    
    r == 1 & is.finite(n) ~ a * n,
    is.finite(n) ~ (a * (1 - (r^n)) / (1 - r)), # normal case
    TRUE ~ NA_real_
  )
  
}


#' Net present value of fixed periodic flows, vectorized over flows
#' 
#' Each flow to be summed is represented by scalar pmt, n, r. Function is vectorized to compute many NPV's of many such series of periodic flows.
#' 
#' @param pmt fixed payment amount
#' @param n the number of payments (may be fractional)
#' @param r the discount rate
#' @param first_pmt_t when the first payment occurs. The default 1 indicates that the first payment occurs at the end of the first period, whereas a value of 0 would indicate the first payment occurs at the beginning of the first period.
#' @param disc_to_t when to discount the series to. The default 0 indicates that the series of payments would be discounted to the beginning of the first period.
#' @export
#' @examples 
#' npv_fixed_pmts(100, 1, .05)
npv_fixed_pmts <- function(pmt, n, r, 
                            first_pmt_t = 1, disc_to_t = 0) {
  
  if (! is.numeric(pmt) & 
      is.numeric(n) & 
      is.numeric(r)) stop("NPV of fixed payments requires terms a, r, n to all be numeric")
  
  if (some(list(pmt, n, r), is.infinite)) stop("All values for the terms must be finite.")
  
  if (! all(n >= 0)) stop("Must not be a negative number of periods.")
  
  common_ratios <- 1 / (1 + r)
  initial_values <- pmt * (common_ratios^(first_pmt_t - disc_to_t))
  return(sum_of_geometric_series(initial_values, common_ratios, n))
}


#' Net present value of series of flows (t, pmt)
#' 
#' @param t numeric vector of x (time) values of payments
#' @param pmt numeric vector of corresponding payment amounts
#' @param r the discount rate (numeric scalar)
#' @param first_pmt_t when the first payment occurs. The default 1 indicates that the first payment occurs at the end of the first period, whereas a value of 0 would indicate the first payment occurs at the beginning of the first period.
#' @param disc_to_t when to discount the series to. The default 0 indicates that the series of payments would be discounted to the beginning of the first period.

npv_xy_pmts <- function(t, pmt, r, disc_to_t = 0) {
  
  common_ratio <- 1 / (1 + r)
  
  return(pmt * common_ratio^(t - disc_to_t))
}


#' Calculate equivalent per-period value from present value for a vector of projects
#' 
#' @param pv present values (numeric vector) to convert to per-period amounts
#' @param n number of periods to spread payments
#' @param r the real disount rate
#' @param first_pmt_t when the first payment occurs. The default value 1 indicates the first payment is one period away.
#' @return equivalent per-period real amount, with first payment one period away
equiv_annualized_value <- function(pv, n, r, first_pmt_t = 1) {
  
  common_ratios <- 1 / (1 + r)
  
  case_when(
    r == 1 ~ pv / n,
    
    TRUE ~ (pv / (common_ratios^first_pmt_t)) * (1 - common_ratios) / (1 - common_ratios^n)
  )
  
}