# helper_funs.R

#' Helper to be used in combination with subsetting operators
#' 
#' @examples 
#' df <- tibble(x = c(1, 1, 2), y = c("a", "b", "b"), z = c(100, 105, 200))
#' group_by(df, x) %>% mutate(prop = z / z[y == "a"])
#' group_by(df, x) %>% mutate(prop = z / z[y == "a"] %ifempty% NA_real_)
#' 
#' df2 <- tibble(x = c(1, 1, 1), y = c("a", "a", "b"), z = c(100, 105, 200))
#' group_by(df2, x) %>% mutate(prop = z / z[y == "a"])
#' group_by(df2, x) %>% mutate(prop = z / z[y == "a"] %ifempty% NA_real_)
`%ifempty%` <- function (x, y) 
{
  if (length(x) > 1) stop("length of `x` must be 1")
  
  if (is_empty(x)) {
    y
  }
  else {
    x
  }
}