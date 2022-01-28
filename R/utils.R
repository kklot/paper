#' List to tibble handling NULL element
#' 
#' 
#' @param x a list with possible NULL element
#' @return a tibble with NULL in elements replaced by NA
#' @export
list_as_tibble <- function(x) 
{
    tibble::as_tibble(purrr::modify_if(x, is.null, ~NA))
}