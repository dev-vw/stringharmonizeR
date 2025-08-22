#' Generates a random crosswalk IDs from capital letters and numbers
#'
#' @param len an int, defaulting to 8, specifying char length of crosswalk ID
#'
#' @return a string, defaulting to 8 characters
#' @export
#'
#' @examples
gen_crosswalkid <- function(len = 8) {
  chars <- c(LETTERS, 0:9)

  return(paste0(sample(chars, len, replace = TRUE), collapse = ""))
}


#' Generates a vector of unique crosswalk IDs
#'
#' @param n an int, specifying length of output vector
#' @param len an int, defaulting to 8, specifying char length of crosswalk ID
#'
#' @return a vector of strings
#' @export
#'
#' @examples
gen_crosswalkid_vector <- function(wordvec, len = 8) {
  candidates <- replicate(length(wordvec) * 2, gen_crosswalkid(len))

  candidates <- sample(unique(candidates), length(wordvec), replace = FALSE)

  candidates[which(is.na(wordvec))] <- NA

  return(candidates)
}
