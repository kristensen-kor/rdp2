#' Check if Values are Present in a Variable
#'
#' The `has` function checks if specific values are present within the elements of a given variable. The input variable can be a list or a vector.
#'
#' @param var A variable to check, which can be a list or a vector.
#' @param ... One or more values to check for within `var`.
#'
#' @return A logical vector indicating if the specified values are found within the elements of `var`. If a single value is provided, it checks for the presence of that value. If multiple values are provided, it checks for the presence of any of those values.
#'
#' @examples
#' # Check for a single value in a list
#' has(list(c(1, 2, 3), c(4, 5, 6)), 2)
#'
#' # Check for multiple values in a list
#' has(list(c(1, 2, 3), c(4, 5, 6)), 2, 5)
#'
#' # Check for a single value in a vector
#' has(c(1, 2, 3, 4, 5), 3)
#'
#' # Check for multiple values in a vector
#' has(c(1, 2, 3, 4, 5), 3:4, 6)
#'
#' @export
has = function(var, ...) {
	values = c(...)

	if (length(values) == 0) {
		warning("No values provided to has() to check for presence.", call. = F)
		return(rep(F, length(var)))
	}

	if (is.list(var)) {
		if (length(values) == 1) {
			has_list_single_cpp(var, values)
		} else {
			has_list_multiple_cpp(var, values)
		}
	} else {
		if (length(values) == 1) {
			result = var == values
			result[is.na(result)] = F
			result
		} else {
			var %in% values
		}
	}
}
