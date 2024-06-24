#' @include rdp2.R

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

	if (length(values) == 1) {
		has1(var, values)
	} else {
		has_mult(var, values)
	}
}


has1 = function(...) UseMethod("has1")

has1.list = function(var, value) {
	result = logical(length(var))

	for (i in seq_along(var)) {
		result[i] = any(var[[i]] == value)
	}

	result
}

has1.default = function(var, value) {
	xs = var == value
	xs[is.na(xs)] = F
	xs
}


has_mult = function(...) UseMethod("has_mult")

has_mult.list = function(var, values) {
	result = logical(length(var))

	for (i in seq_along(var)) {
		result[i] = any(var[[i]] %in% values)
	}

	result
}

has_mult.default = function(var, values) var %in% values


# replace with C++ version? 3x faster

# has1.list = function(var, value) {
# 	has1_list_cpp(var, value)
# }
#
# Rcpp::cppFunction("
# LogicalVector has1_list_cpp(List var, int value) {
#   int n = var.size();
#   LogicalVector result(n, false);
#
#   for (int i = 0; i < n; i++) {
#     IntegerVector vec = var[i];
#     int len = vec.size();
#     for (int j = 0; j < len; j++) {
#       if (vec[j] == value) {
#         result[i] = true;
#         break;
#       }
#     }
#   }
#
#   return result;
# }
# ")
