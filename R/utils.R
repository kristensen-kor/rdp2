# utils.R


#' Formats elapsed time for display in seconds.
#' @export
elapsed_fmt = \(x) x |> as.numeric(units = "secs") |> round(1) |> paste0("s")

# Wrap a string in quotes
dquote = \(x) sprintf("\"%s\"", x)


#' Concatenates variable names with an underscore separator.
#' @export
paste_vars = function(...) {
	args = list(...)
	grid = expand.grid(rev(args), KEEP.OUT.ATTRS = F, stringsAsFactors = F)
	pmap_chr(grid[, rev(seq_along(args)), drop = F], \(...) paste(..., sep = "_"))
}

#' Returns unique, sorted, finite values from a numeric vector.
#' @export
mrcheck = \(xs) mrcheck_cpp(xs)
# reference implementation:
# mrcheck = function(xs) xs[!is.finite(xs)] |> unique() |> sort()

#' Adds a value to a multiple-response set, ensuring uniqueness and order.
#' @export
add_to_mrset = \(vec, value) add_to_mrset_cpp(vec, value)
# reference implementation:
# add_to_mrset = function(var, value) c(var, value) |> mrcheck()




#' Check whether variable elements contain no response
#'
#' For ordinary vectors, missing values are empty. Empty strings are also
#' considered empty for character vectors. For multiple-response list
#' columns, elements containing no selected values are empty.
#'
#' @param x A variable vector or multiple-response list column.
#'
#' @return A logical vector indicating which elements are empty.
#' @export
is_empty = function(x) {
	if (is.list(x)) {
		lengths(x) == 0
	} else if (is.character(x)) {
		is.na(x) | x == ""
	} else {
		is.na(x)
	}
}


#' Check whether variable elements contain a response
#'
#' This is the inverse of `is_empty()`.
#'
#' @inheritParams is_empty
#'
#' @return A logical vector indicating which elements contain a response.
#' @export
is_present = function(x) !is_empty(x)


#' @export
is_valid = function(x) {
	.Deprecated("is_present", package = "rdp2")
	is_present(x)
}

#' @export
var_empty = function(x) {
	.Deprecated("is_empty", package = "rdp2")
	is_empty(x)
}


# legacy stuff

# Internal method to determine validity based on input type.
# .is_valid = function(...) UseMethod(".is_valid")
# .is_valid.list = function(xs) lengths(xs) > 0
# .is_valid.default = function(xs) !is.na(xs)

#' #' Checks if elements are valid based on their type.
#' #' @export
#' is_valid = function(xs) .is_valid(xs)

# .var_empty = function(...) UseMethod(".var_empty")
# .var_empty.list = function(xs) lengths(xs) == 0
# .var_empty.default = function(xs) is.na(xs) | xs == ""

# # Checks if variables are empty.
# var_empty = function(...) .var_empty(...)






# Evaluates and validates a row condition against a dataset.
eval_row_mask = function(condition, data) {
	mask = rlang::eval_tidy(condition, data = data)
	n = nrow(data)

	if (!is.logical(mask)) {
		stop(glue("condition must evaluate to a logical vector."), call. = F)
	}

	if (length(mask) == 1) {
		mask = rep(mask, n)
	} else if (length(mask) != n) {
		stop(glue("condition must have length 1 or {n}, not {length(mask)}."), call. = F)
	}

	mask[is.na(mask)] = F
	mask
}

# eval_row_mask that defaults to T on NULL condition
eval_optional_row_mask = function(condition, data) {
	if (rlang::quo_is_null(condition)) {
		rep(T, nrow(data))
	} else {
		eval_row_mask(condition, data)
	}
}
