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

#' Filters and returns unique, sorted, non-NA values from a vector.
#' @export
mrcheck = \(xs) mrcheck_cpp(xs)
# reference implementation:
# mrcheck = function(xs) xs[!is.na(xs)] |> unique() |> sort()

#' Adds a value to a multiple-response set, ensuring uniqueness and order.
#' @export
add_to_mrset = \(vec, value) add_to_mrset_cpp(vec, value)
# reference implementation:
# add_to_mrset = function(var, value) c(var, value) |> mrcheck()



# Internal method to determine validity based on input type.
.is_valid = function(...) UseMethod(".is_valid")
.is_valid.list = function(xs) lengths(xs) > 0
.is_valid.default = function(xs) !is.na(xs)

#' Checks if elements are valid based on their type.
#' @export
is_valid = function(xs) .is_valid(xs)


.var_empty = function(...) UseMethod(".var_empty")
.var_empty.list = function(xs) lengths(xs) == 0
.var_empty.default = function(xs) is.na(xs) | xs == ""

# Checks if variables are empty.
var_empty = function(...) .var_empty(...)




.recode = function(...) UseMethod(".recode")
.recode.list = function(var, ...) map(var, \(x) case_match_vec_copy(x, ...) |> mrcheck())
.recode.default = function(var, ...) case_match_vec_copy(var, ...)

#' Recode function for variables based on their type.
#' @export
recode = function(...) .recode(...)


# Performs case-based matching and replacement on a vector.
case_match_vec_copy = function(xs, ...) {
	cases = rlang::list2(...)
	result = xs

	for (case in cases) {
		condition = rlang::eval_tidy(rlang::f_lhs(case))
		value = rlang::eval_tidy(rlang::f_rhs(case))
		mask = if (length(condition) == 1) xs == condition else xs %in% condition
		result[mask] = value
	}

	result
}


.transfer = function(...) UseMethod(".transfer")
.transfer.list = function(var, ...) map(var, \(x) case_match(x, ...) |> mrcheck())
.transfer.default = function(var, ...) case_match(var, ...)

#' Transfers values of variables based on matching conditions.
#' @export
transfer = function(...) .transfer(...)
