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
.recode.list = function(var, lhs_list, rhs_vec) map(var, \(x) case_match_vec_copy(x, lhs_list, rhs_vec) |> mrcheck())
.recode.default = function(var, lhs_list, rhs_vec) case_match_vec_copy(var, lhs_list, rhs_vec)

#' Recode function for variables based on their type.
#' @export
recode = function(xs, ...) {
	cases = rlang::list2(...)
	n = length(cases)

	if (n == 0) return(xs)

	lhs_list = vector("list", n)
	rhs_vec = numeric(n)

	for (i in seq_len(n)) {
		lhs = rlang::eval_tidy(rlang::f_lhs(cases[[i]]))
		rhs = rlang::eval_tidy(rlang::f_rhs(cases[[i]]))

		if (!is.numeric(lhs)) stop("Left-hand side of recode rules must be numeric values (e.g., 1, 1:3), not logical or expressions.", call. = F)
		if (length(rhs) != 1 || (!is.numeric(rhs) && !is.na(rhs))) stop("Right-hand side of a recode must be a numeric scalar (length 1).", call. = F)

		lhs_list[[i]] = lhs
		rhs_vec[i] = rhs
	}

	lhs_values = unlist(lhs_list)
	if (any(duplicated(lhs_values))) stop("Overlapping recode patterns detected for values: ", paste(unique(lhs_values[duplicated(lhs_values)]), collapse = ", "), call. = F)

	.recode(xs, lhs_list, rhs_vec)
}

# Performs case-based matching and replacement on a vector.
case_match_vec_copy = function(xs, lhs_list, rhs_vec) {
	result = xs
	for (i in seq_along(lhs_list)) {
		mask = if (length(lhs_list[[i]]) == 1) xs == lhs_list[[i]] else xs %in% lhs_list[[i]]
		result[mask] = rhs_vec[i]
	}
	result
}


.transfer = function(...) UseMethod(".transfer")
.transfer.list = function(var, ...) map(var, \(x) case_match(x, ...) |> mrcheck())
.transfer.default = function(var, ...) case_match(var, ...)

#' Transfers values of variables based on matching conditions.
#' @export
transfer = function(...) .transfer(...)
