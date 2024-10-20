

# aux


#' @export
base_name = function(xs) {
	warning("base_name() is deprecated. Please use base() instead", call. = F)
	matches(sprintf("^%s_\\d+$", xs))
}

#' @export
base = function(...) matches(paste(sprintf("^%s_\\d+$", c(...)), collapse = "|"))
# base = function(xs) matches(sprintf("^%s_\\d+$", xs))



#' @export
paste_vars = function(...) paste(..., sep = "_")

#' @export
mrcheck = function(xs) {
	xs = xs[!is.na(xs)]
	if (length(xs) == 0) numeric(0) else unique(xs[order(xs)])
}
# mrcheck = function(xs) xs[!is.na(xs)] |> unique() |> sort()

#' @export
add_to_mrset = function(vec, value) {
	if (is.na(value)) return(vec)
	if (length(vec) == 0) return(value)
	if (value %in% vec) return(vec)
	if (value > max(vec, na.rm = TRUE)) return(c(vec, value))
	c(vec, value) |> mrcheck()
}
# add_to_mrset = function(var, value) c(var, value) |> mrcheck()


# #' @export
# is_valid = function(...) UseMethod("is_valid")
#
# #' @export
# #' @method is_valid list
# is_valid.list = function(xs) lengths(xs) > 0
#
# #' @export
# #' @method is_valid default
# is_valid.default = function(xs) !is.na(xs)

.is_valid = function(...) UseMethod(".is_valid")
.is_valid.list = function(xs) lengths(xs) > 0
.is_valid.default = function(xs) !is.na(xs)
#' @export
is_valid = function(xs) .is_valid(xs)


.var_empty = function(...) UseMethod(".var_empty")
.var_empty.list = function(xs) lengths(xs) == 0
.var_empty.default = function(xs) is.na(xs)

var_empty = function(...) .var_empty(...)



# recode = function(...) UseMethod("recode")
# recode.list = function(var, ...) map(var, \(x) case_match(x, ..., .default = x) |> mrcheck())
# recode.default = function(var, ...) case_match(var, ..., .default = var)

.recode = function(...) UseMethod(".recode")
.recode.list = function(var, ...) map(var, \(x) case_match_vec_copy(x, ...) |> mrcheck())
.recode.default = function(var, ...) case_match_vec_copy(var, ...)
#' @export
recode = function(...) .recode(...)


case_match_vec_copy = function(xs, ...) {
	cases = rlang::list2(...)
	result = xs

	for (case in cases) {
		condition = rlang::eval_tidy(rlang::f_lhs(case))
		value = rlang::f_rhs(case)
		mask = if (length(condition) == 1) xs == condition else xs %in% condition
		result[mask] = value
	}

	result
}


.transfer = function(...) UseMethod(".transfer")
.transfer.list = function(var, ...) map(var, \(x) case_match(x, ...) |> mrcheck())
.transfer.default = function(var, ...) case_match(var, ...)
#' @export
transfer = function(...) .transfer(...)



#' @export
elapsed_fmt = function(x) {
	elapsed_in_seconds = as.numeric(x, units = "secs")
	if (elapsed_in_seconds < 60) {
		round(elapsed_in_seconds, 1)
	} else {
		sprintf("%02d:%04.1f", floor(elapsed_in_seconds / 60), round(elapsed_in_seconds %% 60, 1))
	}
}



