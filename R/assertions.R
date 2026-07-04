# assertions.R

assert_string = function(x, arg = deparse(substitute(x))) {
	if (!rlang::is_string(x)) {
		stop(sprintf("`%s` must be a string.", arg), call. = F)
	}

	invisible(x)
}

assert_nonempty_string = function(x, arg = deparse(substitute(x))) {
	if (!(rlang::is_string(x) && nzchar(x))) {
		stop(sprintf("`%s` must be a non-empty string.", arg), call. = F)
	}

	invisible(x)
}

validate_label = function(x, arg = deparse(substitute(x))) {
	if (is.null(x)) return(invisible(x))
	if (!rlang::is_string(x) || !nzchar(x)) stop(glue("`{arg}` must be a non-empty character scalar."), call. = F)
	invisible(x)
}

# rlang::is_scalar_character(NA_character_)
# rlang::is_string("")
# rlang::is_string(NA_character_)
# rlang::is_string(character(1))
# nzchar("sd")
# is_scalar_double(NA_real_)
# is.na(NA_character_)
# is.na(NaN)
# is.na(Inf)
# is.finite(Inf)
# is_scalar_vector(double(1))
# integer(1)
# character(1)
