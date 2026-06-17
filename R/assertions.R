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
