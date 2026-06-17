# ds-vars-set-add.R

#' @include rdp2.R class-ds-where.R

normalize_value = function(value) {
	if (length(value) != 1L) stop("`value` must be a scalar.", call. = F)
	if (is.nan(value)) stop("`value` cannot be `NaN`.", call. = F)
	if (is.na(value)) return(NA)
	if (!(is.integer(value) || is.double(value)) || !is.finite(value)) stop("`value` must be a finite numeric scalar or `NA`.", call. = F)
	value
}

# Sets values of a variable based on provided logical conditions and optionally adds a label.
DS$set("public", "set_if", function(vars, value, condition, label = NULL) {
	# if (!(is.null(label) || (rlang::is_string(label) && nzchar(label)))) stop("Value label must be a non-empty character scalar", call. = F)
	value = normalize_value(value)
	validate_label(label)
	mask = eval_row_mask(rlang::enquo(condition), self$data)

	for (var in self$names({{ vars }})) {
		if (is_multiple(self$data[[var]])) {
			self$data[[var]][mask] = list(mrcheck(value))
		} else {
			self$data[[var]][mask] = value
		}
	}

	if (!is.null(label)) self$add_val_labels({{ vars }}, setNames(value, label))
})

# Shorthand for set_if(..., value = NA).
# Sets values of a variable to NA based on provided logical conditions.
DS$set("public", "set_na_if", function(vars, condition) {
	self$set_if({{ vars }}, NA, {{ condition }})
})

# Adds a value to a multiple-response variable based on provided conditions and optionally adds a label.
DS$set("public", "add_if", function(vars, value, condition, label = NULL) {
	var_names = self$names({{ vars }})
	value = normalize_value(value)
	validate_label(label)
	mask = eval_row_mask(rlang::enquo(condition), self$data)
	if (!all(map_lgl(self$data[var_names], is_multiple))) stop("Error: Expecting variables of multiple type.", call. = F)
	# if (!(is.null(label) || (rlang::is_string(label) && nzchar(label)))) stop("Value label must be a non-empty character scalar", call. = F)

	for (var in var_names) {
		self$data[[var]][mask] = add_to_mc_col_cpp(self$data[[var]][mask], value)
	}

	if (!is.null(label)) self$add_val_labels({{ vars }}, setNames(value, label))
})


# Adds a net value to multiple-response variables based on provided conditions and optionally adds a label.
DS$set("public", "add_net", function(vars, value, ..., label = NULL, filter = NULL) {
	# if (!(is.null(label) || (rlang::is_string(label) && nzchar(label)))) stop("Value label must be a non-empty character scalar", call. = F)

	if (!all(self$data |> select({{ vars }}) |> map_lgl(is_multiple))) stop("Error: Expecting variables of multiple type.", call. = F)
	value = normalize_value(value)
	validate_label(label)

	# filter_quosure = rlang::enquo(filter)
	# filter_mask = rep(T, self$nrow)
	# if (!rlang::quo_is_null(filter_quosure)) filter_mask = eval_row_mask(filter_quosure, self$data)
	filter_mask = eval_optional_row_mask(rlang::enquo(filter), self$data)

	for (var in self$names({{ vars }})) {
		mask = filter_mask & has(self$data[[var]], ...)
		self$data[[var]][mask] = add_to_mc_col_cpp(self$data[[var]][mask], value)
	}

	if (!is.null(label)) self$add_val_labels({{ vars }}, setNames(value, label))
})


# Sets selected variables a value for rows matching the condition.
DSWhere$set("public", "set", \(vars, value, label = NULL) {
	private$ds$set_if({{ vars }}, value, !!private$condition, label)
})

# Shorthand for $set(..., value = NA).
# Sets values of a variable to NA for rows matching the condition.
DSWhere$set("public", "clear", function(...) {
	private$ds$set_na_if(c(...), !!private$condition)
})

# Adds a value to a multiple-response variable based on provided conditions and optionally adds a label.
DSWhere$set("public", "add", \(vars, value, label = NULL) {
	private$ds$add_if({{ vars }}, value, !!private$condition, label)
})

# Adds a net value to multiple-response variables based on provided conditions and optionally adds a label.
DSWhere$set("public", "add_net", \(vars, value, ..., label = NULL) {
	private$ds$add_net({{ vars }}, value, ..., label = label, filter = !!private$condition)
})
