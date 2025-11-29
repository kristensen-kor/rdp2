#' @include rdp2.R

# DS class constraints
#
# 1. `data` columns must be one of:
#      • numeric vectors
#      • character vectors
#      • lists of numeric vectors (multiple-response)
#
# 2. Numeric vectors:
#      • may contain finite numbers or NA
#      • must NOT contain NaN or ±Inf
#
# 3. Multiple-response vectors (lists of numeric vectors):
#      • each element must be a sorted numeric vector
#      • elements may be empty (numeric(0))
#      • elements must contain only finite numbers (no NA, NaN, ±Inf)
#      • element lengths may differ
#
# 4. Variable labels:
#      • must be NULL or a scalar character string (empty allowed)
#
# 5. Value labels (if present):
#      • must be a named numeric vector
#      • numeric values must be finite, unique, and sorted
#      • names must not be NA (empty names allowed)

# Validates dataset.
DS$set("public", "validate", function() {
	if (!tibble::is_tibble(self$data)) stop("`data` must be a tibble. It appears to be a regular data.frame or other type.", call. = F)
	if (!is.list(self$var_labels))     stop("`var_labels` must be a list.", call. = F)
	if (!is.list(self$val_labels))     stop("`val_labels` must be a list.", call. = F)

	is_scalar_character = \(x) is.character(x) && length(x) == 1 && !is.na(x)

	check_val_labels = function(var_name) {
		val = self$val_labels[[var_name]]

		if (!is.numeric(val))                   stop(glue::glue("`val_labels[[\"{var_name}\"]]` must be numeric vector."), call. = F)
		if (is.null(names(val)))                stop(glue::glue("`val_labels[[\"{var_name}\"]]` must be named numeric vector."), call. = F)
		if (anyNA(names(val)))                  stop(glue::glue("`val_labels[[\"{var_name}\"]]` can't contain NA names."), call. = F)
		if (!all(is.finite(val)))               stop(glue::glue("`val_labels[[\"{var_name}\"]]` must contain only finite numbers (no NA, NaN, or Inf)."), call. = F)
		if (length(unique(val)) != length(val)) stop(glue::glue("`val_labels[[\"{var_name}\"]]` must contain unique values."), call. = F)
		if (is.unsorted(val))                   stop(glue::glue("`val_labels[[\"{var_name}\"]]` must be sorted."), call. = F)
	}

	for (var_name in self$variables) {
		var = self$data[[var_name]]

		if (!(is.null(self$var_labels[[var_name]]) || is_scalar_character(self$var_labels[[var_name]]))) {
			stop(glue::glue("`var_labels[[\"{var_name}\"]]` must be NULL or a scalar character."), call. = F)
		}

		if (is.character(var)) next

		if (!is.null(self$val_labels[[var_name]])) check_val_labels(var_name)

		if (is.numeric(var)) {
			if (any(is.infinite(var)) || any(is.nan(var)))    stop(glue::glue("Variable \"{var_name}\" must contain only numbers or NA (no Inf/NaN)."), call. = F)
			next
		}

		if (is.list(var)) {
			if (!all(vapply(var, is.numeric, logical(1))))    stop(glue::glue("Variable \"{var_name}\" must contain only numeric vectors."), call. = F)
			if (!all(var |> map_lgl(\(x) all(is.finite(x))))) stop(glue::glue("Variable \"{var_name}\" must contain only numeric vectors with finite values (no NA, NaN, or Inf)."), call. = F)
			if (any(var |> map_lgl(\(x) is.unsorted(x))))     stop(glue::glue("Variable \"{var_name}\" must contain only sorted numeric vectors."), call. = F)
			next
		}

		stop(glue::glue("Variable \"{var_name}\" is of unsupported type."), call. = F)
	}

	message("Dataset is valid.")
	invisible(T)
})

# Tries to fix common issues
DS$set("public", "auto_repair", function() {
	for (var_name in self$variables) {
		var = self$data[[var_name]]

		if (is.character(var)) next
		if (is.logical(var) || is.integer(var)) self$data[[var_name]] = as.numeric(var)

		if (is.numeric(var) && (any(is.infinite(var)) || any(is.nan(var)))) {
			self$data[[var_name]][is.infinite(var) | is.nan(var)] = NA
		}

		if (is.list(var)) {
			mask = vapply(var, \(x) is.logical(x) | is.integer(x) | is.null(x), logical(1))
			self$data[[var_name]][mask] = self$data[[var_name]][mask] |> map(as.numeric)
			var = self$data[[var_name]]
			if (!all(var |> map_lgl(\(x) all(is.finite(x))))) self$data[[var_name]] = self$data[[var_name]] |> map(mrcheck)
			var = self$data[[var_name]]
			if (any(var |> map_lgl(\(x) is.unsorted(x)))) self$data[[var_name]] = self$data[[var_name]] |> map(mrcheck)
		}
	}
})
