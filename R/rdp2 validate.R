#' @include rdp2.R

# DS class constraints
#
# 1. Dataset:
#      • `data` must be a tibble
#      • therefore all columns must have the same length
#      • variable names must be unique, non-missing, and non-empty
#      • columns must be numeric vectors, character vectors, or multiple-response vectors
#
# 2. Numeric vectors:
#      • may be integer or double vectors
#      • may contain finite numbers or NA
#      • must not contain NaN or ±Inf
#      • double is the preferred canonical representation
#
# 3. Character vectors:
#      • may contain character values, NA, or empty strings
#      • must not have value labels
#
# 4. Multiple-response vectors:
#      • must be lists of numeric vectors representing sets of response codes
#      • elements may contain integer or double values
#      • each element must contain sorted, unique values
#      • elements may be empty (numeric(0))
#      • elements must contain only finite values
#      • NA, NaN, and ±Inf are not allowed
#      • element lengths may differ
#      • double vectors are the preferred canonical representation
#
# 5. Metadata containers:
#      • `var_labels` and `val_labels` must be lists
#      • entries are optional; a variable may have no corresponding entry
#      • non-empty lists must have unique, non-missing, and non-empty names
#      • metadata names must correspond to variables present in `data`
#
# 6. Variable labels:
#      • must be NULL or a non-missing scalar character string
#      • empty strings are allowed
#
# 7. Value labels:
#      • must be NULL or a named numeric vector
#      • codes must be finite, unique, and sorted
#      • names must not be NA
#      • empty names are allowed
#      • character variables must not have value labels
#
# 8. Value-label coverage:
#      • a numeric or multiple-response variable may have no value labels, in which case it is treated as numeric
#      • if value labels are present, observed codes should normally have labels
#      • observed codes without labels are allowed, but represent incomplete metadata
#      • value labels for codes not currently observed are allowed

# Removes metadata entries for variables no longer present in the dataset.
DS$set("public", "vacuum", function() {
	self$var_labels = self$var_labels[names(self$var_labels) %in% self$variables]
	self$val_labels = self$val_labels[names(self$val_labels) %in% self$variables]
	invisible(NULL)
})

# Tries to fix common issues
DS$set("public", "repair", function() {
	is_scalar_character = \(x) is.character(x) && length(x) == 1 && !is.na(x)

	repaired = character()

	# data
	if (!is.data.frame(self$data)) stop("`data` must be a data frame or tibble.", call. = F)
	if (anyNA(names(self$data))) stop("Dataset variable names can't be NA.", call. = F)
	if (any(names(self$data) == "")) stop("Dataset variable names can't be empty.", call. = F)
	if (anyDuplicated(names(self$data))) stop("Dataset variable names must be unique.", call. = F)

	if (!tibble::is_tibble(self$data)) {
		self$data = tibble::as_tibble(self$data)
		repaired = c(repaired, "converted `data` to tibble")
	}

	# metadata
	if (is.null(self$var_labels)) {
		self$var_labels = list()
		repaired = c(repaired, "created empty `var_labels`")
	} else if (!is.list(self$var_labels)) {
		stop("`var_labels` must be a list.", call. = F)
	}

	if (is.null(self$val_labels)) {
		self$val_labels = list()
		repaired = c(repaired, "created empty `val_labels`")
	} else if (!is.list(self$val_labels)) {
		stop("`val_labels` must be a list.", call. = F)
	}

	check_metadata_names = function(x, field) {
		if (length(x) == 0) return()

		if (is.null(names(x)))       stop(glue("`{field}` must be a named list."), call. = F)
		if (anyNA(names(x)))         stop(glue("`{field}` can't contain NA names."), call. = F)
		if (any(names(x) == ""))     stop(glue("`{field}` can't contain empty names."), call. = F)
		if (anyDuplicated(names(x))) stop(glue("`{field}` must have unique names."), call. = F)
	}

	check_metadata_names(self$var_labels, "var_labels")
	check_metadata_names(self$val_labels, "val_labels")


	repair_val_labels = function(var_name) {
		val = self$val_labels[[var_name]]

		if (!is.numeric(val))     stop(glue("`val_labels[[\"{var_name}\"]]` must be a numeric vector."), call. = F)
		if (is.null(names(val)))  stop(glue("`val_labels[[\"{var_name}\"]]` must be a named numeric vector."), call. = F)
		if (anyNA(names(val)))    stop(glue("`val_labels[[\"{var_name}\"]]` can't contain NA names."), call. = F)
		if (!all(is.finite(val))) stop(glue("`val_labels[[\"{var_name}\"]]` must contain only finite numbers."), call. = F)
		if (anyDuplicated(val))   stop(glue("`val_labels[[\"{var_name}\"]]` must contain unique values."), call. = F)

		if (is.unsorted(val)) {
			self$val_labels[[var_name]] = val[order(val)]
			repaired <<- c(repaired, glue("sorted value labels of `{var_name}`"))
		}
	}

	for (var_name in self$variables) {
		var = self$data[[var_name]]
		var_label = self$var_labels[[var_name]]

		if (!(is.null(var_label) || is_scalar_character(var_label))) stop(glue("`var_labels[[\"{var_name}\"]]` must be NULL or a scalar character."), call. = F)

		if (is.character(var)) {
			if (!is.null(self$val_labels[[var_name]])) stop(glue("Character variable `{var_name}` cannot have value labels."), call. = F)
			next
		}

		if (!is.null(self$val_labels[[var_name]])) repair_val_labels(var_name)

		if (is.logical(var) || is.integer(var)) {
			var = as.numeric(var)
			self$data[[var_name]] = var
			repaired = c(repaired, glue("converted `{var_name}` to numeric"))
		}

		if (is.numeric(var)) {
			bad = is.nan(var) | is.infinite(var)

			if (any(bad)) {
				self$data[[var_name]][bad] = NA
				repaired = c(repaired, glue("replaced {sum(bad)} non-finite values in `{var_name}` with NA"))
			}

			next
		}

		if (is.list(var)) {
			valid_type = map_lgl(var, \(x) is.numeric(x) || is.logical(x) || is.null(x))

			if (!all(valid_type)) {
				stop(glue("Variable `{var_name}` contains unsupported list elements; multiple-response variables must contain numeric vectors."), call. = F)
			}

			convert = map_lgl(var, \(x) is.logical(x) || is.integer(x) || is.null(x))

			if (any(convert)) {
				var[convert] = var[convert] |> map(as.numeric)
				self$data[[var_name]] = var
				repaired = c(repaired, glue("converted {sum(convert)} elements of `{var_name}` to numeric vectors"))
			}

			normalize = map_lgl(var, \(x) !all(is.finite(x)) || is.unsorted(x) || anyDuplicated(x) > 0)

			if (any(normalize)) {
				self$data[[var_name]][normalize] = self$data[[var_name]][normalize] |> map(mrcheck)
				repaired = c(repaired, glue("normalized {sum(normalize)} multiple-response elements in `{var_name}`"))
			}

			next
		}

		stop(glue("Variable `{var_name}` is of unsupported type."), call. = F)
	}

	stale_var_labels = setdiff(names(self$var_labels), self$variables)
	stale_val_labels = setdiff(names(self$val_labels), self$variables)

	self$vacuum()

	if (length(stale_var_labels) > 0) repaired = c(repaired, glue("removed {length(stale_var_labels)} obsolete variable-label entries"))
	if (length(stale_val_labels) > 0) repaired = c(repaired, glue("removed {length(stale_val_labels)} obsolete value-label entries"))


	if (length(repaired) == 0) {
		message("No repairs needed.")
	} else {
		message("Repaired dataset: ", paste(repaired, collapse = "; "), ".")
	}

	# check missing value labels cases
	unlabelled = list()

	for (var_name in self$variables) {
		var = self$data[[var_name]]
		labels = self$val_labels[[var_name]]

		if (is.null(labels) || is.character(var)) next

		values = if (is.list(var)) {
			sort(unique(unlist(var, use.names = F)))
		} else {
			sort(unique(var[!is.na(var)]))
		}

		missing_labels = setdiff(values, unname(labels))

		if (length(missing_labels) > 0) unlabelled[[var_name]] = missing_labels
	}

	if (length(unlabelled) > 0) {
		missing_n = sum(lengths(unlabelled))

		details = unlabelled |> imap_chr(\(codes, var_name) {
			shown = head(codes, 20)
			remaining = length(codes) - length(shown)

			glue("- `{var_name}`: {toString(shown)}", if (remaining > 0) glue(" ... and {remaining} more") else "")
		}) |> paste(collapse = "\n")

		warning(glue(
			"{length(unlabelled)} {ifelse(length(unlabelled) == 1, 'variable has', 'variables have')} {missing_n} observed {ifelse(missing_n == 1, 'value', 'values')} without labels:\n",
			"{details}"
		), call. = F)
	}

	invisible(NULL)
})


# Deprecated.
DS$set("public", "validate", function() {
	warning("`$validate()` is deprecated, please use `$repair()`.", call. = F)
	self$repair()
	invisible(NULL)
})
