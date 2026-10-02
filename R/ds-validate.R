# ds-validate.R

#' @include class-ds.R

# DS class constraints
#
# These rules define valid DS state. Public methods may accept integer inputs, but must convert them to doubles before storing them.
# Direct field assignment is the user's responsibility and may require subsequent validation.
#
# 1. Core fields
#    • DS stores data and metadata in `data`, `var_labels`, and `val_labels`.
#    • `data` must be a tibble with equal-length columns.
#    • Variable names must be unique, non-missing, and non-empty.
#    • Non-syntactic R names are allowed; export formats may impose extra limits.
#    • Zero-row and zero-column datasets are allowed.
#    • Columns must be double vectors, character vectors, or multiple-response lists.
#    • Classed and dimensional columns are unsupported; factors, dates, matrices, arrays, and similar objects are not implicitly converted.
#
# 2. Numeric representation
#    • All stored numbers use double vectors, including categorical responses, MR elements, and value-label codes.
#    • Public methods may accept integer inputs and normalize them to doubles.
#    • Numeric columns may contain finite values or ordinary R NA.
#    • NaN and ±Inf are not allowed.
#    • Continuous numeric values have no additional magnitude restriction.
#    • `$repair()` may convert logical values to doubles using FALSE = 0 and TRUE = 1; this conversion produces a warning because its intended semantics should be verified.
#
# 3. Categorical codes
#    • Single categorical responses, MR responses, and value-label codes must be integer-valued doubles.
#    • Codes must lie within [-(2^53 - 1), 2^53 - 1], inclusive.
#    • Comparisons use exact numeric equality, without rounding or tolerance.
#    • Zero and negative codes are allowed; neither has special meaning.
#    • Single categorical columns may also contain NA.
#
# 4. Character columns
#    • May contain strings, empty strings (""), or NA.
#    • NA and "" are distinct and must not be automatically interchanged.
#    • Must have no corresponding key in `val_labels`.
#    • DS does not automatically trim or otherwise normalize text.
#
# 5. Multiple-response columns
#    • Must be lists of unclassed, non-dimensional double vectors.
#    • Each element represents a set of categorical codes for one row.
#    • Codes must satisfy section 3 and be strictly increasing: ascending, without duplicates.
#    • Element lengths may differ.
#    • Empty responses use numeric(0); NULL elements are not allowed.
#    • NA, NaN, and ±Inf are not allowed inside elements.
#    • An empty response means no stored codes; it does not distinguish answered-none, skipped, or missing responses.
#    • Value labels are optional.
#
# 6. Metadata containers
#    • `var_labels` and `val_labels` must be lists.
#    • Empty containers may be unnamed.
#    • Non-empty containers must have unique, non-missing, and non-empty names.
#    • Every metadata key must identify a variable present in `data`.
#    • Entries are optional.
#    • NULL entries mean absent metadata and may be removed during cleanup.
#    • Operations removing variables must also remove their metadata.
#    • Stale metadata keys and NULL entries may be removed by any DS operation.
#
# 7. Variable labels
#    • Each non-NULL entry must be a non-missing character scalar.
#    • "" is an explicitly empty label and differs from absent/NULL metadata.
#
# 8. Value labels
#    • Each non-NULL entry must be an unclassed, non-dimensional double vector with character names.
#    • Codes must satisfy section 3 and be strictly increasing.
#    • Names are label text; they must not be NA.
#    • Empty and duplicate label text are allowed.
#    • Zero-length vectors are allowed and need not have names.
#    • A zero-length entry declares categorical type without supplying labels.
#
# 9. Variable classification
#    • Double column without a non-NULL `val_labels` entry: numeric.
#    • Double column with a non-NULL `val_labels` entry, even empty: single.
#    • List column satisfying section 5: multiple, regardless of labels.
#    • Character column satisfying section 4: text.
#
# 10. Value-label coverage
#     • Observed categorical codes without labels are valid but indicate incomplete metadata; validation may report them.
#     • Where label text is required, unlabelled codes use their textual numeric representation as the fallback.
#     • Labels for codes not currently observed are allowed.
#
# 11. Attributes
#     • R attributes are not part of the DS data model unless explicitly defined and used by rdp2.
#     • Arbitrary attributes attached to data columns, metadata containers, or metadata values are not guaranteed to be preserved and must not be used to store persistent information.
#     • The `names` attribute of metadata containers and value-label vectors is part of the DS representation and is governed by the rules above.
#     • The `class` and `dim` attributes are structural rather than disposable metadata; objects carrying them where unsupported are rejected rather than implicitly converted.
#
# Format-specific import/export restrictions and normalization are separate from this DS contract.

# Removes stale and NULL metadata entries.
DS$set("public", "vacuum", \() {
	self$var_labels = self$var_labels[names(self$var_labels) %in% self$variables & !vapply(self$var_labels, is.null, logical(1))]
	self$val_labels = self$val_labels[names(self$val_labels) %in% self$variables & !vapply(self$val_labels, is.null, logical(1))]
	invisible(NULL)
})

# Internal method implementing checks and repairs
DS$set("private", "check_state", \(repair = F) {
	# helpers
	show_names = \(x, n = 5) paste0(paste(head(x, n), collapse = ", "), if (length(x) > n) ", ..." else "")

	# Core containers
	if (!is.data.frame(self$data)) stop("`$data` must be a data frame or tibble.", call. = F)
	if (!is.list(self$var_labels)) stop("`$var_labels` must be a list.", call. = F)
	if (!is.list(self$val_labels)) stop("`$val_labels` must be a list.", call. = F)

	if (!checkmate::test_character(self$variables, any.missing = F)) stop("Dataset variable names can't be NA.", call. = F)
	if (!checkmate::test_character(self$variables, min.chars = 1))   stop("Dataset variable names can't be empty.", call. = F)
	if (!checkmate::test_character(self$variables, unique = T))      stop("Dataset variable names must be unique.", call. = F)

	if (!tibble::is_tibble(self$data)) {
		if (repair) {
			self$data = tibble::as_tibble(self$data, .name_repair = "minimal")
			message("Repair: converted `$data` to tibble.")
		} else {
			stop("`$data` must be a tibble; run `$repair()` to convert it.", call. = F)
		}
	}

	check_metadata_names = function(x, field) {
		if (length(x) == 0) return()

		if (is.null(names(x)))                                     stop(glue("`{field}` must be a named list."), call. = F)
		if (!checkmate::test_character(names(x), any.missing = F)) stop(glue("`{field}` can't contain NA names."), call. = F)
		if (!checkmate::test_character(names(x), min.chars = 1))   stop(glue("`{field}` can't contain empty names."), call. = F)
		if (!checkmate::test_character(names(x), unique = T))      stop(glue("`{field}` must have unique names."), call. = F)
	}

	check_metadata_names(self$var_labels, "$var_labels")
	check_metadata_names(self$val_labels, "$val_labels")

	# Any operation may remove stale or NULL metadata entries.
	self$vacuum()


	# Broad column-type pass
	supported = vapply(self$data, \(x) typeof(x) %in% c("logical", "integer", "double", "character", "list") && is.null(dim(x)) && is.null(attr(x, "class", exact = T)), logical(1))

	if (any(!supported)) {
		bad = names(self$data)[!supported]

		stop(glue(
			"Dataset contains {length(bad)} unsupported variable{if (length(bad) != 1) 's'}: {show_names(bad)}. ",
			"Variables must be logical, integer, double, character, or list vectors; classed and dimensional objects are unsupported."
		), call. = F)
	}

	# Variable labels pass
	supported = vapply(self$var_labels, \(x) checkmate::test_character(x, len = 1, any.missing = F), logical(1))

	if (any(!supported)) {
		bad = names(self$var_labels)[!supported]
		stop(glue("Dataset contains {length(bad)} unsupported variable label{if (length(bad) != 1) 's'}: {show_names(bad)}. Variable labels must be non-missing character scalars."), call. = F)
	}



	# Value-label pass

	# Text variables can't have value labels
	bad = intersect(names(self$val_labels), self$names(where(is.character)))

	if (length(bad) > 0) {
		if (!repair) stop(glue("Value labels are defined for {length(bad)} text variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to remove them."), call. = F)
		self$val_labels[bad] = NULL
		message(glue("Repair: removed value labels for {length(bad)} text variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}


	label_vars = names(self$val_labels)

	# Representation
	supported = vapply(self$val_labels, \(x) typeof(x) %in% c("integer", "double") && is.null(dim(x)) && is.null(attr(x, "class", exact = T)), logical(1))

	if (any(!supported)) {
		bad = label_vars[!supported]
		stop(glue("Value labels have unsupported representations for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Value labels must be numeric vectors; classed and dimensional objects are unsupported."), call. = F)
	}

	# Canonical double storage
	convert = vapply(self$val_labels, \(x) typeof(x) == "integer", logical(1))

	if (any(convert)) {
		bad = label_vars[convert]
		if (!repair) stop(glue("Value-label codes must be doubles for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to convert them."), call. = F)
		self$val_labels[convert] = self$val_labels[convert] |> map(\(x) set_names(as.double(x), names(x)))
		message(glue("Repair: converted value-label codes to double for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}

	# Label text
	valid_text = vapply(self$val_labels, \(x) length(x) == 0 || checkmate::test_character(names(x), any.missing = F), logical(1))

	if (any(!valid_text)) {
		bad = label_vars[!valid_text]
		stop(glue("Value labels have missing label text for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Non-empty value-label vectors require non-missing character names."), call. = F)
	}

	# Categorical codes
	valid_codes = vapply(self$val_labels, \(x) all(is.finite(x)) && all(abs(x) <= 2^53 - 1) && all(x == trunc(x)), logical(1))

	if (any(!valid_codes)) {
		bad = label_vars[!valid_codes]
		stop(glue("Value labels contain invalid categorical codes for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Codes must be finite integers within ±(2^53 - 1)."), call. = F)
	}

	# Duplicate codes
	duplicates = vapply(self$val_labels, anyDuplicated, integer(1)) > 0

	if (any(duplicates)) {
		bad = label_vars[duplicates]
		stop(glue("Value labels contain duplicate codes for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Codes must be unique."), call. = F)
	}

	# Ordering
	unsorted = vapply(self$val_labels, is.unsorted, logical(1))

	if (any(unsorted)) {
		bad = label_vars[unsorted]
		if (!repair) stop(glue("Value-label codes are not sorted for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to sort them."), call. = F)
		self$val_labels[unsorted] = map(self$val_labels[unsorted], \(x) x[order(x)])
		message(glue("Repair: sorted value labels by code for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}


	# Variable checks

	# Logical numeric
	bad = setdiff(self$names(where(is.logical)), names(self$val_labels))

	if (length(bad) > 0) {
		if (!repair) stop(glue("Numeric variables use logical storage for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to convert FALSE/TRUE to 0/1."), call. = F)
		self$data[bad] = self$data[bad] |> map(as.double)
		warning(glue("Repair: converted FALSE/TRUE to 0/1 for {length(bad)} numeric variable{if (length(bad) != 1) 's'}: {show_names(bad)}. This conversion is unusual; verify that logical values were intended as numeric 0/1."))
	}

	# Integer numeric
	bad = setdiff(self$names(where(is.integer)), names(self$val_labels))

	if (length(bad) > 0) {
		if (!repair) stop(glue("Numeric variables use integer storage for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to convert them to doubles."), call. = F)
		self$data[bad] = self$data[bad] |> map(as.double)
		message(glue("Repair: converted integer storage to double for {length(bad)} numeric variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}

	# Logical single categorical
	bad = intersect(self$names(where(is.logical)), names(self$val_labels))

	if (length(bad) > 0) {
		if (!repair) stop(glue("Single categorical variables use logical storage for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to convert FALSE/TRUE to codes 0/1."), call. = F)
		self$data[bad] = self$data[bad] |> map(as.double)
		warning(glue("Repair: converted FALSE/TRUE to categorical codes 0/1 for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Verify that 0/1 are the intended category codes."), call. = F)
	}

	# Integer single categorical
	bad = intersect(self$names(where(is.integer)), names(self$val_labels))

	if (length(bad) > 0) {
		if (!repair) stop(glue("Single categorical variables use integer storage for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to convert them to doubles."), call. = F)
		self$data[bad] = self$data[bad] |> map(as.double)
		message(glue("Repair: converted integer storage to double for {length(bad)} single categorical variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}



	# Numeric non-finite values
	numeric_vars = setdiff(self$names(where(is.double)), names(self$val_labels))
	bad = numeric_vars[vapply(self$data[numeric_vars], \(x) any(is.nan(x) | is.infinite(x)), logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Numeric variables contain NaN or infinity for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to replace them with NA."), call. = F)

		for (var_name in bad) {
			self$data[[var_name]][is.nan(self$data[[var_name]]) | is.infinite(self$data[[var_name]])] = NA_real_
		}

		message(glue("Repair: replaced NaN/infinite values with NA for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}


	# Single categorical non-finite values
	single_vars = intersect(self$names(where(is.double)), names(self$val_labels))
	bad = single_vars[vapply(self$data[single_vars], \(x) any(is.nan(x) | is.infinite(x)), logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Single categorical variables contain NaN or infinity for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to replace them with NA."), call. = F)

		for (var_name in bad) {
			self$data[[var_name]][is.nan(self$data[[var_name]]) | is.infinite(self$data[[var_name]])] = NA_real_
		}

		message(glue("Repair: replaced NaN/infinite values with NA for {length(bad)} single categorical variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}

	# Single categorical code domain
	bad = single_vars[vapply(self$data[single_vars], \(x) {
		x = x[!is.na(x)]
		any(abs(x) > 2^53 - 1 | x != trunc(x))
	}, logical(1))]

	if (length(bad)) {
		stop(glue("Single categorical variables contain invalid codes for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Codes must be integer-valued doubles within ±(2^53 - 1), or NA."), call. = F)
	}


	# Unsupported MR element representations
	mr_vars = self$names(where(is.list))

	invalid = vapply(self$data[mr_vars], \(var) any(vapply(var, \(x) {
		if (is.null(x)) return(F)
		!is.null(dim(x)) || !is.null(attr(x, "class", exact = T)) || !typeof(x) %in% c("logical", "integer", "double")
	}, logical(1))), logical(1))

	if (any(invalid)) {
		bad = mr_vars[invalid]
		stop(glue("Multiple-response variables contain unsupported elements for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. MR elements must ultimately be unclassed, non-dimensional double vectors."), call. = F)
	}


	# NULL MR elements
	bad = mr_vars[vapply(self$data[mr_vars], \(var) any(vapply(var, is.null, logical(1))), logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Multiple-response variables contain NULL elements for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to replace them with empty responses."), call. = F)

		for (var_name in bad) {
			var = self$data[[var_name]]
			convert = vapply(var, is.null, logical(1))
			var[convert] = replicate(sum(convert), numeric(), simplify = F)
			self$data[[var_name]] = var
		}

		message(glue("Repair: replaced NULL elements with empty responses for {length(bad)} multiple-response variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}

	# Logical MR elements
	bad = mr_vars[vapply(self$data[mr_vars], \(var) any(vapply(var, is.logical, logical(1))), logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Multiple-response variables contain logical elements for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to convert FALSE/TRUE to 0/1."), call. = F)

		for (var_name in bad) {
			var = self$data[[var_name]]
			convert = vapply(var, is.logical, logical(1))
			var[convert] = var[convert] |> map(as.double)
			self$data[[var_name]] = var
		}

		warning(glue("Repair: converted FALSE/TRUE to categorical codes 0/1 for {length(bad)} multiple-response variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Verify that 0/1 are the intended category codes."), call. = F)
	}


	# Integer MR elements
	bad = mr_vars[vapply(self$data[mr_vars], \(var) any(vapply(var, is.integer, logical(1))), logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Multiple-response variables contain integer elements for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to convert them to doubles."), call. = F)

		for (var_name in bad) {
			var = self$data[[var_name]]
			convert = vapply(var, is.integer, logical(1))
			var[convert] = var[convert] |> map(as.double)
			self$data[[var_name]] = var
		}

		message(glue("Repair: converted integer elements to doubles for {length(bad)} multiple-response variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}


	# Missing and non-finite MR codes
	bad = mr_vars[vapply(self$data[mr_vars], \(var) {
		any(vapply(var, \(x) any(!is.finite(x)), logical(1)))
	}, logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Multiple-response variables contain NA, NaN, or infinity for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to remove them."), call. = F)

		for (var_name in bad) {
			var = self$data[[var_name]]
			var = map(var, \(x) x[is.finite(x)])
			self$data[[var_name]] = var
		}

		message(glue("Repair: removed NA, NaN, and infinite codes from {length(bad)} multiple-response variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}


	# Multiple-response code domain
	bad = mr_vars[vapply(self$data[mr_vars], \(var) any(vapply(var, \(x) any(abs(x) > 2^53 - 1 | x != trunc(x)), logical(1))), logical(1))]

	if (length(bad) > 0) {
		stop(glue("Multiple-response variables contain non-integer or out-of-range categorical codes for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}. Codes must be integer-valued doubles within ±(2^53 - 1)."), call. = F)
	}


	# Duplicate MR codes
	bad = mr_vars[vapply(self$data[mr_vars], \(var) {
		any(vapply(var, anyDuplicated, integer(1)) > 0)
	}, logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Multiple-response variables contain duplicate response codes for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to remove duplicates."), call. = F)

		for (var_name in bad) {
			self$data[[var_name]] = self$data[[var_name]] |> map(unique)
		}

		message(glue("Repair: removed duplicate response codes for {length(bad)} multiple-response variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}


	# Multiple-response ordering
	bad = mr_vars[vapply(self$data[mr_vars], \(var) {
		any(vapply(var, is.unsorted, logical(1)))
	}, logical(1))]

	if (length(bad) > 0) {
		if (!repair) stop(glue("Multiple-response variables contain unsorted response sets for {length(bad)} variable{if (length(bad) != 1) 's'}: {show_names(bad)}; run `$repair()` to sort them."), call. = F)

		for (var_name in bad) {
			self$data[[var_name]] = self$data[[var_name]] |> map(sort)
		}

		message(glue("Repair: sorted response sets for {length(bad)} multiple-response variable{if (length(bad) != 1) 's'}: {show_names(bad)}."))
	}


	# Value-label coverage
	bad = names(self$val_labels)[vapply(names(self$val_labels), \(var_name) {
		x = self$data[[var_name]]
		observed = if (is.list(x)) unique(unlist(x, use.names = F)) else unique(x[!is.na(x)])
		any(!observed %in% self$val_labels[[var_name]])
	}, logical(1))]

	if (length(bad)) {
		warning(glue("Incomplete value-label coverage for {length(bad)} categorical variable{if (length(bad) != 1) 's'}: {show_names(bad)}."), call. = F)
	}


	invisible(NULL)
})

# Validates DS class instance for corresponding rdp2 constraints
DS$set("public", "validate", \() private$check_state())

# Tries to fix common issues
DS$set("public", "repair", \() private$check_state(repair = T))
