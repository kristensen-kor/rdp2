#' @include rdp2.R

# Adds an externally calculated weight variable matched by an ID.
DS$set("public", "add_weight", function(data, by = NULL, name = NULL, label = "Weight") {
	if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = F)
	if (ncol(data) != 2) stop("`data` must contain exactly two columns: a matching key and a weight.", call. = F)

	validate_label(label)
	validate_label(name)

	if (is.null(by)) {
		common_vars = intersect(names(data), self$variables)

		if (length(common_vars) == 0) stop("Cannot infer `by`: neither column in `data` exists in the dataset.", call. = F)
		if (length(common_vars) > 1) stop(glue("Cannot infer `by`: multiple columns in `data` exist in the dataset: {toString(common_vars)}."), call. = F)

		by = common_vars[1]
	} else {
		assert_nonempty_string(by)
	}

	if (!(by %in% self$variables)) stop(glue("Join variable `{by}` does not exist in the dataset."), call. = F)
	if (!(by %in% names(data))) stop(glue("Join variable `{by}` does not exist in `data`."), call. = F)

	weight = setdiff(names(data), by)
	weight_name = name %||% weight

	if (weight_name == by) stop("The weight variable cannot have the same name as the matching variable.", call. = F)

	ds_key = self$data[[by]]
	weight_key = data[[by]]
	weight_values = data[[weight]]

	validate_key = function(x, source) {
		if (!is.numeric(x) && !is.character(x)) stop(glue("Matching variable `{by}` in {source} must be numeric or character."), call. = F)
		if (is.numeric(x) && any(!is.finite(x))) stop(glue("Matching variable `{by}` in {source} contains missing or non-finite values."), call. = F)
		if (is.character(x) && (anyNA(x) || any(trimws(x) == ""))) stop(glue("Matching variable `{by}` in {source} contains missing or empty values."), call. = F)

		duplicated_keys = unique(x[duplicated(x)])

		if (length(duplicated_keys) > 0) stop(glue("Matching variable `{by}` in {source} contains duplicated values: {toString(head(duplicated_keys, 10))}..."), call. = F)
	}

	validate_key(ds_key, "the dataset")
	validate_key(weight_key, "`data`")

	keys_compatible = (is.numeric(ds_key) && is.numeric(weight_key)) || (is.character(ds_key) && is.character(weight_key))

	if (!keys_compatible) stop(glue("Matching variable `{by}` has incompatible types: {typeof(ds_key)} in the dataset and {typeof(weight_key)} in `data`."), call. = F)
	if (!is.numeric(weight_values)) stop(glue("Weight variable `{weight}` must be numeric."), call. = F)
	if (any(!is.finite(weight_values))) stop(glue("Weight variable `{weight}` contains missing or non-finite values."), call. = F)
	if (any(weight_values <= 0)) stop(glue("Weight variable `{weight}` must contain positive values."), call. = F)

	match_pos = match(ds_key, weight_key)
	matched_n = sum(!is.na(match_pos))
	unmatched_n = self$nrow - matched_n
	unused_n = sum(!(weight_key %in% ds_key))

	if (matched_n == 0) stop(glue("No values of `{by}` in `data` match the dataset."), call. = F)

	overwriting = weight_name %in% self$variables

	if (overwriting) warning(glue("Variable `{weight_name}` already exists and will be overwritten."), call. = F)

	if (unmatched_n > 0 || unused_n > 0) {
		issues = character()
		if (unmatched_n > 0) issues = c(issues, glue("{unmatched_n} dataset {ifelse(unmatched_n == 1, 'row has', 'rows have')} no weight"))
		if (unused_n > 0) issues = c(issues, glue("{unused_n} weight {ifelse(unused_n == 1, 'record was', 'records were')} unused"))
		warning(glue("Weight matching was incomplete: {paste(issues, collapse = '; ')}."), call. = F)
	}

	self$data[[weight_name]] = weight_values[match_pos]

	if (!is.null(label)) self$var_labels[[weight_name]] = label

	message(glue("{ifelse(overwriting, 'Updated', 'Added')} weight variable `{weight_name}`: {matched_n} of {self$nrow} rows matched."))

	invisible(NULL)
})
