#' @include rdp2.R

validate_merge_vars = function(ds, ...) {
	var_names = ds$names(...)

	if (length(var_names) < 2) stop("At least two variables must be supplied to `merge_vars()`.", call. = F)

	var_types = ds$var_type(all_of(var_names))

	if (identical(var_types[1], "multiple")) {
		invalid_vars = var_names[!(var_types %in% c("single", "numeric", "multiple"))]
		if (length(invalid_vars) > 0) stop(glue("A multiple-response target can only merge numeric or multiple-response variables. Invalid variables: {toString(invalid_vars)}."), call. = F)
	} else {
		invalid_vars = var_names[!var_types %in% c("single", "numeric")]
		if (length(invalid_vars) > 0) stop(glue("A single-response target can only merge numeric variables. Invalid variables: {toString(invalid_vars)}."), call. = F)
	}

	var_names
}

merge_value_labels = function(val_labels) {
	label_rows = map2(val_labels, names(val_labels), \(labels, var_name) {
		if (is.null(labels)) return(NULL)

		tibble(
			variable = var_name,
			code = unname(labels),
			label = names(labels)
		)
	}) |> compact()

	if (length(label_rows) == 0) {
		return(list(labels = NULL, added = numeric(), conflicts = 0))
	}

	label_data = list_rbind(label_rows)
	target = names(val_labels)[1]

	# The first definition of each code wins.
	first_positions = match(label_data$code, label_data$code)
	first_labels = label_data$label[first_positions]

	# Count later definitions that disagree with the first definition.
	label_conflicts = sum(label_data$label != first_labels)

	winning_rows = !duplicated(label_data$code)
	winners = label_data[winning_rows, ]

	added = winners[winners$variable != target, ]

	merged_labels = set_names(winners$code, winners$label) |> sort()
	added_labels = set_names(added$code, added$label) |> sort()

	list(labels = merged_labels, added = added_labels, conflicts = label_conflicts)
}

# Merges variables row-wise into the first selected variable.
DS$set("public", "merge_vars", function(..., label = NULL, .remove = F) {
	var_names = validate_merge_vars(self, ...)
	validate_label(label)
	target = var_names[1]
	sources = var_names[-1]

	merged = self$data[var_names] |> pmap(\(...) mrcheck(c(...)))

	if (!is_multiple(self$data[[target]])) {
		conflict_rows = which(lengths(merged) > 1)

		if (length(conflict_rows) > 0) {
			plural_s = \(n) if (n == 1) "row contains" else "rows contain"
			stop(glue("Cannot merge variables because {length(conflict_rows)} {plural_s(length(conflict_rows))} conflicting values. First conflicting rows: {toString(head(conflict_rows, 10))}."), call. = F)
		}

		merged = map_dbl(merged, \(x) if (length(x) == 0) NA_real_ else x[[1]])
	}

	label_merge = merge_value_labels(self$val_labels[var_names])

	self$data[[target]] = merged
	self$val_labels[[target]] = label_merge$labels

	if (!is.null(label)) self$var_labels[[target]] = label

	if (.remove) self$remove(all_of(sources))
	# suppressMessages(self$remove(all_of(sources))) # if to change $remove()

	if (label_merge$conflicts > 0) {
		warning(glue("Ignored {label_merge$conflicts} conflicting value-label {ifelse(label_merge$conflicts == 1, 'definition', 'definitions')}; earlier labels were retained."), call. = F)
	}

	report = glue("Merged {length(var_names)} variables into `{target}`")
	if (.remove) report = glue("{report}; removed {length(sources)} source {ifelse(length(sources) == 1, 'variable', 'variables')}")
	report = paste0(report, ".")
	if (length(label_merge$added) == 1) report = glue("{report} Added value label {unname(label_merge$added[1])} = `{names(label_merge$added)[1]}`.")
	if (length(label_merge$added) > 1) report = glue("{report} Added {length(label_merge$added)} value labels.")
	message(report)

	invisible(NULL)
})


# consolidate_vars?
DS$set("public", "collapse_vars", function(..., label = NULL) {
	self$merge_vars(..., label = label, .remove = T)
})
