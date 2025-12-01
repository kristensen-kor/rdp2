#' @include rdp2.R

# Renames variables in the dataset and updates associated metadata accordingly.
DS$set("public", "rename", function(names_from, names_to) {
	cur_names = self$variables

	if (length(names_from) != length(names_to)) stop("'names_from' and 'names_to' vectors must have the same length", call. = F)
	if (any(duplicated(names_from))) stop(glue("duplicates found in 'names_from' vector: {paste(unique(names_from[duplicated(names_from)]), collapse = ', ')}."), call. = F)
	if (any(duplicated(names_to))) stop(glue("duplicates found in 'names_to' vector: {paste(unique(names_to[duplicated(names_to)]), collapse = ', ')}."), call. = F)

	missing_vars = setdiff(names_from, cur_names)
	if (length(missing_vars) > 0) {
		if (!(var_name %in% cur_names)) stop(glue("variables not found: {paste(missing_vars, collapse = ', ')}"), call. = F)
	}

	conflicts = intersect(setdiff(cur_names, names_from), names_to)
	if (length(conflicts) > 0) stop(glue("renaming would create duplicate names: {paste(conflicts, collapse = ', ')}"), call. = F)


	rename_map = set_names(names_to, names_from)
	names(self$data)[cur_names %in% names_from] = rename_map[cur_names[cur_names %in% names_from]]

	if (!is.null(names(self$var_labels))) {
		mask = names(self$var_labels) %in% names_from
		names(self$var_labels)[mask] = rename_map[names(self$var_labels)[mask]]
	}

	if (!is.null(names(self$val_labels))) {
		mask = names(self$val_labels) %in% names_from
		names(self$val_labels)[mask] = rename_map[names(self$val_labels)[mask]]
	}
})

# Renames variables by their base name in the dataset and updates associated metadata accordingly.
DS$set("public", "rename_base", function(base, with) {
	if (length(base) != 1 || length(with) != 1) stop("`base` and `with` must be single strings.", call. = F)

	old_vars = self$base_name(base)
	if (length(old_vars) == 0) stop(glue("No variables found for base {dquote(base)}"), call. = F)

	new_vars = sub(base, with, old_vars, fixed = T)
	self$rename(old_vars, new_vars)
})
