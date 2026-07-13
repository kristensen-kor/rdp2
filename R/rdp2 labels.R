#' @include class-ds.R

# Retrieves the variable label for a specified variable.
DS$set("public", "get_var_label", \(var) self$var_labels[[var]] %||% NA_character_)

# Retrieves variable labels for a set of specified variables.
DS$set("public", "get_var_labels", \(...) self$names(...) |> map_chr(self$get_var_label))

# Returns value labels of selected variables as a long-format tibble.
DS$set("public", "get_val_labels", function(...) {
	self$names(...) |> map(\(var_name) tibble(
		var = var_name,
		var_label = self$get_var_label(var_name),
		value = unname(self$val_labels[[var_name]]),
		label = names(self$val_labels[[var_name]])
	)) |> bind_rows()
})


# Adds a suffix to the variable labels of specified variables.
DS$set("public", "add_label_suffix", function(vars, suffix, sep = " ") {
	vars = intersect(names(self$var_labels), self$names({{ vars }}))
	self$var_labels[vars] = map(self$var_labels[vars], \(label) paste(label, suffix, sep = sep))
})

# Adds a prefix to the variable labels of specified variables.
DS$set("public", "add_label_prefix", function(vars, prefix, sep = " ") {
	vars = intersect(names(self$var_labels), self$names({{ vars }}))
	self$var_labels[vars] = map(self$var_labels[vars], \(label) paste(prefix, label, sep = sep))
})


# Sets or updates the label for a specified variable.
DS$set("public", "set_var_label", function(var, label) {
	if (!(var %in% self$variables)) stop(glue("Variable {var} not found."), call. = F)
	self$var_labels[[var]] = label
})

conv_to_labels = function(labels) {
	if (length(labels) == 1) {
		lines = labels |> strsplit("\n") |> unlist() |> trimws()

		valid_lines = grep("^\\d+\\s+\\w", lines, value = T)

		if (length(valid_lines) == 0) stop("Parsed labels have length 0. Please check the input labels.", call. = F)

		numbers = sub("^(\\d+).*", "\\1", valid_lines) |> as.numeric()
		names = sub("^\\d+\\s+(.*)", "\\1", valid_lines)

		setNames(numbers, names)
	} else {
		setNames(seq_along(labels), labels)
	}
}


# Converts supported shorthand forms to a canonical named numeric value-label vector.
as_val_labels = function(x) {
	if (is.character(x)) {
		if (length(x) == 0) stop("Labels cannot be empty.", call. = F)
		if (anyNA(x)) stop("Labels cannot contain missing values.", call. = F)

		is_code_label = grepl("^\\s*\\d+\\s+\\S", x)

		if (all(is_code_label)) {
			codes = sub("^\\s*(\\d+)\\s+.*$", "\\1", x) |> as.double()
			labels = sub("^\\s*\\d+\\s+", "", x) |> trimws()
			x = setNames(codes, labels)
		} else {
			x = setNames(as.double(seq_along(x)), x)
		}
	} else if (is.numeric(x) && is.null(names(x))) {
		if (!all(is.finite(x))) stop("Label codes must be finite.", call. = F)
		x = setNames(as.double(x), formatC(x, format = "f", big.mark = "", drop0trailing = T))
	}

	if (!is.numeric(x) || is.null(names(x))) {
		stop("Labels must be a character vector, numeric vector, or named numeric vector.", call. = F)
	}

	if (length(x) == 0) stop("Labels cannot be empty.", call. = F)
	if (anyNA(names(x))) stop("Label names cannot be missing.", call. = F)
	if (!all(is.finite(x))) stop("Label codes must be finite.", call. = F)
	if (anyDuplicated(x)) stop("Label codes must be unique.", call. = F)

	sort(setNames(as.double(x), names(x)))
}



# Sets or updates the value labels for specified variables.
DS$set("public", "set_val_labels", function(vars, labels) {
	labels = as_val_labels(labels)

	for (var in self$names({{ vars }})) {
		self$val_labels[[var]] = sort(labels[!duplicated(labels, fromLast = T)])
	}
})

# Sets both variable labels and value labels for a specified variable.
DS$set("public", "set_labels", function(var, label, labels) {
	self$set_var_label({{ var }}, label)
	self$set_val_labels({{ var }}, as_val_labels(labels))
})

# Adds new value labels to specified variables.
DS$set("public", "add_val_labels", function(vars, labels) {
	new_labels = as_val_labels(labels)

	for (var in self$names({{ vars }})) {
		current_labels = c(self$val_labels[[var]], new_labels)
		self$val_labels[[var]] = sort(current_labels[!duplicated(current_labels, fromLast = T)])
	}
})

# Removes specified value labels from specified variables.
DS$set("public", "remove_labels", function(vars, ...) {
	vars = self$names({{ vars }})
	values = c(...)

	if (length(values) == 0) {
		self$val_labels[vars] = NULL
	} else {
		self$val_labels[vars] = map(self$val_labels[vars], \(labels) labels[!(labels %in% values)])
		self$val_labels[lengths(self$val_labels) == 0] = NULL
	}
})

# Removes empty value labels from specified variables.
DS$set("public", "remove_empty_labels", function(...) {
	self$names(...) |> walk(\(var) {
		empty_ids = setdiff(self$val_labels[[var]], unlist(self$data[[var]]))
		if (length(empty_ids) > 0) self$remove_labels(all_of(var), empty_ids)
	})
})
