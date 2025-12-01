#' @include rdp2.R

# Retrieves the variable label for a specified variable.
DS$set("public", "get_var_label", \(var) self$var_labels[[var]] %||% NA_character_)

# Retrieves variable labels for a set of specified variables.
DS$set("public", "get_var_labels", \(...) self$names(...) |> map_chr(self$get_var_label))

# Adds a suffix to the variable labels of specified variables.
DS$set("public", "add_label_suffix", function(vars, suffix, sep = " ") {
	var = intersect(names(self$var_labels), vars)
	self$var_labels[vars] = map(self$var_labels[vars], \(label) paste(label, suffix, sep = sep))
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

# Sets or updates the value labels for specified variables.
DS$set("public", "set_val_labels", function(vars, ...) {
	labels = list(...) |> map(\(x) if (is.character(x)) conv_to_labels(x) else x) |> unlist()

	for (var in self$names({{ vars }})) {
		self$val_labels[[var]] = sort(labels[!duplicated(labels, fromLast = T)])
	}
})

# Sets both variable labels and value labels for a specified variable.
DS$set("public", "set_labels", function(var, label, labels) {
	self$set_var_label({{ var }}, label)
	self$set_val_labels({{ var }}, labels)
})

# Adds new value labels to specified variables.
DS$set("public", "add_val_labels", function(vars, ...) {
	labels_list = list(...) |> map(\(x) if (is.character(x)) conv_to_labels(x) else x)

	for (var in self$names({{ vars }})) {
		for (new_labels in labels_list) {
			labels = c(self$val_labels[[var]], new_labels)
			self$val_labels[[var]] = sort(labels[!duplicated(labels, fromLast = T)])
		}
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
