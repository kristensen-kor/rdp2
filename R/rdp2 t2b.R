#' @include rdp2.R

# Internal method to recode values from source variables to target variables based on a condition and a specified value.
DS$set("private", "recode_from_to_case", function(from_vars, to_vars, condition, value) {
	walk2(from_vars, to_vars, \(var, new_var) {
		mask = has(self$data[[var]], condition)
		self$data[[new_var]][mask] = value
	})
})

# Recodes values from source variables to target variables based on specified conditions and replacement values.
DS$set("public", "recode_from_to", function(from_vars, to_vars, ...) {
	cases = rlang::list2(...)

	for (case in cases) {
		condition = rlang::eval_tidy(rlang::f_lhs(case))
		value = rlang::eval_tidy(rlang::f_rhs(case))
		if (length(value) != 1) stop("Rhs of the '~' must be of length 1, not ", paste(value, collapse = ", "), ".", call. = F)

		private$recode_from_to_case(from_vars, to_vars, condition, value)
	}
})

# Creates top-box and bottom-box variables from specified variables, assigning labels and optionally recoding based on value ranges.
DS$set("public", "make_tb", function(vars, box_size = 2, ..., suffix = NULL, label_suffix = NULL, labels = NULL, is_reverse = F, range = NULL, move = T, suffix_position = "auto") {
	# checks
	types = self$var_type({{ vars }})
	var_names = self$names({{ vars }})
	bad_vars = var_names[!(types %in% c("single", "numeric"))]

	if (length(bad_vars) > 0) {
		error_message = "Error: The following variables must be 'single' or 'numeric':\n"
		for (var in bad_vars) {
			error_message = paste0(error_message, "- ", var, ": ", types[var], "\n")
		}
		stop(error_message, call. = F)
	}

	if (!is.null(range) && length(range) < (2 * box_size)) stop("Error: 'range' should contain at least ", 2 * box_size, " values.", call. = F)


	# main logic
	if (is.null(suffix)) suffix = paste0("T", box_size, "B")
	if (is.null(label_suffix)) label_suffix = paste0("(T", box_size, "B)")

	if (is.null(labels)) {
		labels = c(paste0("Top-", box_size, "-Box"), "Middle", paste0("Bottom-", box_size, "-Box"))
		if (is_reverse) labels = rev(labels)
	}

	if (length(var_names) == 1 || suffix_position == "end") {
		new_vars = var_renamer(var_names, suffix = suffix)
	} else {
		new_vars = var_renamer(var_names, presuffix = suffix)
	}

	walk2(var_names, new_vars, \(var, new_var) {
		if (new_var %in% self$variables) cat("Warning:", new_var, "is already present. Replacing.\n")

		self$data[[new_var]] = rep(NA_real_, self$nrow)
		self$var_labels[[new_var]] = self$var_labels[[var]]
		self$val_labels[[new_var]] = self$val_labels[[var]]
	})

	self$var_labels[new_vars] = map(self$var_labels[new_vars], \(text) paste(text, label_suffix, sep = " "))
	self$set_val_labels(all_of(new_vars), labels)

	if (move) self$data = self$data |> relocate(all_of(new_vars), .after = {{ vars }})

	if (!is.null(range)) {
		if (is_reverse) range = rev(range)

		n = length(range)

		top_boxes = range |> head(box_size)
		bottom_boxes = range |> tail(box_size)
		middle = if (n > 2 * box_size) range[(box_size + 1):(n - box_size)] else NULL

		private$recode_from_to_case(var_names, new_vars, top_boxes, if (!is_reverse) 1 else 3)
		private$recode_from_to_case(var_names, new_vars, bottom_boxes, if (!is_reverse) 3 else 1)
		if (!is.null(middle)) private$recode_from_to_case(var_names, new_vars, middle, 2)
	}

	if (length(rlang::list2(...)) > 0) self$recode_from_to(var_names, new_vars, ...)
})

# Creates T2B (Top-2 Box) variables from specified variables, assigning appropriate labels and handling recoding.
DS$set("public", "make_t2b", function(vars, ..., suffix = "T2B", label_suffix = "(T2B)", labels = NULL, is_reverse = F, range = NULL, move = T, suffix_position = "auto") {
	self$make_tb(vars = {{ vars }}, box_size = 2, ..., suffix = suffix, label_suffix = label_suffix, labels = labels, is_reverse = is_reverse, range = range, move = move, suffix_position = suffix_position)
})

# Creates T3B (Top-3 Box) variables from specified variables, assigning appropriate labels and handling recoding.
DS$set("public", "make_t3b", function(vars, ..., suffix = "T3B", label_suffix = "(T3B)", labels = NULL, is_reverse = F, range = NULL, move = T, suffix_position = "auto") {
	self$make_tb(vars = {{ vars }}, box_size = 3, ..., suffix = suffix, label_suffix = label_suffix, labels = labels, is_reverse = is_reverse, range = range, move = move, suffix_position = suffix_position)
})

