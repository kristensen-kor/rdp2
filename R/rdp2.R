#' DS Class
#'
#' The `DS` class manages both raw data and its associated metadata, facilitating streamlined data manipulation.
#'
#' @field data A tibble containing the dataset, with each column representing a different variable (e.g., age, gender, survey responses).
#' @field var_labels A named list mapping each variable's identifier to a descriptive label, enhancing readability in outputs and reports.
#' @field val_labels A named list for categorical variables, where each entry maps numeric codes to meaningful category labels (e.g., 1 = "Agree", 2 = "Disagree").
#'
#' @export
#' @noRd
DS = R6::R6Class("DS", list(
	data = tibble(),
	var_labels = list(),
	val_labels = list()
))



# read/write

# Reads an SPSS (.sav) file and loads the data and metadata into the DS object.
DS$set("public", "get_spss", function(filename) {
	start_time = Sys.time()
	on.exit(cat("Read spss:", elapsed_fmt(Sys.time() - start_time), sprintf(" (%s rows, %s variables)", self$nrow, length(self$variables)), "\n"))

	df_raw = haven::read_spss(filename)

	self$data = df_raw |> modify(\(x) `attributes<-`(x, NULL))
	self$var_labels = df_raw |> map(\(x) attr(x, "label", exact = T)) |> compact()
	self$val_labels = df_raw |> map(\(x) attr(x, "labels", exact = T)) |> compact()
})

# Loads data and metadata from an RDS file into the DS object.
DS$set("public", "get_rds", function(filename) {
	save_data = readRDS(filename)

	self$data = save_data$data
	self$var_labels = save_data$var_labels
	self$val_labels = save_data$val_labels
})

# Initializes the DS object, optionally loading data from a specified file.
DS$set("public", "initialize", function(filename = NULL) {
	if (!is.null(filename)) {
		if (tools::file_ext(filename) == "") filename = paste0(filename, ".rds")

		if (!file.exists(filename)) stop("File does not exist: ", filename, call. = F)

		file_extension = tools::file_ext(filename) |> tolower()

		if (file_extension == "sav") {
			self$get_spss(filename)
		} else if (file_extension == "rds") {
			self$get_rds(filename)
		} else {
			stop("Unknown file format: ", file_extension, ". Only .rds and .sav formats are supported.", call. = F)
		}
	}
})

# Saves the current data and metadata of the DS object to an RDS file.
DS$set("public", "save", function(filename) {
	if (!endsWith(tolower(filename), ".rds")) filename = paste0(filename, ".rds")

	save_data = list(var_labels = self$var_labels, val_labels = self$val_labels, data = self$data)
	saveRDS(save_data, file = filename)
})


# basic

# Active binding that returns the names of variables in the dataset.
DS$set("active", "variables", \() names(self$data))

# Active binding that returns the number of rows in the dataset.
DS$set("active", "nrow", \() nrow(self$data))

# Deprecated method
DS$set("public", "get_col_names", \(...) {
	warning("$get_col_names() is deprecated. Please use $names() instead", call. = F)
	self$names(...)
})

# Retrieves the names of columns based on specified selection criteria.
DS$set("public", "names", \(...) self$data |> select(...) |> names())

# Gets the base names of columns after applying the base() function.
DS$set("public", "base_name", \(...) self$names(base(...)))
# DS$set("public", "base_name", \(xs) self$names(base(xs)))



# types

#' @export
is_multiple = \(x) is.list(x) && all(vapply(x, is.numeric, logical(1)))
# is_multiple = function(...) is.list(...)

# Determines and returns the type of specified variables in the dataset.
DS$set("public", "var_type", function(...) {
	map_chr(self$names(...), \(var) {
		if (is.numeric(self$data[[var]]) && var %in% names(self$val_labels)) {
			"single"
		} else if (is_multiple(self$data[[var]])) {
			"multiple"
		} else if (is.numeric(self$data[[var]])) {
			"numeric"
		} else if (is.character(self$data[[var]])) {
			"text"
		} else {
			warning("Variable does not match any expected type: ", var, call. = F)
			NA_character_
		}
	})
})

# Checks if the specified variables are nominal (single or multiple categorical).
DS$set("public", "is_nominal", \(vars) self$var_type(vars) %in% c("single", "multiple"))




# Filters the dataset based on provided conditions.
DS$set("public", "filter", \(...) self$data = self$data |> filter(...))

# Retains only the specified variables in the dataset and associated metadata.
DS$set("public", "keep", function(...) {
	var_names = self$names(...)

	self$data = self$data |> select(all_of(var_names))
	self$var_labels = self$var_labels[intersect(names(self$var_labels), var_names)]
	self$val_labels = self$val_labels[intersect(names(self$val_labels), var_names)]
})

# Removes the specified variables from the dataset and associated metadata.
DS$set("public", "remove", function(...) {
	var_names = self$names(...)

	self$data = self$data[setdiff(names(self$data), var_names)]
	self$var_labels = self$var_labels[setdiff(names(self$var_labels), var_names)]
	self$val_labels = self$val_labels[setdiff(names(self$val_labels), var_names)]
})


# Renames variables in the dataset and updates associated metadata accordingly.
DS$set("public", "rename", function(names_from, names_to) {
	cur_names = self$variables

	if (length(names_from) != length(names_to)) stop("'names_from' and 'names_to' vectors must have the same length")
	if (any(duplicated(names_from))) stop(sprintf("duplicates found in 'names_from' vector: %s", paste(unique(names_from[duplicated(names_from)]), collapse = ", ")))
	if (any(duplicated(names_to))) stop(sprintf("duplicates found in 'names_to' vector: %s", paste(unique(names_to[duplicated(names_to)]), collapse = ", ")))
	conflict_names = intersect(setdiff(cur_names, names_from), names_to)
	if (length(conflict_names) > 0) stop(sprintf("renaming would produce duplicated names: %s", paste(conflict_names, collapse = ", ")))

	for (var_name in names_from) {
		if (!(var_name %in% cur_names)) stop(sprintf("%s not found", var_name))
	}

	new_names_order = match(cur_names[cur_names %in% names_from], names_from)
	names(self$data)[cur_names %in% names_from] = names_to[new_names_order]

	calc_renames = function(xs) {
		cur_names = names(xs)
		present_names = names_from %in% cur_names
		filtered_names_from = names_from[present_names]
		filtered_names_to = names_to[present_names]
		new_names_order = match(cur_names[cur_names %in% filtered_names_from], filtered_names_from)
		list(new_names = filtered_names_to[new_names_order], cur_names_mask = cur_names %in% filtered_names_from)
	}

	renames = calc_renames(self$var_labels)
	names(self$var_labels)[renames$cur_names_mask] = renames$new_names

	renames = calc_renames(self$val_labels)
	names(self$val_labels)[renames$cur_names_mask] = renames$new_names
})

# Renames variables by their base name in the dataset and updates associated metadata accordingly.
DS$set("public", "rename_base", function(base, with) {
	old_vars = self$base_name(base)
	if (length(old_vars) == 0) stop(sprintf("No variables found for base \"%s\"", base))

	existing_new_base = self$base_name(with)
	if (length(existing_new_base) > 0) stop(sprintf("Cannot rename to base \"%s\" because these already exist: %s", with, paste(existing_new_base, collapse = ", ")))

	new_vars = sub(base, with, old_vars, fixed = T)
	self$rename(old_vars, new_vars)
})


# Changes the order of specified variables in the dataset.
DS$set("public", "move", function(..., after = NULL, before = NULL) {
	self$data = self$data |> relocate(..., .after = {{ after }}, .before = {{ before }})
})

# Creates a clone of the DS object with data filtered by specified conditions.
DS$set("public", "clone_if", function(...) {
	tds = self$clone()
	tds$filter(...)
	tds
})



# labels

# Cleans up metadata by removing labels for variables no longer in the dataset.
DS$set("public", "vacuum", function() {
	self$var_labels = self$var_labels[names(self$var_labels) %in% self$variables]
	self$val_labels = self$val_labels[names(self$val_labels) %in% self$variables]
})

# Retrieves the variable label for a specified variable.
DS$set("public", "get_var_label", \(var) self$var_labels[[var]] %||% NA_character_)

# Retrieves the value labels for a specified variable (private method).
DS$set("private", "get_val_labels", \(var) self$val_labels[[var]] %||% NA)

# Retrieves variable labels for a set of specified variables.
DS$set("public", "get_var_labels", \(...) self$names(...) |> map_chr(self$get_var_label))

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

# Adds a suffix to the variable labels of specified variables.
DS$set("public", "add_label_suffix", function(vars, suffix, sep = " ") {
	self$var_labels[vars] = map(self$var_labels[vars], \(label) paste(label, suffix, sep = sep))
})

# Sets or updates the label for a specified variable.
DS$set("public", "set_var_label", function(var, label) {
	if (!(var %in% self$variables)) stop(sprintf("Variable %s not found.", var), call. = F)
	self$var_labels[[var]] = label
})

# Sets or updates the value labels for specified variables.
DS$set("public", "set_val_labels", function(vars, ...) {
	labels = list(...) |> map(\(x) if (is.character(x)) conv_to_labels(x) else x) |> unlist()
	# if (is.character(labels)) labels = conv_to_labels(labels)

	for (var in self$names({{ vars }})) {
		self$val_labels[[var]] = sort(labels[!duplicated(labels, fromLast = T)])
	}
})

# Sets both variable labels and value labels for a specified variable.
DS$set("public", "set_labels", function(var, label, labels) {
	self$set_var_label({{ var }}, label)
	self$set_val_labels({{ var }}, labels)
})

# Adds new variable labels to specified variables.
DS$set("public", "add_val_labels", function(vars, ...) {
	labels_list = list(...) |> map(\(x) if (is.character(x)) conv_to_labels(x) else x)
	# if (is.character(new_labels)) new_labels = conv_to_labels(new_labels)

	for (var in self$names({{ vars }})) {
		for (new_labels in labels_list) {
			labels = c(self$val_labels[[var]], new_labels)
			self$val_labels[[var]] = sort(labels[!duplicated(labels, fromLast = T)])
		}
	}
})

# Adds new variable labels to specified variables.
DS$set("public", "add_labels", function(vars, ...) self$add_val_labels({{ vars }}, ...))

# Removes specified value labels from specified variables.
DS$set("public", "remove_labels", function(vars, ...) {
	vars = self$names({{ vars }})
	values = c(...)

	if (length(values) == 0) {
		self$val_labels[vars] = NULL
	} else {
		self$val_labels[vars] = map(self$val_labels[vars], \(label) label[!label %in% values])
	}
})




# Restructures the dataset by converting specified variable groups into individual cases.
DS$set("public", "vars_to_cases", function(index, ..., index_label = NULL, index_values = NULL, index_labels = NULL) {
	start_time = Sys.time()
	on.exit(cat("Restruct:", elapsed_fmt(Sys.time() - start_time), "\n"))

	cols_empty = \(df) Reduce(`&`, df |> map(var_empty))

	cols_list = c(...)
	all_cols = unlist(cols_list)

	if (length(unique(lengths(cols_list))) > 1) stop("All column groups must have the same length.")
	if (!all(all_cols %in% self$variables)) stop("Not all variables are present in the dataframe.")

	base_df = self$data |> select(-all_of(all_cols))

	if (is.null(index_values)) index_values = seq_along(cols_list[[1]])

	self$data = index_values |> imap(\(index_value, i) {
		group = map_chr(cols_list, i)
		group_df = self$data |> select(all_of(group))
		selected_cols = !cols_empty(group_df)
		base_df_slice = base_df[selected_cols, ]
		base_df_slice[[index]] = index_value
		bind_cols(base_df_slice, group_df[selected_cols, ])
	}) |> list_rbind()

	cols_list |> iwalk(\(cols, var_name) {
		self$set_val_labels({{ var_name }}, self$val_labels[[cols[1]]])
		self$set_var_label(var_name, self$var_labels[[cols[1]]])
	})

	if (is.null(index_labels)) index_labels = as.character(index_values)
	self$set_val_labels({{ index }}, setNames(index_values, index_labels))

	if (!is.null(index_label)) self$var_labels[[index]] = index_label

	self$vacuum()
})


# Deprecated method.
DS$set("public", "set_multiples", \() {
	warning("$set_multiples() is deprecated. Please use $conv_multiples() instead", call. = F)
	self$conv_multiples()
})


# Converts multiple indicator variables into single multiple-response variables.
DS$set("public", "conv_multiples", function() {
	start_time = Sys.time()
	on.exit(cat("Convert multiples:", elapsed_fmt(Sys.time() - start_time), "\n"))

	sep = ": "

	mdset_data = tibble(var_name = self$variables) |>
		filter(grepl("_[0-9]+$", var_name)) |>
		filter(map_lgl(var_name, \(x) identical(self$val_labels[[x]], c("-" = 0, "+" = 1)))) |>
		mutate(label = self$get_var_labels(all_of(var_name))) |>
		filter(!is.na(label)) |>
		mutate(tokens = strsplit(label, sep, fixed = T)) |>
		filter(lengths(tokens) > 1) |>
		mutate(prefix = map_chr(tokens, \(x) x[1])) |>
		mutate(label = map_chr(tokens, \(x) paste(x[-1], collapse = sep))) |>
		select(-tokens) |>
		mutate(base_name = sub("_[0-9]+$", "", var_name)) |>
		mutate(id = sub(".*_([0-9]+)$", "\\1", var_name) |> as.numeric()) |>
		group_by(base_name) |>
		filter(n_distinct(prefix) == 1) |>
		ungroup() |>
		arrange(base_name, id)

	mdsets = mdset_data$base_name |> unique()

	for (mdset in mdsets) {
		current_slice = mdset_data[mdset_data$base_name == mdset, ]

		col_data = do.call(rbind, seq_len(nrow(current_slice)) |> lapply(\(i) {
			if_else(has(self$data[[current_slice$var_name[i]]], 1), current_slice$id[i], NA_real_)
		})) |> as.data.frame() |> as.list() |> lapply(\(x) x[!is.na(x)]) |> unname()

		self$data[[mdset]] = col_data
		self$data = self$data |> relocate(all_of(mdset), .before = current_slice$var_name[1])
		self$data[current_slice$var_name] = NULL

		self$var_labels[[mdset]] = current_slice$prefix[1]
		self$val_labels[[mdset]] = setNames(current_slice$id, current_slice$label)
	}

	self$vacuum()
})


# Provides a summary view of variables, including their names, labels, types, and value labels, optionally filtered by name or label.
DS$set("public", "var_view", function(name = NULL, label = NULL) {
	val_labels_format = function(xs) {
		if (all(is.na(xs))) NA
		else paste0("[", xs, "] ", names(xs)) |> paste(collapse = "; ")
	}

	res = tibble(
		pos = seq_along(self$variables),
		variable = self$variables,
		label = self$get_var_labels(everything())
	)

	if (!is.null(name)) {
		if (is.numeric(name)) {
			res = res |> slice(name)
		} else {
			res = res |> filter(grepl(name, variable))
		}
	}

	if (!is.null(label)) {
		res = res |> filter(grepl(.env$label, .data$label))
	}

	res = res |> mutate(
		type = self$var_type(all_of(variable)),
		type = ifelse(is.na(type), "type error", type),
		type = ifelse(type == "single", NA_character_, type),
		val_labels = variable |> map_chr(\(var_name) val_labels_format(private$get_val_labels(var_name)))
	) |> select(pos, variable, type, label, everything())

	res
})


# Checks variables for properties like uniqueness, emptiness, and validity based on specified criteria.
DS$set("public", "var_check", function(name = NULL, label = NULL) {
	res = self$var_view(name, label)

	res = res |> mutate(
		unique_values = variable |> map(\(var) self$data[[var]] |> unlist() |> unique() |> discard(\(x) is.na(x) || x == "")),
		empty = ifelse(lengths(unique_values) == 0, "1", ""),
		same = ifelse(lengths(unique_values) == 1, "1", ""),
		valid = variable |> map_dbl(\(var) self$data[[var]] |> map(\(x) if (all(is.na(x)) || all(x == "")) NULL else x) |> compact() |> length()),
		distinct = lengths(unique_values),
		unique_values = NULL
	)

	res
})




# Exports a copy of the dataset, optionally selecting specific variables and formatting value labels for readability.
DS$set("public", "export_copy", function(...) {
	df = if (length(enquos(...)) == 0) self$data else self$data |> select(...)

	mvars = names(df) |> discard(\(x) is.character(df[[x]]) || (!(x %in% names(self$val_labels)) && !is_multiple(df[[x]])))

	df = df |> mutate(across(all_of(mvars), \(var) {
		values = self$prepare_val_labels(cur_column())
		labels = sprintf("[%s] %s", values, names(values))

		if (is_multiple(var)) {
			var |> map_chr(\(x) paste0(labels[match(x, values)], collapse = "; "))
		} else {
			labels[match(var, values)]
		}
	}))

	setNames(df, names(df) |> modify_if(\(x) x %in% names(self$var_labels), \(x) paste(x, self$var_labels[[x]], sep = "|")))
})



