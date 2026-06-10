#' @include rdp2.R

# Expands native multiple-response variables into SPSS-style binary indicator groups.
DS$set("public", "expand_multiples", function(..., sep = ": ", labels = c("-" = 0, "+" = 1)) {
	start_time = Sys.time()

	assert_nonempty_string(sep)
	if (!is.numeric(labels) || length(labels) != 2 || is.null(names(labels)) || anyNA(labels) || any(names(labels) == "")) {
		stop("`labels` must be a named numeric vector of length 2 without missing values.", call. = F)
	}
	if (anyDuplicated(unname(labels))) {
		stop("Values in `labels` must be unique.", call. = F)
	}

	selected_vars = if (length(rlang::enquos(...)) == 0) self$variables else self$names(...)

	multiple_vars = selected_vars[map_lgl(self$data[selected_vars], is_multiple)]

	if (length(multiple_vars) == 0) {
		message(glue("No multiple-response variables found: {elapsed_fmt(Sys.time() - start_time)}"))
		return(invisible(NULL))
	}

	# Prepare and validate the complete expansion before changing the dataset.
	expansion = map(multiple_vars, \(var) {
		value_labels = self$val_labels[[var]]

		if (is.null(value_labels) || !is.numeric(value_labels) || is.null(names(value_labels)) || anyNA(value_labels) || any(names(value_labels) == "")) {
			stop(glue("Multiple-response variable `{var}` must have a named numeric value-label vector.", call. = F))
		}

		list(var = var, new_vars = paste_vars(var, unname(value_labels)), codes = unname(value_labels), category_labels = names(value_labels), var_label = self$var_labels[[var]] %||% var)
	})

	all_new_vars = expansion |> map("new_vars") |> unlist()
	name_collisions = intersect(all_new_vars, self$variables)
	if (length(name_collisions) > 0) {
		stop("Cannot expand multiple-response variables because target variables already exist: ", paste(name_collisions, collapse = ", "), ".", call. = F)
	}

	unselected_value = unname(labels[1])
	selected_value = unname(labels[2])

	for (item in expansion) {
		var = item$var
		indicator_data = item$codes |> map(\(code) map_dbl(self$data[[var]], \(values) any(values == code))) |> set_names(item$new_vars) |> as_tibble()
		self$data = bind_cols(self$data, indicator_data) |> relocate(all_of(item$new_vars), .after = all_of(var)) |> select(-all_of(var))

		self$var_labels[[var]] = NULL
		self$val_labels[[var]] = NULL

		for (i in seq_along(item$new_vars)) {
			new_var = item$new_vars[i]
			self$var_labels[[new_var]] = paste0(item$var_label, sep, item$category_labels[i])
			self$val_labels[[new_var]] = labels
		}
	}

	message(glue(
		"Expanded {length(multiple_vars)} multiple-response ",
		"variable{if (length(multiple_vars) == 1) '' else 's'} into ",
		"{length(all_new_vars)} indicator ",
		"variable{if (length(all_new_vars) == 1) '' else 's'}: ",
		"{elapsed_fmt(Sys.time() - start_time)}"
	))

	invisible(NULL)
})



# Exports a copy of the dataset, optionally selecting specific variables and formatting value labels for readability.
DS$set("public", "export_copy", function(...) {
	df = if (length(enquos(...)) == 0) self$data else self$data |> select(...)

	mvars = names(df) |> discard(\(x) is.character(df[[x]]) || (!(x %in% names(self$val_labels)) && !is_multiple(df[[x]])))

	df = df |> mutate(across(all_of(mvars), \(var) {
		values = self$prepare_val_labels(cur_column())
		labels = sprintf("[%s] %s", values, chartr("\t\n", "  ", names(values)))

		if (is_multiple(var)) {
			var |> map_chr(\(x) paste0(labels[match(x, values)], collapse = "; "))
		} else {
			labels[match(var, values)]
		}
	}))

	setNames(df, names(df) |> modify_if(\(x) x %in% names(self$var_labels), \(x) paste(x, self$var_labels[[x]], sep = "|")))
})



# Exports dataset to SPSS .sav format.
DS$set("public", "export_spss", function(sav_name, sps_name, CP1251 = F) {
	start_time = Sys.time()
	on.exit(cat("Export:", elapsed_fmt(Sys.time() - start_time), "\n"))

	str_cleanup = function(xs) gsub("\n", "; ", gsub("\t", " ", xs, fixed = T), fixed = T)
	quotes_escape = function(xs) gsub("\"", "\"\"", xs, fixed = T)

	multiples = self$names(where(is_multiple))
	string_vars = self$names(where(is.character))
	single_vars = setdiff(self$variables, c(multiples, string_vars))

	for (var_name in multiples) {
		var_names = self$base_name(var_name)
		if (length(var_names) > 0) stop(paste0(paste0(var_names, collapse = ", "), " names conflict"), call. = F)
	}

	df = self$data

	for (var_name in multiples) {
		values = self$prepare_val_labels(var_name)

		tdf = values |> set_names(paste_vars(var_name, values)) |> map(\(value) map_dbl(df[[var_name]], \(x) any(x == value))) |> bind_cols()
		df = bind_cols(df, tdf)
		df = df |> relocate(all_of(paste_vars(var_name, values)), .after = all_of(var_name)) |> select(-all_of(var_name))

		for (i in seq_along(values)) {
			var = paste_vars(var_name, values[i])

			attr(df[[var]], "label") = str_cleanup(paste0(self$get_var_label(var_name), ": ", names(values)[i]))
			attr(df[[var]], "labels") = c("-" = 0, "+" = 1)
			attr(df[[var]], "format.spss") = "F1.0"
			class(df[[var]]) = "haven_labelled"
		}
	}

	df = df |> mutate(across(where(is.character), str_cleanup))
	df = df |> mutate(across(where(is.character), \(var) tidyr::replace_na(var, "")))

	# add_labels
	for (var in single_vars) {
		if (var %in% names(self$var_labels)) attr(df[[var]], "label") = str_cleanup(self$var_labels[[var]])
		if (var %in% names(self$val_labels)) attr(df[[var]], "labels") = set_names(as.double(self$val_labels[[var]]), str_cleanup(names(self$val_labels[[var]])))
		attr(df[[var]], "format.spss") = "F2.0"
		class(df[[var]]) = "haven_labelled"
	}

	text = c(sprintf("CD \"%s\".", getwd()), sprintf("GET FILE = \"%s\".", sav_name))

	# fix nominal types
	all_vars = names(df)
	is_nominal = df |> map_lgl(\(x) !is.null(attr(x, "labels", exact = T)) || is.character(x))

	nominal_blocks = character()
	current_block = character()

	for (i in seq_along(is_nominal)) {
		if (is_nominal[i]) current_block = c(current_block, all_vars[i])

		if (!is_nominal[i] || i == length(is_nominal)) {
			if (length(current_block) == 1) nominal_blocks = c(nominal_blocks, current_block)
			if (length(current_block) > 1) nominal_blocks = c(nominal_blocks, paste(current_block[1], "TO", tail(current_block, 1)))
			if (length(current_block) > 0) 	current_block = character()
		}
	}

	if (length(nominal_blocks) > 0) text = c(text, sprintf("VARIABLE LEVEL\n%s\n(NOMINAL).", paste(nominal_blocks, collapse = "\n")))

	text = c(text, sprintf("SAVE OUTFILE = \"%s\".", sav_name))

	sps = text |> paste(collapse = "\n\n") |> paste0("\n")
	if (CP1251) sps = sps |> iconv(from = "UTF-8", to = "CP1251")
	cat(sps, file = sps_name)

	haven::write_sav(df, sav_name)
})


