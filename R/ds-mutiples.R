# ds-multiples.R

#' @include class-ds.R


#' Returns unique, sorted, finite values from a numeric vector.
#' @export
mrcheck = \(xs) mrcheck_cpp(xs)
# reference implementation:
# mrcheck = function(xs) xs[is.finite(xs)] |> unique() |> sort()

#' Adds a value to a multiple-response set, ensuring uniqueness and order.
#' @export
add_to_mrset = \(vec, value) add_to_mrset_cpp(vec, value)
# reference implementation:
# add_to_mrset = function(var, value) c(var, value) |> mrcheck()



# Converts SPSS-style binary indicator groups into native multiple-response variables.
DS$set("public", "conv_multiples", \(sep = ": ", labels = c("-" = 0, "+" = 1)) {
	start_time = Sys.time()

	assert_nonempty_string(sep)
	labels = as_val_labels(labels)
	if (!(1 %in% unname(labels))) stop("Indicator value labels must contain selected code 1.", call. = F)

	indicator_data = tibble(var_name = self$variables) |>
		filter(grepl("^.+_[0-9]+$", var_name)) |>
		mutate(
			base_name = sub("_[0-9]+$", "", var_name),
			id = sub(".*_([0-9]+)$", "\\1", var_name) |> as.double(),
			is_numeric = map_lgl(var_name, \(var) is.numeric(self$data[[var]])),
			labels_match = map_lgl(var_name, \(var) identical(self$val_labels[[var]], labels))
		)

	mixed_label_bases = indicator_data |>
		filter(any(labels_match) & any(!labels_match), .by = base_name) |>
		summarize(matching = paste(var_name[labels_match], collapse = ", "), not_matching = paste(var_name[!labels_match], collapse = ", "), .by = base_name)
	if (nrow(mixed_label_bases) > 0) {
		warning(
			"Multiple-response indicator groups contain mixed value labels:\n",
			paste0("- ", mixed_label_bases$base_name, ": matching [", mixed_label_bases$matching, "], not matching [", mixed_label_bases$not_matching, "]", collapse = "\n"),
			call. = F
		)
	}
	indicator_data = indicator_data |> filter(labels_match) |> select(-labels_match)

	invalid_types = indicator_data |> filter(!is_numeric)
	if (nrow(invalid_types) > 0) warning(
		"Non-numeric variables with matching indicator labels were omitted: ",
		toString(invalid_types$var_name),
		call. = F
	)
	indicator_data = indicator_data |> filter(is_numeric) |> select(-is_numeric)

	duplicate_ids = indicator_data |> count(base_name, id) |> filter(n > 1)
	if (nrow(duplicate_ids) > 0) {
		details = duplicate_ids |> left_join(indicator_data, by = c("base_name", "id")) |> summarize(vars = paste(var_name, collapse = ", "), .by = c(base_name, id))
		stop("Duplicate numeric indicator IDs within multiple-response groups:\n", paste0("- ", details$base_name, " ID ", details$id, ": ", details$vars, collapse = "\n"), call. = F)
	}


	indicator_data = indicator_data |> mutate(
		var_label = map_chr(var_name, self$get_var_label),
		has_label = !is.na(var_label)
	)

	missing_labels = indicator_data |> filter(!has_label)
	if (nrow(missing_labels) > 0) warning("Multiple-response indicators without variable labels were omitted: ", toString(missing_labels$var_name), call. = F)
	indicator_data = indicator_data |> filter(has_label) |> select(-has_label)


	indicator_data = indicator_data |>
		mutate(
			tokens = strsplit(var_label, sep, fixed = T),
			has_separator = map_lgl(tokens, \(x) length(x) > 1)
		) |>
		select(-var_label)

	bad_labels = indicator_data |> filter(!has_separator)
	if (nrow(bad_labels) > 0) warning_glue("Multiple-response indicators whose labels do not contain `{sep}` were omitted: ", toString(bad_labels$var_name))
	indicator_data = indicator_data |> filter(has_separator) |> select(-has_separator)


	indicator_data = indicator_data |>
		mutate(
			prefix = map_chr(tokens, \(x) trimws(x[1])),
			label = map_chr(tokens, \(x) trimws(paste(x[-1], collapse = sep))),
			has_prefix = nzchar(prefix),
			has_option_label = nzchar(label)
		) |>
		select(-tokens)

	missing_prefixes = indicator_data |> filter(!has_prefix)
	if (nrow(missing_prefixes) > 0) warning("Multiple-response indicators without a label prefix were omitted: ", toString(missing_prefixes$var_name), call. = F)
	indicator_data = indicator_data |> filter(has_prefix) |> select(-has_prefix)

	missing_option_labels = indicator_data |> filter(!has_option_label)
	if (nrow(missing_option_labels) > 0) warning("Multiple-response indicators without an option label were omitted: ", toString(missing_option_labels$var_name), call. = F)
	indicator_data = indicator_data |> filter(has_option_label) |> select(-has_option_label)


	inconsistent_prefix_bases = indicator_data |> filter(n_distinct(prefix) > 1, .by = base_name) |> summarise(details = paste0(var_name, " = \"", prefix, "\"", collapse = ", "), .by = base_name)
	if (nrow(inconsistent_prefix_bases) > 0) warning(
		"Multiple-response groups with inconsistent variable-label prefixes were omitted:\n",
		paste0("- ", inconsistent_prefix_bases$base_name, ": ", inconsistent_prefix_bases$details, collapse = "\n"),
		call. = F
	)

	indicator_data = indicator_data |> filter(!base_name %in% inconsistent_prefix_bases$base_name) |> arrange(base_name, id)


	mdsets = split(indicator_data, indicator_data$base_name)

	if (length(mdsets) == 0) {
		message("No multiple-response sets found")
		return(invisible(NULL))
	}

	existing_targets = intersect(names(mdsets), self$variables)
	if (length(existing_targets) > 0) {
		stop_glue("Cannot convert multiple-response sets because target variables already exist: {toString(existing_targets)}.")
	}

	single_indicator_sets = mdsets[map_int(mdsets, nrow) == 1]
	if (length(single_indicator_sets) > 0) warning(
		"Multiple-response groups with only one indicator will be converted:\n",
		paste0("- ", names(single_indicator_sets), ": ", map_chr(single_indicator_sets, \(x) x$var_name[1]), collapse = "\n"),
		call. = F
	)

	unexpected_values = indicator_data |> mutate(values = map(var_name, \(var) setdiff(unique(self$data[[var]]), c(unname(labels), NA_real_)))) |> filter(lengths(values) > 0)
	if (nrow(unexpected_values) > 0) warning(
		"Multiple-response indicators contain values outside expected codes [", toString(unname(labels)), "]. These values will be treated as not selected and discarded during conversion:\n",
		paste0("- ", unexpected_values$var_name, ": ", map_chr(unexpected_values$values, toString), collapse = "\n"),
		call. = F
	)

	n_indicators = nrow(indicator_data)

	target_names = names(mdsets)
	source_vars = indicator_data$var_name
	anchors = map_chr(mdsets, \(x) x$var_name[1])
	new_columns = setNames(vector("list", length(mdsets)), target_names)

	for (target_name in target_names) {
		mdset = mdsets[[target_name]]

		new_columns[[target_name]] = do.call(rbind, lapply(seq_len(nrow(mdset)), \(i) {
			replace(rep(NA_real_, self$nrow), has(self$data[[mdset$var_name[i]]], 1), mdset$id[i])
		})) |> as.data.frame() |> lapply(\(x) x[!is.na(x)]) |> unname()
	}

	final_names = self$variables
	final_names[match(anchors, final_names)] = target_names
	final_names = final_names[!final_names %in% source_vars]
	self$data = bind_cols(self$data, as_tibble(new_columns)) |> select(all_of(final_names))
	self$var_labels[target_names] = map(mdsets, \(x) x$prefix[1])
	self$val_labels[target_names] = map(mdsets, \(x) setNames(as.double(x$id), x$label))

	self$vacuum()

	message_glue("Converted {length(mdsets)} multiple-response set{if (length(mdsets) == 1) '' else 's'} from {n_indicators} indicator variable{if (n_indicators == 1) '' else 's'}: {elapsed_fmt(Sys.time() - start_time)}")

	invisible(NULL)
})


# Converts specified variables to multiple-response type.
DS$set("public", "to_multiple", \(...) {
	for (var in self$names(...)) {
		if (!is_multiple(self$data[[var]])) self$data[[var]] = map(self$data[[var]], mrcheck)
	}

	invisible(NULL)
})
