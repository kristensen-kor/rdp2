# ds-vars-to-cases.R

#' @include class-ds.R


#' Marks a variable group with the label of the resulting long-format variable.
#' @export
cases_var = function(..., label = NULL) {
	assert_nonempty_string(label)
	structure(c(...), label = label)
}

#' Describes the index variable created by `$clone_to_cases()`.
#' @export
cases_index = \(name, label = NULL, val_labels = NULL) list(name = name, label = label, val_labels = val_labels)


#' Clones a dataset and converts specified variable groups into cases.
#' Index codes are normalized into ascending order and assigned to variable positions in that order.
#'
#' @return A transformed clone of the dataset.
DS$set("public", "clone_to_cases", \(index, ..., index_label = NULL, index_labels = NULL) {
	start_time = Sys.time()

	if (is.list(index)) {
		if (!is.null(index_label) || !is.null(index_labels)) stop("`index_label` and `index_labels` must not be supplied when using `cases_index()`.", call. = F)
		index_label = index$label
		index_labels = index$val_labels
		index = index$name
	}

	assert_nonempty_string(index)
	if (!is.null(index_label)) assert_nonempty_string(index_label)

	cols_list = c(...)
	if (length(cols_list) == 0) stop("At least one variable group must be supplied.", call. = F)
	if (is.null(names(cols_list)) || any(names(cols_list) == "") || anyDuplicated(names(cols_list))) stop("Variable groups must have unique, non-empty names.", call. = F)

	output_names = names(cols_list)
	output_labels = map(cols_list, \(vars) attr(vars, "label", exact = T))

	group_lengths = lengths(cols_list)
	if (any(group_lengths == 0)) stop("Variable groups cannot be empty.", call. = F)
	if (length(unique(group_lengths)) > 1) stop("All variable groups must have the same length.", call. = F)

	n_cases = group_lengths[[1]]
	all_cols = unlist(cols_list, use.names = F)

	duplicated_cols = unique(all_cols[duplicated(all_cols)])
	if (length(duplicated_cols)) warning_glue("Certain variables occur in multiple groups: {toString(duplicated_cols)}.")

	missing_vars = setdiff(all_cols, self$variables)
	if (length(missing_vars)) stop_glue("Not all variables are present in the dataset. Missing: {toString(missing_vars)}.")

	base_vars = setdiff(self$variables, all_cols)
	conflicting_names = intersect(output_names, base_vars)

	if (length(conflicting_names)) stop_glue("Resulting variables conflict with variables retained in the dataset: {toString(conflicting_names)}.")
	if (index %in% c(base_vars, output_names)) stop_glue("Index variable `{index}` conflicts with another resulting variable.")

	if (is.null(index_labels)) {
		index_labels = as_val_labels(seq_len(n_cases))
	} else {
		index_labels = as_val_labels(index_labels)
		if (length(index_labels) != n_cases) stop_glue("`index_labels` must define exactly {n_cases} values.")
	}

	index_values = unname(index_labels)


	base_df = self$data |> select(-all_of(all_cols))
	cols_empty = \(df) Reduce(`&`, df |> map(is_empty))

	tds = self$clone()
	tds$data = seq_len(n_cases) |> map(\(i) {
		group_df = tds$data |> select(all_of(map_chr(cols_list, \(cols) cols[[i]]))) |> set_names(output_names)
		selected_rows = !cols_empty(group_df)
		result = base_df[selected_rows, , drop = F]
		result[[index]] = index_values[[i]]
		bind_cols(result, group_df[selected_rows, , drop = F])
	}) |> list_rbind()

	tds$vacuum()

	cols_list |> iwalk(\(cols, var) {
		tds$var_labels[[var]] = output_labels[[var]] %||% self$var_labels[[cols[[1]]]] %||% var
		tds$val_labels[[var]] = self$val_labels[[cols[[1]]]]
	})

	tds$val_labels[[index]] = index_labels
	tds$var_labels[[index]] = index_label %||% index

	message(glue("Clone to cases: {elapsed_fmt(Sys.time() - start_time)}"))

	tds
})



# Restructures the dataset by converting specified variable groups into individual cases.
DS$set("public", "vars_to_cases", function(index, ..., index_label = NULL, index_values = NULL, index_labels = NULL) {
	.Deprecated("clone_to_cases", package = "rdp2", old = "vars_to_cases")

	start_time = Sys.time()

	cols_empty = \(df) Reduce(`&`, df |> map(is_empty))

	cols_list = c(...)
	all_cols = unlist(cols_list)

	if (length(unique(lengths(cols_list))) > 1) stop("All column groups must have the same length.", call. = F)
	if (!all(all_cols %in% self$variables)) {
		missing_vars = setdiff(all_cols, self$variables)
		stop(sprintf("Not all variables are present in the dataframe. Missing: %s", paste(missing_vars, collapse = ", ")), call. = F)
	}

	base_df = self$data |> select(-all_of(all_cols))

	if (is.null(index_values)) index_values = seq_along(cols_list[[1]])

	self$data = index_values |> imap(\(index_value, i) {
		group = map_chr(cols_list, i)
		group_df = self$data |> select(all_of(group))
		selected_cols = !cols_empty(group_df)
		base_df_slice = base_df[selected_cols, ]
		base_df_slice[[index]] = as.numeric(index_value)
		bind_cols(base_df_slice, group_df[selected_cols, ])
	}) |> list_rbind()

	cols_list |> iwalk(\(cols, var_name) {
		if (!is.null(self$val_labels[[cols[1]]])) self$set_val_labels({{ var_name }}, self$val_labels[[cols[1]]])
		self$set_var_label(var_name, self$var_labels[[cols[1]]])
	})

	if (is.null(index_labels)) index_labels = as.character(index_values)
	self$set_val_labels({{ index }}, setNames(index_values, index_labels))

	if (!is.null(index_label)) self$var_labels[[index]] = index_label

	self$vacuum()

	message(glue("Restruct: {elapsed_fmt(Sys.time() - start_time)}"))

	invisible(NULL)
})
