#' @include rdp2.R

# Restructures the dataset by converting specified variable groups into individual cases.
DS$set("public", "vars_to_cases", function(index, ..., index_label = NULL, index_values = NULL, index_labels = NULL) {
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
		self$set_val_labels({{ var_name }}, self$val_labels[[cols[1]]])
		self$set_var_label(var_name, self$var_labels[[cols[1]]])
	})

	if (is.null(index_labels)) index_labels = as.character(index_values)
	self$set_val_labels({{ index }}, setNames(index_values, index_labels))

	if (!is.null(index_label)) self$var_labels[[index]] = index_label

	self$vacuum()

	message(glue("Restruct: {elapsed_fmt(Sys.time() - start_time)}"))

	invisible(NULL)
})
