#' @include rdp2.R

# Merges datasets together into one dataset.
DS$set("public", "merge_data", function(...) {
	start_time = Sys.time()
	on.exit(cat("Merge time:", elapsed_fmt(Sys.time() - start_time), "seconds\n"))

	dss = list(...)

	cat("Starting merge process...\n")
	cat(sprintf("Primary DS - Variables: %d; Rows: %d\n", length(self$variables), self$nrow))

	for (i in seq_along(dss)) {
		ds = dss[[i]]
		cat(sprintf("Dataset %d - Variables: %d; Rows: %d\n", i + 1, length(ds$variables), ds$nrow))
	}

	cat("\n")

	type_mismatches = list()

	for (i in seq_along(dss)) {
		ds = dss[[i]]

		common_vars = intersect(self$variables, ds$variables)

		if (length(common_vars) > 0) {
			self_types = self$var_type(all_of(common_vars))
			ds_types = ds$var_type(all_of(common_vars))

			mismatch_vars = common_vars[which(self_types != ds_types & !(self_types == "single" & ds_types == "numeric"))]

			if (length(mismatch_vars) > 0) {
				for (var in mismatch_vars) {
					type_mismatches[[length(type_mismatches) + 1]] = list(
						variable = var,
						self_type = self_types[which(common_vars == var)],
						ds_type = ds_types[which(common_vars == var)],
						dataset_id = i + 1
					)
				}
			}
		}
	}

	if (length(type_mismatches) > 0) {
		error_message = "Type mismatches found in the following variables:\n"
		for (m in type_mismatches) {
			error_message = paste0(
				error_message,
				"- ", m$variable, " (Dataset ", m$dataset_id, "): self type = ", m$self_type,
				", ds type = ", m$ds_type, "\n"
			)
		}
		stop(error_message, call. = F)
	}

	for (i in seq_along(dss)) {
		ds = dss[[i]]

		matched_cols = intersect(self$variables, ds$variables)
		new_cols = setdiff(ds$variables, self$variables)
		missing_cols_in_ds = setdiff(self$variables, ds$variables)

		report_line = sprintf("Merging Dataset %d: Matched columns: %d", i + 1, length(matched_cols))
		if (length(new_cols) > 0) report_line = paste0(report_line, sprintf("; New columns added: %d", length(new_cols)))
		if (length(missing_cols_in_ds) > 0) report_line = paste0(report_line, sprintf("; Columns missing in ds: %d", length(missing_cols_in_ds)))
		cat(report_line, "\n")

		self$data = self$data |> bind_rows(ds$data)

		for (col in new_cols) {
			if (col %in% names(ds$var_labels)) self$var_labels[[col]] = ds$var_labels[[col]]
			if (col %in% names(ds$val_labels)) self$val_labels[[col]] = ds$val_labels[[col]]
		}
	}

	self$data = self$data |> mutate(across(where(is.list), \(var) map(var, \(x) if (is.null(x)) numeric(0) else x)))

	cat("\nFinal merged dataset:\n")
	cat(sprintf("Total variables: %d\n", length(self$variables)))
	cat(sprintf("Total rows: %d\n\n", self$nrow))
})
