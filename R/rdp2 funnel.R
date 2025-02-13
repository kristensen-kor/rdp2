#' @include rdp2.R

# The calc_funnel method creates a funnel analysis table by sequentially evaluating specified variables that represent different stages of a process.
# It ensures that the value labels across these variables are consistent, calculates the distribution of responses at each stage, and computes the conversion rates between consecutive stages.
# Additionally, the method can exclude certain codes, apply weights, and export the resulting funnel table to a file for reporting and further analysis.
DS$set("public", "calc_funnel", function(..., weight = NULL, exclude_codes = NULL, sheet = "", filename = NULL, caption = NULL) {
	vars = self$names(...)

	values = vars |> map(\(var) self$prepare_val_labels(var, warning = T))

	if (!all(map_lgl(values, \(x) identical(x, values[[1]])))) {
		mismatched_vars = which(!map_lgl(values, \(x) identical(x, values[[1]])))
		error_message = "Error: value labels do not match across variables:\n"

		for (var in mismatched_vars) {
			var_1 = values[[1]]
			var_n = values[[var]]

			base_only = setdiff(var_1, var_n)
			comp_only = setdiff(var_n, var_1)

			if (length(base_only) > 0) {
				error_message = paste0(error_message, "- ", vars[1], " exclusive: ", 	paste0(sprintf("[%s] %s", base_only, names(var_1)[var_1 %in% base_only]), collapse = "; "), "\n")
			}

			if (length(comp_only) > 0) {
				error_message = paste0(error_message, "- ", vars[var], " exclusive: ", 	paste0(sprintf("[%s] %s", comp_only, names(var_n)[var_n %in% comp_only]), collapse = "; "), "\n")
			}

			value_matches = intersect(var_1, var_n)
			name_mismatches = which(map_lgl(value_matches, \(val) {
				names(var_1)[which(var_1 == val)] != names(var_n)[which(var_n == val)]
			}))

			mismatched_in_vars_1 = c()
			mismatched_in_vars_n = c()

			for (i in name_mismatches) {
				val = value_matches[i]
				x_name = names(var_1)[which(var_1 == val)]
				y_name = names(var_n)[which(var_n == val)]
				mismatched_in_vars_1 = c(mismatched_in_vars_1, paste0("[", val, "] ", x_name))
				mismatched_in_vars_n = c(mismatched_in_vars_n, paste0("[", val, "] ", y_name))
			}

			if (length(name_mismatches) > 0) {
				error_message = paste0(
					error_message,
					"- ", vars[1], ": ", paste0(mismatched_in_vars_1, collapse = "; "), "; and ", vars[var], ": ", paste0(mismatched_in_vars_n, collapse = "; "), "\n"
				)
			}
		}

		stop(error_message, call. = F)
	}


	distr = vars |> map(\(var) private$calc_raw_table(var, weight))
	bases = distr |> map_dbl(\(x) x[1])

	if (!every(bases, \(x) isTRUE(all.equal(x, bases[1], tolerance = 1e-3)))) stop("Error: variables should have same bases")

	res_table = tibble(
		value = values[[1]],
		label = names(values[[1]]),
		!!!setNames(map(distr, \(x) x[-1]), vars)
	)

	if (!is.null(exclude_codes)) res_table = res_table |> filter(!(value %in% exclude_codes))

	for (i in 2:length(vars)) {
		cur_var = vars[i]
		prev_var = vars[i - 1]
		replacement_indices = which(res_table[[cur_var]] > res_table[[prev_var]])

		if (length(replacement_indices) > 0) {
			for (x in res_table$label[replacement_indices]) {
				warning(paste(cur_var, "is larger than", prev_var, "for", x), call. = F)
			}
			res_table[[cur_var]] = pmin(res_table[[cur_var]], res_table[[prev_var]])
		}
	}


	for (i in seq_along(vars[-1])) {
		conv = res_table[[vars[i + 1]]] /  res_table[[vars[i]]]
		conv[is.na(conv) | is.infinite(conv)] = 0

		if (any(conv > 1)) {
			cat("Warning: some conversions > 100%\n")
			conv[conv > 1] = 1
		}

		col_name = paste_vars("conv", i)
		res_table = res_table |> mutate("{ col_name }" := conv, .after = vars[i])
	}

	res = list(type = "funnel", vars = vars, labels = self$get_var_labels(all_of(vars)), res_table = res_table, base = bases[1], caption = caption, sheet = sheet)

	if (!is.null(filename)) self$funnel_tables(res, filename = filename)

	invisible(res)
})

add_funnel_sheet = function(wb, sheet, data, caption = NULL, coords = NULL, place = NULL, margin = -1) {
	vars = data$vars
	labels = data$labels
	res_table = data$res_table
	base = data$base

	rows_pad = 1
	cols_pad = 0

	if (place == "below" && !is.null(coords)) rows_pad = coords$bottom + margin + 1
	if (place == "right" && !is.null(coords)) cols_pad = coords$right + margin


	ids1 = seq_along(vars) * 2 - 1
	ids2 = seq_along(vars[-1]) * 2

	totals = vars |> map_dbl(\(var) sum(res_table[[var]]))

	avg_brands = rep(NA, length(vars) * 2 - 1)
	avg_brands[ids1] = totals

	avg_convestions = rep(NA, length(vars) * 2 - 1)
	avg_convestions_raw = tail(totals, -1) / head(totals, -1)
	avg_convestions[ids2] = avg_convestions_raw

	# styling
	setColWidths(wb, sheet = sheet, cols = 1:2 + cols_pad, widths = c(3, 25))

	addStyle(wb, sheet = sheet, createStyle(fgFill = "#EFEFEC"), rows = 1:2 + rows_pad, cols = 1:ncol(res_table) + cols_pad, gridExpand = T, stack = T)

	addStyle(wb, sheet = sheet, createStyle(wrapText = T, valign = "top"), rows = 2 + rows_pad, cols = ids1 + 2 + cols_pad, gridExpand = F, stack = T)

	dummy_matrix = matrix(F, ncol = ncol(res_table), nrow = nrow(res_table) + 2)
	dummy_matrix[, ids1] = T
	cells = which(dummy_matrix, arr.ind = TRUE)
	addStyle(wb, sheet = sheet, createStyle(halign = "right", numFmt = "0.0"), rows = cells[, 1] + 2 + rows_pad, cols = cells[, 2] + 2 + cols_pad)

	dummy_matrix = matrix(F, ncol = ncol(res_table), nrow = nrow(res_table) + 2)
	dummy_matrix[, ids2] = T
	cells = which(dummy_matrix, arr.ind = TRUE)
	addStyle(wb, sheet = sheet, createStyle(halign = "right", numFmt = "0%"), rows = cells[, 1] + 2 + rows_pad, cols = cells[, 2] + 2 + cols_pad)

	for (i in seq_len(length(vars) - 1)) {
		cur_bases = res_table[[1 + i * 2]] * base
		sigs = sign_pct_vec(base, avg_convestions_raw[i], cur_bases, res_table[[2 + i * 2]])

		coords = which(matrix(sigs == "+", ncol = 1), arr.ind = TRUE)
		addStyle(wb, sheet = sheet, createStyle(fontColour  = "#66e466"), rows = coords[, 1] + 2 + rows_pad, cols = coords[, 2] + 1 + i * 2 + cols_pad, stack = T)

		coords = which(matrix(sigs == "-", ncol = 1), arr.ind = TRUE)
		addStyle(wb, sheet = sheet, createStyle(fontColour  = "#ed6666"), rows = coords[, 1] + 2 + rows_pad, cols = coords[, 2] + 1 + i * 2 + cols_pad, stack = T)

		coords = which(matrix(cur_bases < 10, ncol = 1), arr.ind = TRUE)
		addStyle(wb, sheet = sheet, createStyle(fontColour  = "#A6A6A6"), rows = coords[, 1] + 2 + rows_pad, cols = coords[, 2] + 1 + i * 2 + cols_pad, stack = T)
	}

	brand_labels = rep(NA, length(vars) * 2 - 1)
	brand_labels[ids1] = labels
	brand_labels[ids2] = "Conversion"

	brand_names = rep(NA, length(vars) * 2 - 1)
	brand_names[ids1] = vars

	res_table = res_table |> mutate(across(all_of(vars), \(x) x * 100))

	writeData(wb, sheet = sheet, xy = c(3 + cols_pad, 1 + rows_pad), x = matrix(brand_names, nrow = 1), colNames = F)
	writeData(wb, sheet = sheet, xy = c(3 + cols_pad, 2 + rows_pad), x = matrix(brand_labels, nrow = 1), colNames = F)
	writeData(wb, sheet = sheet, xy = c(1 + cols_pad, 3 + rows_pad), x = res_table, colNames = F)
	writeData(wb, sheet = sheet, xy = c(2 + cols_pad, nrow(res_table) + 3 + rows_pad), x = "Average conversion")
	writeData(wb, sheet = sheet, xy = c(3 + cols_pad, nrow(res_table) + 3 + rows_pad), x = matrix(avg_convestions, nrow = 1), colNames = F)
	writeData(wb, sheet = sheet, xy = c(2 + cols_pad, nrow(res_table) + 4 + rows_pad), x = "Average number of brands")
	writeData(wb, sheet = sheet, xy = c(3 + cols_pad, nrow(res_table) + 4 + rows_pad), x = matrix(avg_brands, nrow = 1), colNames = F)

	writeData(wb, sheet = sheet, xy = c(1 + cols_pad, nrow(res_table) + 6 + rows_pad), x = "n =")
	addStyle(wb, sheet = sheet, createStyle(halign = "right", numFmt = "0"), rows = nrow(res_table) + 6 + rows_pad, cols = 2 + cols_pad)
	writeData(wb, sheet = sheet, xy = c(2 + cols_pad, nrow(res_table) + 6 + rows_pad), x = base)

	# caption
	addStyle(wb, sheet = sheet, createStyle(textDecoration = "bold"), rows = 1 + rows_pad - 1, cols = 2 + cols_pad)
	addStyle(wb, sheet = sheet, createStyle(fgFill = "#EFEFEC", border = "Bottom"), rows = 1 + rows_pad - 1, cols = 1:ncol(res_table) + cols_pad, gridExpand = T, stack = T)
	caption = paste("Conversion", caption %||% sheet, sep = "|")
	writeData(wb, sheet = sheet, xy = c(2 + cols_pad, 1 + rows_pad - 1), x = caption)

	list(coords = list(bottom = nrow(res_table) + 6 + rows_pad, right = ncol(res_table) + cols_pad), links = list(list(row = 1 + rows_pad - 1, col = 1 + cols_pad, text = caption)))
}

DS$set("public", "funnel_tables", function(..., filename = NULL) {
	if (is.null(filename)) stop("Please specify filename")
	if (!endsWith(filename, ".xlsx")) filename = paste0(filename, ".xlsx")

	data = list(...)

	wb = createWorkbook(creator = "rdp2")
	modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

	sheets = map_chr(data, \(x) x$sheet)
	sheets = ifelse(sheets == "", paste0("Sheet", seq_along(sheets)), sheets)

	walk2(data, sheets, \(x, sheet) {
		addWorksheet(wb, sheet)
		add_funnel_sheet(wb, sheet, x$vars, x$labels, x$res_table, x$base)
	})

	saveWorkbook(wb, filename, overwrite = T)
})
