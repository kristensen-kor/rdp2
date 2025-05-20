#' @include rdp2.R

# add block frames?

#' @export
rows_block = function(vars = NULL, filter_var = NULL, filter_value = NULL, caption = NULL) {
	list(caption = caption, vars = vars, filter_var = filter_var, filter_value = filter_value) |> discard(is.null)
}

#' @export
table_block = function(caption = NULL, vars = NULL, filter_var = NULL, filter_value = NULL) {
	cat("table_block() is deprecated, please use rows_block() instead.\n")
	rows_block(caption, vars, filter_var, filter_value)
}


DS$set("public", "prepare_val_labels", function(var, warning = F) {
	values_dic = sort(self$val_labels[[var]])
	values_real = sort(unique(unlist(self$data[[var]])))

	if (warning && !all(values_real %in% values_dic)) cat("Warning, ", var, "has values with no labels\n")

	values_real = setNames(values_real, rep("!!! NO LABEL !!!", length(values_real)))
	values = c(values_dic, values_real)
	values[!duplicated(values)]
})


#' @export
sign_pct = function(n1, p1, n2, p2, sig = 0.05, min_base = 10) {
	if (n1 <= min_base || n2 <= min_base || p1 == p2) return("")
	p = (p1 * n1 + p2 * n2) / (n1 + n2)
	x = abs(p1 - p2) / (p * (1 - p) * (1 / n1 + 1 / n2)) ^ 0.5
	pvalue = pnorm(-x) * 2
	if (pvalue > sig) return("")
	if (p2 > p1) "+" else "-"
}

#' @export
sign_pct_vec = function(n1, p1, n2, p2, sig = 0.05, min_base = 10) {
	result = rep("", length(n2))

	if (n1 <= min_base) return(result)

	p = (p1 * n1 + p2 * n2) / (n1 + n2)
	x = abs(p1 - p2) / (p * (1 - p) * (1 / n1 + 1 / n2)) ^ 0.5
	pvalue = pnorm(-x) * 2

	cond = n2 > min_base & p1 != p2 & pvalue <= sig
	signs = if_else(p2 > p1, "+", "-", "")

	result[cond] = signs[cond]
	result
}

#' @export
sign_mean = function(n1, p1, sd1, n2, p2, sd2, sig = 0.05, min_base = 10) {
	if (n1 <= min_base || n2 <= min_base || p1 == p2) return("")
	s1 = sd1 ^ 2 / n1
	s2 = sd2 ^ 2 / n2
	df = (s1 + s2) ^ 2 / (s1 ^ 2 / (n1 - 1) + s2 ^ 2 / (n2 - 1))
	t = (p1 - p2) / (s1 + s2) ^ 0.5
	pvalue = pt(-abs(t), df) * 2
	if (pvalue > sig) return("")
	if (p2 > p1) "+" else "-"
}

#' @export
sign_mean_vec = function(n1, p1, sd1, n2, p2, sd2, sig = 0.05, min_base = 10) {
	result = rep("", length(n2))

	if (n1 <= min_base) return(result)

	s1 = sd1 ^ 2 / n1
	s2 = sd2 ^ 2 / n2
	df = (s1 + s2) ^ 2 / (s1 ^ 2 / (n1 - 1) + s2 ^ 2 / (n2 - 1))
	t = (p1 - p2) / (s1 + s2) ^ 0.5
	pvalue = pt(-abs(t), df) * 2

	cond = n2 > min_base & p1 != p2 & pvalue <= sig
	signs = if_else(p2 > p1, "+", "-", "")

	result[cond] = signs[cond]
	result
}

calc_sigs_first = function(res, rows, cols, sig) {
	sig_ind_block = map(which(rows$type == "total"), \(x) {
		indices = seq_along(rows$type)
		indices = indices[-(1:x)]
		next_index = rows$type[indices] |> detect_index(\(a) a %in% c("total", "filter", "spacer", "block"))
		if (next_index == 0) x:length(rows$type) else x:(x + next_index - 1)
	})

	do.call(rbind, map(sig_ind_block, \(x) {
		if (rows$type[x[2]] == "mean") {
			n1 = res[[x[1], 1]]
			p1 = res[[x[2], 1]]
			sd1 = res[[x[3], 1]]

			s = sign_mean_vec(n1, p1, sd1, res[x[1], -1], res[x[2], -1], res[x[3], -1], sig = sig)

			res = rbind("", c("", s), "")
		} else {
			n1 = res[x[1], 1]

			s = do.call(rbind, map(x[-1], \(y) {
				p1 = res[[y, 1]]
				sign_pct_vec(n1, p1, res[x[1], -1], res[y, -1], sig = sig)
			}))

			res = rbind("", cbind("", s))
		}

		if (x[1] > 1 && rows$type[x[1] - 1] %in% c("spacer", "block", "filter")) res = rbind("", res)
		if (x[1] > 2 && rows$type[x[1] - 2] %in% c("spacer", "block", "filter")) res = rbind("", res)
		if (x[1] > 3 && rows$type[x[1] - 3] %in% c("spacer", "block", "filter")) res = rbind("", res)
		res
	}))
}

calc_sigs_wave = function(res, rows, cols, sig) {
	sig_ind_block = map(which(rows$type == "total"), \(x) {
		indices = seq_along(rows$type)
		indices = indices[-(1:x)]
		next_index = rows$type[indices] |> detect_index(\(a) a %in% c("total", "filter", "spacer", "block"))
		if (next_index == 0) x:length(rows$type) else x:(x + next_index - 1)
	})

	col_ind_blocks = cols |> mutate(id = row_number()) |> summarise(indices = list(id[-1]), .by = c(var, val)) |> pull(indices)

	do.call(rbind, map(sig_ind_block, \(x) {
		if (rows$type[x[2]] == "mean") {
			s = do.call(cbind, map(col_ind_blocks, \(col_ind_block) {
				cbind("", do.call(cbind, map(col_ind_block, \(ci) {
					sign_mean(res[x[1], ci - 1], res[[x[2], ci - 1]], res[[x[3], ci - 1]], res[x[1], ci], res[[x[2], ci]], res[[x[3], ci]], sig = sig)
				})))
			}))

			res = rbind("", s, "")
		} else {
			s = do.call(rbind, map(x[-1], \(y) {
				do.call(cbind, map(col_ind_blocks, \(col_ind_block) {
					cbind("", do.call(cbind, map(col_ind_block, \(ci) {
						sign_pct(res[x[1], ci - 1], res[[y, ci - 1]], res[x[1], ci], res[y, ci], sig = sig)
					})))
				}))
			}))

			res = rbind("", s)
		}

		if (x[1] > 1 && rows$type[x[1] - 1] %in% c("spacer", "block", "filter")) res = rbind("", res)
		if (x[1] > 2 && rows$type[x[1] - 2] %in% c("spacer", "block", "filter")) res = rbind("", res)
		if (x[1] > 3 && rows$type[x[1] - 3] %in% c("spacer", "block", "filter")) res = rbind("", res)
		res
	}))
}

gen_free_name = function(xs, name) {
	repeat {
		if (!(name %in% xs)) return(name)
		name = paste0(name, "0")
	}
}

calc_rows_simple = function(vec, weights, row_values = NULL) {
	if (!is.null(row_values)) {
		if (length(vec) == 0) {
			res = c(0, rep(NA, length(row_values)))
		} else {
			res = calc_raw_table_nominal(vec, weights, row_values)
		}
	} else {
		res = calc_raw_table_mean(vec, weights)
	}

	matrix(res)
}

validate_weight = function(tds, weight) {
	if (is.null(weight)) {
		weight = gen_free_name(tds$variables, "weight")
		tds$nvs(weight, fill = 1)
	} else {
		if (!is.numeric(tds$data[[weight]])) stop("Weight variable can only be numeric.", call. = F)
		if (any(is.na(tds$data[[weight]]))) stop("Weight variable can't contain NA.", call. = F)
	}

	weight
}

validate_col_vars = function(tds, col_vars) {
	if (is.null(col_vars)) {
		if ("total" %in% tds$variables) {
			if (tds$var_labels$total != "Total" || tds$val_labels$total != c("Total" = 1) || !all(tds$data$total == 1)) stop("Cannot use existing total variable.", call. = F)
		} else {
			tds$add_total()
		}
		col_vars = "total"
	}

	mask = tds$var_type(all_of(col_vars)) %in% c("single", "multiple")

	if (!all(mask)) {
		warning("Omitting column variables that does not match any expected type: ", paste(col_vars[!mask], collapse = ", "), call. = F)
		col_vars = col_vars[mask]
		if (length(col_vars) == 0) stop("No valid column variables left.", call. = F)
	}

	col_vars
}

validate_waves = function(tds, waves) {
	if (!is.null(waves) && !all(tds$var_type(all_of(waves)) %in% c("single", "multiple"))) stop("Waves variable can be only categorical type.", call. = F)

	waves
}

validate_row_vars = function(tds, row_vars) {
	row_vars = as.list(row_vars)

	if (length(row_vars) == 0) stop("Please specify at least one row variable.", call. = F)

	cleaned_rows = list()

	for (var in row_vars) {
		if (!is.list(var)) {
			if (tds$var_type(all_of(var)) %in% c("single", "multiple", "numeric")) {
				cleaned_rows = c(cleaned_rows, list(var))
			} else {
				warning("Omitting row variable that does not match any expected type: ", var, call. = F)
			}
		} else {
			mask = tds$var_type(all_of(var$vars)) %in% c("single", "multiple", "numeric")

			if (!all(mask)) {
				warning("Omitting rows variables that does not match any expected type: ", paste(var$vars[!mask], collapse = ", "), call. = F)
				var$vars = var$vars[mask]
			}

			if (length(var$vars) > 0) {
				cleaned_rows = c(cleaned_rows, list(var))
			}
		}
	}

	if (length(cleaned_rows) == 0) stop("No valid rows variables left.", call. = F)

	cleaned_rows
}


# add filter?
# change sheet = "" to sheet = NULL
# how to treat duplicates in columns?

# The calc_table method generates a cross-tabulation table based on specified row and column variables, optionally applying weights and filtering across different waves or subgroups.
# It handles the aggregation of data, computes summary statistics, manages variable labels, and includes significance markers as specified.
# Additionally, the method can export the resulting table to an Excel file if a filename is provided, facilitating easy reporting and analysis of survey or marketing data.
DS$set("public", "calc_table", function(row_vars, col_vars = NULL, weight = NULL, waves = NULL, sig = "first", conf = 0.95, sheet = "", filename = NULL, open = F) {
	row_vars = self$names({{ row_vars }})
	self$calc_table_rblocks(row_vars, col_vars, weight, waves, sig, conf, sheet, filename, open)
})


# The calc_table_blocks method generates a cross-tabulation table based on specified row and column variables, optionally applying weights and filtering across different waves or subgroups.
# It handles the aggregation of data, computes summary statistics, manages variable labels, and includes significance markers as specified.
# Additionally, the method can export the resulting table to an Excel file if a filename is provided, facilitating easy reporting and analysis of survey or marketing data.
DS$set("public", "calc_table_rblocks", function(row_vars, col_vars = NULL, weight = NULL, waves = NULL, sig = "first", conf = 0.95, sheet = "", filename = NULL, open = F) {
	start_time = Sys.time()
	on.exit(cat("Calculating table", sheet, ":", elapsed_fmt(Sys.time() - start_time), "\n"))

	tds = self$clone()

	weight = validate_weight(tds, weight)
	col_vars = validate_col_vars(tds, col_vars)
	waves = validate_waves(tds, waves)
	row_vars = validate_row_vars(tds, row_vars)

	vars_used = c(
		col_vars,
		waves,
		weight,
		row_vars |> discard(is.list) |> unlist(),
		row_vars |> keep(is.list) |> map("vars") |> unlist(),
		row_vars |> keep(is.list) |> keep(\(x) "filter_var" %in% names(x)) |> map_chr("filter_var")
	)

	tds$data = tds$data |> select(all_of(vars_used))



	tds$variables[!(tds$variables %in% names(tds$var_labels)) & tds$variables != weight] |> walk(\(x) {
		cat("Warning, no label for:", x, "\n")
		tds$var_labels[[x]] = x
	})

	values_list = list()

	for (var_name in tds$variables[tds$is_nominal(tds$variables)]) {
		values_list[[var_name]] = tds$prepare_val_labels(var_name, warning = T)
	}

	waves_cols = "none"
	if (!is.null(waves)) waves_cols = values_list[[waves]]

	res = do.call(cbind, map(col_vars, \(col_var) {
		col_vec = tds$data[[col_var]]

		do.call(cbind, map(values_list[[col_var]], \(col_var_value) {

			cond = has(col_vec, col_var_value)

			do.call(cbind, map(waves_cols, \(wave_var_value) {
				if (wave_var_value != "none") {
					wave_vec = tds$data[[waves]]

					wave_cond = has(wave_vec, wave_var_value)

					cond = cond & wave_cond
				}

				tdf = tds$data[cond, ]

				do.call(rbind, map(row_vars, \(row_var) {
					if (!is.list(row_var)) {
						valid_mask = is_valid(tdf[[row_var]])
						calc_rows_simple(tdf[[row_var]][valid_mask], tdf[[weight]][valid_mask], values_list[[row_var]])
					} else {
						if ("filter_var" %in% names(row_var)) {
							col_vec = tdf[[row_var$filter_var]]

							cond = has(col_vec, row_var$filter_value)

							tdf = tdf[cond, ]
						}

						blocks = do.call(rbind, map(row_var$vars, \(row_var_name) {
							valid_mask = is_valid(tdf[[row_var_name]])
							calc_rows_simple(tdf[[row_var_name]][valid_mask], tdf[[weight]][valid_mask], values_list[[row_var_name]])
						}))

						blocks = rbind(NA, blocks)
						if ("filter_var" %in% names(row_var)) blocks = rbind(NA, blocks)
						if ("caption" %in% names(row_var)) blocks = rbind(NA, blocks)
						blocks
					}
				}))
			}))
		}))
	}))

	cols_tibble = bind_rows(map(col_vars, \(col_var) {
		tibble(
			var = col_var,
			var_label = tds$get_var_label(col_var),
			val = values_list[[col_var]],
			val_label = names(values_list[[col_var]])
		)
	}))

	if (!is.null(waves)) cols_tibble = tidyr::expand_grid(cols_tibble, wave = names(waves_cols))

	rows_tibble = bind_rows(map(row_vars, \(row_var) {
		row_var_names = if(is.list(row_var)) row_var$vars else row_var

		res = bind_rows(map(row_var_names, \(row_var_name) {
			var_vec = tds$data[[row_var_name]]

			if (tds$is_nominal(row_var_name)) {
				val_labels = values_list[[row_var_name]]

				res = tibble(
					var = c(row_var_name, rep(NA, length(val_labels))),
					val = c(NA, val_labels),
					val_label = c(tds$get_var_label(row_var_name), names(val_labels)),
					type = c("total", rep(ifelse(is_multiple(var_vec), "multiple", "single"), length(val_labels)))
				)
			} else {
				res = tibble(
					var = c(row_var_name, NA, NA),
					val = NA,
					val_label = c(tds$get_var_label(row_var_name), NA, NA),
					type = c("total", "mean", "stddev")
				)
			}
		}))

		if (is.list(row_var) && "filter_var" %in% names(row_var)) {
			val_labels = tds$val_labels[[row_var$filter_var]]

			special_row = tibble(
				var = NA,
				val = NA,
				val_label = paste(tds$get_var_label(row_var$filter_var), names(val_labels)[val_labels == row_var$filter_value], sep = ": "),
				type = c("filter")
			)

			res = bind_rows(special_row, res)
		}
		if (is.list(row_var) && "caption" %in% names(row_var)) res = bind_rows(tibble(var = NA, val = NA, val_label = row_var$caption, type = c("block")), res)
		if (is.list(row_var)) res = bind_rows(tibble(var = NA, val = NA, val_label = "", type = c("spacer")), res)

		res
	}))

	# print(cols_tibble)
	# print(res)
	# print(apply(res, 2, \(col) all(col == 0 | is.na(col))))
	# print(cols_tibble = cols_tibble |>
	# 				mutate(is_empty = apply(res, 2, \(col) all(col == 0 | is.na(col)))))

	# remove empty columns
	cols_tibble = cols_tibble |>
		mutate(is_empty = apply(res, 2, \(col) all(col == 0 | is.na(col)))) |>
		group_by(var, val) |> mutate(empty_group = all(is_empty)) |> ungroup()
	# cols_tibble = cols_tibble[which(!apply(res, 2, \(col) all(col == 0 | is.na(col)))), ]

	# sigs = sigs[, -which(apply(res, 2, \(col) all(col == 0)))]
	# res = res[, which(!apply(res, 2, \(col) all(col == 0 | is.na(col)))), drop = F]
	res = res[, which(!cols_tibble$empty_group), drop = F]

	cols_tibble = cols_tibble |> filter(!empty_group) |> select(-is_empty, -empty_group)

	# cols_tibble = cols_tibble |> mutate(across(c(var, var_label, val_label), \(x) ifelse(duplicated(unique_key), NA_character_, x)))

	# print(cols_tibble)
	# print(res)

	if (ncol(res) == 1 || (!is.null(waves) && length(waves_cols) == 1)) sig = "none"

	if (sig == "none") sigs = matrix("", nrow = nrow(res), ncol = ncol(res))

	if (sig == "first" && is.null(waves)) sigs = calc_sigs_first(res, rows_tibble, cols_tibble, 1 - conf)
	if (sig == "first" && !is.null(waves)) sigs = calc_sigs_wave(res, rows_tibble, cols_tibble, 1 - conf)



	if (is.null(waves)) {
		cols_tibble = cols_tibble |> mutate(across(c(var, var_label), \(x) ifelse(duplicated(var), NA_character_, x)))
	} else  {
		cols_tibble = cols_tibble |>
			mutate(unique_key = sprintf("%s - %s", var, val)) |>
			mutate(across(c(var, var_label, val_label), \(x) ifelse(duplicated(unique_key), NA_character_, x))) |>
			select(-unique_key)
		# cols_tibble = cols_tibble |> mutate(across(c(var, var_label, val_label), \(x) ifelse(duplicated(sprintf("%s - %s", var, val)), NA_character_, x)))
		cols_tibble = cols_tibble |> mutate(across(c(var, var_label), \(x) ifelse(duplicated(var), NA_character_, x)))
	}

	cols_tibble = cols_tibble |> select(-val)


	filter_out_stddev = rows_tibble$type != "stddev"
	res = res[filter_out_stddev, , drop = F]
	sigs = sigs[filter_out_stddev, , drop = F]
	rows_tibble = rows_tibble[filter_out_stddev, ]
	# rows_tibble = rows_tibble |> filter(type != "stddev")

	table = list(type = "ctable", res = res, rows = rows_tibble, cols = cols_tibble, sigs = sigs, sheet_name = sheet, filename = filename)

	if (!is.null(filename) || open) {
		if (is.null(filename)) filename = "temp.xlsx"
		if (!endsWith(filename, ".xlsx")) filename = paste0(filename, ".xlsx")
		table$filename = NULL

		xls = XL$new(filename)
		xls$add(table)
		xls$write()
		table$filename = filename
		if (open) shell.exec(filename)
	}

	invisible(table)
})


form_sheet = function(wb, res_tables, sheet, options_format = "num", coords = NULL, place = NULL, margin = -1) {
	rows = res_tables$rows
	cols = res_tables$cols
	res = res_tables$res
	sigs = res_tables$sigs

	off_y = 0
	off_x = 0
	if (!is.null(coords) && place == "below") off_y = coords$bottom + margin + 1
	if (!is.null(coords) && place == "right") off_x = coords$right + margin

	rows_start = if ("wave" %in% names(cols)) 5 else 4
	cols_start = 5
	rows_pad = cols_start - 1
	cols_pad = rows_start - 1


	if (options_format == "num") res[which(rows$type == "single" | rows$type == "multiple"), ] = res[which(rows$type == "single" | rows$type == "multiple"), , drop = F] * 100

	# remove empty first line
	# if (rows$type[[1]] == "spacer") {
	# 	res = res[-1, , drop = F]
	# 	rows = rows[-1, , drop = F]
	# }


	# styling
	if (is.null(coords)) freezePane(wb, sheet = sheet, firstActiveRow = rows_start, firstActiveCol = cols_start)
	setColWidths(wb, sheet = sheet, cols = off_x + 2:4, widths = c(3, 25, 8))

	#EFEFEC
	#F9F9F6

	styles = list()

	# total format
	cells = expand.grid(row = which(rows$type == "total"), col = seq_len(ncol(res)))
	style = createStyle(fgFill = "#EFEFEC", border = "TopBottom", borderStyle = "thin", halign = "right", numFmt = "0")
	styles[[length(styles) + 1]] = list(style = style, rows = cells$row + cols_pad, cols = cells$col + rows_pad)

	# block format
	cells = expand.grid(row = which(rows$type == "block"), col = seq_len(ncol(res)))
	styles[[length(styles) + 1]] = list(style = createStyle(fgFill = "#CCDFFF"), rows = cells$row + cols_pad, cols = cells$col + rows_pad)

	# total < 10
	cells = which(rows$type == "total" & res < 10, arr.ind = T)
	styles[[length(styles) + 1]] = list(style = createStyle(fgFill = "#F9F9F6"), rows = cells[, 1] + cols_pad, cols = cells[, 2] + rows_pad, stack = T)

	# percent values format
	cells = expand.grid(row = which(rows$type == "single" | rows$type == "multiple"), col = seq_len(ncol(res)))
	style = createStyle(halign = "right", numFmt = if (options_format == "pct") "0.0%" else "0.0")
	styles[[length(styles) + 1]] = list(style = style, rows = cells$row + cols_pad, cols = cells$col + rows_pad)

	# mean format
	cells = expand.grid(row = which(rows$type == "mean"), col = seq_len(ncol(res)))
	style = createStyle(halign = "right", numFmt = "0.0")
	styles[[length(styles) + 1]] = list(style = style, rows = cells$row + cols_pad, cols = cells$col + rows_pad)

	# total row format
	cells = expand.grid(row = which(rows$type == "total"), col = seq_len(ncol(rows)))
	style = createStyle(fgFill = "#EFEFEC", border = "TopBottom", borderStyle = "thin", halign = "left")
	styles[[length(styles) + 1]] = list(style = style, rows = cells$row + cols_pad, cols = cells$col, stack = T)

	# filter row format (only column 3)
	style = createStyle(textDecoration = c("bold", "italic"), halign = "left")
	styles[[length(styles) + 1]] = list(style = style, rows = which(rows$type == "filter") + cols_pad, cols = 3, stack = T, gridExpand = T)

	# block caption row format (only column 3)
	style = createStyle(textDecoration = "bold", halign = "left")
	styles[[length(styles) + 1]] = list(style = style, rows = which(rows$type == "block") + cols_pad, cols = 3, stack = T, gridExpand = T)

	# block row format (all columns)
	cells = expand.grid(row = which(rows$type == "block"), col = seq_len(ncol(rows)))
	style = createStyle(fgFill = "#CCDFFF", halign = "left")
	styles[[length(styles) + 1]] = list(style = style, rows = cells$row + cols_pad, cols = cells$col, stack = T)

	# column borders
	style = createStyle(border = "Left", borderStyle = "thin")
	if("wave" %in% names(cols)){
		border_cols = which(cols$val_label != "")
	} else {
		border_cols = which(cols$var != "")
	}
	cells = expand.grid(row = seq_len(nrow(res)), col = border_cols)
	styles[[length(styles) + 1]] = list(style = style, rows = cells$row + cols_pad, cols = cells$col + rows_pad, stack = T)

	cells = expand.grid(row = seq_len(ncol(cols)), col = which(cols$var != ""))
	styles[[length(styles) + 1]] = list(style = style, rows = cells$row, cols = cells$col + rows_pad, stack = T)

	if ("wave" %in% names(cols)) {
		cells = expand.grid(row = 3:4, col = which(cols$val_label != ""))
		styles[[length(styles) + 1]] = list(style = style, rows = cells$row, cols = cells$col + rows_pad, stack = T)
	}

	# columns
	styles[[length(styles) + 1]] = list(style = createStyle(fgFill = "#EFEFEC"), rows = 1:cols_pad, cols = seq_len(nrow(cols)) + rows_pad, stack = T, gridExpand = T)
	styles[[length(styles) + 1]] = list(style = createStyle(wrapText = T, valign = "top"), rows = cols_pad, cols = seq_len(nrow(cols)) + rows_pad, stack = T, gridExpand = T)
	# addStyle(wb, sheet = sheet, createStyle(wrapText = T, fgFill = "red"), rows = 3, cols = seq_len(nrow(cols)) + cols_start, gridExpand = T, stack = T)

	# NET: bold (only column 3)
	net_rows = which(rows$type == "multiple" & startsWith(rows$val_label, "NET:"))
	styles[[length(styles) + 1]] = list(style = createStyle(textDecoration = "bold"), rows = net_rows + cols_pad, cols = 3, stack = T, gridExpand = T)

	# col values border
	styles[[length(styles) + 1]] = list(style = createStyle(border = "Bottom"), rows = 2, cols = seq_len(nrow(cols)) + 4, gridExpand = T, stack = T)

	# table border
	styles[[length(styles) + 1]] = list(style = createStyle(border = "Left"), rows = seq_len(nrow(rows) + cols_pad), cols = 1, gridExpand = T, stack = T)
	styles[[length(styles) + 1]] = list(style = createStyle(border = "Right"), rows = seq_len(nrow(rows) + cols_pad), cols = nrow(cols) + rows_pad, gridExpand = T, stack = T)
	styles[[length(styles) + 1]] = list(style = createStyle(border = "Top"), rows = 1, cols = seq_len(nrow(cols) + rows_pad), gridExpand = T, stack = T)
	styles[[length(styles) + 1]] = list(style = createStyle(border = "Bottom"), rows = nrow(rows) + cols_pad, cols = seq_len(nrow(cols) + rows_pad), gridExpand = T, stack = T)

	# top left corner
	styles[[length(styles) + 1]] = list(style = createStyle(fgFill = "#EFEFEC"), rows = 1:cols_pad, cols = 1:rows_pad, gridExpand = T, stack = T)

	# low base
	find_blocks = function(xs) {
		n = length(xs)

		pairs = which(xs == "total") |> map(\(from) {
			to = from

			while (to < n && xs[to + 1] %in% c("single", "multiple", "mean")) {
				to = to + 1
			}

			seq(from, to)
		})

		pairs
	}

	qblocks = find_blocks(rows$type)

	res_logical = matrix(F, nrow = nrow(res), ncol = ncol(res))

	for (y in seq_len(ncol(res))) {
		for (x in qblocks) {
			if (res[x[1], y] < 10) res_logical[x, y] = T
		}
	}

	coords = which(res_logical, arr.ind = T)

	styles[[length(styles) + 1]] = list(style = createStyle(fontColour = "#A6A6A6"), rows = coords[, 1] + cols_pad, cols = coords[, 2] + rows_pad, stack = T)


	# sigs
	coords = which(matrix(sigs == "+", ncol = ncol(sigs)), arr.ind = TRUE)
	styles[[length(styles) + 1]] = list(style = createStyle(fgFill  = "#66e466"), rows = coords[, 1] + cols_pad, cols = coords[, 2] + rows_pad, stack = T)

	coords = which(matrix(sigs == "-", ncol = ncol(sigs)), arr.ind = TRUE)
	styles[[length(styles) + 1]] = list(style = createStyle(fgFill  = "#ed6666"), rows = coords[, 1] + cols_pad, cols = coords[, 2] + rows_pad, stack = T)

	# setRowHeights(wb, sheet = 1, rows = 3, heights = 38)
	# addStyle(wb, sheet = 1, createStyle(wrapText = T), rows = seq_len(nrow(rows)) + rows_start, cols = 3, gridExpand = T, stack = T)

	# apply styles
	for (x in styles) {
		addStyle(wb, sheet = sheet, x$style, rows = x$rows, cols = off_x + x$cols, stack = x$stack %||% F, gridExpand = x$gridExpand %||% F)
	}

	rows$type[rows$type %in% c("spacer", "block")] = ""

	# write data
	writeData(wb, sheet = sheet, xy = c(off_x + cols_start, rows_start), x = res, colNames = F)
	writeData(wb, sheet = sheet, xy = c(off_x + 1, rows_start), x = rows, colNames = F)
	writeData(wb, sheet = sheet, xy = c(off_x + cols_start, 1), x = t(cols), colNames = F)


	idx = which(!is.na(rows$var))

	links = tibble(
		row = idx + cols_pad,
		text = paste0(rows[idx, ]$var, "|", rows[idx, ]$val_label)
	) |> transpose()

	list(coords = list(bottom = nrow(res) + 6 + rows_start, right = off_x + ncol(res) + cols_start - 1), links = links)
}
