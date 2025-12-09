#' @include rdp2.R

DS$set("public", "open_export", function(key, ..., add = NULL, type = "multiple", filename = "open temp", sheet = NULL, alias = NULL, labels = NULL) {
	start_time = Sys.time()
	on.exit(cat("Open export time:", elapsed_fmt(Sys.time() - start_time), "\n"))

	if (!endsWith(filename, ".xlsx")) filename = paste0(filename, ".xlsx")

	if (is.null(sheet)) sheet = if (!is.null(alias) && length(alias) == 1) alias else "Sheet1"

	cols = self$names(...)

	if (!is.null(alias) && length(alias) != length(cols)) stop("varaiables list does not match length of alias list", call. = F)
	if (is.null(alias)) alias = cols

	exp_df = map2(cols, alias, \(x, y) {
		if (is.null(add)) {
			temp_df = self$data |> mutate(var = {{ y }}, codes = NA_real_) |> select(id = {{ key }}, var, text = {{ x }}, codes)
		} else {
			temp_df = self$data |> mutate(var = {{ y }}, codes = NA_real_) |> select(id = {{ key }}, var, all_of(add), text = {{ x }}, codes)
		}

		temp_df |> filter(text != "")
	}) |> bind_rows()

	exp_df = exp_df |> mutate(text = gsub("\n", "; ", chartr("\t\"", "  ", text), fixed = T))

	# labels block
	if (!is.null(labels)) {
		if (is.character(labels)) labels = conv_to_labels(labels)

		labels = split(labels, ceiling(seq_along(labels) / 10))

		labels_block = data.frame(matrix(ncol = 0, nrow = 10))
		for (i in seq_along(labels)) {
			col1 = labels[[i]]
			col2 = names(labels[[i]])
			length(col1) = 10
			length(col2) = 10
			labels_block[[(i - 1) * 3 + 1]] = col1
			labels_block[[(i - 1) * 3 + 2]] = col2
			if (i != length(labels)) labels_block[[(i - 1) * 3 + 3]] = NA
		}
		labels_block = as.data.frame(labels_block)
		labels_block_header = matrix(head(rep(c("code", "label", NA), length(labels)), -1), nrow = 1)
	} else {
		labels_block = NA
		labels_block_header = matrix(c("code", "label"), nrow = 1)
	}

	wb = createWorkbook(creator = "rdp2")
	modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
	addWorksheet(wb, sheetName = sheet)
	freezePane(wb, sheet = 1, firstActiveRow = 13)
	writeData(wb, sheet = 1, xy = c(1, 1), x = "key")
	writeData(wb, sheet = 1, xy = c(2, 1), x = key)
	writeData(wb, sheet = 1, xy = c(1, 2), x = "type")
	writeData(wb, sheet = 1, xy = c(2, 2), x = type)
	writeData(wb, sheet = 1, xy = c(4, 1), x = labels_block_header, colNames = F)
	writeData(wb, sheet = 1, xy = c(4, 2), x = labels_block, colNames = F)
	# writeData(wb, sheet = 1, xy = c(4, 1), x = "code")
	# writeData(wb, sheet = 1, xy = c(5, 1), x = "label")
	writeData(wb, sheet = 1, xy = c(1, 12), x = exp_df)
	saveWorkbook(wb, filename, overwrite = T)
})


DS$set("public", "open_import", function(path, sheets = NULL, after = NULL, before = NULL) {
	if (is.null(sheets)) sheets = getSheetNames(path)

	added_vars = character(0)

	for (sheet in sheets) {
		sheet_data = openxlsx::readWorkbook(path, sheet, colNames = F, skipEmptyRows = F, na.strings = NULL) |> as.matrix()

		params = sheet_data[seq_len(which(is.na(sheet_data[, 1]))[1] - 1), 1:2]
		params = as.list(setNames(params[, 2], params[, 1]))

		data_start = which(sheet_data[, 1] == "id")[1]

		header_vec = sheet_data[1, ]

		cols_ind = list()

		for (i in seq_len(length(header_vec) - 1)) {
			if (!is.na(header_vec[i]) && !is.na(header_vec[i + 1]) && header_vec[i] == "code" && header_vec[i + 1] == "label") {
				cols_ind = c(cols_ind, list(c(i, i + 1)))
			}
		}

		val_labels = do.call(rbind, cols_ind |> map(\(x) sheet_data[seq_len(data_start - 2) + 1, x]))
		val_labels = setNames(as.numeric(val_labels[, 1]), val_labels[, 2])
		val_labels = val_labels[!is.na(val_labels)]
		val_labels = val_labels[!is.na(names(val_labels))]

		header = sheet_data[data_start, ]
		new_header = header[is.na(header) | header == "codes"]
		header[is.na(header) | header == "codes"] = paste_vars("codes", seq_along(new_header))

		data = sheet_data[(data_start + 1):nrow(sheet_data), ] |> as_tibble()
		colnames(data) = header

		all_codes = data |> select(starts_with("codes")) |> unlist()
		all_codes = all_codes[all_codes != ""]
		all_codes = ifelse(is.na(as.numeric(all_codes)), all_codes, NA)
		all_codes = all_codes[!is.na(all_codes)]
		if (length(all_codes) > 0) stop(paste(sheet, "Non-valid codes found:", paste0(all_codes, collapse = ", ")))

		data = data |> mutate(across(starts_with("codes"), as.numeric)) |> select(where(\(x) !all(is.na(x))))

		#add param for char id
		data = data |> mutate(id = as.numeric(id))


		data = data |>
			mutate(codes = do.call(rbind, data |> select(starts_with("codes_"))) |> as.data.frame() |> as.list()) |>
			summarize(codes = list(reduce(codes, c)), .by = c(id, var)) |>
			mutate(codes = codes |> map(\(x) x[!is.na(x)]) |> map(\(x) x[!duplicated(x)]))



		if (any(lengths(data$codes) == 0)) {
			print("warning, empty codings are found")
			data = data |> filter(lengths(codes) > 0)
		}

		vars = unique(data$var)

		if (any(vars %in% self$variables)) stop("Open variables already present in the dataset")

		# check for non-dic values
		data_codes = data$codes |> unlist()
		non_dic_codes = setdiff(data_codes, val_labels)
		unused_codes = setdiff(val_labels, data_codes)

		if (length(non_dic_codes) > 0) cat("Warning: non-dictionary codes found:", paste0(non_dic_codes, collapse = ", "), "\n")
		if (length(unused_codes) > 0) cat("Warning: unused codes found:", paste0(unused_codes, collapse = ", "), "\n")


		if (params$type == "multiple") {
			res = data |>
				mutate(codes = map(codes, sort)) |>
				tidyr::pivot_wider(names_from = var, values_from = codes, values_fill = list(double(0))) |>
				rename("{params$key}" := id)
		}

		if (params$type == "single") {
			res = data |>
				tidyr::pivot_wider(names_from = var, values_from = codes) |>
				mutate(across(!id, \(var) map_dbl(var, \(x) if (is.null(x)) NA else x[1]))) |>
				rename("{params$key}" := id)
		}

		if (params$type == "order") {
			data = data |> tidyr::pivot_wider(names_from = var, values_from = codes, values_fill = list(double(0)))
			new_vars = c()

			for (var in vars) {
				for (i in seq_len(data[[var]] |> lengths() |> max())) {
					new_var = paste_vars(var, i)
					new_vars = c(new_vars, new_var)
					data[[new_var]] = data[[var]] |> map_dbl(\(x) if (length(x) >= i) x[i] else NA)
				}
			}

			res = data |> select(-all_of(vars)) |> rename("{params$key}" := id)

			vars = new_vars
		}

		self$data = self$data |> left_join(res, by = params$key)

		for (var in vars) {
			if (is.list(self$data[[var]])) {
				self$data[[var]][map_lgl(self$data[[var]], is.null)] = list(numeric(0))
				# self$data[[var]] = tidyr::replace_na(self$data[[var]], list(numeric(0)))
			}
		}

		self$set_val_labels({{ vars }}, val_labels)

		if (!(is.null(params$label) || is.na(params$label))) {
			for (var in vars) {
				self$set_var_label(var, params$label)
			}
		}

		cat("Added:", paste0(vars, collapse = ", "), "\n")

		added_vars = c(added_vars, vars)
	}

	if (!rlang::quo_is_null(enquo(after)) || !rlang::quo_is_null(enquo(before))) self$move(all_of(added_vars), after = {{ after }}, before = {{ before }})
})
