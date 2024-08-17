#' @include rdp2.R

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
		if (length(var_names) > 0) stop(paste0(paste0(var_names, collapse = ", "), " names conflict"))
	}

	prefix_name = character(0)

	df = self$data

	for (var_name in multiples) {
		values = self$prepare_val_labels(var_name)

		tdf = values |> set_names(paste_vars(var_name, values)) |> map(\(value) map_dbl(df[[var_name]], \(x) any(x == value))) |> bind_cols()

		df = bind_cols(df, tdf)

		if (length(values) == 1) {
			vars = paste_vars(var_name, values)
		} else {
			vars = paste(paste_vars(var_name, values[1]), "TO", paste_vars(var_name, tail(values, 1)))
		}

		prefix_name = c(prefix_name, sprintf("%s ATTRIBUTE = PrefixName(\"%s\")", vars, quotes_escape(str_cleanup(self$get_var_label(var_name)))))

		df = df |> relocate(all_of(paste_vars(var_name, values)), .after = all_of(var_name)) |> select(-all_of(var_name))

		for (i in seq_along(values)) {
			var = paste_vars(var_name, values[i])

			attr(df[[var]], "label") = names(values)[i]
			attr(df[[var]], "labels") = c("-" = 0, "+" = 1)
			attr(df[[var]], "format.spss") = "F1.0"
			class(df[[var]]) = "haven_labelled"
		}
	}

	df = df |> mutate(across(where(is.character), str_cleanup))
	df = df |> mutate(across(where(is.character), \(var) tidyr::replace_na(var, "")))

	# add_labels
	for (var in single_vars) {
		attr(df[[var]], "label") = self$var_labels[[var]]
		if (var %in% names(self$val_labels)) attr(df[[var]], "labels") = setNames(as.double(self$val_labels[[var]]), names(self$val_labels[[var]]))
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

	# mrset
	if (length(prefix_name) > 0) text = c(text, sprintf("VARIABLE ATTRIBUTE\n%s.", sub("/VARIABLES", "VARIABLES", paste0(sprintf("/VARIABLES = %s", prefix_name), collapse = "\n"))))
	if (length(prefix_name) > 0) text = c(text, "KK_MRSET AUTO.")

	text = c(text, sprintf("SAVE OUTFILE = \"%s\".", sav_name))

	sps = text |> paste(collapse = "\n\n") |> paste0("\n")
	if (CP1251) sps = sps |> iconv(from = "UTF-8", to = "CP1251")
	cat(sps, file = sps_name)

	haven::write_sav(df, sav_name)
})
