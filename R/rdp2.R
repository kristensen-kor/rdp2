

# aux


#' @export
base_name = function(xs) matches(sprintf("^%s_\\d+$", xs))

#' @export
is_multiple = is.list

#' @export
paste_vars = function(...) paste(..., sep = "_")

#' @export
mrcheck = function(xs) {
	xs = xs[!is.na(xs)]
	unique(xs[order(xs)])
}
# mrcheck = function(xs) xs[!is.na(xs)] |> unique() |> sort()

#' @export
add_to_mrset = function(vec, value) {
	if (is.na(value)) return(vec)
	if (length(vec) == 0) return(value)
	if (value %in% vec) return(vec)
	if (value > max(vec, na.rm = TRUE)) return(c(vec, value))
	c(vec, value) |> mrcheck()
}
# add_to_mrset = function(var, value) c(var, value) |> mrcheck()

is_valid = function(...) UseMethod("is_valid")
is_valid.list = function(xs) lengths(xs) > 0
is_valid.default = function(xs) !is.na(xs)

# recode = function(...) UseMethod("recode")
# recode.list = function(var, ...) map(var, \(x) case_match(x, ..., .default = x) |> mrcheck())
# recode.default = function(var, ...) case_match(var, ..., .default = var)

recode = function(...) UseMethod("recode")
recode.list = function(var, ...) map(var, \(x) case_match_vec_copy(x, ...) |> mrcheck())
recode.default = function(var, ...) case_match_vec_copy(var, ...)

case_match_vec_copy = function(xs, ...) {
	cases = rlang::list2(...)
	result = xs

	for (case in cases) {
		condition = rlang::eval_tidy(rlang::f_lhs(case))
		value = rlang::f_rhs(case)
		mask = if (length(condition) == 1) xs == condition else xs %in% condition
		result[mask] = value
	}

	result
}


transfer = function(...) UseMethod("transfer")
transfer.list = function(var, ...) map(var, \(x) case_match(x, ...) |> mrcheck())
transfer.default = function(var, ...) case_match(var, ...)

#' @export
var_empty = function(...) UseMethod("var_empty")
#' @export
var_empty.list = function(var) lengths(var) == 0
#' @export
var_empty.default = function(var) is.na(var)

#' @export
recode_empty = function(var, value) {
	var[!is_valid(var)] = value
	var
}

#' @export
elapsed_fmt = function(x) {
	elapsed_in_seconds = as.numeric(x, units = "secs")
	if (elapsed_in_seconds < 60) {
		round(elapsed_in_seconds, 1)
	} else {
		sprintf("%02d:%04.1f", floor(elapsed_in_seconds / 60), round(elapsed_in_seconds %% 60, 1))
	}
}

# DS class

# to do
# print

#' @export
DS = R6::R6Class("DS", list(
	data = NULL,
	var_labels = NULL,
	val_labels = NULL
))

DS$set("public", "get_spss", function(filename) {
	start_time = Sys.time()
	on.exit(cat("Read spss:", elapsed_fmt(Sys.time() - start_time), "\n"))

	df_raw = haven::read_spss(filename)

	self$data = df_raw |> modify(\(x) `attributes<-`(x, NULL))
	self$var_labels = df_raw |> map(\(x) attr(x, "label", exact = T)) |> compact()
	self$val_labels = df_raw |> map(\(x) attr(x, "labels", exact = T)) |> compact()
})

DS$set("public", "get_rds", function(filename) {
	save_data = readRDS(filename)

	self$data = save_data$data
	self$var_labels = save_data$var_labels
	self$val_labels = save_data$val_labels
})

DS$set("public", "initialize", function(filename = NULL) {
	if (!is.null(filename)) self$open(filename)
})

DS$set("public", "save", function(filename) {
	if (!endsWith(tolower(filename), ".rds")) filename = paste0(filename, ".rds")

	save_data = list(var_labels = self$var_labels, val_labels = self$val_labels, data = self$data)
	saveRDS(save_data, file = filename)
})

DS$set("public", "open", function(filename) {
	if (endsWith(tolower(filename), ".sav")) {
		self$get_spss(filename)
	} else if (endsWith(tolower(filename), ".rds")) {
		self$get_rds(filename)
	} else {
		stop("Unknown file format")
	}
})

DS$set("active", "variables", \() names(self$data))

DS$set("active", "nrow", \() nrow(self$data))



DS$set("public", "get_var_label", \(var) self$var_labels[[var]] %||% NA)

DS$set("public", "get_var_labels", \(...) self$names(...) |> map_chr(self$get_var_label))

DS$set("public", "get_val_labels", \(var) self$val_labels[[var]] %||% NA)

DS$set("public", "is_nominal", \(vars) (vars %in% names(self$val_labels)) | map_lgl(vars, \(var) is_multiple(self$data[[var]])))

DS$set("public", "get_col_names", \(...) {
	warning("$get_col_names() is deprecated. Please use $names() instead", call. = F)
	self$data |> select(...) |> names()
})

DS$set("public", "names", \(...) self$data |> select(...) |> names())


DS$set("public", "vacuum", function() {
	self$var_labels = self$var_labels[names(self$var_labels) %in% self$variables]
	self$val_labels = self$val_labels[names(self$val_labels) %in% self$variables]
})

DS$set("public", "keep", function(...) {
	var_names = self$names(...)

	self$data = self$data |> select(all_of(var_names))
	self$var_labels = self$var_labels[intersect(names(self$var_labels), var_names)]
	self$val_labels = self$val_labels[intersect(names(self$val_labels), var_names)]
})

DS$set("public", "remove", function(...) {
	var_names = self$names(...)

	# self$data = self$data |> select(-all_of(var_names))
	self$data = self$data[setdiff(names(self$data), var_names)]
	self$var_labels = self$var_labels[setdiff(names(self$var_labels), var_names)]
	self$val_labels = self$val_labels[setdiff(names(self$val_labels), var_names)]
})


DS$set("public", "rename", function(names_from, names_to) {
	cur_names = self$variables

	if (length(names_from) != length(names_to)) stop("'names_from' and 'names_to' vectors must have the same length")
	if (any(duplicated(names_from))) stop("duplicates found in 'names_from' vector")
	if (any(duplicated(names_to))) stop("duplicates found in 'names_to' vector")
	if (any(setdiff(cur_names, names_from) %in% names_to)) stop("renaming would produce duplicated names")

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

DS$set("public", "move", function(..., after = NULL, before = NULL) {
	self$data = self$data |> relocate(..., .after = {{ after }}, .before = {{ before }})
})

DS$set("public", "clone_if", function(...) {
	tds = self$clone()
	tds$data = tds$data |> filter(...)
	tds
})



#' @export
conv_to_labels = function(labels) {
	if (length(labels) == 1) {
		lines = labels |> strsplit("\n") |> unlist() |> trimws()

		valid_lines = grep("^\\d+\\s+\\w", lines, value = T)

		numbers = sub("^(\\d+).*", "\\1", valid_lines) |> as.numeric()
		names = sub("^\\d+\\s+(.*)", "\\1", valid_lines)

		setNames(numbers, names)
	} else {
		setNames(seq_along(labels), labels)
	}
}



DS$set("public", "add_total", \() self$nvn("total", "Total", c("Total" = 1), fill = 1))

DS$set("public", "add_label_suffix", function(vars, suffix, sep = " ") {
	self$var_labels[vars] = map(self$var_labels[vars], \(label) paste(label, suffix, sep = sep))
})

# not needed?
DS$set("public", "set_var_label", function(var, label) {
	self$var_labels[[var]] = label
})

DS$set("public", "set_val_labels", function(vars, labels) {
	if (is.character(labels)) labels = conv_to_labels(labels)

	for (var in self$names({{ vars }})) {
		self$val_labels[[var]] = sort(labels[!duplicated(labels, fromLast = TRUE)])
	}
})

DS$set("public", "set_labels", function(var, label, labels) {
	self$set_var_label({{ var }}, label)
	self$set_val_labels({{ var }}, labels)
})

DS$set("public", "add_labels", function(vars, new_labels) {
	if (is.character(new_labels)) new_labels = conv_to_labels(new_labels)

	for (var in self$names({{ vars }})) {
		labels = c(self$val_labels[[var]], new_labels)
		self$val_labels[[var]] = sort(labels[!duplicated(labels, fromLast = TRUE)])
	}
})



DS$set("public", "remove_labels", function(vars, ...) {
	vars = self$names({{ vars }})

	if (length(list(...)) == 0) self$val_labels[vars] = NULL
	else self$val_labels[vars] = map(self$val_labels[vars], \(label) label[!label %in% c(...)])
})



# add between

#' @export
bitcount = function(var, ...) {
	values = c(...)

	if (length(values) == 0) {
		lengths(var)
	} else {
		map_dbl(var, \(x) length(x[x %in% c(...)]))
	}
}


DS$set("public", "autocode_single", function(...) {
	vars = self$names(...)

	for (var_name in vars) {
		vec = self$data[[var_name]]
		values = NULL

		if (is.numeric(vec)) {
			values = vec |> unique() |> sort()
			vec = formatC(vec, format = "f", big.mark = "", drop0trailing = T)
			values = formatC(values, format = "f", big.mark = "", drop0trailing = T)
		} else {
			values = vec[vec != ""] |> unique() |> sort()
		}

		self$data[[var_name]] = match(vec, values) |> as.double()
		self$set_val_labels(!!sym(var_name), setNames(seq_along(values), values))
	}
})



# usage ds$flip_scale(base_name("Z6C1"), 1:5)
DS$set("public", "flip_scale", function(vars, ...) {
	arg_ids = c(...)

	for (var in self$names({{ vars }})) {
		ids = if (length(arg_ids) == 0) self$val_labels[[var]] |> unname() else arg_ids

		from_index = match(ids, ds$val_labels[[var]])
		to_index = rev(from_index)

		if(length(from_index) == 0) stop("Specified values not found in the vector.")

		names(self$val_labels[[var]])[from_index] = names(self$val_labels[[var]])[to_index]

		self$recode({{ var }}, !!!map(sprintf("%s ~ %s", ids, rev(ids)), as.formula))
	}
})


DS$set("public", "vars_to_cases", function(index, ..., index_label = NULL, index_values = NULL, index_labels = NULL) {
	start_time = Sys.time()
	on.exit(cat("Restruct:", elapsed_fmt(Sys.time() - start_time), "\n"))

	cols_empty = \(df) rowSums(do.call(cbind, df |> map(var_empty))) == length(df)

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
		base_df = base_df[selected_cols, ] |> mutate("{ index }" := index_value)
		bind_cols(base_df, group_df[selected_cols, ])
	}) |> list_rbind()

	cols_list |> iwalk(\(cols, var_name) {
		self$set_val_labels({{ var_name }}, self$val_labels[[cols[1]]])
		self$set_var_label(var_name, self$var_labels[[cols[1]]])
	})

	if (is.null(index_labels)) index_labels = as.character(index_values)
	self$set_val_labels({{ index }}, index_labels)

	if (!is.null(index_label)) self$var_labels[[index]] = index_label
})

DS$set("public", "set_multiples", \() self$conv_multiples())

DS$set("public", "conv_multiples", function() {
	start_time = Sys.time()
	on.exit(cat("Convert multiples:", elapsed_fmt(Sys.time() - start_time), "\n"))

	sep = ": "

	mdset_data = tibble(var_name = self$variables) |>
		filter(grepl("_[0-9]+$", var_name)) |>
		filter(map_lgl(var_name, \(x) identical(self$get_val_labels(x), c("-" = 0, "+" = 1)))) |>
		mutate(label = map_chr(var_name, \(x) self$get_var_label(x))) |>
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

		# col_data = do.call(rbind, current_slice$var_name |> map(\(var_name) {
		# 	id = current_slice[current_slice$var_name == var_name, ]$id
		# 	ifelse(self$data[[var_name]] == 1, id, NA)
		# })) |> as.data.frame() |> as.list() |> map(\(x) x[!is.na(x)]) |> unname()
		#
		# self$data = self$data |> mutate("{ mdset }" := col_data, .before = current_slice$var_name[1]) |> select(-all_of(current_slice$var_name))
		# self$var_labels[[mdset]] = current_slice[1, ]$prefix
		# self$val_labels[[mdset]] = setNames(current_slice$id, current_slice$label)

		col_data = do.call(rbind, seq_len(nrow(current_slice)) |> lapply(\(i) {
			ifelse(self$data[[current_slice$var_name[i]]] == 1, current_slice$id[i], NA)
		})) |> as.data.frame() |> as.list() |> lapply(\(x) x[!is.na(x)]) |> unname()

		self$data[[mdset]] = col_data
		self$data = self$data |> relocate(all_of(mdset), .before = current_slice$var_name[1])
		self$data = self$data[setdiff(names(self$data), current_slice$var_name)]

		self$var_labels[[mdset]] = current_slice$prefix[1]
		self$val_labels[[mdset]] = setNames(current_slice$id, current_slice$label)
	}

	self$vacuum()
})



DS$set("public", "var_view", function(name = NULL, label = NULL, check = F) {
	val_labels_format = function(xs) {
		if (all(is.na(xs))) NA
		else paste0("[", xs, "] ", names(xs)) |> paste(collapse = "; ")
	}

	get_type = function(var_name) {
		if (typeof(self$data[[var_name]]) == "character") return("string")
		if (is_multiple(self$data[[var_name]])) return("multiple")
		if (!var_name %in% names(self$val_labels)) return("scale")
		NA
	}

	res = tibble(
		pos = seq_along(self$variables),
		variable = self$variables,
		label = self$variables |> map_chr(self$get_var_label)
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
		type = variable |> map_chr(get_type),
		val_labels = variable |> map_chr(\(var_name) val_labels_format(self$get_val_labels(var_name)))
	) |> select(pos, variable, type, label, everything())

	if (check) {
		res = res |> mutate(
			unique_values = variable |> map(\(var) self$data[[var]] |> unlist() |> unique() |> discard(\(x) is.na(x) || x == "")),
			empty = ifelse(lengths(unique_values) == 0, "1", ""),
			same = ifelse(lengths(unique_values) == 1, "1", ""),
			valid = variable |> map_dbl(\(var) self$data[[var]] |> map(\(x) if (all(is.na(x)) || all(x == "")) NULL else x) |> compact() |> length()),
			distinct = lengths(unique_values),
			unique_values = NULL
		)
	}

	res
})


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



