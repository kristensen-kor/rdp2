#' @include rdp2.R

#' Counts occurrences of specified values within a variable.
#' @export
bitcount = function(var, ...) {
	values = c(...)

	if (length(values) == 0) {
		lengths(var)
	} else {
		map_dbl(var, \(x) length(x[x %in% c(...)]))
	}
}

# Automatically codes single-response variables based on provided labels.
DS$set("public", "autocode_single", function(..., labels = NULL, nomatch = NA) {
	vars = self$names(...)

	for (var_name in vars) {
		vec = self$data[[var_name]]
		values = NULL

		if (is.numeric(vec)) {
			values = vec |> unique() |> sort()
			vec = formatC(vec, format = "f", big.mark = "", drop0trailing = T)
			values = formatC(values, format = "f", big.mark = "", drop0trailing = T)
		} else {
			if (is.null(labels)) {
				values = vec[vec != ""] |> unique() |> sort()
			} else {
				# if (is.character(labels)) labels = conv_to_labels(labels)
				# values = names(labels)

				values = labels
			}

			not_found_counts = vec[!(vec %in% values)] |> as_tibble() |> count(value, sort = T)

			if (nrow(not_found_counts) > 0) {
				cat(var_name, "values not from the list:\n")
				print(not_found_counts)
			}
		}

		self$data[[var_name]] = match(vec, values, nomatch = nomatch) |> as.double()
		self$set_val_labels(all_of(var_name), setNames(seq_along(values), values))
	}
})

# Merges multiple variables into a single variable by combining their values.
DS$set("public", "merge_vars", function(...) {
	# var_names = list(...)
	var_names = self$names(...)
	self$data[[var_names[[1]]]] = var_names |> map(\(var_name) self$data[[var_name]]) |> pmap(\(...) c(...) |> mrcheck())
})

# Converts specified variables to multiple-response type.
DS$set("public", "to_multiple", function(...) {
	for (var in self$names(...)) {
		if (!is_multiple(self$data[[var]])) self$data[[var]] = map(self$data[[var]], mrcheck)
	}
})

# Converts specified variables to single-response type.
DS$set("public", "to_single", function(...) {
	for (var in self$names(...)) {
		if (is_multiple(self$data[[var]])) {
			if (!all(lengths(self$data[[var]]) <= 1)) stop(sprintf("Error, %s has more than 1 value", var))
			self$data[[var]] = map_dbl(self$data[[var]], \(x) if (length(x) == 0) NA else x[[1]])
		}
	}
})

# Recodes specified variables based on provided mapping.
DS$set("public", "recode", function(vars, ...) {
	for (var in self$names({{ vars }})) {
		self$data[[var]] = recode(self$data[[var]], ...)
	}
})

# Transfers values of specified variables based on provided conditions.
DS$set("public", "transfer", function(vars, ...) {
	for (var in self$names({{ vars }})) {
		self$data[[var]] = transfer(self$data[[var]], ...)
	}
})

# Recodes empty values in specified columns to a given value and optionally adds a label.
DS$set("public", "recode_empty", function(cols, value, label = NULL, filter = NULL) {
	filter_quosure = rlang::enquo(filter)
	mask = rep(T, self$nrow)
	if (!rlang::quo_is_null(filter_quosure)) mask = rlang::eval_tidy(filter_quosure, data = self$data)

	recode_empty = function(var, value) {
		var[var_empty(var) & mask] = value
		var
	}

	self$data = self$data |> mutate(across({{ cols }}, \(var) recode_empty(var, value)))

	if (!is.null(label)) self$add_labels({{ cols }}, setNames(value, label))
})

# Recalculates empty values by discarding existing ones and recoding to a new value.
DS$set("public", "recalc_empty", function(cols, value, label = NULL, filter = NULL) {
	self$vdiscard({{ cols }}, value)
	self$recode_empty({{ cols }}, value, label, {{ filter }})
})


# Computes a new variable based on an expression.
DS$set("public", "compute", function(var_name, expr) {
	self$data = self$data |> mutate("{ var_name }" := !!enquo(expr))
})

# Applies mutation functions to the dataset.
DS$set("public", "mutate", function(...) {
	self$data = self$data |> mutate(...)
})




# Renames variables by adding a suffix or prefix.
var_renamer = function(var_names, suffix = NULL, presuffix = NULL) {
	stopifnot(xor(is.null(suffix), is.null(presuffix)))

	if (!is.null(suffix)) {
		paste_vars(var_names, suffix)
	} else {
		parts = strsplit(var_names, "_", fixed = T)
		stopifnot(lengths(parts) > 1)
		parts |> map_chr(\(x) paste(c(head(x, -1), presuffix, last(x)), collapse = "_"))
	}
}

# Creates new variables from source variables with optional suffixes and labels.
DS$set("public", "nvn_src", function(vars, suffix = NULL, label_suffix = NULL, labels = NULL, move = T, presuffix = NULL) {
	var_names = self$names({{ vars }})
	new_vars = var_renamer(var_names, suffix, presuffix)

	walk2(
		var_names,
		new_vars,
		\(var, new_var) self$nvn(new_var, self$var_labels[[var]], labels %||% self$val_labels[[var]])
	)

	if (!is.null(label_suffix)) self$var_labels[new_vars] = self$var_labels[new_vars] |> map(\(text) paste(text, label_suffix, sep = " "))
	if (move) self$data = self$data |> relocate(all_of(new_vars), .after = {{ vars }})

	invisible(new_vars)
})

# Creates new numeric source variables and removes their labels.
DS$set("public", "nvs_src", function(vars, suffix = NULL, label_suffix = NULL, move = T, presuffix = NULL) {
	new_vars = self$nvn_src({{ vars }}, suffix = suffix, label_suffix = label_suffix, move = move, presuffix = presuffix)
	self$remove_labels(all_of(new_vars))
	invisible(new_vars)
})


# Transfers values from one set of variables to another.
DS$set("public", "transfer_to", function(new_vars, from_vars, ...) {
	walk2(
		self$names({{ new_vars }}),
		self$names({{ from_vars }}),
		\(x, y) self$data[[x]] = self$data[[y]]
	)

	if (length(list(...)) > 0) self$transfer({{ new_vars }}, ...)
})

# Clones a variable to a new variable with optional label and position.
DS$set("public", "nvclone_to", function(new_var, from_var, label = NULL, after = NULL, filter) {
	self$data = self$data |> mutate("{ new_var }" := .data[[from_var]], .after = {{ after }})

	if (!missing(filter)) self$set_na_if({{ new_var }}, !{{ filter }})

	self$var_labels[[new_var]] = label %||% self$var_labels[[from_var]]
	self$val_labels[[new_var]] = self$val_labels[[from_var]]
})



# Internal method to set values based on logical conditions.
set_if_lgl = function(...) UseMethod(".set_if_lgl")
.set_if_lgl.list = function(var, value, condition) modify_at(var, which(condition), \(x) mrcheck(value))
.set_if_lgl.default = function(var, value, condition) replace(var, condition, value)

# Sets values of a variable based on provided logical conditions and optionally adds a label.
DS$set("public", "set_if", function(var, value, ..., label = NULL) {
	if (!(is.null(label) || (rlang::is_string(label) && nzchar(label)))) stop("Value label must be a non-empty character scalar", call. = F)
	# condition = Reduce(`|`, map(enquos(...), \(cond) rlang::eval_tidy(cond, data = self$data)))
	#
	# self$data[[var]] = set_if_lgl(self$data[[var]], value, condition)

	if (length(enquos(...)) > 1) {
		stop("Multiple logical conditions are not supported. Please provide only one condition.", call. = F)
	}

	condition = Reduce(`|`, map(enquos(...), \(cond) rlang::eval_tidy(cond, data = self$data)))

	self$data[[var]] = set_if_lgl(self$data[[var]], value, condition)

	if (!is.null(label)) self$add_labels({{ var }}, setNames(value, label))
})

# Sets values of a variable to NA based on provided logical conditions.
DS$set("public", "set_na_if", function(vars, ...) {
	for (var in self$names({{ vars }})) {
		self$set_if(var, NA, ...)
	}
})

# Adds a value to a multiple-response variable based on provided conditions and optionally adds a label.
DS$set("public", "add_if", function(var, value, condition, label = NULL) {
	if (!(is.null(label) || (rlang::is_string(label) && nzchar(label)))) stop("Value label must be a non-empty character scalar", call. = F)
	if (!is_multiple(self$data[[var]])) stop("Error: Expecting variable of multiple type.", call. = F)

	mask = rlang::eval_tidy(enquo(condition), data = self$data)

	self$data[[var]][mask] = add_to_mc_col_cpp(self$data[[var]][mask], value)

	if (!is.null(label)) self$add_labels({{ var }}, setNames(value, label))
})

# Adds a net value to multiple-response variables based on provided conditions and optionally adds a label.
DS$set("public", "add_net", function(vars, value, ..., label = NULL) {
	if (!(is.null(label) || (rlang::is_string(label) && nzchar(label)))) stop("Value label must be a non-empty character scalar", call. = F)

	if (!all(self$data |> select({{ vars }}) |> map_lgl(is_multiple))) stop("Error: Expecting variables of multiple type.", call. = F)

	for (var in self$names({{ vars }})) {
		mask = has(self$data[[var]], ...)
		self$data[[var]][mask] = add_to_mc_col_cpp(self$data[[var]][mask], value)
	}

	if (!is.null(label)) self$add_labels({{ vars }}, setNames(value, label))
})

# Discards specified values from variables.
DS$set("public", "vdiscard", function(vars, ...) {
	values = c(...)

	for (var in self$names({{ vars }})) {
		if (is_multiple(self$data[[var]])) {
			self$data[[var]] = self$data[[var]] |> map(\(x) x[is.na(match(x, values))])
		} else {
			self$data[[var]][has(self$data[[var]], values)] = NA
		}
	}
})

# Discards specified values and removes their labels from variables.
DS$set("public", "vstrip", function(vars, ...) {
	self$vdiscard({{ vars }}, ...)
	self$remove_labels({{ vars }}, ...)
})

# Clones variables with optional suffixes, labels, and repositioning.
DS$set("public", "nvclone", function(vars, ..., suffix = NULL, label_suffix = NULL, labels = NULL, move = T, suffix_position = "auto", else_copy = F) {
	var_names = self$names({{ vars }})

	if (length(var_names) == 1 || suffix_position == "end") {
		new_vars = var_renamer(var_names, suffix = suffix)
	} else {
		new_vars = var_renamer(var_names, presuffix = suffix)
	}

	walk2(var_names, new_vars, \(var, new_var) {
		if (new_var %in% self$variables) cat("Warning:", new_var, "is already present. Replacing.\n")

		self$nvclone_to(new_var, var)
	})

	if (!is.null(label_suffix)) self$var_labels[new_vars] = map(self$var_labels[new_vars], \(text) paste(text, label_suffix, sep = " "))
	if (!is.null(labels)) self$set_val_labels(all_of(new_vars), labels)

	if (move) self$data = self$data |> relocate(all_of(new_vars), .after = {{ vars }})

	if (length(list(...)) > 0 && else_copy) self$recode(all_of(new_vars), ...)
	if (length(list(...)) > 0 && !else_copy) self$transfer(all_of(new_vars), ...)

	invisible(new_vars)
})

# Creates mean scores for specified variables with optional suffixes and labels.
DS$set("public", "make_means", function(vars, ..., suffix = "MEAN", label_suffix = "(MEAN)", move = T, suffix_position = "auto", vdiscard = NULL, else_copy = F) {
	self$nvclone({{ vars }}, ..., suffix = suffix, label_suffix = label_suffix, move = move, suffix_position = suffix_position, else_copy = else_copy)

	var_names = self$names({{ vars }})

	if (length(var_names) == 1 || suffix_position == "end") {
		new_vars = var_renamer(var_names, suffix = suffix)
	} else {
		new_vars = var_renamer(var_names, presuffix = suffix)
	}

	self$val_labels[new_vars] = NULL

	if (!is.null(vdiscard)) self$vdiscard({{ new_vars }}, vdiscard)
})

# Creates NPS groups based on specified scoring criteria with optional suffixes and labels.
DS$set("public", "make_nps0_groups", function(vars, suffix = "GROUP", label_suffix = "(GROUP)", move = T, labels = c("Detractors", "Neutrals", "Promoters"), suffix_position = "auto") {
	self$nvclone({{ vars }}, suffix = suffix, label_suffix = label_suffix, labels = labels, move = move, suffix_position = suffix_position, 0:6 ~ 1, 7:8 ~ 2, 9:10 ~ 3)
})

# Creates NPS groups based on an alternative scoring criteria with optional suffixes and labels.
DS$set("public", "make_nps1_groups", function(vars, suffix = "GROUP", label_suffix = "(GROUP)", move = T, labels = c("Detractors", "Neutrals", "Promoters"), suffix_position = "auto") {
	self$nvclone({{ vars }}, suffix = suffix, label_suffix = label_suffix, labels = labels, move = move, suffix_position = suffix_position, 1:7 ~ 1, 8:9 ~ 2, 10:11 ~ 3)
})

# Creates NPS scores based on specified scoring criteria with optional suffixes and labels.
DS$set("public", "make_nps0_scores", function(vars, suffix = "SCORE", label_suffix = "(SCORE)", move = T, suffix_position = "auto") {
	self$make_means({{ vars }}, suffix = suffix, label_suffix = label_suffix, move = move, suffix_position = suffix_position, 0:6 ~ -100, 7:8 ~ 0, 9:10 ~ 100)
})

# Creates NPS groups based on an alternative scoring criteria with optional suffixes and labels.
DS$set("public", "make_nps1_scores", function(vars, suffix = "SCORE", label_suffix = "(SCORE)", move = T, suffix_position = "auto") {
	self$make_means({{ vars }}, suffix = suffix, label_suffix = label_suffix, move = move, suffix_position = suffix_position, 1:7 ~ -100, 8:9 ~ 0, 10:11 ~ 100)
})




