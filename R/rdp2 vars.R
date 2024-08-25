#' @include rdp2.R

DS$set("public", "merge_vars", function(...) {
	# var_names = list(...)
	var_names = self$names(...)
	self$data[[var_names[[1]]]] = var_names |> map(\(var_name) self$data[[var_name]]) |> pmap(\(...) c(...) |> mrcheck())
})

DS$set("public", "to_multiple", function(...) {
	for (var in self$names(...)) {
		if (!is_multiple(self$data[[var]])) self$data[[var]] = map(self$data[[var]], mrcheck)
	}
})

DS$set("public", "to_single", function(...) {
	for (var in self$names(...)) {
		if (is_multiple(self$data[[var]])) {
			if (!all(lengths(self$data[[var]]) <= 1)) stop(sprintf("Error, %s has more than 1 value", var))
			self$data[[var]] = map_dbl(self$data[[var]], \(x) if (length(x) == 0) NA else x[[1]])
		}
	}
})


DS$set("public", "recode", function(vars, ...) {
	for (var in self$names({{ vars }})) {
		self$data[[var]] = recode(self$data[[var]], ...)
	}
})

DS$set("public", "transfer", function(vars, ...) {
	for (var in self$names({{ vars }})) {
		self$data[[var]] = transfer(self$data[[var]], ...)
	}
})

DS$set("public", "recode_empty", function(cols, value, label = NULL) {
	self$data = self$data |> mutate(across({{ cols }}, \(var) recode_empty(var, value)))

	if (!is.null(label)) self$add_labels({{ cols }}, setNames(value, label))
})

DS$set("public", "recalc_empty", function(cols, value, label = NULL) {
	self$vdiscard({{ cols }}, value)
	self$recode_empty({{ cols }}, value, label)
})



DS$set("public", "compute", function(var_name, expr) {
	self$data = self$data |> mutate("{ var_name }" := !!enquo(expr))
})


DS$set("public", "mutate", function(...) {
	self$data = self$data |> mutate(...)
})

DS$set("private", "nv_generic", function(name, label = NULL, labels = NULL, fill = NA, after = NULL, before = NULL, var_type = "single") {
	if (name %in% self$variables) cat("Warning:", name, "is already present. Replacing.\n")

	if (var_type == "single") {
		self$data = self$data |> mutate("{ name }" := fill)
	} else {
		self$data[[name]] = rep(list(numeric(0)), self$nrow)
	}

	if (xor(!is.null(after), !is.null(before))) self$data = self$data |> relocate(all_of(name), .after = {{ after }}, .before = {{ before }})
	if (!is.null(label)) self$var_labels[[name]] = label
	if (!is.null(labels)) self$set_val_labels({{ name }}, labels)
})

DS$set("public", "nvn", function(name, label = NULL, labels = NULL, fill = NA, after = NULL, before = NULL) {
	private$nv_generic(name, label, labels, fill, after, before, "single")
})

DS$set("public", "nvs", function(name, label = NULL, fill = NA, after = NULL, before = NULL) {
	private$nv_generic(name, label, labels = NULL, fill, after, before, "single")
})

DS$set("public", "nvm", function(name, label = NULL, labels = NULL, after = NULL, before = NULL) {
	private$nv_generic(name, label, labels, NULL, after, before, "multiple")
})


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

DS$set("public", "nvs_src", function(vars, suffix = NULL, label_suffix = NULL, move = T, presuffix = NULL) {
	new_vars = self$nvn_src({{ vars }}, suffix = suffix, label_suffix = label_suffix, move = move, presuffix = presuffix)
	self$remove_labels(all_of(new_vars))
	invisible(new_vars)
})



DS$set("public", "transfer_to", function(new_vars, from_vars, ...) {
	walk2(
		self$names({{ new_vars }}),
		self$names({{ from_vars }}),
		\(x, y) self$data[[x]] = self$data[[y]]
	)

	if (length(list(...)) > 0) self$transfer({{ new_vars }}, ...)
})

DS$set("public", "nvclone_to", function(new_var, from_var, label = NULL, after = NULL) {
	self$data = self$data |> mutate("{ new_var }" := .data[[from_var]], .after = {{ after }})

	self$var_labels[[new_var]] = label %||% self$var_labels[[from_var]]
	self$val_labels[[new_var]] = self$val_labels[[from_var]]
})



# to private?
#' @export
set_if_lgl = function(...) UseMethod("set_if_lgl")
#' @export
set_if_lgl.list = function(var, value, condition) modify_at(var, which(condition), \(x) mrcheck(value))
#' @export
set_if_lgl.default = function(var, value, condition) replace(var, condition, value)

DS$set("public", "set_if", function(var, value, ..., label = NULL) {
	condition = Reduce(`|`, map(enquos(...), \(cond) rlang::eval_tidy(cond, data = self$data)))

	self$data[[var]] = set_if_lgl(self$data[[var]], value, condition)

	if (!is.null(label)) self$add_labels({{ var }}, setNames(value, label))
})

DS$set("public", "set_na_if", function(var, ...) {
	self$set_if(var, NA_real_, ...)
})

DS$set("public", "add_if", function(var, value, ..., label = NULL) {
	if (!is_multiple(self$data[[var]])) stop("Error: Expecting variable of multiple type.")

	condition = Reduce(`|`, map(enquos(...), \(cond) rlang::eval_tidy(cond, data = self$data)))

	self$data[[var]] = self$data[[var]] |> modify_at(which(condition), \(x) add_to_mrset(x, value))

	if (!is.null(label)) self$add_labels({{ var }}, setNames(value, label))
})

DS$set("public", "add_net", function(vars, value, ..., label = NULL) {
	# condition = Reduce(`|`, map(enquos(...), \(cond) rlang::eval_tidy(cond, data = self$data)))

	if (!all(self$data |> select({{ vars }}) |> map_lgl(is_multiple))) stop("Error: Expecting variables of multiple type.")

	for (var in self$names({{ vars }})) {
		self$data[[var]] = self$data[[var]] |> modify_at(has(self$data[[var]], ...), \(x) add_to_mrset(x, value))
	}

	if (!is.null(label)) self$add_labels({{ vars }}, setNames(value, label))
})

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

DS$set("public", "vstrip", function(vars, ...) {
	self$vdiscard({{ vars }}, ...)
	self$remove_labels({{ vars }}, ...)
})


DS$set("public", "nvclone", function(vars, ..., suffix = NULL, label_suffix = NULL, labels = NULL, move = T, suffix_position = "auto", else_copy = F) {
	var_names = self$names({{ vars }})

	if (length(var_names) > 1) {
		new_vars = var_renamer(var_names, presuffix = suffix)
	} else {
		new_vars = var_renamer(var_names, suffix = suffix)
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
})

DS$set("public", "make_t2b", function(vars, ..., suffix = "T2B", label_suffix = "(T2B)", labels = NULL, move = T, suffix_position = "auto", else_copy = F) {
	self$nvclone({{ vars }}, ..., suffix = suffix, label_suffix = label_suffix, labels = labels, move = move, suffix_position = suffix_position, else_copy = else_copy)
})

DS$set("public", "make_means", function(vars, ..., suffix = "MEAN", label_suffix = "(MEAN)", move = T, suffix_position = "auto", vdiscard = NULL, else_copy = F) {
	self$nvclone({{ vars }}, ..., suffix = suffix, label_suffix = label_suffix, move = move, suffix_position = suffix_position, else_copy = else_copy)

	var_names = self$names({{ vars }})

	if (length(var_names) > 1) {
		new_vars = var_renamer(var_names, presuffix = suffix)
	} else {
		new_vars = var_renamer(var_names, suffix = suffix)
	}

	self$val_labels[new_vars] = NULL

	if (!is.null(vdiscard)) ds$vdiscard({{ new_vars }}, vdiscard)
})

DS$set("public", "make_nps0_groups", function(vars, suffix = "GROUP", label_suffix = "(GROUP)", move = T, labels = c("Detractors", "Neutrals", "Promoters"), suffix_position = "auto") {
	self$nvclone({{ vars }}, suffix = suffix, label_suffix = label_suffix, labels = labels, move = move, suffix_position = suffix_position, 0:6 ~ 1, 7:8 ~ 2, 9:10 ~ 3)
})

DS$set("public", "make_nps1_groups", function(vars, suffix = "GROUP", label_suffix = "(GROUP)", move = T, labels = c("Detractors", "Neutrals", "Promoters"), suffix_position = "auto") {
	self$nvclone({{ vars }}, suffix = suffix, label_suffix = label_suffix, labels = labels, move = move, suffix_position = suffix_position, 1:7 ~ 1, 8:9 ~ 2, 10:11 ~ 3)
})

DS$set("public", "make_nps0_scores", function(vars, suffix = "SCORE", label_suffix = "(SCORE)", move = T, suffix_position = "auto") {
	self$make_means({{ vars }}, suffix = suffix, label_suffix = label_suffix, move = move, suffix_position = suffix_position, 0:6 ~ -100, 7:8 ~ 0, 9:10 ~ 100)
})

DS$set("public", "make_nps1_scores", function(vars, suffix = "SCORE", label_suffix = "(SCORE)", move = T, suffix_position = "auto") {
	self$make_means({{ vars }}, suffix = suffix, label_suffix = label_suffix, move = move, suffix_position = suffix_position, 1:7 ~ -100, 8:9 ~ 0, 10:11 ~ 100)
})




