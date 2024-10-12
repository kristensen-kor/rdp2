#' @include rdp2.R

DS$set("public", "flip_scale", function(vars, ...) {
	stop("$flip_scale() is deprecated. Please use $scale_flip() instead", call. = F)
})

# usage ds$scale_flip(base_name("Z6C1"), 1:5)
DS$set("public", "scale_flip", function(vars, ...) {
	arg_ids = c(...)

	for (var in self$names({{ vars }})) {
		ids = if (length(arg_ids) == 0) self$val_labels[[var]] |> unname() else arg_ids

		from_index = match(ids, self$val_labels[[var]])
		to_index = rev(from_index)

		if (length(from_index) == 0) stop("Specified values not found in the vector.")

		names(self$val_labels[[var]])[from_index] = names(self$val_labels[[var]])[to_index]

		self$recode({{ var }}, !!!map(sprintf("%s ~ %s", ids, rev(ids)), as.formula))
	}
})


DS$set("public", "scale_dense", function(vars) {
	for (var in self$names({{ vars }})) {
		if (!(var %in% names(self$val_labels))) stop("Non-categorical variable")

		ids = self$val_labels[[var]] |> unname()
		self$val_labels[[var]] = setNames(seq_along(ids), names(self$val_labels[[var]]))


		recode_simple = function(xs, ids) {
			result = xs

			for (i in seq_along(ids)) {
				result[xs == ids[i]] = i
			}

			result
		}

		if (is_multiple(self$data[[var]])) {
			self$data[[var]] = self$data[[var]] |> map(\(x) recode_simple(x, ids) |> mrcheck())
		} else {
			self$data[[var]] = recode_simple(self$data[[var]], ids)
		}
	}
})


DS$set("public", "scale_shift", function(vars, from, amount = 1) {
	for (var in self$names({{ vars }})) {
		mask = self$val_labels[[var]] >= from
		self$val_labels[[var]][mask] = self$val_labels[[var]][mask] + amount

		if (is_multiple(self$data[[var]])) {
			self$data[[var]] = self$data[[var]] |> map(\(x) ifelse(x >= from, x + amount, x))
		} else {
			mask = self$data[[var]] >= from
			self$data[[var]][mask] = self$data[[var]][mask] + amount
		}
	}
})


DS$set("public", "scale_move", function(vars, ...) {
	values = rlang::list2(...)

	for (value in values) {
		lhs = rlang::eval_tidy(rlang::f_lhs(value))
		rhs = rlang::eval_tidy(rlang::f_rhs(value))
		if (length(lhs) != 1 || length(rhs) != 1) stop("Both sides of the '~' must be of length 1.", call. = F)

		for (var in self$names({{ vars }})) {
			if (rhs %in% unlist(self$data[[var]])) stop(var, " already has " , rhs, call. = F)
			if (!(lhs %in% self$val_labels[[var]])) stop(var, " has no label for ", lhs, call. = F)
		}
	}

	self$recode(vars, ...)

	for (value in values) {
		lhs = rlang::eval_tidy(rlang::f_lhs(value))
		rhs = rlang::eval_tidy(rlang::f_rhs(value))

		for (var in self$names({{ vars }})) {
			self$add_labels({{ var }}, setNames(rhs, names(which(self$val_labels[[var]] == lhs))))
		}
		self$remove_labels(vars, lhs)
	}
})
