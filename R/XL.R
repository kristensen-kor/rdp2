#' @include rdp2.R

#' XL Class
#'
#' A class to manage Excel files.
#'
#' @name XL
#' @export
XL = R6::R6Class("XL", list(
	filename = "",
	wb = NULL,
	sheets = character(),
	links = list(),
	coords = list(),
	options_format = "num"
))


# Initializes a new Excel workbook with a specified filename.
XL$set("public", "initialize", function(filename = NULL) {
	if (!is.null(filename)) {
		if (!endsWith(filename, ".xlsx")) filename = paste0(filename, ".xlsx")

		self$filename = filename
	} else {
		cat("Creating temp.xlsx workbook.\n")
		self$filename = "temp.xlsx"
	}

	self$wb = createWorkbook(creator = "rdp2")
	modifyBaseFont(self$wb, fontSize = 10, fontName = "Arial")
})


# Adds a new sheet with the provided table to the Excel workbook.
XL$set("public", "add", \(sheet, table = NULL) {
	if (is.null(table)) {
		table = sheet
		sheet = paste0("Sheet", length(self$sheets) + 1)
	}

	start_time = Sys.time()
	on.exit(cat(paste0("Added ", sheet, ":"), elapsed_fmt(Sys.time() - start_time), "\n"))

	if (!is.null(table$filename)) stop("Can't add table with filename")

	if (!(sheet %in% self$sheets)) {
		addWorksheet(self$wb, sheet)
		self$sheets = c(self$sheets, sheet)
	}

	if (table$type == "ctable") {
		res = form_sheet(self$wb, table, sheet, options_format = self$options_format)
		self$links[[sheet]] = res$links
		self$coords[[sheet]] = res$coords
	} else if (table$type == "funnel") {
		res = add_funnel_sheet(self$wb, sheet, table, table$caption)
		self$links[[sheet]] = c(self$links[[sheet]], res$links)
		self$coords[[sheet]] = res$coords
	} else {
		stop("Unrecognized table format")
	}
})


XL_add_append = function(self, sheet, table, place, margin) {
	start_time = Sys.time()
	on.exit(cat(paste0("Added ", sheet, ":"), elapsed_fmt(Sys.time() - start_time), "\n"))

	if (!is.null(table$filename)) stop("Can't add table with filename")
	# if (table$type == "ctable") stop("$add_right() and $add_below are not supported for ctables")

	if (!(sheet %in% self$sheets)) {
		addWorksheet(self$wb, sheet)
		self$sheets = c(self$sheets, sheet)
	}

	if (table$type == "ctable") {
		res = form_sheet(self$wb, table, sheet, options_format = self$options_format, coords = self$coords[[sheet]], place = place, margin = margin)
		# if (!(sheet %in% self$links)) self$links[[sheet]] = res$links
		self$coords[[sheet]] = res$coords
	} else if (table$type == "funnel") {
		res = add_funnel_sheet(self$wb, sheet, table, table$caption, coords = self$coords[[sheet]], place = place, margin = margin)
		self$links[[sheet]] = c(self$links[[sheet]], res$links)
		self$coords[[sheet]] = res$coords
	} else {
		stop("Unrecognized table format")
	}
}

# Appends a table to the right side of the specified sheet in the Excel workbook.
XL$set("public", "add_right", \(sheet, table, margin = 1) XL_add_append(self, sheet, table, place = "right", margin))

# Appends a table below the specified sheet in the Excel workbook.
XL$set("public", "add_below", \(sheet, table, margin = 1) XL_add_append(self, sheet, table, place = "below", margin))


XL_add_contents = function(self) {
	contents_sheet = length(self$sheets) + 1

	addWorksheet(self$wb, sheetName = "Contents")

	writeData(self$wb, sheet = contents_sheet, xy = c(1, 1), x = "Contents")

	nrows = c(0, map_dbl(self$links, \(x) length(x)) |> cumsum())

	formulas = map_chr(seq_along(self$links), \(i) {
		makeHyperlinkString(
			sheet = "Contents", row = 4 + length(self$links) + nrows[[i]] + i * 2 - 2, col = 1,
			text = self$sheets[[i]]
		)
	})

	writeFormula(self$wb, sheet = contents_sheet, xy = c(1, 3), x = formulas)

	seq_along(self$links) |> walk(\(sheet) {
		formulas = map_chr(self$links[[sheet]], \(link) {
			makeHyperlinkString(
				sheet = self$sheets[[sheet]], row = link$row, col = link$col %||% 1,
				text = gsub('"', '""', link$text, fixed = TRUE)
			)
		})
		sheet_link = makeHyperlinkString(sheet = self$sheets[[sheet]], row = 1, col = 1, text = self$sheets[[sheet]])
		writeFormula(self$wb, sheet = contents_sheet, xy = c(1, 4 + length(self$links) + nrows[[sheet]] + sheet * 2 - 2), x = sheet_link)
		writeFormula(self$wb, sheet = contents_sheet, xy = c(2, 5 + length(self$links) + nrows[[sheet]] + sheet * 2 - 2), x = formulas)
	})

	worksheetOrder(self$wb) = c(contents_sheet, seq_len(length(self$sheets)))
	activeSheet(self$wb) = contents_sheet
}

# Adds a "Contents" sheet with hyperlinks to all other sheets in the workbook.
XL$set("public", "add_contents", \() XL_add_contents(self))


XL_write = function(self) {
	start_time = Sys.time()
	on.exit(cat(self$filename, "writing time:", elapsed_fmt(Sys.time() - start_time), "\n"))

	saveWorkbook(self$wb, self$filename, overwrite = T)
}

# Writes the Excel workbook to the specified filename, saving all changes.
XL$set("public", "write", \() XL_write(self))

# Adds the "Contents" sheet and then writes the workbook to file.
XL$set("public", "add_contents_and_write", function() {
	self$add_contents()
	self$write()
})

# Open tables in Excel.
XL$set("public", "open", function() {
	shell.exec(self$filename)
})


# wrapper for simple one table exports
qexport = function(table, filename = NULL) {
	if (is.null(filename)) stop("filename is required.")

	xls = XL$new(filename)
	xls$add(table)
	xls$write()
}


# xls = ExcelTable$new("11299-2 tables waves 2024-07-01 v6")
# # xls$options(fmt = "pct")
# xls$add("Sell", ...)
# xls$add_right("Sell", ...)
# xls$add_below("Sell", ...)
# xls$append("Sell", ...)
# xls$overwrite("Sell", ...)
# xls$write()
