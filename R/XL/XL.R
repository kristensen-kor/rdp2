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
	options_format = "num"
))


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


XL_add = function(self, sheet, table) {
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

	self$links[[sheet]] = form_sheet(self$wb, table, sheet, options_format = self$options_format)
}

XL$set("public", "add", \(sheet, table = NULL) XL_add(self, sheet, table))


XL_add_contents = function(self) {
	start_time = Sys.time()
	on.exit(cat("Contents time:", elapsed_fmt(Sys.time() - start_time), "\n"))

	contents_sheet = length(self$sheets) + 1

	addWorksheet(self$wb, sheetName = "Contents")

	writeData(self$wb, sheet = contents_sheet, xy = c(1, 1), x = "Contents")

	nrows = c(0, map_dbl(self$links, \(x) length(x$row)) |> cumsum())

	formulas = map_chr(seq_along(self$links), \(i) {
		makeHyperlinkString(
			sheet = "Contents", row = 4 + length(self$links) + nrows[[i]] + i * 2 - 2, col = 1,
			text = self$sheets[[i]]
		)
	})

	writeFormula(self$wb, sheet = contents_sheet, xy = c(1, 3), x = formulas)

	seq_along(self$links) |> walk(\(i) {
		formulas = map2_chr(self$links[[i]]$row, self$links[[i]]$text, \(row, text) {
			makeHyperlinkString(
				sheet = self$sheets[[i]], row = row, col = 1,
				text = gsub('"', '""', text, fixed = TRUE)
			)
		})
		sheet_link = makeHyperlinkString(sheet = self$sheets[[i]], row = 1, col = 1, text = self$sheets[[i]])
		writeFormula(self$wb, sheet = contents_sheet, xy = c(1, 4 + length(self$links) + nrows[[i]] + i * 2 - 2), x = sheet_link)
		writeFormula(self$wb, sheet = contents_sheet, xy = c(2, 5 + length(self$links) + nrows[[i]] + i * 2 - 2), x = formulas)
	})

	worksheetOrder(self$wb) = c(contents_sheet, seq_len(length(self$sheets)))
	activeSheet(self$wb) = contents_sheet
}


XL_write = function(self, add_contents) {
	start_time = Sys.time()
	on.exit(cat(self$filename, "writing time:", elapsed_fmt(Sys.time() - start_time), "\n"))

	if (add_contents) XL_add_contents(self)

	saveWorkbook(self$wb, self$filename, overwrite = T)
}

XL$set("public", "write", \(add_contents = F) XL_write(self, add_contents))
XL$set("public", "write_wc", \() XL_write(self, add_contents = T))




# xls = ExcelTable$new("11299-2 tables waves 2024-07-01 v6")
# # xls$options(fmt = "pct")
# xls$add("Sell", ...)
# xls$add_right("Sell", ...)
# xls$add_below("Sell", ...)
# xls$append("Sell", ...)
# xls$overwrite("Sell", ...)
# xls$write()
