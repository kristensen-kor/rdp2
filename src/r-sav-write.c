/* Standalone SAV writer. Separate from the reader: strict preflight, then a
 * streaming little-endian $FL2 dictionary and bytecode case stream. R's arena
 * owns the validated model; unwind cleanup owns only the open file/temp path. */
#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L
#endif
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <sys/stat.h>
#else
#include <unistd.h>
#endif
#include "sav-encoding.h"

#define WRITE_MAX_WIDTH 32767
#define WRITE_MAX_VARIABLES 100000
#define WRITE_MAX_LABELS 1000000

typedef struct { const unsigned char *bytes; size_t length; } WriteText;
typedef struct {
	SEXP values;
	const double *numbers;
	const char *name;
	WriteText encoded_name, label;
	WriteText *label_text;
	const double *label_codes;
	size_t label_count, width, segments, first_alias, dictionary_index;
	int has_label, has_fraction;
} WriteColumn;
typedef struct {
	SEXP x, path, encoding;
	WriteColumn *columns;
	size_t column_count, physical_count, slots;
	R_xlen_t rows, row;
	size_t column;
	int cp1251;
	unsigned char inline_text_buffer[WRITE_MAX_WIDTH];
	unsigned char *text_buffer;
	size_t text_capacity;
	FILE *file;
	char *target, *temporary;
	int temp_exists;
#ifdef _WIN32
	wchar_t *temporary_wide, *target_wide;
#endif
	unsigned char commands[8], literals[64];
	size_t command_count, literal_count, interrupt_budget;
} WriteContext;

static void write_error(WriteContext *w, const char *message) {
	const char *path = w->target ? w->target : "(preflight)";
	if (w->columns && w->column < w->column_count) {
		const char *name = w->columns[w->column].name ? w->columns[w->column].name : "?";
		if (w->row >= 0)
			Rf_error("SAV_E_WRITE: %s; variable '%s', case %.0f.\nFile: %s", message, name, (double)w->row+1, path);
		Rf_error("SAV_E_WRITE: %s; variable '%s'.\nFile: %s", message, name, path);
	}
	Rf_error("SAV_E_WRITE: %s.\nFile: %s", message, path);
}
static void poll_write(WriteContext *w, size_t work) {
	w->interrupt_budget += work;
	if (w->interrupt_budget >= 65536) { w->interrupt_budget = 0; R_CheckUserInterrupt(); }
}
static char *copy_string(const char *s) {
	size_t n = strlen(s) + 1;
	char *p = R_alloc(n, 1); memcpy(p, s, n); return p;
}

/* Strict UTF-8 validation in both modes. Unlike import, export never repairs
 * a truncated suffix or substitutes an unrepresentable CP1251 character. */
static WriteText encode_text(WriteContext *w, SEXP s, size_t limit, const char *field) {
	if (s == NA_STRING) write_error(w, "Missing character value is unsupported");
	if (Rf_getCharCE(s) == CE_BYTES) write_error(w, "Byte-marked strings are unsupported");
	const unsigned char *p = (const unsigned char *)Rf_translateCharUTF8(s);
	size_t n = strlen((const char *)p), pos = 0, out = 0;
	if (w->cp1251) {
		if (!w->text_buffer) { w->text_buffer = w->inline_text_buffer; w->text_capacity = WRITE_MAX_WIDTH; }
		/* Large variable labels may exceed cell width. Grow only for those;
		 * R's call-scoped arena also owns this scratch buffer on error/interrupt. */
		size_t needed = n < limit ? n : limit;
		if (needed > w->text_capacity) {
			w->text_buffer = (unsigned char *)R_alloc(needed, 1);
			w->text_capacity = needed;
		}
	}
	while (pos < n) {
		unsigned char a = p[pos];
		size_t k = 1; uint32_t cp = a, minimum = 0;
		if (a >= 0xc2 && a <= 0xdf) { k = 2; cp = a & 31; minimum = 0x80; }
		else if (a >= 0xe0 && a <= 0xef) { k = 3; cp = a & 15; minimum = 0x800; }
		else if (a >= 0xf0 && a <= 0xf4) { k = 4; cp = a & 7; minimum = 0x10000; }
		else if (a >= 0x80) write_error(w, "Invalid UTF-8 leading byte");
		if (k > n - pos) write_error(w, "Truncated UTF-8 sequence");
		for (size_t j = 1; j < k; j++) {
			if (p[pos+j] < 0x80 || p[pos+j] > 0xbf) write_error(w, "Invalid UTF-8 continuation byte");
			cp = (cp << 6) | (p[pos+j] & 63);
		}
		if (cp < minimum || cp > 0x10ffff || (cp >= 0xd800 && cp <= 0xdfff))
			write_error(w, "Invalid UTF-8 code point");
		if (w->cp1251) {
			unsigned int byte = cp;
			if (cp >= 128) {
				byte = 256;
				if (cp >= 0x0410 && cp <= 0x044f) byte = cp - 0x0410 + 0xc0;
				else for (unsigned int j = 0; j < 128; j++) if (cp1251[j] == cp) { byte = j + 128; break; }
				if (byte == 256) write_error(w, "Character cannot be represented in Windows-1251");
			}
			if (out < limit) w->text_buffer[out] = (unsigned char)byte;
			out++;
		} else {
			out += k;
		}
		pos += k;
		/* Long labels must remain cancellable even within a single field. */
		if (n > WRITE_MAX_WIDTH) poll_write(w, k);
	}
	if (out > limit) {
		char message[192];
		snprintf(message, sizeof message, "%s requires %zu encoded bytes; limit is %zu bytes (%s)", field, out, limit, w->cp1251 ? "windows-1251" : "UTF-8");
		write_error(w, message);
	}
	return (WriteText){w->cp1251 ? w->text_buffer : p, out};
}
static WriteText own_text(WriteContext *w, SEXP s, size_t limit, const char *field) {
	WriteText t = encode_text(w, s, limit, field);
	unsigned char *p = (unsigned char *)R_alloc(t.length + 1, 1);
	memcpy(p, t.bytes, t.length); p[t.length] = 0;
	return (WriteText){p, t.length};
}
static SEXP member(SEXP x, const char *name) {
	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	if (TYPEOF(x) != VECSXP || TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(x))
		Rf_error("Expected x with named data, var_labels and val_labels components.");
	SEXP value = R_NilValue; int found = 0;
	for (R_xlen_t i = 0; i < XLENGTH(x); i++)
		if (STRING_ELT(names, i) != NA_STRING && !strcmp(Rf_translateCharUTF8(STRING_ELT(names, i)), name)) {
			if (found++) Rf_error("Duplicate x component '%s'.", name);
			value = VECTOR_ELT(x, i);
		}
	if (!found) Rf_error("Missing x component '%s'.", name);
	return value;
}
static SEXP base_call1(const char *name, SEXP x) {
	SEXP call = PROTECT(Rf_lang2(Rf_install(name), x));
	SEXP result = Rf_eval(call, R_BaseEnv); UNPROTECT(1); return result;
}
static void validate_names(WriteContext *w, SEXP names) {
	/* Unicode classes and case folding use base R once for the whole dictionary,
	 * rather than a large private Unicode table or per-variable R callbacks. */
	SEXP regex = PROTECT(Rf_mkString("^[\\pL@]([\\pL\\pN\\pSc._$#@]*[\\pL\\pN\\pSc_$#@])?$"));
	SEXP call = PROTECT(Rf_lang4(Rf_install("grepl"), regex, names, Rf_ScalarLogical(1)));
	SET_TAG(CDDR(CDR(call)), Rf_install("perl"));
	SEXP valid = PROTECT(Rf_eval(call, R_BaseEnv));
	SEXP lower = PROTECT(base_call1("tolower", names));
	SEXP duplicate = PROTECT(base_call1("duplicated", lower));
	SEXP upper = PROTECT(base_call1("toupper", names));
	const char *reserved[] = {"ALL", "AND", "BY", "EQ", "GE", "GT", "LE", "LT", "NE", "NOT", "OR", "TO", "WITH"};
	for (size_t i = 0; i < w->column_count; i++) {
		w->column = i;
		if (STRING_ELT(names, i) == NA_STRING || !LOGICAL(valid)[i]) write_error(w, "Invalid SAV variable name");
		if (LOGICAL(duplicate)[i]) write_error(w, "Case-insensitive duplicate variable name");
		const char *s = Rf_translateCharUTF8(STRING_ELT(upper, i));
		for (size_t j = 0; j < sizeof reserved / sizeof *reserved; j++)
			if (!strcmp(s, reserved[j])) write_error(w, "Reserved SAV variable name");
	}
	UNPROTECT(6);
}
static int compare_codes(const void *a, const void *b) {
	double x = *(const double *)a, y = *(const double *)b; return (x > y) - (x < y);
}
static void assign_labels(WriteContext *w, SEXP labels, SEXP names, int values) {
	if (TYPEOF(labels) != VECSXP) write_error(w, "Labels must be named lists");
	w->column = w->column_count;
	R_xlen_t n = XLENGTH(labels);
	if (!n) return;
	if (n > (R_xlen_t)w->column_count) write_error(w, "Too many label definitions");
	SEXP keys = Rf_getAttrib(labels, R_NamesSymbol);
	if (TYPEOF(keys) != STRSXP || XLENGTH(keys) != n) write_error(w, "Labels must be named lists");
	SEXP duplicate = PROTECT(base_call1("duplicated", keys));
	SEXP call = PROTECT(Rf_lang3(Rf_install("match"), keys, names));
	SEXP indices = PROTECT(Rf_eval(call, R_BaseEnv));
	size_t total = 0;
	for (R_xlen_t i = 0; i < n; i++) {
		w->column = w->column_count;
		int index = INTEGER(indices)[i];
		if (index == NA_INTEGER || LOGICAL(duplicate)[i]) write_error(w, "Unknown or duplicate variable in labels");
		w->column = (size_t)index - 1;
		WriteColumn *c = &w->columns[w->column]; SEXP label = VECTOR_ELT(labels, i);
		if (!values) {
			if (TYPEOF(label) != STRSXP || XLENGTH(label) != 1 || Rf_isObject(label)) write_error(w, "Variable label must be one plain string");
			/* Variable labels have a signed 32-bit byte length. No application
			 * cap or truncation: let the caller test SPSS compatibility. */
			c->label = own_text(w, STRING_ELT(label, 0), INT32_MAX, "Variable label"); c->has_label = 1;
		} else {
			if (!c->numbers || TYPEOF(label) != REALSXP || Rf_isObject(label)) write_error(w, "Value labels require a plain double vector and a numeric column");
			R_xlen_t count = XLENGTH(label); SEXP text = Rf_getAttrib(label, R_NamesSymbol);
			if ((size_t)count > WRITE_MAX_LABELS - total) write_error(w, "More than one million value labels");
			total += (size_t)count;
			if (count && (TYPEOF(text) != STRSXP || XLENGTH(text) != count)) write_error(w, "Value label vector requires label text as names");
			if (R_getAttribCount(label) != (Rf_getAttrib(label, R_NamesSymbol) != R_NilValue))
				write_error(w, "Value label vectors may only have names attributes");
			c->label_count = (size_t)count; c->label_codes = REAL(label);
			c->label_text = (WriteText *)R_alloc((size_t)count, sizeof(WriteText));
			double *sorted = (double *)R_alloc((size_t)count, sizeof(double));
			for (R_xlen_t j = 0; j < count; j++) {
				double code = c->label_codes[j];
				if (!R_FINITE(code) || code == -DBL_MAX) write_error(w, "Value label codes must be finite, nonmissing and not the SAV missing sentinel");
				/* Unused codes also need fractional display precision. */
				if (!c->has_fraction && code != trunc(code)) c->has_fraction = 1;
				sorted[j] = code; c->label_text[j] = own_text(w, STRING_ELT(text, j), 255, "Value-label text");
				poll_write(w, 1);
			}
			qsort(sorted, (size_t)count, sizeof(double), compare_codes);
			for (R_xlen_t j = 1; j < count; j++) if (sorted[j] == sorted[j-1]) write_error(w, "Duplicate numeric value label code");
		}
	}
	UNPROTECT(3);
}
static void preflight(WriteContext *w) {
	SEXP data = member(w->x, "data"), var_labels = member(w->x, "var_labels"), val_labels = member(w->x, "val_labels");
	if (TYPEOF(data) != VECSXP || XLENGTH(data) < 1 || XLENGTH(data) > WRITE_MAX_VARIABLES)
		write_error(w, "Data must have 1..100000 columns");
	w->column_count = (size_t)XLENGTH(data);
	w->columns = (WriteColumn *)R_alloc(w->column_count, sizeof(WriteColumn));
	memset(w->columns, 0, w->column_count * sizeof(WriteColumn));
	SEXP names = Rf_getAttrib(data, R_NamesSymbol);
	if (TYPEOF(names) != STRSXP || XLENGTH(names) != (R_xlen_t)w->column_count) write_error(w, "Data columns must be named");
	for (size_t i = 0; i < w->column_count; i++) {
		w->column = i;
		if (STRING_ELT(names, i) == NA_STRING) write_error(w, "Missing variable name");
		w->columns[i].name = copy_string(Rf_translateCharUTF8(STRING_ELT(names, i)));
	}
	validate_names(w, names);
	for (size_t i = 0; i < w->column_count; i++) {
		w->column = i; w->row = -1;
		WriteColumn *c = &w->columns[i]; c->values = VECTOR_ELT(data, i);
		if (R_getAttribCount(c->values) != 0 || (TYPEOF(c->values) != REALSXP && TYPEOF(c->values) != STRSXP))
			write_error(w, "Columns must be plain double or character vectors without attributes");
		if (!i) w->rows = XLENGTH(c->values);
		if (XLENGTH(c->values) != w->rows) write_error(w, "Unequal column lengths");
		c->encoded_name = own_text(w, STRING_ELT(names, i), 64, "Variable name");
		if (TYPEOF(c->values) == REALSXP) {
			c->numbers = REAL(c->values);
			for (R_xlen_t j = 0; j < w->rows; j++) {
				double value = c->numbers[j]; w->row = j;
				if ((!R_FINITE(value) && !R_IsNA(value)) || value == -DBL_MAX)
					write_error(w, "Numbers must be finite or R NA; -DBL_MAX is reserved for SAV system missing");
				/* Display precision only: stored doubles are never rounded. NA
				 * and empty columns keep F16.0 unless a label code is fractional. */
				if (!c->has_fraction && R_FINITE(value) && value != trunc(value)) c->has_fraction = 1;
				poll_write(w, 1);
			}
		} else {
			c->width = 1; /* Empty columns/empty strings still have a legal A1 slot. */
			for (R_xlen_t j = 0; j < w->rows; j++) {
				w->row = j; WriteText t = encode_text(w, STRING_ELT(c->values, j), WRITE_MAX_WIDTH, "String cell");
				if (t.length > c->width) c->width = t.length;
				poll_write(w, t.length + 1);
			}
		}
		c->segments = c->width > 255 ? (c->width + 251) / 252 : 1;
		c->first_alias = w->physical_count + 1; c->dictionary_index = w->slots + 1;
		w->physical_count += c->segments;
		w->slots += c->width > 255 ? (c->segments - 1) * 32 + (c->width - (c->segments - 1) * 252 + 7) / 8 : c->width ? (c->width + 7) / 8 : 1;
		if (w->physical_count > WRITE_MAX_VARIABLES || w->slots > INT32_MAX)
			write_error(w, "Physical dictionary exceeds the supported SAV layout");
	}
	w->row = -1;
	assign_labels(w, var_labels, names, 0); assign_labels(w, val_labels, names, 1);
	/* Match the reader's defensive dictionary ceiling, rather than produce an
	 * otherwise valid file that this project cannot read back. This is a
	 * dictionary-only bound; there is no case/cell/file-size policy cap. */
	size_t records = w->slots + 6 + (w->rows > INT32_MAX);
	int long_strings = 0;
	for (size_t i = 0; i < w->column_count; i++) {
		if (w->columns[i].label_count) records += 2;
		if (w->columns[i].segments > 1) long_strings = 1;
	}
	if (records + (size_t)long_strings > 200000) write_error(w, "Dictionary exceeds 200000 records");
	w->column = w->column_count;
}

static void warn_long_labels(WriteContext *w) {
	/* The binary variable record can store labels beyond 256 bytes, but SPSS
	 * documents a 256-byte application limit and may warn on import. Count in
	 * the selected output encoding: Cyrillic usually takes two UTF-8 bytes but
	 * one CP1251 byte. Preserve all text; warn once after successful preflight
	 * and before opening a file, so options(warn=2) cannot publish output. */
	size_t count = 0, first = 0;
	for (size_t i = 0; i < w->column_count; i++) if (w->columns[i].has_label && w->columns[i].label.length > 256) {
		if (!count) first = i;
		count++;
	}
	if (!count) return;
	Rf_warning("SAV_W_LONG_VARIABLE_LABEL: Labels for %zu %s exceed SPSS's documented 256-byte limit in %s. First: '%s' (%zu encoded bytes). Labels will be written in full; SPSS may warn or truncate them. Shorten or omit the affected labels.%s",
		count, count == 1 ? "variable" : "variables", w->cp1251 ? "windows-1251" : "UTF-8",
		w->columns[first].name, w->columns[first].label.length,
		w->cp1251 ? "" : " Alternatively, use windows-1251 if all text is representable and the labels fit its byte limit.");
}

static void emit(WriteContext *w, const void *p, size_t n) {
	if (n && fwrite(p, 1, n, w->file) != n) write_error(w, "File write failed");
}
static void pack32(unsigned char p[4], uint32_t value) {
	for (unsigned int i = 0; i < 4; i++) p[i] = (unsigned char)(value >> (8*i));
}
static void pack64(unsigned char p[8], uint64_t value) {
	for (unsigned int i = 0; i < 8; i++) p[i] = (unsigned char)(value >> (8*i));
}
static void pack_number(unsigned char p[8], double number) {
	uint64_t bits; memcpy(&bits, &number, 8); pack64(p, bits);
}
static void integer(WriteContext *w, uint32_t value) { unsigned char p[4]; pack32(p, value); emit(w, p, 4); }
static void number(WriteContext *w, double value) { unsigned char p[8]; pack_number(p, value); emit(w, p, 8); }
static void padding(WriteContext *w, size_t n, unsigned char fill) {
	unsigned char p[8]; memset(p, fill, sizeof p);
	while (n) { size_t k = n < sizeof p ? n : sizeof p; emit(w, p, k); n -= k; }
}
static void alias(char p[9], size_t index) { snprintf(p, 9, "V%07u", (unsigned int)index); }
static void extension(WriteContext *w, uint32_t subtype, uint32_t size, size_t count) {
	if (count > INT32_MAX) write_error(w, "Extension payload is too large");
	integer(w, 7); integer(w, subtype); integer(w, size); integer(w, (uint32_t)count);
}
static void dictionary(WriteContext *w) {
	unsigned char header[176]; memset(header, ' ', sizeof header); memcpy(header, "$FL2", 4);
	const char *product = "@(#) SPSS DATA FILE - rdp2 SAV writer 0.5.5";
	memcpy(header+4, product, strlen(product));
	pack32(header+64, 2); pack32(header+68, (uint32_t)w->slots); pack32(header+72, 1);
	pack32(header+76, 0); pack32(header+80, w->rows <= INT32_MAX ? (uint32_t)w->rows : UINT32_MAX);
	pack_number(header+84, 100);
	time_t now = time(NULL); struct tm *utc = gmtime(&now);
	if (utc) {
		const char *months[] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
		char date[16], clock[16];
		snprintf(date, sizeof date, "%02d %s %02d", utc->tm_mday, months[utc->tm_mon], (utc->tm_year+1900)%100);
		snprintf(clock, sizeof clock, "%02d:%02d:%02d", utc->tm_hour, utc->tm_min, utc->tm_sec);
		memcpy(header+92, date, 9); memcpy(header+101, clock, 8);
	}
	memset(header+173, 0, 3); emit(w, header, sizeof header);
	for (size_t i = 0; i < w->column_count; i++) {
		w->column = i; WriteColumn *c = &w->columns[i];
		for (size_t j = 0; j < c->segments; j++) {
			size_t width = c->width;
			if (c->segments > 1) width = j+1 < c->segments ? 255 : c->width - (c->segments-1)*252;
			uint32_t format = width ? (1u<<16) | ((uint32_t)width<<8) : (5u<<16) | (16u<<8) | (c->has_fraction ? 2u : 0u);
			integer(w, 2); integer(w, (uint32_t)width); integer(w, !j && c->has_label); integer(w, 0);
			integer(w, format); integer(w, format); char name[9]; alias(name, c->first_alias+j); emit(w, name, 8);
			if (!j && c->has_label) {
				integer(w, (uint32_t)c->label.length); emit(w, c->label.bytes, c->label.length); padding(w, (4-c->label.length%4)%4, 0);
			}
			for (size_t k = 1; k < (width+7)/8; k++) {
				integer(w, 2); integer(w, UINT32_MAX);
				for (int m = 0; m < 4; m++) integer(w, 0);
				padding(w, 8, ' ');
			}
			poll_write(w, 1);
		}
	}
	for (size_t i = 0; i < w->column_count; i++) {
		w->column = i; WriteColumn *c = &w->columns[i]; if (!c->label_count) continue;
		integer(w, 3); integer(w, (uint32_t)c->label_count);
		for (size_t j = 0; j < c->label_count; j++) {
			number(w, c->label_codes[j]); unsigned char n = (unsigned char)c->label_text[j].length; emit(w, &n, 1);
			emit(w, c->label_text[j].bytes, n); padding(w, (8-(n+1)%8)%8, ' '); poll_write(w, 1);
		}
		integer(w, 4); integer(w, 1); integer(w, (uint32_t)c->dictionary_index);
	}
	w->column = w->column_count;
	extension(w, 3, 4, 8);
	uint32_t machine[] = {20,0,0,UINT32_MAX,1,1,2,w->cp1251 ? 1251u : 65001u};
	for (size_t i = 0; i < 8; i++) integer(w, machine[i]);
	extension(w, 4, 8, 3); number(w, -DBL_MAX); number(w, DBL_MAX); number(w, nextafter(-DBL_MAX, 0));
	extension(w, 11, 4, w->physical_count*3);
	for (size_t i = 0; i < w->column_count; i++) {
		WriteColumn *c = &w->columns[i];
		for (size_t j = 0; j < c->segments; j++) {
			integer(w, c->width || c->label_count ? 1 : 3); integer(w, c->width ? 30 : 12); integer(w, c->width ? 0 : 1);
		}
	}
	size_t length = w->column_count-1;
	for (size_t i = 0; i < w->column_count; i++) length += 9+w->columns[i].encoded_name.length;
	extension(w, 13, 1, length);
	for (size_t i = 0; i < w->column_count; i++) {
		if (i) emit(w, "\t", 1);
		char name[9]; alias(name, w->columns[i].first_alias); emit(w, name, 8); emit(w, "=", 1);
		emit(w, w->columns[i].encoded_name.bytes, w->columns[i].encoded_name.length);
	}
	length = 0; size_t long_count = 0;
	for (size_t i = 0; i < w->column_count; i++) if (w->columns[i].segments > 1) {
		char width[16]; int n = snprintf(width, sizeof width, "%zu", w->columns[i].width); length += 10+(size_t)n; long_count++;
	}
	if (long_count) {
		extension(w, 14, 1, length+long_count-1); size_t emitted = 0;
		for (size_t i = 0; i < w->column_count; i++) if (w->columns[i].segments > 1) {
			if (emitted++) emit(w, "\t", 1);
			char name[9], width[16]; alias(name, w->columns[i].first_alias);
			emit(w, name, 8); emit(w, "=", 1); int n = snprintf(width, sizeof width, "%zu", w->columns[i].width); emit(w, width, (size_t)n); padding(w, 1, 0);
		}
	}
	if (w->rows > INT32_MAX) {
		extension(w, 16, 8, 2); unsigned char p[8]; pack64(p, 1); emit(w, p, 8); pack64(p, (uint64_t)w->rows); emit(w, p, 8);
	}
	const char *encoding = w->cp1251 ? "windows-1251" : "UTF-8";
	extension(w, 20, 1, strlen(encoding)); emit(w, encoding, strlen(encoding)); integer(w, 999); integer(w, 0);
}
static void flush_commands(WriteContext *w) {
	emit(w, w->commands, 8); emit(w, w->literals, w->literal_count);
	w->command_count = w->literal_count = 0; memset(w->commands, 0, 8);
}
static void command(WriteContext *w, unsigned char code, const unsigned char *literal) {
	w->commands[w->command_count++] = code;
	if (literal) { memcpy(w->literals+w->literal_count, literal, 8); w->literal_count += 8; }
	if (w->command_count == 8) flush_commands(w);
}
static void cases(WriteContext *w) {
	for (R_xlen_t row = 0; row < w->rows; row++) {
		w->row = row;
		for (size_t i = 0; i < w->column_count; i++) {
			w->column = i; WriteColumn *c = &w->columns[i];
			if (c->numbers) {
				double value = c->numbers[row];
				if (R_IsNA(value)) command(w, 255, NULL);
				else if (value >= -99 && value <= 151 && value == trunc(value) && !(value == 0 && signbit(value))) command(w, (unsigned char)(value+100), NULL);
				else { unsigned char p[8]; pack_number(p, value); command(w, 253, p); }
				poll_write(w, 1);
			} else {
				WriteText t = encode_text(w, STRING_ELT(c->values, row), WRITE_MAX_WIDTH, "String cell");
				size_t consumed = 0;
				/* VLS declarations use a 252-byte size formula, but the physical
				 * payload carries 255 bytes per 256-byte segment. The spare byte
				 * and final alignment padding are spaces, never text or NUL. */
				for (size_t j = 0; j < c->segments; j++) {
					size_t width = c->segments > 1 ? (j+1 < c->segments ? 255 : c->width-(c->segments-1)*252) : c->width;
					for (size_t k = 0; k < (width+7)/8; k++) {
						unsigned char p[8]; memset(p, ' ', 8);
						size_t available = width-k*8; if (available > 8) available = 8;
						size_t count = t.length-consumed; if (count > available) count = available;
						memcpy(p, t.bytes+consumed, count); consumed += count;
						int literal = memcmp(p, "        ", 8) != 0;
						command(w, literal ? 253 : 254, literal ? p : NULL);
						poll_write(w, 1);
					}
				}
			}
		}
	}
	w->row = -1; w->column = w->column_count; command(w, 252, NULL); if (w->command_count) flush_commands(w);
}

#ifdef _WIN32
static wchar_t *wide_path(WriteContext *w, const char *path) {
	int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, NULL, 0);
	if (!n) write_error(w, "Invalid UTF-8 file path");
	wchar_t *p = (wchar_t *)R_alloc((size_t)n, sizeof(wchar_t));
	if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, p, n)) write_error(w, "File path conversion failed");
	return p;
}
#endif
static void open_temporary(WriteContext *w) {
	size_t n = strlen(w->target);
	w->temporary = R_alloc(n+80, 1);
#ifdef _WIN32
	/* Exclusive creation prevents collisions. The target is never opened until
	 * the completed temporary file can replace it in one filesystem operation. */
	w->target_wide = wide_path(w, w->target);
	int fd = -1;
	for (unsigned int attempt = 0; attempt < 100; attempt++) {
		snprintf(w->temporary, n+80, "%s.rdp2-%lu-%u.tmp", w->target, (unsigned long)GetCurrentProcessId(), attempt);
		w->temporary_wide = wide_path(w, w->temporary);
		fd = _wopen(w->temporary_wide, _O_WRONLY|_O_CREAT|_O_EXCL|_O_BINARY, _S_IREAD|_S_IWRITE);
		if (fd >= 0 || errno != EEXIST) break;
	}
	if (fd < 0) write_error(w, "Could not create temporary file beside destination");
	w->temp_exists = 1; w->file = _fdopen(fd, "wb");
	if (!w->file) { _close(fd); write_error(w, "Could not open temporary stream"); }
#else
	snprintf(w->temporary, n+80, "%s.rdp2-XXXXXX", w->target);
	int fd = mkstemp(w->temporary);
	if (fd < 0) write_error(w, "Could not create temporary file beside destination");
	w->temp_exists = 1; w->file = fdopen(fd, "wb");
	if (!w->file) { close(fd); write_error(w, "Could not open temporary stream"); }
#endif
}
static void cleanup_write(void *pointer, Rboolean jump) {
	(void)jump; WriteContext *w = pointer;
	if (w->file) fclose(w->file);
	if (w->temp_exists) {
#ifdef _WIN32
		_wremove(w->temporary_wide);
#else
		unlink(w->temporary);
#endif
	}
}
static SEXP write_to_file(void *pointer) {
	WriteContext *w = pointer;
	w->target = copy_string(Rf_translateCharUTF8(STRING_ELT(w->path, 0)));
	const char *input_encoding = Rf_translateCharUTF8(STRING_ELT(w->encoding, 0));
	char encoding[32]; size_t length = strlen(input_encoding);
	if (length >= sizeof encoding) write_error(w, "Encoding must be UTF-8 or windows-1251");
	for (size_t i = 0; i <= length; i++) encoding[i] = (char)tolower((unsigned char)input_encoding[i]);
	if (!strcmp(encoding, "windows-1251") || !strcmp(encoding, "cp1251") || !strcmp(encoding, "cp-1251")) w->cp1251 = 1;
	else if (strcmp(encoding, "utf-8") && strcmp(encoding, "utf8")) write_error(w, "Encoding must be UTF-8 or windows-1251");
	if (sizeof(double) != 8 || DBL_MANT_DIG != 53 || DBL_MAX_EXP != 1024) write_error(w, "IEEE binary64 platform required");
	preflight(w); warn_long_labels(w); R_CheckUserInterrupt(); open_temporary(w); dictionary(w); cases(w);
	FILE *file = w->file; w->file = NULL;
	if (fclose(file)) write_error(w, "Closing output failed");
	R_CheckUserInterrupt();
#ifdef _WIN32
	if (!MoveFileExW(w->temporary_wide, w->target_wide, MOVEFILE_REPLACE_EXISTING|MOVEFILE_WRITE_THROUGH))
		write_error(w, "Could not replace destination with completed output");
#else
	if (rename(w->temporary, w->target)) write_error(w, "Could not replace destination with completed output");
#endif
	w->temp_exists = 0; return w->path;
}
SEXP sav_write_c(SEXP x, SEXP path, SEXP encoding) {
	if (TYPEOF(path) != STRSXP || XLENGTH(path) != 1 || STRING_ELT(path, 0) == NA_STRING || !LENGTH(STRING_ELT(path, 0)))
		Rf_error("Expected one nonempty, nonmissing output path.");
	if (TYPEOF(encoding) != STRSXP || XLENGTH(encoding) != 1 || STRING_ELT(encoding, 0) == NA_STRING)
		Rf_error("Expected one nonmissing encoding name.");
	WriteContext context = {0}; context.row = -1; context.x = x; context.path = path; context.encoding = encoding;
	return R_UnwindProtect(write_to_file, &context, cleanup_write, &context, NULL);
}
