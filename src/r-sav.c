/* Standalone SAV reader 0.5.5: path-based R entry point. */
#define _FILE_OFFSET_BITS 64
#define _POSIX_C_SOURCE 200809L
/* Keep R's short-name macros from rewriting native fields such as error. */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include "sav.h"
#include "sav-reader.h"
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Utils.h>
#include <R_ext/Visibility.h>
#ifdef _WIN32
#include <windows.h>
#endif

static SEXP raw_field(const unsigned char *bytes, size_t n) {
	SEXP result = Rf_allocVector(RAWSXP, (R_xlen_t)n);
	if (n) memcpy(RAW(result), bytes, n);
	return result;
}

static SEXP header_to_r(const SavHeader *source) {
	SavHeader h = *source;
	const char *names[] = {"signature", "byte_order", "layout_code", "nominal_case_size", "compression", "weight_dictionary_index", "case_count_declared", "bias", "product_raw", "creation_date_raw", "creation_time_raw", "file_label_raw", "padding_raw"};
	SEXP header = PROTECT(Rf_allocVector(VECSXP, 13));
	SEXP header_names = PROTECT(Rf_allocVector(STRSXP, 13));
	for (int i = 0; i < 13; i++) SET_STRING_ELT(header_names, i, Rf_mkChar(names[i]));
	Rf_setAttrib(header, R_NamesSymbol, header_names);
	SET_VECTOR_ELT(header, 0, Rf_mkString("$FL2"));
	if (h.compression == 2) SET_VECTOR_ELT(header, 0, Rf_mkString("$FL3"));
	SET_VECTOR_ELT(header, 1, Rf_mkString("little"));
	SET_VECTOR_ELT(header, 2, Rf_ScalarInteger(h.layout_code));
	SET_VECTOR_ELT(header, 3, Rf_ScalarInteger(h.nominal_case_size));
	SET_VECTOR_ELT(header, 4, Rf_ScalarInteger(h.compression));
	SET_VECTOR_ELT(header, 5, Rf_ScalarInteger(h.weight_index));
	SET_VECTOR_ELT(header, 6, Rf_ScalarInteger(h.case_count));
	SET_VECTOR_ELT(header, 7, Rf_ScalarReal(h.bias));
	SET_VECTOR_ELT(header, 8, raw_field(h.product, 60));
	SET_VECTOR_ELT(header, 9, raw_field(h.creation_date, 9));
	SET_VECTOR_ELT(header, 10, raw_field(h.creation_time, 8));
	SET_VECTOR_ELT(header, 11, raw_field(h.file_label, 64));
	SET_VECTOR_ELT(header, 12, raw_field(h.padding, 3));
	UNPROTECT(2);
	return header;
}

typedef struct {
	int count, range;
	double lower, upper, values[3];
	unsigned char *strings;
	size_t width;
} PreparedMissing;

typedef struct {
	SEXP path, override;
	int debug, user_na;
	SavDocument document;
	FILE *file;
	unsigned char *input, *text_buffer;
	size_t input_size, text_capacity, rows;
	SEXP columns;
	PreparedMissing *missing;
} RReadContext;

static SEXP named_list(int count, const char **names) {
	SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
	SEXP keys = PROTECT(Rf_allocVector(STRSXP, count));
	for (int i = 0; i < count; i++) SET_STRING_ELT(keys, i, Rf_mkChar(names[i]));
	Rf_setAttrib(result, R_NamesSymbol, keys);
	UNPROTECT(2);
	return result;
}

static void stop_reader(RReadContext *context) {
	SavDocument *d = &context->document;
	const char *category = "malformed";
	if (!strncmp(d->error.code, "SAV_E_UNSUPPORTED", 17) || !strcmp(d->error.code, "SAV_E_MACHINE_PROFILE") || !strcmp(d->error.code, "SAV_E_FLOAT_PROFILE")) category = "unsupported";
	if (!strcmp(d->error.code, "SAV_E_RESOURCE") || !strcmp(d->error.code, "SAV_E_MEMORY")) category = "resource";
	if (!strcmp(d->error.code, "SAV_E_IO")) category = "io";
	if (!strcmp(d->error.code, "SAV_E_STATE") || !strcmp(d->error.code, "SAV_E_OUTPUT_ABORT")) category = "internal";
	char message[2048];
	snprintf(message, sizeof message, "%s [%s] at %s, byte offset 0x%08zx: %s", d->error.code, category, d->error.stage, d->error.offset, d->error.message);
	size_t used = strlen(message);
	snprintf(message + used, sizeof message - used, "\nFile: %s", Rf_translateCharUTF8(STRING_ELT(context->path, 0)));
	const char *names[] = {"message", "call", "code", "category", "stage", "offset", "detail", "filename"};
	SEXP condition = PROTECT(named_list(8, names));
	SET_VECTOR_ELT(condition, 0, Rf_ScalarString(Rf_mkCharCE(message, CE_UTF8)));
	SET_VECTOR_ELT(condition, 2, Rf_mkString(d->error.code));
	SET_VECTOR_ELT(condition, 3, Rf_mkString(category));
	SET_VECTOR_ELT(condition, 4, Rf_mkString(d->error.stage));
	SET_VECTOR_ELT(condition, 5, Rf_ScalarReal((double)d->error.offset));
	SET_VECTOR_ELT(condition, 6, Rf_ScalarString(Rf_mkCharCE(d->error.message, CE_UTF8)));
	SET_VECTOR_ELT(condition, 7, context->path);
	SEXP classes = PROTECT(Rf_allocVector(STRSXP, 3));
	SET_STRING_ELT(classes, 0, Rf_mkChar("sav_read_error"));
	SET_STRING_ELT(classes, 1, Rf_mkChar("error"));
	SET_STRING_ELT(classes, 2, Rf_mkChar("condition"));
	Rf_setAttrib(condition, R_ClassSymbol, classes);
	SEXP call = PROTECT(Rf_lang2(Rf_install("stop"), condition));
	Rf_eval(call, R_BaseEnv);
	UNPROTECT(3);
	Rf_error("SAV_E_STATE: stop() unexpectedly returned.");
}

static SEXP text_to_r(RReadContext *context, SavSlice slice) {
	SavDocument *d = &context->document;
	if (slice.length > 16 * 1024 * 1024) Rf_error("SAV_E_RESOURCE: text field exceeds 16 MiB development limit.");
	if (!slice.length) return Rf_mkChar("");
	if (d->encoding == 1) return Rf_mkCharLenCE((const char *)slice.bytes, (int)slice.length, CE_UTF8);
	size_t capacity = slice.length * 3;
	if (capacity > context->text_capacity) {
		unsigned char *next = realloc(context->text_buffer, capacity);
		if (!next) Rf_error("SAV_E_MEMORY: could not allocate UTF-8 conversion buffer.");
		context->text_buffer = next;
		context->text_capacity = capacity;
	}
	size_t length;
	if (!sav_decode_text(d, slice, context->text_buffer, context->text_capacity, &length)) stop_reader(context);
	if (!length) return Rf_mkChar("");
	return Rf_mkCharLenCE((const char *)context->text_buffer, (int)length, CE_UTF8);
}

/* Returned string codes follow cell padding normalization. Raw dictionary
 * keys remain unchanged and strictly validated; missing matching is bytewise. */
static SEXP string_code_to_r(RReadContext *context, SavSlice slice) {
	while (slice.length && (slice.bytes[slice.length - 1] == ' ' || slice.bytes[slice.length - 1] == 0)) slice.length--;
	return text_to_r(context, slice);
}

static SEXP text_scalar(RReadContext *context, SavSlice slice) {
	SEXP result = PROTECT(Rf_allocVector(STRSXP, 1));
	SET_STRING_ELT(result, 0, text_to_r(context, slice));
	UNPROTECT(1);
	return result;
}

static SEXP format_to_r(int32_t raw) {
	const char *names[] = {"raw_code", "type_code", "width", "decimals"};
	SEXP result = PROTECT(named_list(4, names));
	uint32_t packed = (uint32_t)raw;
	SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(raw));
	SET_VECTOR_ELT(result, 1, Rf_ScalarInteger((int)((packed >> 16) & 255)));
	SET_VECTOR_ELT(result, 2, Rf_ScalarInteger((int)((packed >> 8) & 255)));
	SET_VECTOR_ELT(result, 3, Rf_ScalarInteger((int)(packed & 255)));
	UNPROTECT(1);
	return result;
}

static SEXP missing_to_r(RReadContext *context, SavVariable variable) {
	const char *names[] = {"declaration_code", "kind", "values", "range", "raw"};
	SEXP result = PROTECT(named_list(5, names));
	SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(variable.missing_count));
	const char *kind = "none";
	if (variable.missing_count > 0) kind = "values";
	if (variable.missing_count == -2) kind = "range";
	if (variable.missing_count == -3) kind = "range_and_value";
	SET_VECTOR_ELT(result, 1, Rf_mkString(kind));
	size_t count = (size_t)abs(variable.missing_count);
	size_t start = 0;
	if (variable.missing_count < 0) {
		start = 2;
		SEXP range = PROTECT(Rf_allocVector(REALSXP, 2));
		REAL(range)[0] = sav_number_le(variable.missing_raw.bytes);
		REAL(range)[1] = sav_number_le(variable.missing_raw.bytes + 8);
		SET_VECTOR_ELT(result, 3, range);
		UNPROTECT(1);
	}
	SEXP values;
	if (!variable.storage_type) values = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)(count - start)));
	else values = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)(count - start)));
	for (size_t i = start; i < count; i++) {
		const unsigned char *raw = variable.missing_raw.bytes + 8 * i;
		size_t stored_length = 8;
		size_t stored_offset = variable.missing_raw.offset + 8 * i;
		if (variable.storage_type && variable.string_missing[i].bytes) {
			raw = variable.string_missing[i].bytes;
			stored_length = variable.string_missing[i].length;
			stored_offset = variable.string_missing[i].offset;
		}
		if (!variable.storage_type) REAL(values)[i - start] = sav_number_le(raw);
		else {
			size_t width = (size_t)variable.storage_type;
			unsigned char *padded = (unsigned char *)R_alloc(width, 1);
			memset(padded, ' ', width);
			size_t stored = width;
			if (stored > stored_length) stored = stored_length;
			memcpy(padded, raw, stored);
			SET_STRING_ELT(values, (R_xlen_t)(i - start), string_code_to_r(context, (SavSlice){padded, width, stored_offset}));
		}
	}
	SET_VECTOR_ELT(result, 2, values);
	SET_VECTOR_ELT(result, 4, raw_field(variable.missing_raw.bytes, variable.missing_raw.length));
	UNPROTECT(2);
	return result;
}

static SEXP variables_to_r(RReadContext *context) {
	SavDocument *d = &context->document;
	SEXP result = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)d->variable_count));
	SEXP keys = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)d->variable_count));
	const char *names[] = {"id", "dictionary_index", "record_offset", "name", "short_name", "long_name", "storage_type", "string_width_bytes", "slot_count", "label", "print_format", "write_format", "missing", "label_set_id", "display", "segments"};
	for (size_t i = 0; i < d->variable_count; i++) {
		SavVariable variable = d->variables[i];
		SavSlice name = variable.short_name;
		if (variable.long_name.bytes) name = variable.long_name;
		SEXP item = PROTECT(named_list(16, names));
		SET_VECTOR_ELT(item, 0, Rf_ScalarInteger((int)i + 1));
		SET_VECTOR_ELT(item, 1, Rf_ScalarReal((double)variable.dictionary_index));
		SET_VECTOR_ELT(item, 2, Rf_ScalarReal((double)variable.record_offset));
		SET_VECTOR_ELT(item, 3, text_scalar(context, name));
		SET_VECTOR_ELT(item, 4, text_scalar(context, variable.short_name));
		if (variable.long_name.bytes) SET_VECTOR_ELT(item, 5, text_scalar(context, variable.long_name));
		const char *type = "numeric";
		if (variable.storage_type) type = "string";
		SET_VECTOR_ELT(item, 6, Rf_mkString(type));
		SET_VECTOR_ELT(item, 7, Rf_ScalarInteger(variable.storage_type));
		SET_VECTOR_ELT(item, 8, Rf_ScalarReal((double)variable.slot_count));
		if (variable.has_label) SET_VECTOR_ELT(item, 9, text_scalar(context, variable.label));
		SET_VECTOR_ELT(item, 10, format_to_r(variable.print_format));
		SET_VECTOR_ELT(item, 11, format_to_r(variable.write_format));
		SET_VECTOR_ELT(item, 12, missing_to_r(context, variable));
		if (variable.label_set_index >= 0) SET_VECTOR_ELT(item, 13, Rf_ScalarInteger(variable.label_set_index + 1));
		if (variable.has_display) {
			const char *display_names[] = {"measure_code", "width", "alignment_code"};
			SEXP display = PROTECT(named_list(3, display_names));
			SET_VECTOR_ELT(display, 0, Rf_ScalarInteger(variable.measure));
			if (variable.display_width >= 0) SET_VECTOR_ELT(display, 1, Rf_ScalarInteger(variable.display_width));
			SET_VECTOR_ELT(display, 2, Rf_ScalarInteger(variable.alignment));
			SET_VECTOR_ELT(item, 14, display);
			UNPROTECT(1);
		}
		SEXP segments = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)variable.segment_count));
		const char *segment_names[] = {"dictionary_index", "record_offset", "width_bytes", "slot_count", "short_name", "label"};
		for (size_t j = 0; j < variable.segment_count; j++) {
			SavVariable physical = d->physical_variables[variable.segment_start + j];
			SEXP segment = PROTECT(named_list(6, segment_names));
			SET_VECTOR_ELT(segment, 0, Rf_ScalarReal((double)physical.dictionary_index));
			SET_VECTOR_ELT(segment, 1, Rf_ScalarReal((double)physical.record_offset));
			SET_VECTOR_ELT(segment, 2, Rf_ScalarInteger(physical.storage_type));
			SET_VECTOR_ELT(segment, 3, Rf_ScalarReal((double)physical.slot_count));
			SET_VECTOR_ELT(segment, 4, text_scalar(context, physical.short_name));
			if (physical.has_label) SET_VECTOR_ELT(segment, 5, text_scalar(context, physical.label));
			SET_VECTOR_ELT(segments, (R_xlen_t)j, segment);
			UNPROTECT(1);
		}
		SET_VECTOR_ELT(item, 15, segments);
		UNPROTECT(1);
		SET_STRING_ELT(keys, (R_xlen_t)i, text_to_r(context, name));
		SET_VECTOR_ELT(result, (R_xlen_t)i, item);
		UNPROTECT(1);
	}
	Rf_setAttrib(result, R_NamesSymbol, keys);
	UNPROTECT(2);
	return result;
}

static SEXP labels_to_r(RReadContext *context) {
	SavDocument *d = &context->document;
	SEXP result = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)d->label_set_count));
	const char *names[] = {"id", "key_type", "values", "labels", "record_offset"};
	for (size_t i = 0; i < d->label_set_count; i++) {
		SavLabelSet set = d->label_sets[i];
		SEXP item = PROTECT(named_list(5, names));
		SEXP values;
		if (set.key_type == 0) values = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)set.count));
		else values = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)set.count));
		SEXP labels = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)set.count));
		for (size_t j = 0; j < set.count; j++) {
			if (set.key_type == 0) REAL(values)[j] = sav_number_le(set.entries[j].value.bytes);
			else SET_STRING_ELT(values, (R_xlen_t)j, string_code_to_r(context, (SavSlice){set.entries[j].value.bytes, (size_t)set.key_type, set.entries[j].value.offset}));
			SET_STRING_ELT(labels, (R_xlen_t)j, text_to_r(context, set.entries[j].label));
		}
		SET_VECTOR_ELT(item, 0, Rf_ScalarInteger((int)i + 1));
		const char *type = "numeric";
		if (set.key_type) type = "string";
		SET_VECTOR_ELT(item, 1, Rf_mkString(type));
		SET_VECTOR_ELT(item, 2, values);
		SET_VECTOR_ELT(item, 3, labels);
		SET_VECTOR_ELT(item, 4, Rf_ScalarReal((double)set.offset));
		SET_VECTOR_ELT(result, (R_xlen_t)i, item);
		UNPROTECT(3);
	}
	UNPROTECT(1);
	return result;
}

static SEXP records_to_r(RReadContext *context) {
	SavDocument *d = &context->document;
	SEXP result = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)d->record_count));
	const char *names[] = {"type", "subtype", "offset", "length", "element_size", "element_count", "status", "raw"};
	for (size_t i = 0; i < d->record_count; i++) {
		SavRecord record = d->records[i];
		SEXP item = PROTECT(named_list(8, names));
		SET_VECTOR_ELT(item, 0, Rf_ScalarInteger(record.type));
		SET_VECTOR_ELT(item, 1, Rf_ScalarInteger(record.subtype));
		SET_VECTOR_ELT(item, 2, Rf_ScalarReal((double)record.offset));
		SET_VECTOR_ELT(item, 3, Rf_ScalarReal((double)record.length));
		SET_VECTOR_ELT(item, 4, Rf_ScalarInteger(record.element_size));
		SET_VECTOR_ELT(item, 5, Rf_ScalarInteger(record.element_count));
		const char *status = "parsed";
		if (record.type == 7 && (record.subtype == 24 || record.subtype == 5 || record.subtype == 6 || record.subtype == 7 || record.subtype == 17 || record.subtype == 19 || record.subtype == 10 || record.subtype == 18)) status = "preserved_raw";
		SET_VECTOR_ELT(item, 6, Rf_mkString(status));
		/* Record bytes include continuation metadata and all original label order. */
		SET_VECTOR_ELT(item, 7, raw_field(d->bytes + record.offset, record.length));
		SET_VECTOR_ELT(result, (R_xlen_t)i, item);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return result;
}

/* Missing declarations are prepared once per variable before column filling.
 * String codes compare against raw full-width storage, before UTF-8 repair. */
static void prepare_missing(RReadContext *context) {
	if (context->user_na) return;
	SavDocument *d = &context->document;
	context->missing = calloc(d->variable_count, sizeof *context->missing);
	if (!context->missing) Rf_error("SAV_E_MEMORY: could not allocate missing rules.");
	for (size_t i = 0; i < d->variable_count; i++) {
		SavVariable *v = &d->variables[i];
		PreparedMissing *m = &context->missing[i];
		if (!v->missing_count) continue;
		if (!v->storage_type) {
			int start = 0;
			m->count = v->missing_count;
			if (v->missing_count < 0) {
				m->range = 1; m->count = v->missing_count == -3; start = 2;
				m->lower = sav_number_le(v->missing_raw.bytes);
				m->upper = sav_number_le(v->missing_raw.bytes + 8);
			}
			for (int j = 0; j < m->count; j++) m->values[j] = sav_number_le(v->missing_raw.bytes + 8 * (start + j));
		} else {
			m->count = v->missing_count; m->width = (size_t)v->storage_type;
			m->strings = malloc(m->width * (size_t)m->count);
			if (!m->strings) Rf_error("SAV_E_MEMORY: could not allocate string missing rules.");
			memset(m->strings, ' ', m->width * (size_t)m->count);
			for (int j = 0; j < m->count; j++) {
				SavSlice code = v->string_missing[j];
				if (!code.bytes) code = (SavSlice){v->missing_raw.bytes + 8 * j, 8, 0};
				size_t count = code.length;
				if (count > m->width) count = m->width;
				memcpy(m->strings + m->width * (size_t)j, code.bytes, count);
			}
		}
	}
}

static int numeric_user_missing(PreparedMissing *m, double value) {
	if (m->range && value >= m->lower && value <= m->upper) return 1;
	for (int i = 0; i < m->count; i++) if (value == m->values[i]) return 1;
	return 0;
}

static int string_user_missing(PreparedMissing *m, const unsigned char *text) {
	for (int i = 0; i < m->count; i++) if (!memcmp(text, m->strings + m->width * (size_t)i, m->width)) return 1;
	return 0;
}

static int fill_cell(void *pointer, size_t row, size_t variable_index, double value, int missing,
	const unsigned char *text, size_t length, size_t offset) {
	RReadContext *context = pointer;
	if (row >= context->rows || variable_index >= context->document.variable_count) return 0;
	PreparedMissing *m = NULL;
	if (!context->user_na) m = &context->missing[variable_index];
	SEXP column = VECTOR_ELT(context->columns, (R_xlen_t)variable_index);
	if (TYPEOF(column) == REALSXP) {
		if (missing || (!context->user_na && numeric_user_missing(m, value))) REAL(column)[row] = NA_REAL;
		else REAL(column)[row] = value;
	} else {
		if (!context->user_na && string_user_missing(m, text)) SET_STRING_ELT(column, (R_xlen_t)row, NA_STRING);
		else SET_STRING_ELT(column, (R_xlen_t)row, text_to_r(context, (SavSlice){text, length, offset}));
	}
	return 1;
}

static SEXP simple_labels(RReadContext *context, int values) {
	SavDocument *d = &context->document;
	SEXP sets = PROTECT(Rf_allocVector(VECSXP, values ? (R_xlen_t)d->label_set_count : 0));
	if (values) for (size_t i = 0; i < d->label_set_count; i++) {
		SavLabelSet *set = &d->label_sets[i];
		SEXP codes = PROTECT(Rf_allocVector(set->key_type ? STRSXP : REALSXP, (R_xlen_t)set->count));
		SEXP labels = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)set->count));
		for (size_t j = 0; j < set->count; j++) {
			if (set->key_type) SET_STRING_ELT(codes, (R_xlen_t)j, string_code_to_r(context, (SavSlice){set->entries[j].value.bytes, (size_t)set->key_type, set->entries[j].value.offset}));
			else REAL(codes)[j] = sav_number_le(set->entries[j].value.bytes);
			SET_STRING_ELT(labels, (R_xlen_t)j, text_to_r(context, set->entries[j].label));
		}
		Rf_setAttrib(codes, R_NamesSymbol, labels);
		SET_VECTOR_ELT(sets, (R_xlen_t)i, codes);
		UNPROTECT(2);
	}
	size_t count = 0;
	for (size_t i = 0; i < d->variable_count; i++) {
		if (values ? d->variables[i].label_set_index >= 0 : d->variables[i].has_label) count++;
	}
	SEXP result = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)count));
	SEXP keys = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)count));
	size_t index = 0;
	for (size_t i = 0; i < d->variable_count; i++) {
		SavVariable *v = &d->variables[i];
		if (!(values ? v->label_set_index >= 0 : v->has_label)) continue;
		SavSlice name = v->short_name;
		if (v->long_name.bytes) name = v->long_name;
		SET_STRING_ELT(keys, (R_xlen_t)index, text_to_r(context, name));
		if (values) SET_VECTOR_ELT(result, (R_xlen_t)index, VECTOR_ELT(sets, v->label_set_index));
		else SET_VECTOR_ELT(result, (R_xlen_t)index, text_scalar(context, v->label));
		index++;
	}
	Rf_setAttrib(result, R_NamesSymbol, keys);
	UNPROTECT(3);
	return result;
}

static void io_error(RReadContext *context, const char *message) {
	SavError *e = &context->document.error;
	e->code = "SAV_E_IO"; e->stage = "file.read"; e->offset = 0;
	snprintf(e->message, sizeof e->message, "%s", message);
	stop_reader(context);
}

static void load_input(RReadContext *context) {
	const char *path = Rf_translateCharUTF8(STRING_ELT(context->path, 0));
#ifdef _WIN32
	/* R uses UTF-8 paths; the Windows CRT narrow fopen is code-page dependent. */
	int count = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, NULL, 0);
	if (!count) io_error(context, "Invalid UTF-8 file path.");
	wchar_t *wide = (wchar_t *)R_alloc((size_t)count, sizeof *wide);
	if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, wide, count)) io_error(context, "Could not convert file path.");
	context->file = _wfopen(wide, L"rb");
#else
	context->file = fopen(path, "rb");
#endif
	if (!context->file) io_error(context, strerror(errno));
#ifdef _WIN32
	if (_fseeki64(context->file, 0, SEEK_END)) io_error(context, "Could not seek input file.");
	__int64 end = _ftelli64(context->file);
	if (end < 0 || (uint64_t)end > SIZE_MAX) io_error(context, "File length exceeds native size limits.");
	if (_fseeki64(context->file, 0, SEEK_SET)) io_error(context, "Could not rewind input file.");
#else
	if (fseeko(context->file, 0, SEEK_END)) io_error(context, "Could not seek input file.");
	off_t end = ftello(context->file);
	if (end < 0 || (uint64_t)end > SIZE_MAX) io_error(context, "File length exceeds native size limits.");
	if (fseeko(context->file, 0, SEEK_SET)) io_error(context, "Could not rewind input file.");
#endif
	context->input_size = (size_t)end;
	context->input = malloc(context->input_size ? context->input_size : 1);
	if (!context->input) {
		context->document.error.code = "SAV_E_MEMORY";
		context->document.error.stage = "file.allocate";
		snprintf(context->document.error.message, sizeof context->document.error.message, "Could not allocate %zu input bytes.", context->input_size);
		stop_reader(context);
	}
	for (size_t position = 0; position < context->input_size;) {
		size_t count = context->input_size - position;
		if (count > 8 * 1024 * 1024) count = 8 * 1024 * 1024;
		if (fread(context->input + position, 1, count, context->file) != count) io_error(context, "Input became shorter or could not be read.");
		position += count;
		R_CheckUserInterrupt();
	}
	if (fgetc(context->file) != EOF || ferror(context->file)) io_error(context, "Input grew or could not be read.");
	fclose(context->file); context->file = NULL;
}

static SEXP read_to_r(void *pointer) {
	RReadContext *context = pointer;
	SavDocument *d = &context->document;
	load_input(context);
	d->debug = context->debug;
	d->interrupt = R_CheckUserInterrupt;
	const char *override = NULL;
	if (context->override != R_NilValue) override = Rf_translateCharUTF8(STRING_ELT(context->override, 0));
	if (!sav_parse_dictionary(d, context->input, context->input_size, override, NULL, NULL)) stop_reader(context);
	prepare_missing(context);
	if (!sav_decode_cases(d, NULL, NULL, &context->rows)) stop_reader(context);
	if (context->rows > R_XLEN_T_MAX) Rf_error("SAV_E_RESOURCE: case count exceeds R vector limits.");
	R_CheckUserInterrupt();
	const char *names[] = {"data", "var_labels", "val_labels", "schema_version", "status", "file", "variables", "label_sets", "records", "metadata", "verified_until"};
	SEXP result = PROTECT(named_list(context->debug ? 11 : 3, names));
	SET_VECTOR_ELT(result, 1, simple_labels(context, 0));
	SET_VECTOR_ELT(result, 2, simple_labels(context, 1));
	if (context->debug) {
		SET_VECTOR_ELT(result, 3, Rf_mkString("0.4"));
		SET_VECTOR_ELT(result, 4, Rf_mkString("complete"));
		const char *file_names[] = {"header", "encoding", "encoding_declaration_raw", "encoding_override", "encoding_resolution", "data_offset", "storage_slot_count", "case_count_actual", "case_count_extended_raw", "weight_variable_id", "machine_integer_info", "machine_float_info"};
		SEXP file = PROTECT(named_list(12, file_names));
		SET_VECTOR_ELT(file, 0, header_to_r(&d->header));
		const char *encoding = "UTF-8";
		if (d->encoding == 2) encoding = "windows-1251";
		SET_VECTOR_ELT(file, 1, Rf_mkString(encoding));
		if (d->encoding_declaration.bytes) SET_VECTOR_ELT(file, 2, raw_field(d->encoding_declaration.bytes, d->encoding_declaration.length));
		SET_VECTOR_ELT(file, 3, context->override);
		const char *resolution = "machine_code";
		if (d->encoding_declaration.bytes) resolution = "subtype20";
		if (d->encoding_overridden) resolution = "explicit_override";
		SET_VECTOR_ELT(file, 4, Rf_mkString(resolution));
		SET_VECTOR_ELT(file, 5, Rf_ScalarReal((double)d->data_offset));
		SET_VECTOR_ELT(file, 6, Rf_ScalarReal((double)d->slot_count));
		SET_VECTOR_ELT(file, 7, Rf_ScalarReal((double)context->rows));
		if (d->has_extended_count) {
			for (size_t i = 0; i < d->record_count; i++) {
				SavRecord record = d->records[i];
				if (record.type == 7 && record.subtype == 16) SET_VECTOR_ELT(file, 8, raw_field(record.payload.bytes + 8, 8));
			}
		}
		if (d->weight_variable_index != SIZE_MAX) SET_VECTOR_ELT(file, 9, Rf_ScalarInteger((int)d->weight_variable_index + 1));
		if (d->has_machine) {
			SEXP machine = PROTECT(Rf_allocVector(INTSXP, 8));
			for (int i = 0; i < 8; i++) INTEGER(machine)[i] = d->machine[i];
			SET_VECTOR_ELT(file, 10, machine);
			UNPROTECT(1);
		}
		if (d->has_float_info) {
			SEXP machine = PROTECT(Rf_allocVector(REALSXP, 3));
			for (int i = 0; i < 3; i++) REAL(machine)[i] = d->float_info[i];
			SET_VECTOR_ELT(file, 11, machine);
			UNPROTECT(1);
		}
		SET_VECTOR_ELT(result, 5, file);
		SET_VECTOR_ELT(result, 6, variables_to_r(context));
		SET_VECTOR_ELT(result, 7, labels_to_r(context));
		SET_VECTOR_ELT(result, 8, records_to_r(context));
		const char *metadata_names[] = {"documents"};
		SEXP metadata = PROTECT(named_list(1, metadata_names));
		if (d->documents.bytes) {
			size_t lines = d->documents.length / 80;
			SEXP documents = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)lines));
			for (size_t i = 0; i < lines; i++) SET_STRING_ELT(documents, (R_xlen_t)i, text_to_r(context, (SavSlice){d->documents.bytes + 80 * i, 80, d->documents.offset + 80 * i}));
			SET_VECTOR_ELT(metadata, 0, documents);
			UNPROTECT(1);
		}
		SET_VECTOR_ELT(result, 9, metadata);
		UNPROTECT(2); /* file, metadata */
		SET_VECTOR_ELT(result, 10, Rf_ScalarReal((double)d->size));
	}
	context->columns = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)d->variable_count));
	SEXP keys = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)d->variable_count));
	for (size_t i = 0; i < d->variable_count; i++) {
		SavVariable *v = &d->variables[i];
		SET_VECTOR_ELT(context->columns, (R_xlen_t)i, Rf_allocVector(v->storage_type ? STRSXP : REALSXP, (R_xlen_t)context->rows));
		SavSlice name = v->short_name;
		if (v->long_name.bytes) name = v->long_name;
		SET_STRING_ELT(keys, (R_xlen_t)i, text_to_r(context, name));
	}
	Rf_setAttrib(context->columns, R_NamesSymbol, keys);
	/* Count/validate then fill avoids growing final R columns. Streaming and
	 * single-pass allocation for trusted declared counts remain deferred. */
	size_t filled;
	if (!sav_decode_cases(d, fill_cell, context, &filled)) stop_reader(context);
	SET_VECTOR_ELT(result, 0, context->columns);
	if (d->label_adjustments) Rf_warning("SAV_W_TEXT_TRUNCATED: omitted incomplete final UTF-8 characters in %zu label(s). First: %s", d->label_adjustments, d->label_warning);
	if (d->case_adjustments) Rf_warning("SAV_W_TEXT_TRUNCATED: omitted incomplete final UTF-8 characters in %zu cell(s). First: %s", d->case_adjustments, d->case_warning);
	if (context->debug) for (int subtype = 0; subtype < 32; subtype++) {
		if (!(d->extension_seen & (UINT32_C(1) << subtype))) continue;
		const char *feature = NULL;
		switch (subtype) {
			case 5: feature = "GUI variable sets"; break;
			case 6: feature = "date/time-series metadata"; break;
			case 7: case 19: feature = "multiple-response definitions (no automatic grouping)"; break;
			case 10: feature = "extra product information"; break;
			case 17: feature = "file attributes"; break;
			case 18: feature = "variable attributes/roles"; break;
			case 24: feature = "XML display metadata"; break;
		}
		/* Explicit stub: bytes are framed and retained in debug records, but
		 * these optional semantics do not affect data/labels and are not applied. */
		if (feature) Rf_warning("SAV_W_DEFERRED_METADATA: subtype %d, %s: interpretation not implemented; raw record preserved.", subtype, feature);
	}
	UNPROTECT(3);
	return result;
}

static void cleanup_read(void *pointer, Rboolean jump) {
	(void)jump;
	RReadContext *context = pointer;
	if (context->file) fclose(context->file);
	free(context->input);
	if (context->missing) {
		for (size_t i = 0; i < context->document.variable_count; i++) free(context->missing[i].strings);
		free(context->missing);
	}
	sav_document_free(&context->document);
	free(context->text_buffer);
	context->text_buffer = NULL;
}

SEXP sav_read_c(SEXP path, SEXP encoding, SEXP user_na, SEXP debug) {
	if (TYPEOF(path) != STRSXP || XLENGTH(path) != 1 || STRING_ELT(path, 0) == NA_STRING || !LENGTH(STRING_ELT(path, 0))) Rf_error("Expected one nonempty, nonmissing file path.");
	if (encoding != R_NilValue && (TYPEOF(encoding) != STRSXP || XLENGTH(encoding) != 1 || STRING_ELT(encoding, 0) == NA_STRING)) Rf_error("Expected encoding=NULL or one nonmissing encoding name.");
	if (TYPEOF(user_na) != LGLSXP || XLENGTH(user_na) != 1 || LOGICAL(user_na)[0] == NA_LOGICAL) Rf_error("Expected one nonmissing user_na flag.");
	if (TYPEOF(debug) != LGLSXP || XLENGTH(debug) != 1 || LOGICAL(debug)[0] == NA_LOGICAL) Rf_error("Expected one nonmissing debug flag.");
	RReadContext context = {0};
	context.path = path; context.override = encoding;
	context.debug = LOGICAL(debug)[0]; context.user_na = LOGICAL(user_na)[0];
	return R_UnwindProtect(read_to_r, &context, cleanup_read, &context, NULL);
}

