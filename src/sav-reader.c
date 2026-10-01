#include "sav-reader.h"
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

/* Explicit development limits. They are resource errors, not format limits. */
#define SAV_VARIABLE_LIMIT 100000
#define SAV_RECORD_LIMIT 200000
#define SAV_LABEL_LIMIT 1000000

typedef struct {
	SavDocument *document;
	size_t position;
	const char *stage;
} DictionaryReader;

static int reader_error(SavDocument *d, const char *code, const char *stage, size_t offset, const char *fmt, ...) {
	d->error.code = code;
	d->error.stage = stage;
	d->error.offset = offset;
	va_list args;
	va_start(args, fmt);
	vsnprintf(d->error.message, sizeof d->error.message, fmt, args);
	va_end(args);
	return 0;
}

static int take(DictionaryReader *r, size_t n, SavSlice *slice) {
	SavDocument *d = r->document;
	if (r->position > d->size) return reader_error(d, "SAV_E_STATE", r->stage, r->position, "Position exceeds input size.");
	size_t available = d->size - r->position;
	if (n > available) return reader_error(d, "SAV_E_TRUNCATED", r->stage, r->position, "Expected %zu bytes; available %zu.", n, available);
	*slice = (SavSlice){d->bytes + r->position, n, r->position};
	r->position += n;
	return 1;
}

static uint32_t unsigned_le(const unsigned char *p) {
	uint32_t value = p[0];
	value |= (uint32_t)p[1] << 8;
	value |= (uint32_t)p[2] << 16;
	value |= (uint32_t)p[3] << 24;
	return value;
}

static int32_t signed_le(const unsigned char *p) {
	uint32_t value = unsigned_le(p);
	if (value <= INT32_MAX) return (int32_t)value;
	return -1 - (int32_t)(UINT32_MAX - value);
}

static int get_i32(DictionaryReader *r, int32_t *value) {
	SavSlice raw;
	if (!take(r, 4, &raw)) return 0;
	*value = signed_le(raw.bytes);
	return 1;
}

static uint64_t unsigned64_le(const unsigned char *p) {
	uint64_t value = 0;
	for (int i = 0; i < 8; i++) value |= (uint64_t)p[i] << (8 * i);
	return value;
}

double sav_number_le(const unsigned char *bytes) {
	uint64_t bits = unsigned64_le(bytes);
	double value;
	memcpy(&value, &bits, sizeof value);
	return value;
}

static int grow_array(SavDocument *d, void **array, size_t *capacity, size_t count, size_t item_size, size_t limit, size_t offset) {
	if (count < *capacity) return 1;
	if (count >= limit) return reader_error(d, "SAV_E_RESOURCE", "dictionary.allocate", offset, "Item limit %zu reached.", limit);
	size_t next_capacity = 16;
	if (*capacity) next_capacity = *capacity * 2;
	if (next_capacity > limit) next_capacity = limit;
	if (next_capacity > SIZE_MAX / item_size) return reader_error(d, "SAV_E_RESOURCE", "dictionary.allocate", offset, "Allocation size overflow.");
	void *next = realloc(*array, next_capacity * item_size);
	if (!next) return reader_error(d, "SAV_E_MEMORY", "dictionary.allocate", offset, "Could not allocate %zu items.", next_capacity);
	*array = next;
	*capacity = next_capacity;
	return 1;
}

static int add_record(SavDocument *d, SavRecord record) {
	d->dictionary_records++;
	if (d->dictionary_records > SAV_RECORD_LIMIT) return reader_error(d, "SAV_E_RESOURCE", "dictionary.allocate", record.offset, "Dictionary record limit reached.");
	/* Only these extension slices are needed by post-framing data/label parsing.
	 * Normal reads do not retain variable, label, document or optional records. */
	if (!d->debug && !(record.type == 7 && (record.subtype == 13 || record.subtype == 14 || record.subtype == 21 || record.subtype == 22))) return 1;
	void *array = d->records;
	if (!grow_array(d, &array, &d->record_capacity, d->record_count, sizeof *d->records, SAV_RECORD_LIMIT, record.offset)) return 0;
	d->records = array;
	d->records[d->record_count++] = record;

	return 1;
}

static SavSlice trim_name_padding(SavSlice name) {
	while (name.length && name.bytes[name.length - 1] == ' ') name.length--;
	return name;
}

static SavSlice trim_label_padding(SavSlice label) {
	/* Match Haven/ReadStat label text normalization: remove trailing ASCII
	 * spaces and NUL bytes, including those within the declared length.
	 * Preserve leading spaces, tabs and other whitespace. Full original
	 * record bytes remain available; string data and label codes are unchanged. */
	while (label.length && (label.bytes[label.length - 1] == ' ' || label.bytes[label.length - 1] == 0)) label.length--;
	return label;
}

static int parse_variable(DictionaryReader *r, size_t offset, size_t *continuations) {
	SavDocument *d = r->document;
	r->stage = "dictionary.variable";
	SavVariable variable = {0};
	variable.record_offset = offset;
	variable.dictionary_index = d->slot_count + 1;
	variable.label_set_index = -1;
	int32_t label_present;
	if (!get_i32(r, &variable.storage_type)) return 0;
	if (!get_i32(r, &label_present)) return 0;
	if (!get_i32(r, &variable.missing_count)) return 0;
	if (!get_i32(r, &variable.print_format)) return 0;
	if (!get_i32(r, &variable.write_format)) return 0;
	if (!take(r, 8, &variable.short_name)) return 0;
	if (label_present != 0 && label_present != 1)
		return reader_error(d, "SAV_E_LABEL_FLAG", r->stage, offset + 8, "Expected variable-label flag 0 or 1; found %d.", label_present);
	variable.has_label = label_present;
	if (label_present) {
		int32_t label_length;
		if (!get_i32(r, &label_length)) return 0;
		if (label_length < 0) return reader_error(d, "SAV_E_LENGTH", r->stage, r->position - 4, "Negative variable-label length.");
		if (!take(r, (size_t)label_length, &variable.label)) return 0;
		variable.label = trim_label_padding(variable.label);
		size_t padding = (4 - (size_t)label_length % 4) % 4;
		SavSlice ignored;
		if (!take(r, padding, &ignored)) return 0;
	}
	int32_t missing_count = variable.missing_count;
	if (missing_count < -3 || missing_count > 3 || missing_count == -1)
		return reader_error(d, "SAV_E_MISSING_COUNT", r->stage, offset + 12, "Unsupported missing declaration %d.", missing_count);
	size_t missing_items = (size_t)abs(missing_count);
	if (!take(r, missing_items * 8, &variable.missing_raw)) return 0;

	if (*continuations) {
		if (label_present || missing_count) return reader_error(d, "SAV_E_CONTINUATION", r->stage, offset + 8, "Continuation records cannot declare labels or missing values.");
		if (variable.storage_type != -1) return reader_error(d, "SAV_E_CONTINUATION", r->stage, offset + 4, "Expected string continuation type -1.");
		(*continuations)--;
		d->slot_count++;
		return 1;
	}
	if (variable.storage_type < 0 || variable.storage_type > 255)
		return reader_error(d, "SAV_E_VARIABLE_TYPE", r->stage, offset + 4, "Expected numeric type 0 or string width 1..255.");
	if (variable.storage_type && missing_count < 0)
		return reader_error(d, "SAV_E_STRING_MISSING_RANGE", r->stage, offset + 12, "String missing ranges are invalid.");

	variable.short_name = trim_name_padding(variable.short_name);
	if (!variable.short_name.length) return reader_error(d, "SAV_E_NAME", r->stage, offset + 24, "Empty short variable name.");
	variable.slot_count = 1;
	if (variable.storage_type) variable.slot_count = ((size_t)variable.storage_type + 7) / 8;
	*continuations = variable.slot_count - 1;
	void *array = d->variables;
	if (!grow_array(d, &array, &d->variable_capacity, d->variable_count, sizeof *d->variables, SAV_VARIABLE_LIMIT, offset)) return 0;
	d->variables = array;
	d->variables[d->variable_count++] = variable;
	d->slot_count++;
	return 1;
}

static size_t find_dictionary_index(SavDocument *d, int32_t index) {
	if (index <= 0) return SIZE_MAX;
	/* Logical starts are recorded in increasing physical dictionary order. */
	size_t lower = 0;
	size_t upper = d->variable_count;
	while (lower < upper) {
		size_t middle = lower + (upper - lower) / 2;
		size_t candidate = d->variables[middle].dictionary_index;
		if (candidate == (size_t)index) return middle;
		if (candidate < (size_t)index) lower = middle + 1;
		else upper = middle;
	}
	return SIZE_MAX;
}

static int parse_label_set(DictionaryReader *r, size_t offset) {
	SavDocument *d = r->document;
	r->stage = "dictionary.value_labels";
	int32_t count;
	if (!get_i32(r, &count)) return 0;
	if (count < 0) return reader_error(d, "SAV_E_LENGTH", r->stage, offset + 4, "Negative label count.");
	if ((size_t)count > SAV_LABEL_LIMIT || (size_t)count > (d->size - r->position) / 16)
		return reader_error(d, "SAV_E_LABEL_COUNT", r->stage, offset + 4, "Label count exceeds limit or remaining payload.");
	if ((size_t)count > SAV_LABEL_LIMIT - d->total_labels)
		return reader_error(d, "SAV_E_RESOURCE", r->stage, offset, "Total label limit reached.");
	void *array = d->label_sets;
	if (!grow_array(d, &array, &d->label_set_capacity, d->label_set_count, sizeof *d->label_sets, SAV_VARIABLE_LIMIT, offset)) return 0;
	d->label_sets = array;
	SavLabelSet *set = &d->label_sets[d->label_set_count++];
	*set = (SavLabelSet){offset, (size_t)count, NULL, -1};
	d->total_labels += (size_t)count;
	if (count) {
		set->entries = calloc((size_t)count, sizeof *set->entries);
		if (!set->entries) return reader_error(d, "SAV_E_MEMORY", r->stage, offset, "Could not allocate label entries.");
	}
	for (size_t i = 0; i < set->count; i++) {
		if (!take(r, 8, &set->entries[i].value)) return 0;
		SavSlice label_length;
		if (!take(r, 1, &label_length)) return 0;
		size_t n = label_length.bytes[0];
		if (!take(r, n, &set->entries[i].label)) return 0;
		set->entries[i].label = trim_label_padding(set->entries[i].label);
		size_t padding = (8 - (n + 1) % 8) % 8;
		SavSlice ignored;
		if (!take(r, padding, &ignored)) return 0;
	}
	SavRecord label_record = {offset, r->position - offset, 3, 0, 0, count, {0}};
	if (!add_record(d, label_record)) return 0;
	size_t binding_offset = r->position;
	int32_t type, variables;
	r->stage = "dictionary.label_bindings";
	if (!get_i32(r, &type)) return 0;
	if (type != 4) return reader_error(d, "SAV_E_RECORD_ORDER", r->stage, binding_offset, "Expected type 4 immediately after type 3; found %d.", type);
	if (!get_i32(r, &variables)) return 0;
	if (variables <= 0 || (size_t)variables > d->variable_count)
		return reader_error(d, "SAV_E_LABEL_BINDING", r->stage, binding_offset + 4, "Invalid binding count %d.", variables);
	for (int32_t i = 0; i < variables; i++) {
		int32_t index;
		if (!get_i32(r, &index)) return 0;
		size_t variable_index = find_dictionary_index(d, index);
		if (variable_index == SIZE_MAX) return reader_error(d, "SAV_E_LABEL_BINDING", r->stage, r->position - 4, "Index %d is not a logical variable start.", index);
		SavVariable *variable = &d->variables[variable_index];
		if (variable->storage_type > 8) return reader_error(d, "SAV_E_LABEL_BINDING", r->stage, r->position - 4, "Type 3 labels cannot bind to strings wider than 8 bytes.");
		if (variable->label_set_index != -1) return reader_error(d, "SAV_E_DUPLICATE_LABEL_BINDING", r->stage, r->position - 4, "Variable already has a label set.");
		if (set->key_type == -1) set->key_type = variable->storage_type;
		if (set->key_type != variable->storage_type) {
			if (set->key_type > 0 && variable->storage_type > 0) return reader_error(d, "SAV_E_UNSUPPORTED_SHARED_STRING_WIDTH", r->stage, r->position - 4, "Shared string label sets require identical storage widths; widths %d and %d are unsupported.", set->key_type, variable->storage_type);
			return reader_error(d, "SAV_E_LABEL_BINDING", r->stage, r->position - 4, "Label set mixes numeric and string variables.");
		}
		variable->label_set_index = (int)d->label_set_count - 1;
	}
	SavRecord binding_record = {binding_offset, r->position - binding_offset, 4, 0, 4, variables, {0}};
	return add_record(d, binding_record);
}

static const char *unsupported_extension_name(int32_t subtype) {
	/* Diagnostic names only: this does not enable reading these extensions. */
	if (subtype == 5) return "GUI variable sets";
	if (subtype == 24) return "XML display metadata";
	if (subtype == 6) return "date/time-series information";
	if (subtype == 10) return "extra product information";
	if (subtype == 14) return "very long string definitions";
	if (subtype == 7 || subtype == 19) return "multiple-response definitions";
	if (subtype == 17) return "file attributes";
	if (subtype == 21) return "long-string value labels";
	if (subtype == 22) return "long-string missing values";
	return "unrecognized extension";
}

static int supported_extension(int32_t subtype) {
	/* Subtype 5 holds optional GUI variable groups; retain raw definitions.
	 * Interpretation is deferred, with no column grouping or data changes. */
	if (subtype == 5) return 1;
	/* Subtype 24 is optional XML display metadata. Preserve bounded bytes
	 * without XML parsing, transcoding, or applying screen layout to data. */
	if (subtype == 24) return 1;
	/* Subtype 6 is optional date/time-series information. Its grammar is not
	 * interpreted: preserve its generically bounded payload, not just size-4
	 * observations, without changing columns, labels or numeric date storage. */
	if (subtype == 6) return 1;
	/* MR definitions (7/19) are deliberately deferred: preserve bounded raw
	 * metadata without grouping columns or changing data/variable/value labels. */
	/* File attributes (17), like variable attributes (18), are optional
	 * metadata. Preserve them raw; attribute interpretation is deferred. */
	if (subtype == 7 || subtype == 17 || subtype == 19 || subtype == 21 || subtype == 22) return 1;
	if (subtype == 3 || subtype == 4 || subtype == 10 || subtype == 11 || subtype == 13 || subtype == 14) return 1;
	if (subtype == 16 || subtype == 18 || subtype == 20) return 1;
	return 0;
}

static int parse_extension(DictionaryReader *r, size_t offset, SavRecord *record) {
	SavDocument *d = r->document;
	r->stage = "dictionary.extension";
	if (!get_i32(r, &record->subtype)) return 0;
	if (!get_i32(r, &record->element_size)) return 0;
	if (!get_i32(r, &record->element_count)) return 0;
	if (record->element_size <= 0 || record->element_count < 0)
		return reader_error(d, "SAV_E_EXTENSION_LENGTH", r->stage, offset + 8, "Expected positive element size and nonnegative count.");
	size_t size = (size_t)record->element_size;
	size_t count = (size_t)record->element_count;
	if (count > SIZE_MAX / size) return reader_error(d, "SAV_E_EXTENSION_LENGTH", r->stage, offset + 8, "Extension size multiplication overflow.");
	if (!take(r, size * count, &record->payload)) return 0;
	if (!supported_extension(record->subtype))
		return reader_error(d, "SAV_E_UNSUPPORTED_EXTENSION", r->stage, offset + 4, "Extension type 7, subtype %d (%s), record 0x%zx, element size %d, count %d: not implemented; payload was bounded but not interpreted.", record->subtype, unsupported_extension_name(record->subtype), offset, record->element_size, record->element_count);
	uint32_t bit = UINT32_C(1) << (unsigned)record->subtype;
	if ((d->extension_seen & bit) && record->subtype != 18)
		return reader_error(d, "SAV_E_DUPLICATE_EXTENSION", r->stage, offset, "Duplicate subtype %d.", record->subtype);
	d->extension_seen |= bit;
	int32_t subtype = record->subtype;
	if (subtype == 3) {
		if (size != 4 || count != 8) return reader_error(d, "SAV_E_EXTENSION_SHAPE", r->stage, offset, "Subtype 3 requires size 4, count 8.");
		for (int i = 0; i < 8; i++) d->machine[i] = signed_le(record->payload.bytes + 4 * i);
		if (d->machine[4] != 1 || d->machine[6] != 2)
			return reader_error(d, "SAV_E_MACHINE_PROFILE", r->stage, record->payload.offset + 16, "Expected IEEE floats and little-endian machine declaration.");
		d->has_machine = 1;
	}
	if (subtype == 4) {
		if (size != 8 || count != 3) return reader_error(d, "SAV_E_EXTENSION_SHAPE", r->stage, offset, "Subtype 4 requires size 8, count 3.");
		for (int i = 0; i < 3; i++) d->float_info[i] = sav_number_le(record->payload.bytes + 8 * i);
		if (d->float_info[0] != -DBL_MAX || d->float_info[1] != DBL_MAX || (d->float_info[2] != -DBL_MAX && d->float_info[2] != nextafter(-DBL_MAX, 0)))
			return reader_error(d, "SAV_E_FLOAT_PROFILE", r->stage, record->payload.offset, "Unsupported system-missing/range sentinel profile.");
		d->has_float_info = 1;
	}
	if (subtype == 11 && size != 4) return reader_error(d, "SAV_E_EXTENSION_SHAPE", r->stage, offset, "Subtype 11 requires four-byte elements.");
	if (subtype == 24 || subtype == 5 || subtype == 7 || subtype == 17 || subtype == 19 || subtype == 21 || subtype == 22 || subtype == 10 || subtype == 13 || subtype == 14 || subtype == 18 || subtype == 20) {
		if (size != 1) return reader_error(d, "SAV_E_EXTENSION_SHAPE", r->stage, offset, "Text extension requires byte elements.");
	}
	if (subtype == 16) {
		if (size != 8 || count != 2) return reader_error(d, "SAV_E_EXTENSION_SHAPE", r->stage, offset, "Subtype 16 requires size 8, count 2.");
		if (unsigned64_le(record->payload.bytes) != 1) return reader_error(d, "SAV_E_EXTENDED_COUNT", r->stage, record->payload.offset, "Unsupported extended-count marker.");
		uint64_t value = unsigned64_le(record->payload.bytes + 8);
		if (value > INT64_MAX) return reader_error(d, "SAV_E_EXTENDED_COUNT", r->stage, record->payload.offset + 8, "Unsupported negative/overflowing extended count.");
		d->case_count_extended = (int64_t)value;
		d->has_extended_count = 1;
	}
	if (subtype == 20) d->encoding_declaration = record->payload;
	return 1;
}

static int encoding_name(const unsigned char *bytes, size_t length) {
	char name[32];
	if (!length || length >= sizeof name) return 0;
	for (size_t i = 0; i < length; i++) {
		unsigned char ch = bytes[i];
		if (ch == 0 || ch > 127) return 0;
		if (ch >= 'A' && ch <= 'Z') ch += 'a' - 'A';
		name[i] = (char)ch;
	}
	name[length] = 0;
	if (!strcmp(name, "utf-8") || !strcmp(name, "utf8")) return 1;
	if (!strcmp(name, "windows-1251") || !strcmp(name, "cp1251")) return 2;
	return 0;
}

static int resolve_encoding(SavDocument *d, const char *override) {
	if (override) {
		d->encoding = encoding_name((const unsigned char *)override, strlen(override));
		d->encoding_overridden = 1;
	} else if (d->encoding_declaration.bytes) {
		d->encoding = encoding_name(d->encoding_declaration.bytes, d->encoding_declaration.length);
	} else if (d->has_machine) {
		if (d->machine[7] == 65001) d->encoding = 1;
		if (d->machine[7] == 1251) d->encoding = 2;
	}
	if (!d->encoding) return reader_error(d, "SAV_E_UNSUPPORTED_ENCODING", "dictionary.encoding", d->encoding_declaration.offset, "Expected explicit UTF-8 or Windows-1251 encoding; no guessing is allowed.");
	return 1;
}

typedef struct { SavSlice name; size_t variable_index; } NameKey;

static int compare_names(const void *a, const void *b) {
	const NameKey *left = a, *right = b;
	size_t n = left->name.length;
	if (right->name.length < n) n = right->name.length;
	int compared = memcmp(left->name.bytes, right->name.bytes, n);
	if (compared) return compared;
	if (left->name.length < right->name.length) return -1;
	if (left->name.length > right->name.length) return 1;
	return 0;
}

static size_t find_short_name(const NameKey *keys, size_t count, SavSlice name) {
	NameKey search = {name, 0};
	size_t lower = 0;
	size_t upper = count;
	while (lower < upper) {
		size_t middle = lower + (upper - lower) / 2;
		int comparison = compare_names(&keys[middle], &search);
		if (comparison == 0) return keys[middle].variable_index;
		if (comparison < 0) lower = middle + 1;
		else upper = middle;
	}
	return SIZE_MAX;
}

static int resolve_long_name_pairs(SavDocument *d, SavSlice pairs, const NameKey *keys) {
	size_t position = 0;
	while (position < pairs.length) {
		size_t start = position;
		while (position < pairs.length && pairs.bytes[position] != '=' && pairs.bytes[position] != '\t') position++;
		size_t short_length = position - start;
		if (!short_length || short_length > 8 || position == pairs.length || pairs.bytes[position] != '=')
			return reader_error(d, "SAV_E_LONG_NAMES", "dictionary.long_names", pairs.offset + position, "Expected short-name=value tuple.");
		position++;
		size_t long_start = position;
		while (position < pairs.length && pairs.bytes[position] != '\t') position++;
		size_t long_length = position - long_start;
		if (!long_length || long_length > 64) return reader_error(d, "SAV_E_LONG_NAMES", "dictionary.long_names", pairs.offset + long_start, "Expected long name of 1..64 bytes.");
		SavSlice short_name = {pairs.bytes + start, short_length, pairs.offset + start};
		size_t match = find_short_name(keys, d->variable_count, short_name);
		if (match == SIZE_MAX) return reader_error(d, "SAV_E_LONG_NAMES", "dictionary.long_names", pairs.offset + start, "Tuple refers to unknown short name.");
		if (d->variables[match].long_name.bytes) return reader_error(d, "SAV_E_LONG_NAMES", "dictionary.long_names", pairs.offset + start, "Duplicate mapping for short name.");
		d->variables[match].long_name = (SavSlice){pairs.bytes + long_start, long_length, pairs.offset + long_start};
		if (position == pairs.length) break;
		position++;
		if (position == pairs.length) return reader_error(d, "SAV_E_LONG_NAMES", "dictionary.long_names", pairs.offset + position - 1, "Trailing tuple separator is unsupported.");
	}
	return 1;
}

static int resolve_long_names(SavDocument *d, SavSlice pairs) {
	NameKey *keys = calloc(d->variable_count, sizeof *keys);
	if (!keys) return reader_error(d, "SAV_E_MEMORY", "dictionary.long_names", pairs.offset, "Could not allocate short-name index.");
	for (size_t i = 0; i < d->variable_count; i++) {
		keys[i].name = d->variables[i].short_name;
		keys[i].variable_index = i;
	}
	qsort(keys, d->variable_count, sizeof *keys, compare_names);
	int success = resolve_long_name_pairs(d, pairs, keys);
	free(keys);
	return success;
}

/* Add dictionary ownership and bounded readable variable metadata.
 * The underlying decoder retains the precise offending-byte file offset. */
static int validate_text(SavDocument *d, SavSlice slice, const char *field,
	const SavVariable *variable, size_t label_set, size_t entry) {
	size_t length;
	if (sav_decode_text(d, slice, NULL, 0, &length)) return 1;
	char reason[128], owner[80] = "file metadata";
	snprintf(reason, sizeof reason, "%.127s", d->error.message);
	if (variable) {
		char name[9] = {0};
		for (size_t i = 0; i < variable->short_name.length && i < 8; i++) {
			unsigned char byte = variable->short_name.bytes[i];
			name[i] = byte >= 32 && byte <= 126 ? (char)byte : '?';
		}
		snprintf(owner, sizeof owner, "dictionary variable %zu, short name '%s'", variable->dictionary_index, name);
	} else if (label_set) snprintf(owner, sizeof owner, "label set %zu, entry %zu (one-based)", label_set, entry);
	size_t relative = d->error.offset - slice.offset;
	snprintf(d->error.message, sizeof d->error.message,
		"%.32s: %.64s; %.50s; text start=0x%zx, bytes=%zu, relative byte=%zu (0-based).",
		field, reason, owner, slice.offset, slice.length, relative);
	if (variable) {
		char name[260], label[260];
		SavSlice resolved = variable->short_name;
		if (variable->long_name.bytes) resolved = variable->long_name;
		sav_text_preview(d, resolved, name, sizeof name);
		sav_text_preview(d, variable->label, label, sizeof label);
		size_t used = strlen(d->error.message);
		snprintf(d->error.message + used, sizeof d->error.message - used,
			" Variable '%s'; label preview '%s'%s.", name, label, variable->has_label ? "" : " (absent)");
	}
	return 0;
}

/* ReadStat accepts iconv's EINVAL (incomplete final sequence), retaining the
 * valid prefix. Also allow this narrow repair for case text. Names, keys and
 * declarations remain strict. The record bytes are untouched. */
static int validate_label_text(SavDocument *d, SavSlice *label, const char *field,
	const SavVariable *variable, size_t label_set, size_t entry) {
	size_t length;
	if (sav_decode_text(d, *label, NULL, 0, &length)) return 1;
	if (d->encoding != 1 || d->text_kind != SAV_TEXT_INCOMPLETE_UTF8_SUFFIX)
		return validate_text(d, *label, field, variable, label_set, entry);
	size_t offset = d->error.offset;
	size_t removed = label->length - (offset - label->offset);
	label->length -= removed;
	d->label_adjustments++;
	if (d->label_adjustments == 1) {
		if (variable) {
			char name[260];
			SavSlice resolved = variable->short_name;
			if (variable->long_name.bytes) resolved = variable->long_name;
			sav_text_preview(d, resolved, name, sizeof name);
			snprintf(d->label_warning, sizeof d->label_warning,
				"%s, dictionary variable %zu, name '%s', byte offset 0x%08zx: omitted %zu bytes of an incomplete final UTF-8 character.", field, variable->dictionary_index, name, offset, removed);
		} else snprintf(d->label_warning, sizeof d->label_warning,
			"%s, label set %zu, entry %zu (one-based), byte offset 0x%08zx: omitted %zu bytes of an incomplete final UTF-8 character.", field, label_set, entry, offset, removed);
	}
	memset(&d->error, 0, sizeof d->error);
	if (d->trace) {
		char message[220];
		if (variable) snprintf(message, sizeof message,
			"%s; dictionary variable %zu: dropped %zu bytes of an incomplete final UTF-8 character; raw record preserved.", field, variable->dictionary_index, removed);
		else snprintf(message, sizeof message,
			"%s; label set %zu, entry %zu (one-based): dropped %zu bytes of an incomplete final UTF-8 character; raw record preserved.", field, label_set, entry, removed);
		d->trace(d->trace_context, offset, "labels.adjustment", message);
	}
	return 1;
}

static int validate_unique_names(SavDocument *d, int resolved) {
	NameKey *keys = calloc(d->variable_count, sizeof *keys);
	if (!keys) return reader_error(d, "SAV_E_MEMORY", "dictionary.names", 0, "Could not allocate name validation keys.");
	for (size_t i = 0; i < d->variable_count; i++) {
		keys[i].name = d->variables[i].short_name;
		if (resolved && d->variables[i].long_name.bytes) keys[i].name = d->variables[i].long_name;
	}
	qsort(keys, d->variable_count, sizeof *keys, compare_names);
	for (size_t i = 1; i < d->variable_count; i++) {
		if (compare_names(&keys[i - 1], &keys[i]) != 0) continue;
		size_t offset = keys[i].name.offset;
		free(keys);
		return reader_error(d, resolved ? "SAV_E_DUPLICATE_NAME" : "SAV_E_UNSUPPORTED_PHYSICAL_NAMES", "dictionary.names", offset, resolved ? "Duplicate resolved variable name; no automatic repair." : "Repeated physical short names are unsupported, including auxiliary very-long-string segments.");
	}
	free(keys);
	return 1;
}

static int compare_numbers(const void *a, const void *b) {
	double left = *(const double *)a, right = *(const double *)b;
	if (left < right) return -1;
	if (left > right) return 1;
	return 0;
}

static int validate_label_keys(SavDocument *d, SavLabelSet *set) {
	if (set->count < 2) return 1;
	/* Sorting copies checks duplicates without changing source order. */
	if (set->key_type == 0) {
		double *keys = malloc(set->count * sizeof *keys);
		if (!keys) return reader_error(d, "SAV_E_MEMORY", "dictionary.value_labels", set->offset, "Could not allocate label validation keys.");
		for (size_t i = 0; i < set->count; i++) keys[i] = sav_number_le(set->entries[i].value.bytes);
		qsort(keys, set->count, sizeof *keys, compare_numbers);
		for (size_t i = 1; i < set->count; i++) {
			if (keys[i - 1] != keys[i]) continue;
			free(keys);
			return reader_error(d, "SAV_E_DUPLICATE_LABEL", "dictionary.value_labels", set->offset, "Duplicate numeric value-label key.");
		}
		free(keys);
		return 1;
	}
	NameKey *keys = malloc(set->count * sizeof *keys);
	if (!keys) return reader_error(d, "SAV_E_MEMORY", "dictionary.value_labels", set->offset, "Could not allocate string-label validation keys.");
	for (size_t i = 0; i < set->count; i++) keys[i] = (NameKey){
		{set->entries[i].value.bytes, (size_t)set->key_type, set->entries[i].value.offset}, i};
	qsort(keys, set->count, sizeof *keys, compare_names);
	for (size_t i = 1; i < set->count; i++) {
		if (compare_names(&keys[i - 1], &keys[i]) != 0) continue;
		free(keys);
		return reader_error(d, "SAV_E_DUPLICATE_LABEL", "dictionary.value_labels", set->offset, "Duplicate string value-label key.");
	}
	free(keys);
	return 1;
}

static int resolve_very_long_strings(SavDocument *d, SavSlice pairs) {
	NameKey *keys = calloc(d->variable_count, sizeof *keys);
	if (!keys) return reader_error(d, "SAV_E_MEMORY", "dictionary.long_strings", pairs.offset, "Could not allocate name index.");
	for (size_t i = 0; i < d->variable_count; i++) keys[i] = (NameKey){d->variables[i].short_name, i};
	qsort(keys, d->variable_count, sizeof *keys, compare_names);
	size_t position = 0;
	int success = pairs.length != 0;
	while (position < pairs.length) {
		size_t start = position;
		while (position < pairs.length && pairs.bytes[position] != '=' && pairs.bytes[position] != 0 && pairs.bytes[position] != '\t') position++;
		SavSlice name = {pairs.bytes + start, position - start, pairs.offset + start};
		if (!name.length || name.length > 8 || position == pairs.length || pairs.bytes[position] != '=') { success = 0; break; }
		position++;
		uint32_t width = 0;
		size_t digits = 0;
		while (position < pairs.length && pairs.bytes[position] >= '0' && pairs.bytes[position] <= '9') {
			unsigned int digit = pairs.bytes[position++] - '0';
			if (width > (32767U - digit) / 10) { success = 0; break; }
			width = width * 10 + digit;
			digits++;
		}
		if (!success || !digits || width < 256) { success = 0; break; }
		size_t index = find_short_name(keys, d->variable_count, name);
		if (index == SIZE_MAX || d->variables[index].storage_type != 255 || d->variables[index].segment_count) { success = 0; break; }
		d->variables[index].segment_count = (width + 251) / 252;
		/* physical width is recovered from the record when snapshotting segments. */
		d->variables[index].storage_type = (int32_t)width;
		while (position < pairs.length && pairs.bytes[position] == 0) position++;
		if (position == pairs.length) break;
		if (pairs.bytes[position++] != '\t') { success = 0; break; }
	}
	free(keys);
	if (!success) return reader_error(d, "SAV_E_LONG_STRINGS", "dictionary.long_strings", pairs.offset + position, "Invalid, duplicate, unknown or unsupported-width subtype-14 tuple (supported logical width 256..32767).");
	return 1;
}

static int build_logical_variables(SavDocument *d) {
	d->physical_variables = malloc(d->variable_count * sizeof *d->physical_variables);
	if (!d->physical_variables) return reader_error(d, "SAV_E_MEMORY", "dictionary.segments", 0, "Could not retain physical variables.");
	d->physical_variable_count = d->variable_count;
	memcpy(d->physical_variables, d->variables, d->variable_count * sizeof *d->variables);
	size_t logical_count = 0;
	for (size_t start = 0; start < d->physical_variable_count;) {
		SavVariable variable = d->physical_variables[start];
		size_t segments = variable.segment_count;
		if (!segments) segments = 1;
		if (segments > d->physical_variable_count - start) return reader_error(d, "SAV_E_LONG_STRINGS", "dictionary.segments", variable.record_offset, "Missing physical variables for very long string.");
		variable.segment_start = start;
		variable.segment_count = segments;
		variable.slot_count = 0;
		for (size_t j = 0; j < segments; j++) {
			SavVariable *physical = &d->physical_variables[start + j];
			int32_t width = signed_le(d->bytes + physical->record_offset + 4);
			if (segments > 1) {
				size_t required = 255;
				if (j + 1 == segments) required = (size_t)variable.storage_type - (segments - 1) * 252;
				if (width <= 0 || (j + 1 < segments && width != 255) || (j + 1 == segments && ((size_t)width < required || (size_t)width > (required + 7) / 8 * 8)))
					return reader_error(d, "SAV_E_LONG_STRINGS", "dictionary.segments", physical->record_offset + 4, "Physical segment width does not match subtype-14 layout.");
				if (j && physical->segment_count) return reader_error(d, "SAV_E_LONG_STRINGS", "dictionary.segments", physical->record_offset, "Overlapping very long string definitions.");
				if (j && (physical->missing_count || physical->label_set_index >= 0)) return reader_error(d, "SAV_E_LONG_STRINGS", "dictionary.segments", physical->record_offset, "Secondary segment has independent missing/label definitions.");
			}
			physical->storage_type = width;
			physical->segment_start = start + j;
			physical->segment_count = 1;
			variable.slot_count += physical->slot_count;
		}
		d->variables[logical_count++] = variable;
		start += segments;
	}
	d->variable_count = logical_count;
	return validate_unique_names(d, 1);
}

/* Consume only inside this extension's payload, never into the next record. */
static int missing_take(SavDocument *d, SavSlice *input, size_t n, SavSlice *out) {
	if (n > input->length) return reader_error(d, "SAV_E_TRUNCATED", "dictionary.string_missing", input->offset, "Expected %zu payload bytes; available %zu.", n, input->length);
	*out = (SavSlice){input->bytes, n, input->offset};
	input->bytes += n; input->offset += n; input->length -= n;
	return 1;
}

static int resolve_string_missing(SavDocument *d, SavSlice input, const NameKey *keys) {
	while (input.length) {
		size_t start = input.offset;
		SavSlice raw, name;
		if (!missing_take(d, &input, 4, &raw)) return 0;
		int32_t name_length = signed_le(raw.bytes);
		if (name_length <= 0 || name_length > 256) return reader_error(d, "SAV_E_STRING_MISSING", "dictionary.string_missing", raw.offset, "Expected variable name length 1..256.");
		if (!missing_take(d, &input, (size_t)name_length, &name)) return 0;
		size_t index = find_short_name(keys, d->variable_count, name);
		if (index == SIZE_MAX) return reader_error(d, "SAV_E_STRING_MISSING", "dictionary.string_missing", name.offset, "Missing declaration refers to unknown logical variable.");
		SavVariable *v = &d->variables[index];
		if (v->storage_type <= 8) return reader_error(d, "SAV_E_STRING_MISSING", "dictionary.string_missing", name.offset, "Subtype 22 requires a string variable wider than eight bytes.");
		if (v->string_missing[0].bytes) return reader_error(d, "SAV_E_STRING_MISSING", "dictionary.string_missing", name.offset, "Duplicate missing declaration for variable.");
		if (!missing_take(d, &input, 1, &raw)) return 0;
		unsigned count = raw.bytes[0];
		if (count < 1 || count > 3) return reader_error(d, "SAV_E_STRING_MISSING", "dictionary.string_missing", raw.offset, "Expected 1..3 missing codes.");
		if (!missing_take(d, &input, 4, &raw)) return 0;
		int32_t length = signed_le(raw.bytes);
		if (length < 1 || length > 8) return reader_error(d, "SAV_E_UNSUPPORTED_STRING_MISSING", "dictionary.string_missing", raw.offset, "Supported string missing codes occupy 1..8 bytes; found %d.", length);
		SavSlice codes[3] = {{0}};
		for (unsigned i = 0; i < count; i++) {
			/* Old PSPP repeated the length before subsequent values. The marker
			 * includes NULs, which are invalid inside supported string codes. */
			if (i && input.length >= 4 && signed_le(input.bytes) == length) {
				if (!missing_take(d, &input, 4, &raw)) return 0;
			}
			if (!missing_take(d, &input, (size_t)length, &codes[i])) return 0;
			if (!validate_text(d, codes[i], "subtype 22 string missing code", v, 0, 0)) return 0;
		}
		/* Accept a matching legacy type-2 declaration; reject conflicting rules. */
		if (v->missing_count && v->missing_count != (int32_t)count) return reader_error(d, "SAV_E_STRING_MISSING", "dictionary.string_missing", start, "Subtype 22 conflicts with the type-2 missing count.");
		for (unsigned i = 0; i < count; i++) {
			if (v->missing_count) for (size_t j = 0; j < 8; j++) {
				unsigned char value = ' ';
				if (j < codes[i].length) value = codes[i].bytes[j];
				if (v->missing_raw.bytes[8 * i + j] != value) return reader_error(d, "SAV_E_STRING_MISSING", "dictionary.string_missing", codes[i].offset, "Subtype 22 conflicts with a type-2 missing code.");
			}
			v->string_missing[i] = codes[i];
		}
		v->missing_count = (int32_t)count;
		v->missing_raw = (SavSlice){d->bytes + start, input.offset - start, start};
	}
	return 1;
}

static int resolve_all_string_missing(SavDocument *d) {
	int present = 0;
	for (size_t i = 0; i < d->record_count; i++) if (d->records[i].type == 7 && d->records[i].subtype == 22) present = 1;
	if (!present) return 1;
	NameKey *keys = calloc(d->variable_count, sizeof *keys);
	if (!keys) return reader_error(d, "SAV_E_MEMORY", "dictionary.string_missing", 0, "Could not allocate variable-name index.");
	for (size_t i = 0; i < d->variable_count; i++) {
		keys[i].name = d->variables[i].short_name;
		if (d->variables[i].long_name.bytes) keys[i].name = d->variables[i].long_name;
		keys[i].variable_index = i;
	}
	qsort(keys, d->variable_count, sizeof *keys, compare_names);
	int success = 1;
	for (size_t i = 0; i < d->record_count; i++) {
		SavRecord r = d->records[i];
		if (r.type == 7 && r.subtype == 22 && !resolve_string_missing(d, r.payload, keys)) { success = 0; break; }
	}
	free(keys);
	return success;
}

/* Subtype 21 uses unaligned length-prefixed fields. Bind after subtype 14
 * reconstruction so the declared width refers to the complete logical string.
 * Keys are full-width borrowed slices, including source space padding. */
static int long_label_take(SavDocument *d, SavSlice *input, size_t n, SavSlice *out) {
	if (n > input->length) return reader_error(d, "SAV_E_TRUNCATED", "dictionary.long_labels", input->offset, "Expected %zu payload bytes; available %zu.", n, input->length);
	*out = (SavSlice){input->bytes, n, input->offset};
	input->bytes += n; input->offset += n; input->length -= n;
	return 1;
}

static int resolve_long_labels(SavDocument *d, SavSlice input, const NameKey *keys) {
	while (input.length) {
		size_t start = input.offset;
		SavSlice raw, name;
		if (!long_label_take(d, &input, 4, &raw)) return 0;
		int32_t length = signed_le(raw.bytes);
		if (length < 1 || length > 256) return reader_error(d, "SAV_E_LONG_LABELS", "dictionary.long_labels", raw.offset, "Expected variable name length 1..256.");
		if (!long_label_take(d, &input, (size_t)length, &name)) return 0;
		if (!validate_text(d, name, "subtype 21 variable name", NULL, 0, 0)) return 0;
		size_t index = find_short_name(keys, d->variable_count, name);
		if (index == SIZE_MAX) return reader_error(d, "SAV_E_LONG_LABELS", "dictionary.long_labels", name.offset, "Value labels refer to unknown logical variable.");
		SavVariable *v = &d->variables[index];
		if (!long_label_take(d, &input, 4, &raw)) return 0;
		int32_t width = signed_le(raw.bytes);
		if (width < 9 || width > 32767 || width != v->storage_type) return reader_error(d, "SAV_E_LONG_LABELS", "dictionary.long_labels", raw.offset, "Declared width %d must match a logical string wider than eight bytes (variable width %d).", width, v->storage_type);
		if (v->label_set_index >= 0) return reader_error(d, "SAV_E_LONG_LABELS", "dictionary.long_labels", name.offset, "Duplicate value-label binding for variable.");
		if (!long_label_take(d, &input, 4, &raw)) return 0;
		int32_t count = signed_le(raw.bytes);
		if (count < 0 || (size_t)count > SAV_LABEL_LIMIT || (size_t)count > input.length / ((size_t)width + 8)) return reader_error(d, "SAV_E_LABEL_COUNT", "dictionary.long_labels", raw.offset, "Label count exceeds limit or remaining payload.");
		if ((size_t)count > SAV_LABEL_LIMIT - d->total_labels) return reader_error(d, "SAV_E_RESOURCE", "dictionary.long_labels", raw.offset, "Total label limit reached.");
		void *array = d->label_sets;
		if (!grow_array(d, &array, &d->label_set_capacity, d->label_set_count, sizeof *d->label_sets, SAV_VARIABLE_LIMIT, start)) return 0;
		d->label_sets = array;
		v->label_set_index = (int)d->label_set_count;
		SavLabelSet *set = &d->label_sets[d->label_set_count++];
		*set = (SavLabelSet){start, (size_t)count, NULL, width};
		d->total_labels += (size_t)count;
		if (count) {
			set->entries = calloc((size_t)count, sizeof *set->entries);
			if (!set->entries) return reader_error(d, "SAV_E_MEMORY", "dictionary.long_labels", start, "Could not allocate long-string label entries.");
		}
		for (size_t i = 0; i < set->count; i++) {
			SavValueLabel *entry = &set->entries[i];
			if (!long_label_take(d, &input, 4, &raw)) return 0;
			length = signed_le(raw.bytes);
			if (length != width) return reader_error(d, "SAV_E_LONG_LABELS", "dictionary.long_labels", raw.offset, "Value length %d must equal declared string width %d.", length, width);
			if (!long_label_take(d, &input, (size_t)length, &entry->value)) return 0;
			if (!validate_text(d, entry->value, "long-string value-label key", v, d->label_set_count, i + 1)) return 0;
			if (!long_label_take(d, &input, 4, &raw)) return 0;
			length = signed_le(raw.bytes);
			if (length < 0) return reader_error(d, "SAV_E_LONG_LABELS", "dictionary.long_labels", raw.offset, "Negative value-label text length.");
			if (!long_label_take(d, &input, (size_t)length, &entry->label)) return 0;
			entry->label = trim_label_padding(entry->label);
			if (!validate_label_text(d, &entry->label, "long-string value-label text", v, d->label_set_count, i + 1)) return 0;
		}
	}
	return 1;
}

static int resolve_all_long_labels(SavDocument *d) {
	int present = 0;
	for (size_t i = 0; i < d->record_count; i++) if (d->records[i].type == 7 && d->records[i].subtype == 21) present = 1;
	if (!present) return 1;
	NameKey *keys = calloc(d->variable_count, sizeof *keys);
	if (!keys) return reader_error(d, "SAV_E_MEMORY", "dictionary.long_labels", 0, "Could not allocate variable-name index.");
	for (size_t i = 0; i < d->variable_count; i++) keys[i] = (NameKey){
		d->variables[i].long_name.bytes ? d->variables[i].long_name : d->variables[i].short_name, i};
	qsort(keys, d->variable_count, sizeof *keys, compare_names);
	int success = 1;
	for (size_t i = 0; i < d->record_count; i++) {
		SavRecord r = d->records[i];
		if (r.type == 7 && r.subtype == 21 && !resolve_long_labels(d, r.payload, keys)) { success = 0; break; }
	}
	free(keys);
	return success;
}

static int validate_dictionary(SavDocument *d, const char *encoding_override) {
	if (!resolve_encoding(d, encoding_override)) return 0;
	if (!validate_unique_names(d, 0)) return 0;
	if (d->has_extended_count && d->header.case_count >= 0 && d->case_count_extended != d->header.case_count)
		return reader_error(d, "SAV_E_CASE_COUNT", "dictionary.case_count", 80, "Header and extended case counts disagree.");
	if (d->debug && !validate_text(d, (SavSlice){d->header.product, 60, 4}, "product identifier", NULL, 0, 0)) return 0;
	if (d->debug && !validate_text(d, (SavSlice){d->header.file_label, 64, 109}, "file label", NULL, 0, 0)) return 0;
	for (size_t i = 0; i < d->record_count; i++) {
		SavRecord record = d->records[i];
		if (record.type != 7) continue;
		if (record.subtype == 13 && !resolve_long_names(d, record.payload)) return 0;
		/* Optional display semantics: skipped in normal mode; decoded for debug.
		 * Packed formats are retained as codes, never converted to date classes. */
		if (d->debug && record.subtype == 11) {
			size_t stride = 0;
			if ((size_t)record.element_count == d->variable_count * 2) stride = 2;
			if ((size_t)record.element_count == d->variable_count * 3) stride = 3;
			if (!stride) return reader_error(d, "SAV_E_DISPLAY_COUNT", "dictionary.display", record.offset, "Display count must be two or three integers per logical variable.");
			for (size_t j = 0; j < d->variable_count; j++) {
				SavVariable *variable = &d->variables[j];
				const unsigned char *p = record.payload.bytes + 4 * stride * j;
				variable->measure = signed_le(p);
				variable->display_width = -1;
				if (stride == 3) variable->display_width = signed_le(p + 4);
				variable->alignment = signed_le(p + 4 * (stride - 1));
				if (variable->measure < 0 || variable->measure > 3 || variable->alignment < 0 || variable->alignment > 2 || variable->display_width < -1)
					return reader_error(d, "SAV_E_DISPLAY_VALUE", "dictionary.display", record.payload.offset + 4 * stride * j, "Invalid display parameter.");
				variable->has_display = 1;
			}
		}
	}
	for (size_t i = 0; i < d->variable_count; i++) {
		SavVariable *variable = &d->variables[i];
		if (!validate_text(d, variable->short_name, "short variable name", variable, 0, 0)) return 0;
		if (variable->long_name.bytes && !validate_text(d, variable->long_name, "long variable name (subtype 13)", variable, 0, 0)) return 0;
		if (variable->has_label && !validate_label_text(d, &variable->label, "variable label", variable, 0, 0)) return 0;
		for (size_t j = 0; j < (size_t)abs(variable->missing_count); j++) {
			const unsigned char *p = variable->missing_raw.bytes + 8 * j;
			if (variable->storage_type == 0 && !isfinite(sav_number_le(p)))
				return reader_error(d, "SAV_E_MISSING_VALUE", "dictionary.missing", variable->missing_raw.offset + 8 * j, "Nonfinite missing declaration.");
			if (variable->storage_type && !validate_text(d, (SavSlice){p, (size_t)(variable->storage_type < 8 ? variable->storage_type : 8), variable->missing_raw.offset + 8 * j}, "type-2 string missing code", variable, 0, 0)) return 0;
		}
		if (variable->missing_count < 0 && sav_number_le(variable->missing_raw.bytes) > sav_number_le(variable->missing_raw.bytes + 8))
			return reader_error(d, "SAV_E_MISSING_RANGE", "dictionary.missing", variable->missing_raw.offset, "Missing range lower bound exceeds upper bound.");
	}
	for (size_t i = 0; i < d->label_set_count; i++) {
		SavLabelSet *set = &d->label_sets[i];
		for (size_t j = 0; j < set->count; j++) {
			SavValueLabel entry = set->entries[j];
			if (!validate_label_text(d, &set->entries[j].label, "value-label text", NULL, i + 1, j + 1)) return 0;
			if (set->key_type == 0 && !isfinite(sav_number_le(entry.value.bytes))) return reader_error(d, "SAV_E_LABEL_VALUE", "dictionary.value_labels", entry.value.offset, "Nonfinite numeric label code.");
			if (set->key_type > 0 && !validate_text(d, (SavSlice){entry.value.bytes, (size_t)set->key_type, entry.value.offset}, "string value-label key", NULL, i + 1, j + 1)) return 0;

		}
	}
	for (size_t i = 0; i < d->record_count; i++) {
		SavRecord record = d->records[i];
		if (record.type == 7 && record.subtype == 14 && !resolve_very_long_strings(d, record.payload)) return 0;
	}
	if (!build_logical_variables(d)) return 0;
	if (!resolve_all_long_labels(d)) return 0;
	if (!resolve_all_string_missing(d)) return 0;
	if (!validate_unique_names(d, 1)) return 0;
	for (size_t i = 0; i < d->label_set_count; i++) {
		if (!validate_label_keys(d, &d->label_sets[i])) return 0;
	}
	/* Each emitted 80-byte document line is a separate strict string. */
	if (d->debug && d->documents.bytes) for (size_t i = 0; i < d->documents.length / 80; i++) {
		SavSlice line = {d->documents.bytes + 80 * i, 80, d->documents.offset + 80 * i};
		if (!validate_text(d, line, "document line", NULL, 0, i + 1)) return 0;
	}
	d->weight_variable_index = SIZE_MAX;
	if (d->header.weight_index) {
		d->weight_variable_index = find_dictionary_index(d, d->header.weight_index);
		if (d->weight_variable_index == SIZE_MAX || d->variables[d->weight_variable_index].storage_type)
			return reader_error(d, "SAV_E_WEIGHT_INDEX", "dictionary.weight", 76, "Weight index must reference a numeric logical variable.");
	}
	return 1;
}

static int parse_dictionary(SavDocument *d, const unsigned char *bytes, size_t size,
	const char *encoding_override, SavTrace trace, void *context) {
	if (!d) return 0;
	d->dictionary_validated = 0;
	if (d->variables || d->label_sets || d->records)
		return reader_error(d, "SAV_E_STATE", "dictionary.start", 0, "Document must be empty before parsing.");
	d->bytes = bytes;
	d->size = size;
	d->trace = trace;
	d->trace_context = context;
	if (!sav_parse_header(bytes, size, &d->header, &d->error, trace, context)) return 0;
	DictionaryReader r = {d, SAV_HEADER_SIZE, "dictionary.record_type"};
	size_t continuations = 0;
	int phase = 0; /* 0 variables, 1 label pairs, 2 documents, 3 extensions */
	while (1) {
		if (d->interrupt && d->dictionary_records % 1024 == 0) d->interrupt();
		size_t offset = r.position;
		int32_t type;
		r.stage = "dictionary.record_type";
		if (!get_i32(&r, &type)) return 0;
		if (continuations && type != 2) return reader_error(d, "SAV_E_CONTINUATION", r.stage, offset, "Missing string continuation records.");
		SavRecord record = {offset, 0, type, 0, 0, 0, {0}};
		if (type == 2) {
			if (phase) return reader_error(d, "SAV_E_RECORD_ORDER", r.stage, offset, "Variable record after metadata phase.");
			if (!parse_variable(&r, offset, &continuations)) return 0;
		} else if (type == 3) {
			if (phase > 1) return reader_error(d, "SAV_E_RECORD_ORDER", r.stage, offset, "Value labels after documents/extensions.");
			phase = 1;
			if (!parse_label_set(&r, offset)) return 0;
			continue;
		} else if (type == 6) {
			if (phase >= 2) return reader_error(d, "SAV_E_RECORD_ORDER", r.stage, offset, "Misplaced/duplicate documents.");
			phase = 2;
			r.stage = "dictionary.documents";
			int32_t lines;
			if (!get_i32(&r, &lines)) return 0;
			if (lines < 0 || (size_t)lines > SIZE_MAX / 80) return reader_error(d, "SAV_E_LENGTH", r.stage, offset + 4, "Invalid document line count.");
			SavSlice documents;
			if (!take(&r, (size_t)lines * 80, &documents)) return 0;
			/* Optional document text: normal mode frames but neither decodes nor retains it. */
			if (d->debug) d->documents = documents;
			record.payload = documents;
			record.element_count = lines;
			record.element_size = 80;
		} else if (type == 7) {
			phase = 3;
			if (!parse_extension(&r, offset, &record)) return 0;
		} else if (type == 999) {
			int32_t filler;
			r.stage = "dictionary.terminator";
			if (!get_i32(&r, &filler)) return 0;
			if (filler != 0) return reader_error(d, "SAV_E_TERMINATOR", r.stage, offset + 4, "Expected zero dictionary terminator filler.");
			d->data_offset = r.position;
			record.length = r.position - offset;
			if (!add_record(d, record)) return 0;
			break;
		} else return reader_error(d, "SAV_E_UNSUPPORTED_RECORD", r.stage, offset, "Unexpected dictionary record type %d.", type);
		record.length = r.position - offset;
		if (!add_record(d, record)) return 0;
	}
	if (!d->variable_count) return reader_error(d, "SAV_E_UNSUPPORTED_EMPTY", "dictionary.complete", d->data_offset, "Zero-variable files are outside the current profile.");
	if (!validate_dictionary(d, encoding_override)) return 0;
	d->dictionary_validated = 1;

	return 1;
}

static void dictionary_summary(SavDocument *d, int success) {
	if (!d->trace) return;
	size_t numeric = 0, strings = 0, max_width = 0, missing = 0, labels = 0, extensions = 0, opaque = 0;
	for (size_t i = 0; i < d->variable_count; i++) {
		SavVariable v = d->variables[i];
		if (v.storage_type) strings++; else numeric++;
		if (v.storage_type > 0 && (size_t)v.storage_type > max_width) max_width = (size_t)v.storage_type;
		if (v.missing_count) missing++;
	}
	for (size_t i = 0; i < d->label_set_count; i++) labels += d->label_sets[i].count;
	for (size_t i = 0; i < d->record_count; i++) {
		SavRecord r = d->records[i];
		if (r.type == 7) { extensions++; if (r.subtype == 24 || r.subtype == 5 || r.subtype == 6 || r.subtype == 7 || r.subtype == 17 || r.subtype == 19 || r.subtype == 10 || r.subtype == 18) opaque++; }
	}
	char message[256];
	snprintf(message, sizeof message, "%s: %zu variables (%zu numeric, %zu string), %zu slots, maximum string width %zu bytes; %zu records framed.", success ? "Validated logical dictionary" : "Partial dictionary; semantic validation not completed", d->variable_count, numeric, strings, d->slot_count, max_width, d->record_count);
	d->trace(d->trace_context, d->data_offset, "dictionary.summary", message);
	snprintf(message, sizeof message, "%zu label sets / %zu labels; %zu variables with parsed missing declarations; %zu document lines; %zu extensions (%zu opaque).", d->label_set_count, labels, missing, d->documents.length / 80, extensions, opaque);
	d->trace(d->trace_context, d->data_offset, "metadata.summary", message);
	const int subtypes[] = {3, 4, 5, 6, 7, 10, 11, 13, 14, 16, 17, 18, 19, 20, 21, 22, 24};
	for (size_t index = 0; index < sizeof subtypes / sizeof subtypes[0]; index++) {
		size_t count = 0, bytes = 0;
		for (size_t i = 0; i < d->record_count; i++) {
			SavRecord r = d->records[i];
			if (r.type == 7 && r.subtype == subtypes[index]) { count++; bytes += r.payload.length; }
		}
		if (!count) continue;
		const char *status = "interpreted";
		if (subtypes[index] == 24) status = "preserved raw; XML display metadata interpretation deferred";
		if (subtypes[index] == 5) status = "preserved raw; GUI variable-set interpretation deferred";
		if (subtypes[index] == 6) status = "preserved raw; date/time-series metadata interpretation deferred";
		if (subtypes[index] == 10 || subtypes[index] == 18) status = "preserved raw";
		if (subtypes[index] == 7 || subtypes[index] == 19) status = "preserved raw; multiple-response interpretation deferred; no automatic column grouping";
		if (subtypes[index] == 17) status = "preserved raw; file attributes interpretation deferred";
		if (subtypes[index] == 18) status = "preserved raw; variable attributes interpretation deferred";
		if (subtypes[index] == 21) status = "interpreted long-string value labels";
		if (!success && subtypes[index] == 21) status = "framed; long-string value-label validation not completed";
		if (subtypes[index] == 22) status = "interpreted string missing declarations; data codes retained, NA conversion deferred to R";
		if (!success && subtypes[index] == 22) status = "framed; string missing semantic validation not completed";
		if (!success && (subtypes[index] == 11 || subtypes[index] == 13 || subtypes[index] == 14)) status = "framed; full semantic validation not completed";
		snprintf(message, sizeof message, "Subtype %d: %zu records, %zu payload bytes; %s.", subtypes[index], count, bytes, status);
		d->trace(d->trace_context, d->data_offset, "extensions.summary", message);
	}
	if (d->has_machine) {
		snprintf(message, sizeof message, "Producer version declaration %d.%d.%d; IEEE/little-endian profile; machine encoding code %d.", d->machine[0], d->machine[1], d->machine[2], d->machine[7]);
		d->trace(d->trace_context, 0, "producer.summary", message);
	}
	if (d->encoding) {
		const char *rule = "machine code";
		if (d->encoding_declaration.bytes) rule = "subtype 20";
		if (d->encoding_overridden) rule = "explicit override";
		snprintf(message, sizeof message, "Effective encoding %s; resolved by %s. Cases %s.", d->encoding == 1 ? "UTF-8" : "Windows-1251", rule, success ? "not yet read" : "not read");
		d->trace(d->trace_context, d->data_offset, "encoding.summary", message);
	}
}

int sav_parse_dictionary(SavDocument *d, const unsigned char *bytes, size_t size,
	const char *encoding_override, SavTrace trace, void *context) {
	int success = parse_dictionary(d, bytes, size, encoding_override, trace, context);
	if (d) dictionary_summary(d, success);
	return success;
}

void sav_document_free(SavDocument *d) {
	if (!d) return;
	for (size_t i = 0; i < d->label_set_count; i++) free(d->label_sets[i].entries);
	free(d->label_sets);
	free(d->variables);
	free(d->physical_variables);
	free(d->case_text);
	free(d->case_text_offsets);
	free(d->records);
	free(d->zsav_data);
	memset(d, 0, sizeof *d);
}

/* ZSAV is a checked container of independent zlib blocks, whose concatenated
 * output is ordinary SAV bytecode (PSPP Data Record specification). Keep this
 * layer separate from row semantics. Expanded bytes are checked for native size overflow
 * and cached for the R interface's validation/fill passes. */
static int prepare_zsav(SavDocument *d) {
	if (d->zsav_ready) return 1;
	size_t start = d->data_offset;
	if (d->size - start < 24) return reader_error(d, "SAV_E_TRUNCATED", "cases.zsav.header", start, "Incomplete 24-byte ZSAV header.");
	const unsigned char *h = d->bytes + start;
	uint64_t trailer64 = unsigned64_le(h + 8), length64 = unsigned64_le(h + 16);
	if (unsigned64_le(h) != start || trailer64 > d->size || trailer64 < start + 24 || length64 != d->size - trailer64 || length64 < 24 || (length64 - 24) % 24)
		return reader_error(d, "SAV_E_ZSAV_CONTAINER", "cases.zsav.header", start, "Invalid ZSAV header offset, trailer boundary or length.");
	size_t trailer = (size_t)trailer64, blocks = ((size_t)length64 - 24) / 24;
	const unsigned char *t = d->bytes + trailer;
	int32_t block_size = signed_le(t + 16), block_count = signed_le(t + 20);
	if (unsigned64_le(t) != UINT64_MAX - 99 || unsigned64_le(t + 8) || block_size <= 0 || block_count < 0 || (size_t)block_count != blocks)
		return reader_error(d, "SAV_E_ZSAV_CONTAINER", "cases.zsav.trailer", trailer, "Invalid bias, reserved field, block size or block count.");
	size_t compressed = start + 24, expanded = 0;
	for (size_t i = 0; i < blocks; i++) {
		size_t entry_offset = trailer + 24 + i * 24;
		const unsigned char *entry = d->bytes + entry_offset;
		int32_t usize = signed_le(entry + 16), csize = signed_le(entry + 20);
		if (unsigned64_le(entry) != start + expanded || unsigned64_le(entry + 8) != compressed || usize <= 0 || usize > block_size || (i + 1 < blocks && usize != block_size) || csize <= 0 || (size_t)csize > trailer - compressed)
			return reader_error(d, "SAV_E_ZSAV_CONTAINER", "cases.zsav.block", entry_offset, "Invalid block offsets, sizes or contiguity.");
		if ((size_t)usize > SIZE_MAX - start - expanded - 1)
			return reader_error(d, "SAV_E_RESOURCE", "cases.zsav.allocate", entry_offset, "ZSAV expanded bytecode exceeds native size limits.");
		expanded += (size_t)usize;
		compressed += (size_t)csize;
	}
	if (compressed != trailer) return reader_error(d, "SAV_E_ZSAV_CONTAINER", "cases.zsav.trailer", trailer, "Block table does not cover the compressed data region.");
	free(d->zsav_data);
	d->zsav_data = malloc(expanded + 1);
	if (!d->zsav_data) return reader_error(d, "SAV_E_MEMORY", "cases.zsav.allocate", start, "Could not allocate expanded ZSAV bytecode.");
	size_t used = 0;
	for (size_t i = 0; i < blocks; i++) {
		const unsigned char *entry = t + 24 + i * 24;
		size_t c_offset = (size_t)unsigned64_le(entry + 8);
		uInt usize = (uInt)signed_le(entry + 16), csize = (uInt)signed_le(entry + 20);
		z_stream stream = {0};
		stream.next_in = (Bytef *)(d->bytes + c_offset);
		stream.avail_in = csize;
		stream.next_out = d->zsav_data + used;
		/* One extra byte detects blocks that expand past the declared length. */
		stream.avail_out = usize + 1;
		int status = inflateInit(&stream);
		if (status != Z_OK) return reader_error(d, "SAV_E_MEMORY", "cases.zsav.inflate", c_offset, "Could not initialize zlib (%d).", status);
		status = inflate(&stream, Z_FINISH);
		int valid = status == Z_STREAM_END && stream.total_in == csize && stream.total_out == usize;
		inflateEnd(&stream);
		if (status == Z_MEM_ERROR) return reader_error(d, "SAV_E_MEMORY", "cases.zsav.inflate", c_offset, "zlib could not allocate decompression state.");
		if (!valid) return reader_error(d, "SAV_E_ZSAV_ZLIB", "cases.zsav.inflate", c_offset, "Invalid zlib stream, checksum or declared block length (zlib status %d).", status);
		used += usize;
		/* The host may exit nonlocally: zlib state is already released. */
		if (d->interrupt) d->interrupt();
	}
	d->zsav_size = expanded;
	d->zsav_trailer = trailer;
	d->zsav_blocks = blocks;
	d->zsav_ready = 1;
	if (d->trace) {
		char message[220];
		snprintf(message, sizeof message, "%zu zlib blocks validated; %zu bytecode bytes expanded; native size checks passed. Case offsets identify compressed block starts, not exact compressed bytes.", blocks, expanded);
		d->trace(d->trace_context, start, "zsav.summary", message);
	}
	return 1;
}

/* An expanded byte has no exact compressed-byte counterpart. Report the
 * containing block's real file offset, never pretend it is a file address. */
static size_t case_file_offset(SavDocument *d, size_t position) {
	if (d->header.compression != 2) return position;
	if (position >= d->data_offset + d->zsav_size) return d->zsav_trailer;
	size_t lo = 0, hi = d->zsav_blocks;
	while (lo < hi) {
		size_t mid = lo + (hi - lo) / 2;
		const unsigned char *entry = d->bytes + d->zsav_trailer + 24 + mid * 24;
		uint64_t end = unsigned64_le(entry) + unsigned_le(entry + 16);
		if (position >= end) lo = mid + 1;
		else hi = mid;
	}
	if (lo == d->zsav_blocks) return d->zsav_trailer;
	return (size_t)unsigned64_le(d->bytes + d->zsav_trailer + 24 + lo * 24 + 8);
}

typedef struct {
	SavDocument *document;
	size_t position, command_position, command_offset, decoded_slots, work;
	const unsigned char *bytes;
	size_t size;
	unsigned char commands[8];
	int ended;
} CaseReader;

/* Count slots and command groups, including padding that produces no rows.
 * This adds one cheap budget increment per slot/group, not per string byte. */
static void case_interrupt(CaseReader *r) {
	if (++r->work == 65536) {
		r->work = 0;
		if (r->document->interrupt) r->document->interrupt();
	}
}

static int next_slot(CaseReader *r, int string_slot, unsigned char out[8], size_t *offset, int *present) {
	SavDocument *d = r->document;
	*present = 0;
	case_interrupt(r);
	if (r->ended) return 1;
	if (d->header.compression == 0) {
		if (r->position == r->size) { r->ended = 1; return 1; }
		if (r->size - r->position < 8) return reader_error(d, "SAV_E_TRUNCATED", "cases.literal", r->position, "Incomplete uncompressed eight-byte slot.");
		*offset = r->position;
		memcpy(out, r->bytes + (r->position - d->data_offset), 8);
		r->position += 8;
		*present = 1;
		return 1;
	}
	while (1) {
		if (r->command_position == 8) {
			case_interrupt(r);
			if (r->position == r->size) { r->ended = 1; return 1; }
			if (r->size - r->position < 8) return reader_error(d, "SAV_E_TRUNCATED", "cases.commands", r->position, "Incomplete eight-byte command group.");
			r->command_offset = r->position;
			memcpy(r->commands, r->bytes + (r->position - d->data_offset), 8);
			r->position += 8;
			r->command_position = 0;
		}
		size_t command_offset = r->command_offset + r->command_position;
		unsigned char code = r->commands[r->command_position++];
		if (code == 0) continue;
		if (code == 252) {
			for (size_t i = r->command_position; i < 8; i++) {
				if (r->commands[i]) return reader_error(d, "SAV_E_TRAILING_DATA", "cases.end", r->command_offset + i, "Nonzero command after end marker.");
			}
			if (r->position != r->size) return reader_error(d, "SAV_E_TRAILING_DATA", "cases.end", r->position, "Trailing bytes after end marker are unsupported.");
			r->ended = 1;
			return 1;
		}
		*offset = command_offset;
		if (code == 253) {
			if (r->size - r->position < 8) return reader_error(d, "SAV_E_TRUNCATED", "cases.literal", r->position, "Command 253 requires an eight-byte literal.");
			*offset = r->position;
			memcpy(out, r->bytes + (r->position - d->data_offset), 8);
			r->position += 8;
		} else if (code == 254) {
			if (!string_slot) return reader_error(d, "SAV_E_COMMAND_TYPE", "cases.commands", command_offset, "String-space command in a numeric slot.");
			memset(out, ' ', 8);
		} else if (code == 255) {
			if (string_slot) return reader_error(d, "SAV_E_COMMAND_TYPE", "cases.commands", command_offset, "Numeric system-missing command in a string slot.");
			uint64_t bits = UINT64_C(0xffefffffffffffff);
			for (int i = 0; i < 8; i++) out[i] = (unsigned char)(bits >> (8 * i));
		} else {
			if (string_slot) return reader_error(d, "SAV_E_UNSUPPORTED_STRING_COMMAND", "cases.commands", command_offset, "Numeric command in string storage; embedded-NUL variant is unsupported.");
			double value = (double)code - d->header.bias;
			uint64_t bits;
			memcpy(&bits, &value, sizeof bits);
			for (int i = 0; i < 8; i++) out[i] = (unsigned char)(bits >> (8 * i));
		}
		*present = 1;
		return 1;
	}
}

static int finish_cases(SavDocument *d, size_t rows, size_t position, size_t *case_count) {
	if (d->header.case_count >= 0 && rows != (size_t)d->header.case_count)
		return reader_error(d, "SAV_E_CASE_COUNT", "cases.complete", position, "Expected %d cases; decoded %zu.", d->header.case_count, rows);
	if (d->has_extended_count && (uint64_t)rows != (uint64_t)d->case_count_extended)
		return reader_error(d, "SAV_E_CASE_COUNT", "cases.complete", position, "Extended case count differs from decoded count.");
	*case_count = rows;
	if (d->trace) {
		char message[180];
		snprintf(message, sizeof message, "%zu cases decoded; declared counts, boundaries and end of stream validated.", rows);
		d->trace(d->trace_context, d->header.compression == 2 ? d->size : position, "cases.complete", message);
	}
	return 1;
}

static int decode_case_stream(SavDocument *d, SavCell output, void *context, size_t *case_count) {
	if (!d->case_text) {
		size_t capacity = 1;
		for (size_t i = 0; i < d->variable_count; i++) if (d->variables[i].storage_type > 0 && (size_t)d->variables[i].storage_type > capacity) capacity = (size_t)d->variables[i].storage_type;
		unsigned char *text = malloc(capacity);
		size_t *offsets = malloc(capacity * sizeof *offsets);
		if (!text || !offsets) {
			free(text);
			free(offsets);
			return reader_error(d, "SAV_E_MEMORY", "cases.allocate", d->data_offset, "Could not allocate logical-string buffers.");
		}
		d->case_text = text;
		d->case_text_offsets = offsets;
		d->case_text_capacity = capacity;
	}
	CaseReader r = {0};
	r.document = d;
	r.position = d->data_offset;
	r.command_position = 8;
	r.bytes = d->bytes + d->data_offset;
	r.size = d->size;
	if (d->header.compression == 2) {
		r.bytes = d->zsav_data;
		r.size = d->data_offset + d->zsav_size;
	}
	size_t row = 0;
	while (1) {
		for (size_t i = 0; i < d->variable_count; i++) {
			SavVariable variable = d->variables[i];
			unsigned char *text = d->case_text;
			size_t *text_offsets = d->case_text_offsets;
			size_t text_used = 0;
			size_t first_offset = 0;
			double number = 0;
			int system_missing = 0;
			for (size_t segment = 0; segment < variable.segment_count; segment++) {
				SavVariable physical = d->physical_variables[variable.segment_start + segment];
				for (size_t slot = 0; slot < physical.slot_count; slot++) {
					unsigned char raw[8];
					size_t offset;
					int present;
					if (!next_slot(&r, variable.storage_type != 0, raw, &offset, &present)) return 0;
					if (!present) {
						if (i || segment || slot) return reader_error(d, "SAV_E_PARTIAL_CASE", "cases.row", r.position, "End of data inside case %zu.", row + 1);
						return finish_cases(d, row, r.position, case_count);
					}
					if (!segment && !slot) first_offset = offset;
					r.decoded_slots++;
					if (variable.storage_type) {
						size_t physical_used = slot * 8;
						size_t remaining = (size_t)physical.storage_type - physical_used;
						size_t copy_size = 8;
						if (remaining < copy_size) copy_size = remaining;
						size_t logical_remaining = (size_t)variable.storage_type - text_used;
						if (logical_remaining < copy_size) copy_size = logical_remaining;
						memcpy(text + text_used, raw, copy_size);
						for (size_t j = 0; j < copy_size; j++) text_offsets[text_used + j] = offset + j;
						text_used += copy_size;
					} else {
						number = sav_number_le(raw);
						system_missing = number == -DBL_MAX;
						if (!isfinite(number)) return reader_error(d, "SAV_E_UNSUPPORTED_NONFINITE", "cases.numeric", offset, "Nonfinite numeric value is outside the current parser profile.");
					}
				}
			}
			if (row == SIZE_MAX) return reader_error(d, "SAV_E_RESOURCE", "cases.allocate", first_offset, "Case count exceeds native size limits.");
			size_t cell_length = (size_t)variable.storage_type;
			if (variable.storage_type) {
				/* SAV uses fixed-width ASCII padding. Match haven/ReadStat: strip
				 * trailing spaces/NULs before validation and suffix repair; preserve
				 * leading spaces, tabs and all other whitespace. Keep the full raw
				 * buffer untouched so user-missing matching still uses storage bytes. */
				while (cell_length && (text[cell_length - 1] == ' ' || text[cell_length - 1] == 0)) cell_length--;
				size_t padded_length = cell_length;
				size_t text_length;
				if (!sav_decode_text(d, (SavSlice){text, cell_length, first_offset}, NULL, 0, &text_length)) {
					size_t relative = d->error.offset - first_offset;
					if (relative < (size_t)variable.storage_type) d->error.offset = text_offsets[relative];
					if (d->encoding == 1 && d->text_kind == SAV_TEXT_INCOMPLETE_UTF8_SUFFIX) {
						cell_length = relative;
						d->case_adjustments++;
						if (d->case_adjustments == 1 || d->trace) {
							char name[260], label[260], message[768];
							SavSlice resolved = variable.short_name;
							if (variable.long_name.bytes) resolved = variable.long_name;
							sav_text_preview(d, resolved, name, sizeof name);
							sav_text_preview(d, variable.label, label, sizeof label);
							size_t offset = case_file_offset(d, d->error.offset);
							snprintf(message, sizeof message,
								"case %zu, variable %zu (one-based), name '%s', label preview '%s'%s, byte offset 0x%08zx: omitted %zu bytes of an incomplete final UTF-8 character.",
								row + 1, i + 1, name, label, variable.has_label ? "" : " (absent)", offset, padded_length - cell_length);
							if (d->case_adjustments == 1) snprintf(d->case_warning, sizeof d->case_warning, "%s", message);
							if (d->trace) d->trace(d->trace_context, offset, "cases.adjustment", message);
						}
						memset(&d->error, 0, sizeof d->error);
					} else {
						char reason[128], name[260], label[260], preview[160];
						snprintf(reason, sizeof reason, "%.127s", d->error.message);
						SavSlice resolved = variable.short_name;
						if (variable.long_name.bytes) resolved = variable.long_name;
						sav_text_preview(d, resolved, name, sizeof name);
						sav_text_preview(d, variable.label, label, sizeof label);
						sav_text_preview(d, (SavSlice){text, (size_t)variable.storage_type, first_offset}, preview, sizeof preview);
						snprintf(d->error.message, sizeof d->error.message,
							"%s; case %zu, variable %zu (one-based), name '%s'; label preview '%s'%s; value preview '%s'; width=%d bytes, relative byte=%zu (0-based).",
							reason, row + 1, i + 1, name, label, variable.has_label ? "" : " (absent)", preview, variable.storage_type, relative);
						return 0;
					}
				}
			}
			if (output && !output(context, row, i, number, system_missing, text, cell_length, case_file_offset(d, first_offset)))
				return reader_error(d, "SAV_E_OUTPUT_ABORT", "cases.output", first_offset, "Output handler rejected a cell.");
		}
		row++;


	}
}

int sav_decode_cases(SavDocument *d, SavCell output, void *context, size_t *case_count) {
	if (!d || !case_count) return 0;
	*case_count = 0;
	if (!d->dictionary_validated || !d->bytes || !d->variable_count || !d->encoding || d->data_offset > d->size)
		return reader_error(d, "SAV_E_STATE", "cases.start", 0, "Expected a successfully parsed dictionary.");
	if (d->header.compression == 2 && !prepare_zsav(d)) return 0;
	d->case_adjustments = 0;
	d->case_warning[0] = 0;
	int ok = decode_case_stream(d, output, context, case_count);
	if (!ok && d->zsav_ready && d->error.offset >= d->data_offset)
		d->error.offset = case_file_offset(d, d->error.offset);
	return ok;
}
