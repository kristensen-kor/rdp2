#ifndef RDP2_SAV_READER_H
#define RDP2_SAV_READER_H
#include "sav.h"

typedef enum { SAV_TEXT_OK, SAV_TEXT_INVALID, SAV_TEXT_INCOMPLETE_UTF8_SUFFIX } SavTextKind;

/* All slices borrow the input buffer; keep it alive until sav_document_free(). */
typedef struct { const unsigned char *bytes; size_t length, offset; } SavSlice;
typedef struct {
	size_t offset, length;
	int32_t type, subtype, element_size, element_count;
	SavSlice payload;
} SavRecord;
typedef struct {
	size_t record_offset, dictionary_index, slot_count;
	int32_t storage_type, print_format, write_format, missing_count;
	SavSlice short_name, long_name, label, missing_raw;
	int has_label, has_display;
	int32_t measure, display_width, alignment;
	int label_set_index;
	size_t segment_start, segment_count;
	SavSlice string_missing[3]; /* Subtype 22 codes; borrow original payload bytes. */
} SavVariable;
typedef struct { SavSlice value, label; } SavValueLabel;
typedef struct {
	size_t offset, count;
	SavValueLabel *entries;
	int key_type; /* 0 numeric, positive string width */
} SavLabelSet;
typedef struct {
	SavHeader header;
	const unsigned char *bytes;
	size_t size, data_offset, slot_count;
	SavVariable *variables;
	size_t variable_count, variable_capacity;
	SavVariable *physical_variables;
	size_t physical_variable_count;
	unsigned char *case_text;
	size_t *case_text_offsets;
	size_t case_text_capacity;
	SavLabelSet *label_sets;
	size_t label_set_count, label_set_capacity;
	SavRecord *records;
	size_t record_count, record_capacity;
	SavSlice documents, encoding_declaration;
	int encoding; /* 1 UTF-8, 2 Windows-1251 */
	int encoding_overridden, has_machine, has_float_info;
	int dictionary_validated; /* Set only after all framing and semantic checks succeed. */
	int32_t machine[8];
	double float_info[3];
	int64_t case_count_extended;
	int has_extended_count;
	size_t weight_variable_index; /* SIZE_MAX means none */
	SavError error;
	SavTrace trace;
	void *trace_context;
	/* Owned, bounded ZSAV bytecode cache; dictionary slices still borrow input. */
	unsigned char *zsav_data;
	size_t zsav_size, zsav_trailer, zsav_blocks;
	int zsav_ready;
	int debug; /* Retain optional metadata only for diagnostic output. */
	uint32_t extension_seen; /* Framing/duplicate checks without a record ledger. */
	size_t dictionary_records;
	void (*interrupt)(void); /* Optional host hook; checked between bounded units. */
	size_t label_adjustments, case_adjustments;
	size_t total_labels; /* Updated once when either kind of label set is added. */
	SavTextKind text_kind; /* Decoder status, independent of diagnostic wording. */
	char label_warning[768], case_warning[768]; /* First example; R emits aggregate warnings. */
} SavDocument;

typedef int (*SavCell)(void *context, size_t row, size_t variable_index,
	double number, int system_missing, const unsigned char *text, size_t text_length,
	size_t source_offset);
/* Caller zero-initializes document, then always frees it even on failure. */
int sav_parse_dictionary(SavDocument *d, const unsigned char *bytes, size_t size,
	const char *encoding_override, SavTrace trace, void *context);
/* Decode cases after successful dictionary validation; no cell-count policy cap. */
int sav_decode_cases(SavDocument *d, SavCell output, void *context, size_t *case_count);
int sav_decode_text(SavDocument *d, SavSlice text, unsigned char *out, size_t capacity, size_t *length);
void sav_text_preview(SavDocument *d, SavSlice text, char *out, size_t capacity);
double sav_number_le(const unsigned char *bytes);
void sav_document_free(SavDocument *d);
#endif
