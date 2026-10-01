#include "sav.h"
#include <float.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	const unsigned char *bytes;
	size_t size, pos;
	const char *stage;
	SavError *error;
	SavTrace trace;
	void *context;
} SavReader;

static int fail(SavReader *r, const char *code, size_t offset, const char *fmt, ...) {
	r->error->code = code;
	r->error->stage = r->stage;
	r->error->offset = offset;
	va_list args;
	va_start(args, fmt);
	vsnprintf(r->error->message, sizeof r->error->message, fmt, args);
	va_end(args);
	return 0;
}

static int read_bytes(SavReader *r, void *out, size_t n, const char *stage) {
	r->stage = stage;
	if (r->pos > r->size || n > r->size - r->pos)
		return fail(r, "SAV_E_TRUNCATED", r->pos, "Expected %zu bytes; available %zu.", n, r->size - r->pos);
	memcpy(out, r->bytes + r->pos, n);

	r->pos += n;
	return 1;
}

static uint32_t u32_le(const unsigned char *p) {
	return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24;
}

static int read_i32(SavReader *r, int32_t *out, const char *stage) {
	unsigned char bytes[4];
	if (!read_bytes(r, bytes, sizeof bytes, stage)) return 0;
	uint32_t value = u32_le(bytes);
	/* Avoid implementation-defined unsigned-to-signed conversion. */
	if (value <= INT32_MAX) *out = (int32_t)value;
	else *out = -1 - (int32_t)(UINT32_MAX - value);
	return 1;
}

int sav_parse_header(const unsigned char *bytes, size_t size, SavHeader *h,
	SavError *error, SavTrace trace, void *context) {
	if (!h || !error) return 0;
	memset(h, 0, sizeof *h);
	memset(error, 0, sizeof *error);
	SavReader r = {bytes, size, 0, "header", error, trace, context};
	if (!bytes) return fail(&r, "SAV_E_ARGUMENT", 0, "Expected a non-null byte buffer.");
	if (sizeof(double) != 8 || DBL_MANT_DIG != 53 || DBL_MAX_EXP != 1024 || FLT_RADIX != 2)
		return fail(&r, "SAV_E_HOST_FLOAT", 0, "Expected host IEEE 754 binary64 support.");
	if (!read_bytes(&r, h->signature, 4, "header.signature")) return 0;
	if (memcmp(h->signature, "$FL2", 4) && memcmp(h->signature, "$FL3", 4))
		return fail(&r, "SAV_E_SIGNATURE", 0, "Expected ASCII-compatible $FL2 or $FL3 signature.");
	if (!read_bytes(&r, h->product, 60, "header.product")) return 0;
	unsigned char layout[4];
	if (!read_bytes(&r, layout, 4, "header.layout_code")) return 0;
	if (!memcmp(layout, "\0\0\0\2", 4) || !memcmp(layout, "\0\0\0\3", 4))
		return fail(&r, "SAV_E_UNSUPPORTED_ENDIAN", 64, "Recognized big-endian layout; this probe supports little endian only.");
	h->layout_code = (int32_t)u32_le(layout);
	if (h->layout_code == 3)
		return fail(&r, "SAV_E_UNSUPPORTED_LAYOUT", 64, "Recognized layout 3; currently support layout 2 only.");
	if (h->layout_code != 2)
		return fail(&r, "SAV_E_LAYOUT", 64, "Expected layout 2; found unsupported layout bytes.");
	if (!read_i32(&r, &h->nominal_case_size, "header.nominal_case_size")) return 0;
	if (h->nominal_case_size < -1)
		return fail(&r, "SAV_E_CASE_SIZE", 68, "Expected nominal case size >= -1; found %d.", h->nominal_case_size);
	if (!read_i32(&r, &h->compression, "header.compression")) return 0;
	if (h->compression < 0 || h->compression > 2)
		return fail(&r, "SAV_E_UNSUPPORTED_COMPRESSION", 72, "Expected compression 0, 1 or 2; found %d.", h->compression);
	int is_fl3 = !memcmp(h->signature, "$FL3", 4);
	if (is_fl3 != (h->compression == 2))
		return fail(&r, "SAV_E_COMPRESSION_SIGNATURE", 72, "Expected $FL3 exactly when compression is 2.");
	if (!read_i32(&r, &h->weight_index, "header.weight_index")) return 0;
	if (h->weight_index < 0)
		return fail(&r, "SAV_E_WEIGHT_INDEX", 76, "Expected nonnegative weight dictionary index; found %d.", h->weight_index);
	if (!read_i32(&r, &h->case_count, "header.case_count")) return 0;
	if (h->case_count < -1)
		return fail(&r, "SAV_E_CASE_COUNT", 80, "Expected case count >= 0 or -1 (unknown); found %d.", h->case_count);
	unsigned char bias[8];
	if (!read_bytes(&r, bias, 8, "header.bias")) return 0;
	/* Require exact known IEEE little-endian 100.0 before interpreting floats. */
	static const unsigned char canonical_bias[8] = {0, 0, 0, 0, 0, 0, 0x59, 0x40};
	if (memcmp(bias, canonical_bias, 8))
		return fail(&r, "SAV_E_UNSUPPORTED_FLOAT_OR_BIAS", 84, "Expected IEEE little-endian bias 100.0; other bias/float representations are not implemented.");
	h->bias = 100.0;
	if (!read_bytes(&r, h->creation_date, 9, "header.creation_date")) return 0;
	if (!read_bytes(&r, h->creation_time, 8, "header.creation_time")) return 0;
	if (!read_bytes(&r, h->file_label, 64, "header.file_label")) return 0;
	if (!read_bytes(&r, h->padding, 3, "header.padding")) return 0;
	if (trace) {
		char message[256];
		snprintf(message, sizeof message, "%.4s; little endian; layout %d; compression %s; declared cases %d; nominal slots %d; weight dictionary index %d; bias %.0f.", h->signature, h->layout_code, h->compression == 0 ? "uncompressed" : h->compression == 1 ? "bytecode" : "ZSAV", h->case_count, h->nominal_case_size, h->weight_index, h->bias);
		trace(context, 0, "header.summary", message);
		char product[61];
		for (size_t i = 0; i < 60; i++) product[i] = h->product[i] >= 32 && h->product[i] <= 126 ? (char)h->product[i] : '?';
		product[60] = 0;
		snprintf(message, sizeof message, "Product identifier (ASCII preview): %s; dictionary and cases not validated.", product);
		trace(context, 4, "product.summary", message);
	}
	return 1;
}
