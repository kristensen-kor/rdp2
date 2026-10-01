#include "sav-reader.h"
#include <stdio.h>
#include <string.h>

#include "sav-encoding.h"

static int text_error(SavDocument *d, SavSlice text, size_t position, const char *message) {
	if (d->text_kind == SAV_TEXT_OK) d->text_kind = SAV_TEXT_INVALID;
	d->error.code = "SAV_E_TEXT_ENCODING";
	d->error.stage = "text.decode";
	d->error.offset = text.offset + position;
	snprintf(d->error.message, sizeof d->error.message, "%s", message);
	return 0;
}

int sav_decode_text(SavDocument *d, SavSlice text, unsigned char *out, size_t capacity, size_t *length) {
	d->text_kind = SAV_TEXT_OK;
	size_t position = 0, written = 0;
	while (position < text.length) {
		unsigned char first = text.bytes[position];
		if (first == 0) return text_error(d, text, position, "Embedded NUL cannot be represented as an R string.");
		if (d->encoding == 1) {
			size_t count = 1;
			uint32_t value = first;
			uint32_t minimum = 0;
			if (first >= 0xc2 && first <= 0xdf) { count = 2; value = first & 0x1f; minimum = 0x80; }
			else if (first >= 0xe0 && first <= 0xef) { count = 3; value = first & 0x0f; minimum = 0x800; }
			else if (first >= 0xf0 && first <= 0xf4) { count = 4; value = first & 0x07; minimum = 0x10000; }
			else if (first >= 0x80) return text_error(d, text, position, "Invalid UTF-8 leading byte.");
			size_t available = count;
			if (available > text.length - position) available = text.length - position;
			for (size_t i = 1; i < available; i++) {
				unsigned char next = text.bytes[position + i];
				if (next < 0x80 || next > 0xbf) return text_error(d, text, position + i, "Invalid UTF-8 continuation byte.");
				/* Even an incomplete suffix must be a possible valid prefix. */
				if (i == 1 && ((first == 0xe0 && next < 0xa0) || (first == 0xed && next > 0x9f) ||
					(first == 0xf0 && next < 0x90) || (first == 0xf4 && next > 0x8f)))
					return text_error(d, text, position + i, "Overlong, surrogate or out-of-range UTF-8 prefix.");
				value = (value << 6) | (next & 0x3f);
			}
			if (available < count) {
				d->text_kind = SAV_TEXT_INCOMPLETE_UTF8_SUFFIX;
				char message[128];
				snprintf(message, sizeof message, "Truncated UTF-8 sequence: 0x%02x needs %zu bytes; %zu remain", first, count, available);
				return text_error(d, text, position, message);
			}
			if (value < minimum || value > 0x10ffff || (value >= 0xd800 && value <= 0xdfff))
				return text_error(d, text, position, "Overlong, surrogate or out-of-range UTF-8 code point.");
			if (out) {
				if (written > capacity || count > capacity - written) return text_error(d, text, position, "UTF-8 output buffer is too small.");
				memcpy(out + written, text.bytes + position, count);
			}
			written += count;
			position += count;
			continue;
		}
		if (d->encoding != 2) return text_error(d, text, position, "No supported encoding has been resolved.");
		uint32_t value = first;
		if (first >= 128) value = cp1251[first - 128];
		if (!value) return text_error(d, text, position, "Undefined Windows-1251 byte 0x98.");
		unsigned char encoded[3];
		size_t count = 1;
		encoded[0] = (unsigned char)value;
		if (value >= 0x80 && value < 0x800) {
			count = 2;
			encoded[0] = (unsigned char)(0xc0 | (value >> 6));
			encoded[1] = (unsigned char)(0x80 | (value & 0x3f));
		} else if (value >= 0x800) {
			count = 3;
			encoded[0] = (unsigned char)(0xe0 | (value >> 12));
			encoded[1] = (unsigned char)(0x80 | ((value >> 6) & 0x3f));
			encoded[2] = (unsigned char)(0x80 | (value & 0x3f));
		}
		if (out) {
			if (written > capacity || count > capacity - written) return text_error(d, text, position, "UTF-8 output buffer is too small.");
			memcpy(out + written, encoded, count);
		}
		written += count;
		position++;
	}
	*length = written;
	return 1;
}

/* Diagnostics only: preserve valid characters, replace each malformed byte or
 * control with '?', and never change the decoder's original error. A bounded
 * preview is cut only between complete UTF-8 characters, with an ellipsis. */
void sav_text_preview(SavDocument *d, SavSlice text, char *out, size_t capacity) {
	if (!capacity) return;
	SavError saved = d->error;
	SavTextKind saved_kind = d->text_kind;
	size_t position = 0, written = 0;
	while (position < text.length) {
		unsigned char first = text.bytes[position];
		size_t count = 1;
		if (d->encoding == 1) {
			if (first >= 0xc2 && first <= 0xdf) count = 2;
			else if (first >= 0xe0 && first <= 0xef) count = 3;
			else if (first >= 0xf0 && first <= 0xf4) count = 4;
		}
		if (count > text.length - position) count = 1;
		unsigned char encoded[4];
		size_t length = 0;
		if (first < 32 || first == 127 || first == '\'' || !sav_decode_text(d,
			(SavSlice){text.bytes + position, count, text.offset + position}, encoded, sizeof encoded, &length)) {
			encoded[0] = '?'; length = 1; count = 1;
		}
		/* Reserve room for the terminator and a visible truncation marker. */
		if (capacity - written <= length + 3) break;
		memcpy(out + written, encoded, length);
		written += length;
		position += count;
	}
	if (position < text.length && capacity - written >= 4) {
		memcpy(out + written, "...", 3);
		written += 3;
	}
	out[written] = 0;
	d->error = saved;
	d->text_kind = saved_kind;
}
