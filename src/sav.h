#ifndef RDP2_SAV_H
#define RDP2_SAV_H

#include <stddef.h>
#include <stdint.h>

enum { SAV_HEADER_SIZE = 176 };
typedef struct {
	const char *code;
	const char *stage;
	size_t offset;
	char message[1024];
} SavError;

typedef struct {
	unsigned char signature[4], product[60];
	int32_t layout_code, nominal_case_size, compression, weight_index, case_count;
	double bias;
	unsigned char creation_date[9], creation_time[8], file_label[64], padding[3];
} SavHeader;

typedef void (*SavTrace)(void *context, size_t offset, const char *stage, const char *message);
/* Header inspection only. Success does not validate dictionary or cases. */
int sav_parse_header(const unsigned char *bytes, size_t size, SavHeader *header,
	SavError *error, SavTrace trace, void *context);

#endif
