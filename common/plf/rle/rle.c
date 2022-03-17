/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "plf/rle/rle.h"

//-------------------------------------------------------------------

size_t Decompress(uint8_t* input, uint8_t* output, size_t inputSize, size_t maxOutputSize)
{
	uint8_t marker, symbol;
	uint32_t i, inputPosition, outputPosition, count;

	if (inputSize < 1) {
		return 0;
	}

	inputPosition = 0;
	marker = input[inputPosition++];
	outputPosition = 0;

	do {
		symbol = input[inputPosition++];

		if (symbol == marker) {
			count = input[inputPosition++];

			if (count <= 2) {
				for (i = 0; i <= count; ++i) {
					output[outputPosition++] = marker;
          if (outputPosition >= maxOutputSize) return 0;
				}
			} else {
				if (count & 0x80) {
					count = ((count & 0x7f) << 8) + input[inputPosition++];
				}

				symbol = input[inputPosition++];

				for (i = 0; i <= count; ++i) {
					output[outputPosition++] = symbol;
          if (outputPosition > maxOutputSize) return 0;
				}
			}
		} else {
			output[outputPosition++] = symbol;
      if (outputPosition > maxOutputSize) return 0;
		}
	} while (inputPosition < inputSize);
  return outputPosition;
}

//-------------------------------------------------------------------
#ifndef BOOTLOADER
//-------------------------------------------------------------------

static void encodeRepetition (
  uint8_t* output,
  uint32_t* outputPosition,
  uint8_t marker,
  uint8_t symbol,
  uint32_t count)
{
	uint32_t index = *outputPosition;

	if (count <= 3) {
		if (symbol == marker) {
			output[index++] = marker;
			output[index++] = count - 1;
		} else {
      uint32_t i;
			for (i = 0; i < count; ++i) {
				output[index++] = symbol;
			}
		}
	} else {
		output[index++] = marker;
		--count;

		if (count >= 128) {
			output[index++] = (count >> 8) | 0x80;
		}

		output[index++] = count & 0xff;
		output[index++] = symbol;
	}

	*outputPosition = index;
}

static void encodeNonRepetition(uint8_t* output, uint32_t* outputPosition, uint8_t marker, uint8_t symbol)
{
	uint32_t index = *outputPosition;

	if (symbol == marker) {
		output[index++] = marker;
		output[index++] = 0;
	} else {
		output[index++] = symbol;
	}

	*outputPosition = index;
}

size_t Compress(uint8_t* input, uint8_t* output, uint32_t inputSize) 
{
	uint8_t byte1, byte2, marker;
	uint32_t i, inputPosition, outputPosition, count, histogram[256];

	if (inputSize < 1) {
		return 0;
	}

	for (i = 0; i < 256; ++i) {
		histogram[i] = 0;
	}

	for (i = 0; i < inputSize; ++i) {
		++histogram[input[i]];
	}

	marker = 0;
	for (i = 1; i < 256; ++i) {
		if (histogram[i] < histogram[marker]) {
			marker = i;
		}
	}

	output[0] = marker;
	outputPosition = 1;

	byte1 = input[0];
	inputPosition = 1;
	count = 1;

	if (inputSize >= 2) {
		byte2 = input[inputPosition++];
		count = 2;

		do {
			if (byte1 == byte2) {
				while ((inputPosition < inputSize) && (byte1 == byte2) && (count < 32768)) {
					byte2 = input[inputPosition++];
					++count;
				}

				if (byte1 == byte2) {
					encodeRepetition(output, &outputPosition, marker, byte1, count);

					if (inputPosition < inputSize) {
						byte1 = input[inputPosition++];
						count = 1;
					} else {
						count = 0;
					}
				} else {
					encodeRepetition(output, &outputPosition, marker, byte1, count - 1);
					byte1 = byte2;
					count = 1;
				}
			} else {
				encodeNonRepetition(output, &outputPosition, marker, byte1);
				byte1 = byte2;
				count = 1;
			}
			if (inputPosition < inputSize) {
				byte2 = input[inputPosition++];
				count = 2;
			}
		} while ((inputPosition < inputSize) || (count >= 2));
	}

	if (count == 1) {
		encodeNonRepetition(output, &outputPosition, marker, byte1);
	}

	return outputPosition;
}

//-------------------------------------------------------------------

size_t RleCode(
  char        * dstFileName,
  uint8_t     * imageData,
  size_t      size)
{
  FILE *destination;
  uint8_t * compressedData;
  size_t compressedDataSize;
#if defined(DO_TEST)
  uint8_t * decompressedData;
  size_t decompressedDataSize;
#endif
  destination = fopen(dstFileName, "wb");
  if (!destination) return 0;

  compressedData = (uint8_t*)malloc(size * (257 / 256) + 1);
  if (!compressedData) {
    fclose(destination);
    return 0;
  }

  compressedDataSize = Compress(imageData, compressedData, size);

  fwrite(compressedData,compressedDataSize,1,destination);
  fclose(destination);

#if defined(DO_TEST)
  decompressedData = (uint8_t*)malloc(size);
  if (!decompressedData) {
    free(compressedData);
    return 0;
  }

  decompressedDataSize = Decompress(compressedData, decompressedData, compressedDataSize);

  if (decompressedDataSize != size) {
    printf("\n\nWRONG DECOMPRESS SIZE");
  } else {
    size_t s;
    s = memcmp(imageData, decompressedData, size);
    if (s != 0) {
      printf("\n\nWRONG DECOMPRESSD DATA at %u", s);
    }
  }

  free(decompressedData);
#endif

  free(compressedData);
  return compressedDataSize;
}

//-------------------------------------------------------------------
#endif // ifndef BOOTLOADER
//-------------------------------------------------------------------
