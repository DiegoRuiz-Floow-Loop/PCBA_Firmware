/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "plf/plf.h"
#include "plf/json/jsonx.h"

#include "jansson.h"

/******************************************************************************/

//volatile uint32_t mySeq = 0;


void JsonxInit(void)
{
	json_set_alloc_funcs(HeapxMalloc, HeapxFree);
}


/******************************************************************************/

