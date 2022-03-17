/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef DFLASH_DEF_H
#define DFLASH_DEF_H

#include "hal/cfg/dflash_cfg.h"

/******************************************************************************/
// supported devics 

// Used by EVK board (TP_IOT_NT2_Rev1B)
#define DFLASH_32MBIT			  32			// adesto, 	AT25SF321

// Used by Flow Loop board
#define DFLASH_128MBIT			128			// adesto,  AT25SF128
#define DFLASH_512MBIT			512			// Micron,  MT25QL512AB

#if (DFLASH_SIZE_MBIT==DFLASH_32MBIT)
  #define DFLASH_SIZE  	                ((32*1024*1024)/8)
	#define DFLASH_PP_SIZE 				        (256)     // 	
	#define DFLASH_BLOCK_SIZE        	    (64*1024) //
	#define DFLASH_SMALL_BLOCK_SIZE  	    (4*1024)  // smallest erase size

	#define DFLASH_BLOCK_ERASE_TIMEOUT    (457+1000)  // 46 / 457 msec
  #define DFLASH_CHIP_ERASE_TIMEOUT     (35*1000)   // 29 ca. sekunder

#elif (DFLASH_SIZE_MBIT==DFLASH_128MBIT)
  #define DFLASH_SIZE  	                ((128 * 1024 * 1024) / 8)
	#define DFLASH_PP_SIZE 				        (256)     		// page read/write size	
	#define DFLASH_SMALL_BLOCK_SIZE  	    ( 4 * 1024) 	// smallest erase size
	#define DFLASH_BLOCK_SIZE        	    (64 * 1024)		// largest erase size

	#define DFLASH_BLOCK_ERASE_TIMEOUT    (  2 * 1000)	// 4K (70-300 msec), 32K (150-1600 msec), 64K (250-2000 msec)
  #define DFLASH_CHIP_ERASE_TIMEOUT     (120 * 1000)	// 30-120 seconds

#elif (DFLASH_SIZE_MBIT==DFLASH_512MBIT)
  #define DFLASH_SIZE  	                ((512*1024*1024)/8)
	#define DFLASH_PP_SIZE 				        (256)     			      // 	
	#define DFLASH_SMALL_BLOCK_SIZE  	    (4*1024)  	    // don't have smaler block
	#define DFLASH_BLOCK_SIZE        	    (64*1024) 			// 256 KB

  #define DFLASH_BLOCK_ERASE_TIMEOUT    (457+1000)    // xx  msec
  #define DFLASH_CHIP_ERASE_TIMEOUT     (300*1000)   // ca. ?? sekunder

#else
  #error "define/implement used DFLASH !!"
#endif


//#define DFLASH_SECTOR_SIZE					--> DFLASH_SMALL_BLOCK_SIZE

#define DFLASH_BLOCKS               (DFLASH_SIZE / DFLASH_BLOCK_SIZE)	
#define DFLASH_SMALL_BLOCKS  	      (DFLASH_SIZE / DFLASH_SMALL_BLOCK_SIZE) 
#define DFLASH_PAGES                (DFLASH_SIZE / DFLASH_PP_SIZE)	  
	
/******************************************************************************/

#endif /* DFLASH_DEF_H */
