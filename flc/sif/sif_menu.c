/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef __MODULE__
#define __MODULE__ "sif_menu"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "stm32l4xx_hal.h"

#include "hal/hal.h"
#include "hal/wdt/wdt.h"
#include "hal/dflash/dflash.h"
#include "hal/rtc/rtc.h"
#include "hal/adc/adc.h"

#include "plf/rtos/rtos.h"
#include "plf/cli/cli.h"
#include "plf/trc/trc.h"
#include "plf/nvm/nvm.h"
#include "plf/ver/ver.h"
#include "plf/blc/blc.h"
#include "plf/dlog/dlog.h"
#include "plf/elog/elog.h"
#include "plf/util/widechar.h"

#include "plf/blc/blc_crc.h"

#include "modem.h"
#include "net/rest/rest.h"

#include "sup/float_switch.h"
#include "sup/log_sensors.h"
#include "sup/pos_sensor.h"
#include "sup/water_sensor.h"

#include "shc/shc.h"
#include "shc/pump.h"
#include "shc/uv_lamp.h"
#include "shc/valve.h"
#include "shc/flow_knob.h"
#include "shc/hot_supply.h"
#include "shc/cold_supply.h"
#include "shc/prime_pump.h"

#include "sif/sif.h"

/******************************************************************************/

#ifdef CLI_ENABLE


/******************************************************************************/

static int_fast16_t CliShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  char sz[80];
  int16_t i;

  CliWriteLine();
  CliWrite("| ");
  i = cliWriteLen;
  CliWrite(applicationString);
  i += cliWriteLen;
  CliWrite(" - Service Interface (SIF)");
  cliWriteLen += i;
  CliWriteEol();
  CliPrintf("| Software v%u.%u.%c, build " __DATE__ " " __TIME__ ", crc 0x%04X",
    pFWDescriptor->swVerMajor, pFWDescriptor->swVerMinor, pFWDescriptor->swVerType, pFWDescriptor->crc); 
    CliWriteEol();
  CliWriteLine();
  
  //uint64_t bootCount;
  //NvmRead(offsetof(NvmStruct_t, bootCount), (uint8_t *)&bootCount, sizeof(bootCount));
  //CliPrintf("| Boot Count: %llu", bootCount); CliWriteEol();
  
  strcpy(sz, "| UTC/RTC: ");
  (void)RtcGetStr(&sz[strlen(sz)]);
  CliWrite(sz);
  CliWriteEol();
  
  strcpy(sz, "| Up time: ");
  PlfTime_DHMSM_StrGet(&sz[strlen(sz)]);
  CliWrite(sz);
  CliWriteEol();
  CliWriteLine();
  
  //CliPrintf("| Name: %s",  wchar2char(sz, app.name, sizeof(sz))); CliWriteEol();
  //CliPrintf("| No: %u",  app.no); CliWriteEol();
  //CliPrintf("| Application: %s", lstSW_App[app.app]); CliWriteEol();
  //CliWriteLine();
  
  cliShowHeader = false;
  TestModemShow(0,0,0);  
  cliShowHeader = true;
  CliWriteLine();
  
  return CLI_RESULT_OK;
}

/****************************************************************************/

#define BIT_WRITE(var, bit, val)  { \
  if (val) \
    var[bit / 32] |= (1ul << (bit % 32)); \
  else \
    var[bit / 32] &= ~(1ul << (bit % 32)); \
}

#define BIT_READ(var, bit) (var[bit / 32] & (1ul << (bit % 32))) != 0

static void BitWrite(uint32_t var[], uint_fast16_t bit, bool val) 
{ 
  BIT_WRITE(var, bit, val);
}

static bool BitRead(uint32_t var[], uint_fast16_t bit) 
{ 
  return BIT_READ(var, bit);
}

static long RunSieve(void) 
{
  #define SIZE_SIEVE (1024)  // bits
  uint32_t flags[1+((SIZE_SIEVE+31)/32)];
  int i, prime, k, count;
  int iterations = 0;
  PlfTime_t startTime, elapsedTime;
	PlfTimeMsGet(&startTime);
  for (;;) {
    count=0;
    memset(flags, 0xFF, sizeof(flags));
    for (i=0; i<=SIZE_SIEVE; i++) {
       if (BitRead(flags, i)) {
          prime=i+i+3;
          for(k=i+prime; k<=SIZE_SIEVE; k+=prime)
            BitWrite(flags, k, false);
          count++;
       }
    }
    iterations++;
		PlfTimeMsGet(&elapsedTime);
    HalWdtFeed();
    if ( (int32_t)((int32_t)elapsedTime - (int32_t)startTime) >= 5000L) {
      return iterations;
    }
  }
}

static volatile float s = 0.0;

static long RunFloat(void) 
{
  #define SIZE_F (300)
  float f[SIZE_F];
  int iterations = 0;
  int i;
  PlfTime_t startTime, elapsedTime;
  for (i=0; i<SIZE_F; i++)
    f[i] = (1.0/3.3) * i; 
  i=0;
	PlfTimeMsGet(&startTime);
  for (;;) {
    for (i=0; i<SIZE_F-2; i++) {
      s = s + f[i] * f[i+1] / f[i+2];
    }
		PlfTimeMsGet(&elapsedTime);
    iterations++;
    HalWdtFeed();
    if ( (int32_t)((int32_t)elapsedTime - (int32_t)startTime) >= 5000L) {
      return iterations;
    }
  }
}

int_fast16_t DbgTstSpeed(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  CliPrintf("System running %lu MHz" CLI_NL, (SystemCoreClock+500000)/1000000);
  CliPrintf("Speed result, sieve: %lu itterations" CLI_NL, RunSieve());
  CliPrintf("Speed result, float: %lu itterations" CLI_NL, RunFloat());
  return CLI_RESULT_OK;
}

/****************************************************************************/

extern int_fast16_t CliDLogShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);
extern int_fast16_t CliDLogErase(CliParam_t param1, CliParam_t param2, CliParam_t param3);
extern int_fast16_t CliELogShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);
extern int_fast16_t CliELogErase(CliParam_t param1, CliParam_t param2, CliParam_t param3);

static int_fast16_t CliEraseLog(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
	int_fast16_t res;
  res = CliELogErase(0,0,0);
  if (res	== CLI_RESULT_OK)
		res = CliDLogErase(0,0,0);
  return res;
}

CLI_DECLARE_SUBTEST(elog)
CLI_DECLARE_SUBTEST(dlog)

CLI_START_TABLE(log)
  CLI_ENTRY0( "sdat", "Show Data Log", CliDLogShow)
  CLI_ENTRY0( "seve", "Show Event Log", CliELogShow)

  CLI_SUBTEST("dlog", "Data Log System", dlog)
  CLI_SUBTEST("elog", "Event Log System", elog)
  CLI_ENTRY0("erase", "Erase Data and Event logs", CliEraseLog)

#ifdef TRC_ENABLE
  CLI_SUBTEST("trc",  "Trace System", trc)
#endif

  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(log)

/****************************************************************************/

CLI_DECLARE_SUBTEST(trc)        /* in trc.c */
CLI_DECLARE_SUBTEST(nvm)        
//CLI_DECLARE_SUBTEST(nvm_flash)        

#if defined(USE_FWD_BC_BOOT_MENU)
static int_fast16_t CliDfuMenu(CliParam_t p1, CliParam_t param2, CliParam_t param3)
{  
	BlcBtlMenuEnter("DFU Menu");
  return CLI_RESULT_ERROR_UNDEFINED;
}
#endif


static int_fast16_t CliDfuCheck(CliParam_t p1, CliParam_t param2, CliParam_t param3)
{  
  FWDescriptor_t fwd;
	CliWriteLn("Checking Firmware...");
  FwdBootCommand_t bc = BlcCheckDataFlashFirmware(DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, &fwd);
  if (bc != FWD_BC_UNKNOWN) {
		CliPrintf("... %s OK" CLI_NL, bc == FWD_BC_MAKE_DFU_NEW ? "DFU" : "DFX");
		return CLI_RESULT_OK;
	} else {
		CliPrintf("... Firmware in DFLASH is not OK" CLI_NL);
		return CLI_RESULT_ERROR_UNDEFINED;
	}
}


static void crcCheckDone(const bool result)
{
	CliPrintf("CRC result: %s\n", (result ? "OK" : "Failed"));
  CliCompleted(result ? CLI_RESULT_OK : CLI_RESULT_ERROR_UNDEFINED);  
}

static int_fast16_t CliDfuCrc(CliParam_t p1, CliParam_t param2, CliParam_t param3)
{
	CliPrintf("Validating CRC...\n");
  BlcCrcValidate(crcCheckDone);
  return CLI_RESULT_EXECUTING;  
}

static int_fast16_t CliDfuActicate(CliParam_t p1, CliParam_t param2, CliParam_t param3)
{  
  FWDescriptor_t fwd;
  FwdBootCommand_t bc = BlcCheckDataFlashFirmware(DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, &fwd);
  if (bc != FWD_BC_UNKNOWN) {
    BlcActivateNewImage(0, bc);
  } else {
		CliPrintf("... Firmware in DFLASH is not OK" CLI_NL);
    return CLI_RESULT_ERROR_UNDEFINED;
  }
  return CLI_RESULT_OK;
}

static int_fast16_t CfgRset(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  MarkStr_t mark = "DEADBEAF";
  NvmWrite(offsetof(NvmStruct_t, beginMark), (uint8_t *)mark, sizeof(MarkStr_t));
  return CliReboot(0,0,0);
}

static int_fast16_t TestHGetDFU(CliParam_t name, CliParam_t addr, CliParam_t maxLen)
{
#if defined(EVK)
  return TestHGet((CliParam_t)"flc_evk.dfu", (CliParam_t)DFLASH_DFU_START, (CliParam_t)APP_PROGRAM_FLASH_SIZE);
#else
  return TestHGet((CliParam_t)"flc.dfu", (CliParam_t)DFLASH_DFU_START, (CliParam_t)APP_PROGRAM_FLASH_SIZE);
#endif
}

CLI_START_TABLE(sys)
  CLI_ENTRY0( "show",   "Show System Status", CliShow)

  CLI_ENTRY2( "rtc",      "Set UTC/RTC time [yyyy-mm-dd] [hh:mm:ss]", CliRtcSet, CLI_PARAM_STRING, CLI_PARAM_STRING)

#if (PLF_OS == PLF_OS_RTOS)
  CLI_ENTRY0( "rtos",   "RTOS Status", CliRtosStat)
#endif
  CLI_ENTRY0( "evos",   "EvOS Status", CliEvosStat)

  CLI_ENTRY0( "dfuget", "Get DFU File (HTTP)", TestHGetDFU)
	CLI_ENTRY0( "dfuchk", "Check DFU", CliDfuCheck)
	CLI_ENTRY0( "dfucrc", "DFU CRC check", CliDfuCrc)
	CLI_ENTRY0( "dfuact", "Activate DFU", CliDfuActicate)
#if defined(USE_FWD_BC_BOOT_MENU)
	CLI_ENTRY0( "dfumenu",     "Menu DFU", CliDfuMenu)
#endif

  CLI_ENTRY0( "rcfg",     "Reset Configuration", CfgRset)
	CLI_SUBTEST("trc",      "Trace system", trc)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(sys)


/******************************************************************************/

CLI_DECLARE_SUBTEST(hwt)
CLI_DECLARE_SUBTEST(modem)

CLI_START_TABLE(test)
  CLI_ENTRY0( "show",     "Show System Status", CliShow)
	CLI_ENTRY0( "speed",    "MCU Speed", DbgTstSpeed)
  CLI_SUBTEST("modem",     "Modem/SIM7070G test Menu", modem)
//  CLI_SUBTEST("mqtt",     "MQTT test Menu", mqtt)

	CLI_SUBTEST("hwt",      "Hardware Test (hal)", hwt)
	CLI_SUBTEST("trc",      "Trace system", trc)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(test)

/******************************************************************************/

CLI_DECLARE_SUBTEST(adc_mux)
CLI_DECLARE_SUBTEST(log_sensors)
CLI_DECLARE_SUBTEST(pos_sensor)
CLI_DECLARE_SUBTEST(water_sensors)
CLI_DECLARE_SUBTEST(float_switches)
CLI_DECLARE_SUBTEST(user_buttons)
CLI_DECLARE_SUBTEST(pump)
CLI_DECLARE_SUBTEST(uv_lamp)
CLI_DECLARE_SUBTEST(valve)

static int_fast16_t CliShowDrivers(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliPosSensorShowAll(0, 0, 0);
  CliWaterSensorShowAll(0, 0, 0);
  CliFloatSwitchShowAll(0, 0, 0);
  CliLogSensorShowAll(0, 0, 0);
  
  CliUvLampShowAll(0, 0, 0); 
  CliPumpShowAll(0, 0, 0); 
  CliValveShowAll(0, 0, 0); 
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(app_drv)
  CLI_ENTRY0( "show",   "Show combined driver status", CliShowDrivers)

  CLI_SUBTEST("mux",    "ADC multiplexed inputs", adc_mux)
  CLI_SUBTEST("psens",  "Position sensors", pos_sensor)  
  CLI_SUBTEST("wsens",  "Water sensors", water_sensors)
  CLI_SUBTEST("fswch",  "Float switches", float_switches)
  CLI_SUBTEST("lsens",  "Log sensors", log_sensors)
  CLI_SUBTEST("userbtn", "User Buttons", user_buttons)

  CLI_SUBTEST("uv",     "UV Lamps", uv_lamp)
  CLI_SUBTEST("pump",   "Pumps", pump)
  CLI_SUBTEST("valve",  "Valves", valve)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)
CLI_END_TABLE(app_drv)

/******************************************************************************/

CLI_DECLARE_SUBTEST(shc)
CLI_DECLARE_SUBTEST(flow_knob)
CLI_DECLARE_SUBTEST(hot_supply)
CLI_DECLARE_SUBTEST(cold_supply)
CLI_DECLARE_SUBTEST(prime_pump)
CLI_DECLARE_SUBTEST(data_logger)

static int_fast16_t CliShowControllers(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliShcShowAll(0, 0, 0);
  CliFlowKnobShowAll(0, 0, 0);
  CliHotSupplyShowAll(0, 0, 0);
  CliColdSupplyShowAll(0, 0, 0);
  CliPrimePumpShowAll(0, 0, 0);
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(app)
  CLI_ENTRY0( "show",   "Show combined controller status", CliShowControllers)

  CLI_SUBTEST("drv",    "Application HW drivers", app_drv)
  
  CLI_SUBTEST("shc",    "Shower Controller", shc)
  CLI_SUBTEST("knob",   "Flow Knob Controller", flow_knob)
  CLI_SUBTEST("hot",    "Hot Supply Controller", hot_supply)
  CLI_SUBTEST("cold",   "Cold Supply Controller", cold_supply)
  CLI_SUBTEST("prime",  "Prime Pump Controller", prime_pump)

  CLI_SUBTEST("dlog",   "Data logger", data_logger)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)
CLI_END_TABLE(app)

/******************************************************************************/

CLI_DECLARE_SUBTEST(cloud)

/******************************************************************************/

CLI_START_TABLE(root)
  CLI_ENTRY0( "show",   "Show System Status", CliShow)
  CLI_SUBTEST("app",    "Application Menu",   app)
  CLI_SUBTEST("cld",    "Cloud Menu",         cloud)
  CLI_SUBTEST("sys",    "System Menu",        sys)
  CLI_SUBTEST("log",    "Logging Menu",       log)
  CLI_SUBTEST("test",   "Test Menu",          test)
#ifdef TRC_ENABLE     
  CLI_SUBTEST("trc",    "Trace system",       trc)
#endif
  CLI_ENTRY0( "reboot", "Re-boot System",     CliReboot)
CLI_END_TABLE(root)


#endif
