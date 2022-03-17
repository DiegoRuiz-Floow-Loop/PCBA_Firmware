/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_CLI_H
#define PLF_CLI_H

#ifdef __cplusplus
extern "C" {
#endif /* #ifdef __cplusplus */

/******************************** INCLUDE FILES *******************************/

#include "plf/plf.h"
#include "plf/cfg/cli_cfg.h"

#define CLI_CR          '\r'
#define CLI_LF          '\n'

#define CLI_NL_CHAR     CLI_LF   // CR or LF ??
#define CLI_NL          "\n"

extern const char       cliNewLine[];

#ifdef CLI_ENABLE
  
#include <stdio.h>
#include "plf/plf.h"

/*********************************** DEFINES **********************************/

// http://www.termsys.demon.co.uk/vtansi.htm

#define VT100_WindowSet           "\x1B[5;r"  /* lines + 1 */
#define VT100_WindowEnd           "\x1B[;r"

#define VT100_CursorAttrSave      "\x1B\x37"
#define VT100_CursorAttrRestore   "\x1B\x38"
#define VT100_CursorHide          "\x1B[?25l"
#define VT100_CursorShow          "\x1B[?25h"
#define VT100_CursorSave          "\x1B[s"
#define VT100_CursorUnsave        "\x1B[u"
#define VT100_CursorHome          "\x1B[H"
#define VT100_CursorMoveDown      "\x1B[5B"


#define VT100_TextAttr            "\x1B[0;7m"
#define VT100_TextAttrBright      "\x1B[1m"   /* TeraTerm: blue */
#define VT100_TextAttrUnderline   "\x1B[4m"
#define VT100_TextAttrBlink       "\x1B[5m"   /* TeraTerm: read */
#define VT100_TextAttrRecverse    "\x1B[7m"
#define VT100_TextAttrReset       "\x1B[0m"

#define VT100_ClearScreen         "\x1B[2J"
#define VT100_ClearLineFromCursor "\x1B[0K"




#define CLI_MAX_PARAM                     (3)
  
#define CLI_RESULT_EXECUTING              (1)
#define CLI_RESULT_OK                     (0)
#define CLI_RESULT_ERROR_UNDEFINED        (-1)
#define CLI_RESULT_ERROR_PARAMETER_VALUE  (-2)


/************************** INTERFACE DATA DEFINITIONS ************************/

/* Command parameter type. Each parameter to a CLI command is specified
 * by a value of this type.
 */
typedef enum {
  CLI_PARAM_NONE,
  CLI_PARAM_P_UINT64,
  CLI_PARAM_UINT32,
  CLI_PARAM_INT32,
  CLI_PARAM_CHAR,
  CLI_PARAM_STRING,       // This_is_a_string_one  - no spaces
  CLI_PARAM_STRING_LAST,  // This is a string two  - spaces acceptet, but must must be last parameter!
  CLI_PARAM_ONOFF,
  CLI_PARAM_NOYES,
  CLI_PARAM_DISABLEENABLE,
  CLI_PARAM_TRACEMASK,
  CLI_PARAM_TRACEMODE,
  CLI_PARAM_DOMAIN,
  CLI_PARAM_MAC,
  CLI_PARAM_IP,
  CLI_PARAM_LAN_SPEED,
  CLI_PARAM_Last
} CliTestParam_t;

#define CLI_PARAM_INT CLI_PARAM_INT32

/* if CLI_PARAM_STRING_LAST is not used as the last parameter, it is replaced by CLI_PARAM_STRING */
#define PARAM_STRING_LAST_NOT_LAST(param) (param == CLI_PARAM_STRING_LAST) ? CLI_PARAM_STRING : param
        


/**
 * Generic parameter type as supplied by CLI to the command handler function.
 * The command handler must cast the value of this type to the appropriate
 * specific parameter type. Too bad C doesn't provide inheritance.
 */
typedef unsigned long     CliParam_t;

typedef int_fast16_t      (*CliFunc_t)(CliParam_t, CliParam_t, CliParam_t);  /**< Function pointer for general debug command handler. */

typedef struct CliTestEntry_tag {
  const char              *xname;
  const char              *xhelp;
  const CliFunc_t         func;
  const struct CliTestEntry_tag   *table;
  CliTestParam_t          param[CLI_MAX_PARAM];
}                         CliEntry_t;

/**
 * This macro is used to start the declaration of a set of CLI commands.
 * Follow this declaration by a number of CLI_ENTRY declarations and
 * end the list by CLI_END_TABLE.
 * @param name The name of the test table as declared by a corresponding CLI_DECLARE_SUBTEST macro.
 */
#define CLI_START_TABLE(name) \
  CliEntry_t  const name##_Table[] = {

/**
 * This macro ends the declaration of a test command level.
 * @param name Must be the same as used in the preceding CLI_START_TABLE macro.
 */
#define CLI_END_TABLE(name) \
        { "", "", (CliFunc_t)0, 0, { CLI_PARAM_NONE, CLI_PARAM_NONE, CLI_PARAM_NONE } } \
};


/**
 * This macro declares a CLI command which takes no parameters.
 * @param name The name of the test as it must be entered.
 * @param help The descriptive text which will be printed on the help screen.
 * @param func Name of a function which gets invoked when the command is entered.
 */
#define CLI_ENTRY0(name, help, func) \
        { name, help, func, 0, { CLI_PARAM_NONE, CLI_PARAM_NONE, CLI_PARAM_NONE } },

/**
 * This macro declares a CLI command which takes a single argument.
 * @param name The name of the test as it must be entered.
 * @param help The descriptive text which will be printed on the help screen.
 * @param func Name of a function which gets invoked when the command is entered.
 * @param param1 Type definition of the first argument. It must be one of the values defined by CliTestParam_t.
 */
#define CLI_ENTRY1(name, help, func, param1) \
        { name, help, func, 0, { param1, CLI_PARAM_NONE, CLI_PARAM_NONE } },

/**
 * This macro declares a CLI command which takes 2 arguments.
 * @param name The name of the test as it must be entered.
 * @param help The descriptive text which will be printed on the help screen.
 * @param func Name of a function which gets invoked when the command is entered.
 * @param param1 Type definition of the first argument. It must be one of the values defined by CliTestParam_t.
 * @param param1 Type definition of the second argument. It must be one of the values defined by CliTestParam_t.
 */
        
#define CLI_ENTRY2(name, help, func, param1, param2) \
        { name, help, func, 0, { PARAM_STRING_LAST_NOT_LAST(param1), param2, CLI_PARAM_NONE } },

/**
 * This macro declares a CLI command which takes 3 arguments.
 * @param name The name of the test as it must be entered.
 * @param help The descriptive text which will be printed on the help screen.
 * @param func Name of a function which gets invoked when the command is entered.
 * @param param1 Type definition of the first argument. It must be one of the values defined by CliTestParam_t.
 * @param param1 Type definition of the second argument. It must be one of the values defined by CliTestParam_t.
 * @param param1 Type definition of the third argument. It must be one of the values defined by CliTestParam_t.
 */
#define CLI_ENTRY3(name, help, func, param1, param2, param3) \
        { name, help, func, 0, { PARAM_STRING_LAST_NOT_LAST(param1), PARAM_STRING_LAST_NOT_LAST(param2), param3 } },

/**
 * This macro declares a CLI command, which takes the user to a sub-level in the command tree.
 * @param name The name of the sublevel as it must be entered at the prompt.
 * @param help The descriptive text which will be printed on the help screen.
 * @param testtable The name of the test table as declared by a corresponding CLI_DECLARE_SUBTEST macro.
 */
#define CLI_SUBTEST(name, help, table) \
        { name, help, (CliFunc_t)0, table##_Table, { CLI_PARAM_NONE, CLI_PARAM_NONE, CLI_PARAM_NONE } },


/**
 * This macro includes the actual code to generate the command list table.
 * It must be placed in cli_cfg.h
 * @param name The name of the command list table as it will be declared in C.
 */
#define CLI_DECLARE_SUBTEST(name)\
  extern CliEntry_t  const name##_Table[];

typedef void (*CliSend_t)(const char *s);


/************************ INTERFACE FUNCTION PROTOTYPES **********************/

/**
 * Specify the debug output handler. This sets a function which is called every
 * time a line of text is to be printed to the debug output. This function must
 * accept a string pointer and send it to the relevant output medium.
 * @param handler Pointer to output handling function.
 * @return Pointer to previous debug output handler. This could be used to chain
 * different handlers, sending the output to several destinations.
 */
CliSend_t CliOutputDefine(CliSend_t handler);


/**
 * Write a text string to the debug output.
 * @param text Pointer to 0-terminated string.
 */
extern int_fast16_t cliWriteLen;
extern void CliWriteChar(char ch);
extern void CliWrite(const char * text);
extern void CliWriteEol(void);
extern void CliWriteLn(const char * text);
extern void CliWriteLine(void);

/**
 * Write a formatted string to the debug output.
 * @param format The formatting string in usual printf format.
 * @return The number of characters in the formatted output.
 */
int_fast16_t CliPrintf(const char * format, ...);


/**
 * Input handler for debug console input.
 * This function must be called from the relevant debug input source (e.g. serial
 * interrupt handler) whenever a character is received from the user.
 * @param ch The single character.
 * @return 1 if a command was executed otherwise 0.
 */
int_fast16_t CliHandleInput(char ch);


/**
 * Used to signal test is completed
 * If a test function returned CLI_TEST_RESULT_EXECUTING the test must call
 * this routine when the test has completed
 * @param cc Completion code
 */
void CliCompleted(int16_t cc);


/**
 * Initialise the debugging module.
 * This function must be called before any other CLI macro/function.
 */
extern void CliInit(const CliEntry_t * rootTable);

/************************ USED EXTERNAL FUNCTION PROTOTYPES ******************/

extern bool cliShowHeader;
extern void LogToggle(void);                // ^L 
extern void BannerToggle(void);             // ^B
extern void TrcEnable(bool onOff);        // ^T


/*****************************************************************************/
#endif


/** @} */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif

