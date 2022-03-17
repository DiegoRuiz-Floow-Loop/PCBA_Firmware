/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stdio.h>

#include "hal/rtc/rtc.h"

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"

#if (PLF_OS==PLF_OS_RTOS)
  #include "plf/rtos/rtos.h"
#endif

#include "net/rest/rest.h"

#include "net/net_connect.h"
#include "net/iot_flash_config.h"


/****************************************************************************/

#define NET_BUF_SIZE  1000

int RestSetRTCTimeDateFromNetwork(void)
{
  int rc = TD_OK;
  int32_t ret = NET_OK;
  int32_t sock;
  int count = 4;
  
  static const char head[] = "HEAD / HTTP/1.1\r\nHost: " HTTP_HOST "\r\n\r\n";

  
  int len = strlen(head);
  char buffer[NET_BUF_SIZE + 1]; /* +1 to be sure that the buffer is closed by a \0, so that it may be parsed by string commands. */
  memset(buffer, 0, sizeof(buffer));

  sockaddr_in addr;
  addr.sin_len = sizeof(sockaddr_in);

  while( (net_if_gethostbyname(NULL,(sockaddr_t *)&addr, HTTP_HOST) < 0) && (count-- > 0) ) {
    TRACE(TRC_TA_NET, TRC_TL_ERROR, "Could not find URL \"" HTTP_HOST "\" - Retrying...");
    #if (PLF_OS==PLF_OS_RTOS)
      RTOS_TASK_DELAY(1000);
    #else
      PlfDelay(1000);
    #endif
  }

  if (count < 0) {
    TRACE(TRC_TA_NET, TRC_TL_ERROR, "Could not find URL \"" HTTP_HOST "\" - Abandon.");
    return TD_ERR_CONNECT;
  }

  TRACE_VA(TRC_TA_NET, TRC_TL_4, "Connecting to \"%s\" at ip address: %s", HTTP_HOST,net_ntoa_r(&addr.sin_addr,buffer,NET_BUF_SIZE));
  memset(buffer, 0, sizeof(buffer));

  addr.sin_port = net_htons(TIME_SOURCE_HTTP_PORT);
  sock = net_socket(NET_AF_INET, NET_SOCK_STREAM, NET_IPPROTO_TCP);

  if (sock < 0) {
    TRACE(TRC_TA_NET, TRC_TL_ERROR, "Could not create the socket.");
  } else {
    uint32_t timeout=5000;

    ret |= net_setsockopt(sock, NET_SOL_SOCKET, NET_SO_RCVTIMEO, (void *) &timeout, sizeof(uint32_t));
  }

  if (ret != NET_OK) {
    TRACE(TRC_TA_NET, TRC_TL_ERROR, "Could not set the socket options.");
  } else {
    count = 4;
    while( ((ret = net_connect( sock, (sockaddr_t *)&addr, sizeof(addr) )) < 0) && (count-- > 0) ) {
      TRACE_VA(TRC_TA_NET, TRC_TL_ERROR, "Could not open the connection");
      if (ret == NET_ERROR_AUTH_FAILURE) {
        TRACE_VA(TRC_TA_NET, TRC_TL_ERROR, "An incorrect system time may have resulted in a TLS authentication error.");
        rc = TD_ERR_TLS_CERT;
        break;
      }
      TRACE_VA(TRC_TA_NET, TRC_TL_ERROR, "Retrying...");
      #if (PLF_OS==PLF_OS_RTOS)
        RTOS_TASK_DELAY(1000);
      #else
        PlfDelay(1000);
      #endif
    }
  }

  if ( (ret != NET_OK) || (rc != TD_OK) ) {
    TRACE(TRC_TA_NET, TRC_TL_ERROR, "Could not open the connection.");
  } else {
     ret = net_send(sock, (uint8_t *)head, len, 0);
    if (ret != len) {
      TRACE_VA(TRC_TA_NET, TRC_TL_ERROR, "Could not send %d bytes.", len);
    } else {
      char *dateStr = NULL;
      int read = 0;
      do {
         len = net_recv(sock, (uint8_t *) buffer + read, NET_BUF_SIZE - read, 0);
         if (len > 0) {
           read += len;
           dateStr = strstr(buffer, "Date: ");
         }
      } while ( (dateStr == NULL) && ((len >= 0) || (len == NET_TIMEOUT)) && (read < NET_BUF_SIZE));

      if (dateStr == NULL) {
        TRACE(TRC_TA_NET, TRC_TL_ERROR, "No 'Date:' line found in the HTTP response header");
        rc = TD_ERR_HTTP;
      } else {
        rc = TD_OK;
        char prefix[8], dow[8], month[4];
        int day, year, hour, min, sec;

        memset(dow, 0, sizeof(dow));
        memset(month, 0, sizeof(month));
        day = year = hour = min = sec = 0;

        int count = sscanf(dateStr, "%s %s %d %s %d %02d:%02d:%02d ", prefix, dow, &day, month, &year, &hour, &min, &sec);
        if (count < 8) {
          TRACE_VA(TRC_TA_NET, TRC_TL_ERROR, "At time initialization, only %d out of the 8 time/date data could be parsed from the HTTP response %s", count, buffer);
          rc = TD_ERR_HTTP;
        } else {
          char * str = strstr(dateStr, "\r\n");
          str[0] = '\0';
          TRACE_VA(TRC_TA_NET, TRC_TL_4, "Configuring the RTC from %s", dateStr);

          RTC_TimeTypeDef sTime;
          sTime.Hours = hour;
          sTime.Minutes = min;
          sTime.Seconds = sec;
          sTime.DayLightSaving = RTC_DAYLIGHTSAVING_NONE;
          sTime.StoreOperation = RTC_STOREOPERATION_RESET;

          RTC_DateTypeDef sDate;
          if (strcmp(dow, "Mon,") == 0) { sDate.WeekDay = RTC_WEEKDAY_MONDAY; } 
          else if (strcmp(dow, "Tue,") == 0) { sDate.WeekDay = RTC_WEEKDAY_TUESDAY; } 
          else if (strcmp(dow, "Wed,") == 0) { sDate.WeekDay = RTC_WEEKDAY_WEDNESDAY; } 
          else if (strcmp(dow, "Thu,") == 0) { sDate.WeekDay = RTC_WEEKDAY_THURSDAY; } 
          else if (strcmp(dow, "Fri,") == 0) { sDate.WeekDay = RTC_WEEKDAY_FRIDAY; } 
          else if (strcmp(dow, "Sat,") == 0) { sDate.WeekDay = RTC_WEEKDAY_SATURDAY; } 
          else if (strcmp(dow, "Sun,") == 0) { sDate.WeekDay = RTC_WEEKDAY_SUNDAY; } 
          else ret = -1;

          if (strcmp(month, "Jan") == 0) { sDate.Month = RTC_MONTH_JANUARY; } 
          else if (strcmp(month, "Feb") == 0) { sDate.Month = RTC_MONTH_FEBRUARY; } 
          else if (strcmp(month, "Mar") == 0) { sDate.Month = RTC_MONTH_MARCH; } 
          else if (strcmp(month, "Apr") == 0) { sDate.Month = RTC_MONTH_APRIL; } 
          else if (strcmp(month, "May") == 0) { sDate.Month = RTC_MONTH_MAY; } 
          else if (strcmp(month, "Jun") == 0) { sDate.Month = RTC_MONTH_JUNE; } 
          else if (strcmp(month, "Jul") == 0) { sDate.Month = RTC_MONTH_JULY; } 
          else if (strcmp(month, "Aug") == 0) { sDate.Month = RTC_MONTH_AUGUST; } 
          else if (strcmp(month, "Sep") == 0) { sDate.Month = RTC_MONTH_SEPTEMBER; } 
          else if (strcmp(month, "Oct") == 0) { sDate.Month = RTC_MONTH_OCTOBER; } 
          else if (strcmp(month, "Nov") == 0) { sDate.Month = RTC_MONTH_NOVEMBER; } 
          else if (strcmp(month, "Dec") == 0) { sDate.Month = RTC_MONTH_DECEMBER; } 
          else ret = -1;

          sDate.Date = day;
          sDate.Year = year - 2000;

          if (!RtcSetVal(&sDate, &sTime)) {
            rc = TD_ERR_RTC;
          }
        }
      }
    }
    ret = net_closesocket(sock);
  }

  /* Translate a socket closure error in network error. */
  if ((rc == TD_OK) && (ret != NET_OK)) {
    rc = TD_ERR_CONNECT;
  }

  return rc;
}

/****************************************************************************/

