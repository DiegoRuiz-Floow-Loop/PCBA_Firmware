/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stdio.h>

#include "hal/pflash/pflash.h"
#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"
#include "plf/heap/heapx.h"
#include "hal/dflash/dflash.h"

#if (PLF_OS==PLF_OS_RTOS)
  #include "plf/rtos/rtos.h"
#endif

#include "net/rest/rest.h"

/****************************************************************************/

static const char _h1[] =
  "POST /upload HTTP/1.1\r\n"
  "Host: " HTTP_HOST ":" _STRIZE(GET_POST_HTTP_PORT) "\r\n"
  "User-Agent: TekPartner/1.0.0\r\n"
  "Accept: */*\r\n"
  "Content-Length: %u\r\n"
  "Content-Type: multipart/form-data; boundary=------------------------%016llx\r\n"
  "Expect: 100-continue\r\n"
  "\r\n";

static const char _h2[] =
  "--------------------------%016llx\r\n"
  "Content-Disposition: form-data; name=\"key\"\r\n"
  "\r\n"
  "TekPartner\r\n"
  "--------------------------%016llx\r\n"
  "Content-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\n"
  "Content-Type: application/octet-stream\r\n"
  "\r\n";

static const char _f1[] =
  "\r\n"
  "--------------------------%016llx--\r\n"
  "";

//static const char _h1[] = 
//  "POST /upload HTTP/1.1\r\n"
//  "Host: " HTTP_HOST ":" _STRIZE(GET_POST_HTTP_PORT) "\r\n"
//  "User-Agent: TekPartner/1.0.0\r\n"
//  "Accept: */*\r\n"
//  "Content-Length: %u\r\n"
//  "Content-Type: multipart/form-data; boundary=------------------------948c781cff37d940\r\n"
//  "Expect: 100-continue\r\n"
//  "\r\n";

//static const char _h2[] = 
//  "--------------------------948c781cff37d940\r\n"
//  "Content-Disposition: form-data; name=\"key\"\r\n"
//  "\r\n"
//  "TekPartner\r\n"
//  "--------------------------948c781cff37d940\r\n"
//  "Content-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\n"
//  "Content-Type: application/octet-stream\r\n"
//  "\r\n";

//static const char f[] = 
//  "\r\n"
//  "--------------------------948c781cff37d940--\r\n"
//  "";


#define CBUFFER_SIZE  (1024)
#define HEADER_SIZE  (1024)


int RestPost(const char * fileName, uint32_t fileAddr, int32_t fileSize)
{
  int32_t       ret = NET_OK;
  int32_t       sock;
  int           count = 4;
  int           len;
  int           cix;
  char          * cbuf;
  char          * h1, * h2, * f1;
  
  uint64_t u64 =  0x1000000000000LLU * (uint64_t)rand() + 
                  0x100000000LLU * (uint64_t)rand() +
                  0x10000LLU * (uint64_t)rand() +
                  (uint64_t)rand();
  

  TRACE_VA(TRC_TA_CLD, TRC_TL_3, "RestPost(%s, 0x%08X, 0x%X)", fileName, fileAddr, fileSize);

  HeapxClean();
  cbuf = (char *)HeapxMalloc(CBUFFER_SIZE);
  if (cbuf == NULL) 
    return R_ERR_HEAP;
  h1 = (char *)HeapxMalloc(HEADER_SIZE);
  if (h1 == NULL) {
    HeapxFree(cbuf);
    return R_ERR_HEAP;
  }
  h2 = (char *)HeapxMalloc(HEADER_SIZE);
  if (h2 == NULL) {
    HeapxFree(cbuf);
    HeapxFree(h1);
    return R_ERR_HEAP;
  }
  f1 = (char *)HeapxMalloc(HEADER_SIZE);
  if (f1 == NULL) {
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    return R_ERR_HEAP;
  }

  sprintf(f1, _f1, u64);
  sprintf(h2, _h2, u64, u64, fileName);
  sprintf(h1, _h1, (fileSize + strlen(h2) + strlen(f1)), u64);
  
  sockaddr_in addr;
  addr.sin_len = sizeof(sockaddr_in);

  while( (net_if_gethostbyname(NULL,(sockaddr_t *)&addr, HTTP_HOST) < 0) && (count-- > 0) ) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Could not find URL \" HTTP_HOST \" - Retrying");
    #if (PLF_OS==PLF_OS_RTOS)
      RTOS_TASK_DELAY(1000);
    #else
      PlfDelay(1000);
    #endif
  }
  if (count < 0) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Could not find URL \"" HTTP_HOST "\" - Abandon");
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    return R_ERR_DNS;
  }
  TRACE_VA(TRC_TA_CLD, TRC_TL_4, 
    "Connecting to \"" HTTP_HOST "\" at ip address: %s", 
    net_ntoa_r(&addr.sin_addr, (char *)cbuf, CBUFFER_SIZE-1));

  addr.sin_port = net_htons(GET_POST_HTTP_PORT);
  sock = net_socket(NET_AF_INET, NET_SOCK_STREAM, NET_IPPROTO_TCP);
  if (sock < 0) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Could not create the socket.");
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    return R_ERR_SOCKET;
  } 

  uint32_t timeout = 0;
  ret = net_setsockopt(sock, NET_SOL_SOCKET, NET_SO_SNDTIMEO, (void *) &timeout, sizeof(uint32_t));
  if (ret != NET_OK) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Could not set the socket options.");
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    net_closesocket(sock);
    return R_ERR_SOCKET;
  } 
  timeout = 10000;
  ret = net_setsockopt(sock, NET_SOL_SOCKET, NET_SO_RCVTIMEO, (void *) &timeout, sizeof(uint32_t));
  if (ret != NET_OK) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Could not set the socket options.");
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    net_closesocket(sock);
    return R_ERR_SOCKET;
  } 

  count = 4;
  while( ((ret = net_connect( sock, (sockaddr_t *)&addr, sizeof(addr) )) < 0) && (count-- > 0) ) {
    TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "Could not open the connection");
    if (ret == NET_ERROR_AUTH_FAILURE) {
      TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "An incorrect system time may have resulted in a TLS authentication error.");
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    net_closesocket(sock);
      return R_ERR_CONNECT;
    }
    TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "Retrying...");
    #if (PLF_OS==PLF_OS_RTOS)
      RTOS_TASK_DELAY(1000);
    #else
      PlfDelay(1000);
    #endif
  }
  if (count < 0) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Could not open the connection.");
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    net_closesocket(sock);
    return R_ERR_CONNECT;
  } 

  // SEND HEADER 1
  TRACE_BLK(TRC_TA_CLD, "Header 1:", (uint8_t *)h1, strlen(h1));
  len = strlen(h1);
  ret = net_send(sock, (uint8_t*)h1, len, 0);
  if (ret != len) {
    TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "Could not send %d bytes", len);
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    net_closesocket(sock);
    return R_ERR_SEND;
  } 
  
  // READ HEADER RESPONS
  cix = 0;  
  int d=0;
  for (;;) {
     len = net_recv(sock, (uint8_t *)cbuf, CBUFFER_SIZE, 0);
     if (len != NET_TIMEOUT) {
       if (len <= 0) break; 
       uint8_t * b = (uint8_t *)cbuf;
       do {
          if (*b == '\n') {
            if (cix > 0) {
              cbuf[cix] = 0;
              TRACE_VA(TRC_TA_CLD, TRC_TL_4, ">> %s", (char *)cbuf);
              cix = 0;
              if (sscanf((char *)cbuf, "HTTP/1.1 %d ", &d) == 1) {
                if (d != 100) {
                  HeapxFree(cbuf);
                  HeapxFree(h1);
                  HeapxFree(h2);
                  net_closesocket(sock);
                  return R_ERR_RECV;
                } else {
                  break;
                }
              }
            } else {
              break;
            }
          } else if (*b == '\r') {
            // ignore
          } else {
            cbuf[cix++] = *b;
            if (cix >= CBUFFER_SIZE)
              cix--;
          }
          b++;
        } while (len-- > 0);
     }
     if (d==100) break;
  } 
  
  // SEND HEADER 2
  TRACE_BLK(TRC_TA_CLD, "Header 2:", (uint8_t *)h2, strlen(h2));
  len = strlen(h2);
  ret = net_send(sock, (uint8_t*)h2, len, 0);
  if (ret != len) {
    TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "Could not send %d bytes", len);
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    net_closesocket(sock);
    return R_ERR_SEND;
  } 
 
  // SEND FILE CONTENTS
  int res = R_OK;
  int totalData = 0;
  for (;;) {
    if (fileSize <= 0) break;
    len = MIN_VAL(fileSize, CBUFFER_SIZE);
    if (!HalPFlashRead(fileAddr, (uint32_t)cbuf, len)) {
      TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "Could not Read PFlash addr: 0x%08X", fileAddr);
      res = R_ERR_FLASH_READ;
      break;
    }
    TRACE_BLK(TRC_TA_CLD, "Data:", (uint8_t *)cbuf, len);
    int r = net_send(sock, (uint8_t *)cbuf, len, 0);
    if (r < 0) {
      res = R_ERR_SEND_DATA;
      break;
    }
    fileAddr += r;
    fileSize -= r;
    totalData += r;
  } 

  // SEND FOTTER
  TRACE_BLK(TRC_TA_CLD, "Fotter:", (uint8_t *)f1, strlen(f1));
  len = strlen(f1);
  ret = net_send(sock, (uint8_t*)f1, len, 0);
  if (ret != len) {
    TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "Could not send %d bytes", len);
    HeapxFree(cbuf);
    HeapxFree(h1);
    HeapxFree(h2);
    net_closesocket(sock);
    return R_ERR_SEND;
  } 

  // READ DATA RESPONS
  cix = 0;  
  for (;;) {
    len = net_recv(sock, (uint8_t *)cbuf, CBUFFER_SIZE, 0);
    if (len != NET_TIMEOUT) {
      if (len <= 0) break; 
      uint8_t * b = (uint8_t *)cbuf;
      do {
        if (*b == '\n') {
          if (cix > 0) {
            cbuf[cix] = 0;
            TRACE_VA(TRC_TA_CLD, TRC_TL_4, ">> %s", (char *)cbuf);
            cix = 0;
          } else {
            goto done;
          }
        } else if (*b == '\r') {
          // ignore
        } else {
          cbuf[cix++] = *b;
          if (cix >= CBUFFER_SIZE)
            cix--;
        }
        b++;
      } while (len-- > 0);
    }
  } 
  done:

  HeapxFree(cbuf);
  HeapxFree(h1);
  HeapxFree(h2);
  net_closesocket(sock);
  TRACE_VA(TRC_TA_CLD, TRC_TL_3, "Total Data: %d/0x%X bytes\n", totalData, totalData);
  return res;
}

/****************************************************************************/

#ifdef CLI_ENABLE 

#endif

/****************************************************************************/
