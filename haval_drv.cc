/**
 * HAVAL (cryptographic hash function) bindings for Erlang
 *
 * @author  Rakuto Furutani <xri://=rakuto>
 * @date    2008/06/28
 */
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <new>
#include "ei.h"
#include "erl_driver.h"
#include "erl_interface.h"
extern "C" {
// Define constant for haval.h
#define PASS 3
#define NUMBER_OF_BLOCKS 5000
#define FPTLEN 256
#ifndef LITTLE_ENDIAN
#define LITTLE_ENDIAN 1
#endif

#include "haval.h"
}

// Functions are provided by this driver
#define DRV_INFO          1
#define DRV_HAVAL_STRING  2 
#define DRV_HAVAL_FILE    3 

/* Driver Interface Declarations */
static ErlDrvData start_haval_driver(ErlDrvPort port, char *command);
static void stop_haval_driver(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
                    int len, char **rbuf, int rlen);
static void haval_to_hex(unsigned char*, char*);
static ErlDrvBinary* ei_x_to_new_binary(const ei_x_buff*);

/* Driver Entry */
static ErlDrvEntry haval_driver_entry = {
  NULL,
  start_haval_driver,
  stop_haval_driver,
  NULL,
  NULL,
  NULL,
  "haval_drv",
  NULL,
  NULL,
  control,
  NULL,
  NULL
};

typedef struct _drv_data {
  ErlDrvPort port;
} drv_data_t;

/* DRIVER INTERFACE */

static ErlDrvData start_haval_driver(ErlDrvPort port, char *command)
{
  drv_data_t *data;

  data = (drv_data_t*) driver_alloc(sizeof(drv_data_t));
  data->port = port;
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) data;
}

static void stop_haval_driver(ErlDrvData drv_data)
{
  driver_free((char*) drv_data);
}

static int control(ErlDrvData drv_data, unsigned int command, char *buf, int len, char **rbuf, int rlen)
{
  int ret = -1;
  char hex[(FPTLEN >> 3) << 1];
  unsigned char fingerprint[FPTLEN >> 3];
  char *arg1;
  ei_x_buff x_buff;

  try  {
    // argument
    arg1 = new char[len + 1];
    strncpy(arg1, buf, len);
    strcat(arg1, "\0");

    ei_x_new_with_version(&x_buff);
    switch(command) {
    case DRV_INFO:
      ei_x_encode_string(&x_buff, "info");
      ret = sizeof("info") * sizeof(char);
      break;
    case DRV_HAVAL_STRING:
      haval_string(arg1, fingerprint);
      haval_to_hex(&fingerprint[0], &hex[0]);
      ret = sizeof(hex);
      ei_x_encode_string(&x_buff, hex);
      break;
    case DRV_HAVAL_FILE:
      if(!haval_file(arg1, fingerprint)) {
        haval_to_hex(&fingerprint[0], &hex[0]);
        ret = sizeof(hex);
        ei_x_encode_string(&x_buff, hex);
      } else {
        erl_err_sys("haval_file");
      }
      break;
    }
    if(ret > 0) *rbuf = reinterpret_cast<char*>(ei_x_to_new_binary(&x_buff));
    ei_x_free(&x_buff);
  } catch(std::bad_alloc) {
    erl_err_sys("can not allocate memory");
  }

  return ret;
}

static void haval_to_hex(unsigned char *fingerprint, char *hex)
{
  for(int i=0; i < FPTLEN >> 3; ++i) {
    sprintf(&hex[i << 1], "%02X", fingerprint[i]);
  }
}

// Init the driver
extern "C" DRIVER_INIT(haval_drv)
{
  return &haval_driver_entry;
}

// Utilities
static ErlDrvBinary* ei_x_to_new_binary(const ei_x_buff *x_buff)
{
  ErlDrvBinary *bin = driver_alloc_binary(x_buff->index);
  if(bin != NULL) {
    memcpy(bin->orig_bytes, x_buff->buff, x_buff->index);
  }
  return bin;
}
