module esdl.intf.mraa;

enum MRAA_SUCCESS = 0;

enum mraa_gpio_dir_t  {
    MRAA_GPIO_OUT = 0,      /**< Output. A Mode can also be set */
    MRAA_GPIO_IN = 1,       /**< Input */
    MRAA_GPIO_OUT_HIGH = 2, /**< Output. Init High */
    MRAA_GPIO_OUT_LOW = 3   /**< Output. Init Low */
}

enum mraa_result_t {
  MRAA_SUCCESS = 0,                             /**< Expected response */
  MRAA_ERROR_FEATURE_NOT_IMPLEMENTED = 1,       /**< Feature TODO */
  MRAA_ERROR_FEATURE_NOT_SUPPORTED = 2,         /**< Feature not supported by HW */
  MRAA_ERROR_INVALID_VERBOSITY_LEVEL = 3,       /**< Verbosity level wrong */
  MRAA_ERROR_INVALID_PARAMETER = 4,             /**< Parameter invalid */
  MRAA_ERROR_INVALID_HANDLE = 5,                /**< Handle invalid */
  MRAA_ERROR_NO_RESOURCES = 6,                  /**< No resource of that type avail */
  MRAA_ERROR_INVALID_RESOURCE = 7,              /**< Resource invalid */
  MRAA_ERROR_INVALID_QUEUE_TYPE = 8,            /**< Queue type incorrect */
  MRAA_ERROR_NO_DATA_AVAILABLE = 9,             /**< No data available */
  MRAA_ERROR_INVALID_PLATFORM = 10,             /**< Platform not recognised */
  MRAA_ERROR_PLATFORM_NOT_INITIALISED = 11,     /**< Board information not initialised */
  MRAA_ERROR_PLATFORM_ALREADY_INITIALISED = 12, /**< Board is already initialised */

  MRAA_ERROR_UNSPECIFIED = 99 /**< Unknown Error */
}

struct struct_gpio;

alias mraa_gpio_context = struct_gpio*;

extern(C) mraa_gpio_context mraa_gpio_init(int pin);
extern(C) void mraa_result_print (mraa_result_t result);
extern(C) mraa_result_t mraa_gpio_close (mraa_gpio_context dev);
extern(C) int mraa_gpio_read(mraa_gpio_context dev);
extern(C) mraa_result_t mraa_gpio_write (mraa_gpio_context dev, int value);

extern(C) mraa_result_t mraa_gpio_dir(mraa_gpio_context	dev,
	  			      mraa_gpio_dir_t dir);

extern(C) void mraa_init();

