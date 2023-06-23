FUNCTION-POOL zapp_data_transfer.           "MESSAGE-ID ..

* INCLUDE LZAPP_DATA_TRANSFERD...            " Local class definition


TYPES: BEGIN OF ty_scan_request," 扫码请求对象
         bustyp    TYPE  char10,
         ernam     TYPE  ernam,
         erdat     TYPE  erdat,
         erzet     TYPE  erzet,
         data_json TYPE  string,
       END OF ty_scan_request.

TYPES: BEGIN OF ty_scan_response," 扫码返回对象
         code      TYPE char3,
         msg       TYPE char100,
         data_json TYPE string,
       END OF ty_scan_response.

DATA: gs_req_obj TYPE ty_scan_request.
DATA: gs_res_obj TYPE ty_scan_response.


DATA:g_bustyp TYPE char10.
DATA:g_error TYPE char1.
