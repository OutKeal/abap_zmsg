*&---------------------------------------------------------------------*
*& Report ZMSG_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmsg_create.


CALL FUNCTION 'ZMSG_CREATE'
  EXPORTING
    object = 'MSG'
*   OBJECT_ID       =
*   TEXT   =
*   URGENT =
* IMPORTING
*   ES_RETURN       =
* TABLES
*   IT_DATA         =
* EXCEPTIONS
*   ERROR  = 1
*   OTHERS = 2
  .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
