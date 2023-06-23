class ZMSG_CL_GOS_SERVICE definition
  public
  inheriting from CL_GOS_SERVICE
  final
  create public .

public section.

  methods EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZMSG_CL_GOS_SERVICE IMPLEMENTATION.


  METHOD execute.
*    BREAK-POINT.
    DATA:object_id TYPE zmsg_object_id.
    DATA:object TYPE zmsg_object.
    object_id = gs_lporb-instid.
    object = gs_lporb-typeid.

    CALL FUNCTION 'ZMSG_CREATE'
      EXPORTING
        object    = object
        object_id = object_id
*       TEXT      =
*       URGENT    =
*     IMPORTING
*       ES_RETURN =
*      TABLES
*       it_data   =
*     EXCEPTIONS
*       ERROR     = 1
*       OTHERS    = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
ENDCLASS.
