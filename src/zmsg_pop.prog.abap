*&---------------------------------------------------------------------*
*& Report ZMSG_POP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmsg_pop.

TABLES:suid_st_bname.


SELECT-OPTIONS:
    s_user FOR suid_st_bname-bname.

PARAMETERS:
  p_datum TYPE sy-datum ,
  p_start TYPE char1 AS CHECKBOX DEFAULT 'X',
  p_mail  TYPE char1 AS CHECKBOX DEFAULT 'X',
  p_ding  TYPE char1 AS CHECKBOX DEFAULT 'X'.


START-OF-SELECTION.

  CALL FUNCTION 'ZMSG_COLLECT_POP'
    EXPORTING
*     DATUM      =
      set_start  = p_start
      send_mail  = p_mail
      send_ding  = p_ding
    TABLES
      range_user = s_user.
