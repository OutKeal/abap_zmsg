FUNCTION zmsg_display_singel_user_sc.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(UNAME) TYPE  UNAME OPTIONAL
*"     VALUE(STATUS) TYPE  ZMSG_STATUS OPTIONAL
*"     VALUE(ZZPINO) TYPE  ZZPINO OPTIONAL
*"     VALUE(IT_ZZPINO) TYPE  ZTTPPZZPINO OPTIONAL
*"  EXPORTING
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  IF uname IS INITIAL.
    uname = sy-uname.
  ENDIF.

  g_uname = uname.
  g_zzpino = zzpino.
  tt_zzpino = it_zzpino.

  g_object_type = 'REPLY'.

  CLEAR gt_sdisplay[].

  SELECT * FROM zmsg_object_con
    INTO TABLE gt_object.

*  PERFORM frm_reset_start_code USING g_uname.

  PERFORM frm_init_date.

  PERFORM frm_get_todo USING g_uname.

  CALL SCREEN 200 .

ENDFUNCTION.
