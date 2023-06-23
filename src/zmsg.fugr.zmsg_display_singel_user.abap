FUNCTION zmsg_display_singel_user.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(UNAME) TYPE  UNAME OPTIONAL
*"     VALUE(STATUS) TYPE  ZMSG_STATUS OPTIONAL
*"  EXPORTING
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  IF uname IS INITIAL.
    uname = sy-uname.
  ENDIF.

  g_uname = uname.

  g_object_type = 'REPLY'.

  CLEAR gt_sdisplay[].

  SELECT * FROM zmsg_object_con
    INTO TABLE gt_object.

*  PERFORM frm_reset_start_code USING g_uname.

  PERFORM frm_init_date.

  PERFORM frm_get_todo_new USING g_uname. "仅显示未读的

  CALL SCREEN 200 .

ENDFUNCTION.


FORM frm_set_status.
  LOOP AT gt_sdisplay.
*    gt_sdisplay-reply = icon_delivery.
    WHILE 1 = 1.
      REPLACE '//' IN gt_sdisplay-text WITH ';'.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDWHILE.

    PERFORM frm_set_status_single CHANGING gt_sdisplay.
    PERFORM frm_set_btn_single CHANGING gt_sdisplay.

    MODIFY gt_sdisplay.
  ENDLOOP.
ENDFORM.


FORM frm_set_status_single CHANGING cs_sdisplay TYPE ty_zemp.
  CASE cs_sdisplay-object_type.

    WHEN 'SHORT'.
      IF cs_sdisplay-status = ''
        OR  cs_sdisplay-status = 'M'
        OR  cs_sdisplay-status = 'A' .
        cs_sdisplay-icon = icon_msg.
        cs_sdisplay-icontext = TEXT-003."'待阅'.
      ENDIF.

      IF cs_sdisplay-status = 'C'  .
        cs_sdisplay-icon = icon_eml.
        cs_sdisplay-icontext = TEXT-001." '已阅'.
      ENDIF.

    WHEN 'TASK'.
      IF cs_sdisplay-status = ''
          OR  cs_sdisplay-status = 'A'  .
        cs_sdisplay-icon = icon_task.
        cs_sdisplay-icontext = TEXT-002."'待处理'.
      ENDIF.

      IF cs_sdisplay-status = 'B' .
        cs_sdisplay-icon = icon_led_green.
        cs_sdisplay-icontext = TEXT-004."'已查看.
      ENDIF.

      IF cs_sdisplay-status = 'C' .
        cs_sdisplay-icon = icon_complete.
        cs_sdisplay-icontext = TEXT-005."'已关闭'.
      ENDIF.
    WHEN 'REPLY'.
      IF cs_sdisplay-status = ''
          OR  cs_sdisplay-status = 'A'
          OR cs_sdisplay-status = 'M' .
        cs_sdisplay-icon = icon_mail.
        cs_sdisplay-icontext = TEXT-006." '邮件'.
        IF cs_sdisplay-aedati IS NOT INITIAL."原来已读，有新回复的邮件
          cs_sdisplay-clr = 'C110'.
        ENDIF.
      ENDIF.

      IF cs_sdisplay-status = 'C'  .
        cs_sdisplay-icon = icon_status_booked.
        cs_sdisplay-icontext = TEXT-007."'已读邮件'.
      ENDIF.
  ENDCASE.


  IF cs_sdisplay-status = 'D'  .
    cs_sdisplay-icon = icon_delete.
    cs_sdisplay-icontext = TEXT-008." '已删除'.
  ENDIF.
ENDFORM.


FORM frm_set_btn_single CHANGING cs_sdisplay TYPE ty_zemp.
  IF cs_sdisplay-stop = 'X'.
    cs_sdisplay-stop_btn = icon_status_reverse.
  ELSE.
    cs_sdisplay-stop_btn = icon_booking_stop.
  ENDIF.
ENDFORM.


FORM frm_init_date .

  IF g_start_date IS INITIAL.
    g_start_date = '20210101'.
  ENDIF.

  IF g_end_date IS INITIAL.
    g_end_date = sy-datum.
  ENDIF.

ENDFORM.


FORM frm_set_object_range .

  CLEAR r_object_type[].

  IF g_object_type IS INITIAL OR g_object_type = 'ALL'.
    RETURN.
  ELSE.
    r_object_type-sign = 'I'.
    r_object_type-option = 'EQ'.
    r_object_type-low = g_object_type.
    APPEND r_object_type.
    CLEAR r_object_type.
  ENDIF.

ENDFORM.


FORM frm_get_todo USING uname. "全部状态

  RANGES:r_zzpino FOR ztpp0089-zzpino.
  IF g_zzpino IS NOT INITIAL.
    r_zzpino-sign = 'I'.
    r_zzpino-option = 'EQ'.
    r_zzpino-low = g_zzpino.
    APPEND r_zzpino.
  ENDIF.

  IF tt_zzpino IS NOT INITIAL.
    APPEND LINES OF tt_zzpino  TO r_zzpino.
  ENDIF.

  PERFORM frm_set_object_range.

  CLEAR gt_sdisplay[].
  SELECT *
    FROM zmsg_display
    WHERE  ( uname = @uname
            OR ( ernam = @uname AND object_type = 'TASK' ) )
    AND ( status = '' OR status = 'C' OR status = 'D' )
    AND object_type IN @r_object_type
    AND stop = ''
    AND zzpino IN @r_zzpino[]
    INTO CORRESPONDING FIELDS OF TABLE @gt_sdisplay.

  PERFORM frm_set_status.
  SORT gt_sdisplay BY status clr aedati DESCENDING aetimi DESCENDING erdat erzet.
  DELETE ADJACENT DUPLICATES FROM gt_sdisplay COMPARING ALL FIELDS. "add by yuxiaosan 20210830删除重复行，收件人重复就会有重复数据

ENDFORM.


FORM frm_get_todo_new USING uname. "未读状态

  RANGES:r_zzpino FOR ztpp0089-zzpino.
  IF g_zzpino IS NOT INITIAL.
    r_zzpino-sign = 'I'.
    r_zzpino-option = 'EQ'.
    r_zzpino-low = g_zzpino.
    APPEND r_zzpino.
  ENDIF.

  IF tt_zzpino IS NOT INITIAL.
    APPEND LINES OF tt_zzpino  TO r_zzpino.
  ENDIF.

  PERFORM frm_set_object_range.

  CLEAR gt_sdisplay[].

  SELECT * FROM zmsg_display
    WHERE ( uname = @uname
           OR ( ernam = @uname AND object_type = 'TASK' ) )
      AND ( status = '' )
      AND object_type IN @r_object_type
      AND stop = ''
      AND zzpino IN @r_zzpino[]
    INTO CORRESPONDING FIELDS OF TABLE @gt_sdisplay.

  PERFORM frm_set_status.

  SORT gt_sdisplay BY status clr aedati DESCENDING aetimi DESCENDING erdat erzet.
  DELETE ADJACENT DUPLICATES FROM gt_sdisplay COMPARING ALL FIELDS. "add by yuxiaosan 20210830删除重复行，收件人重复就会有重复数据

ENDFORM.


FORM frm_get_do USING uname.""已读状态

  CLEAR gt_sdisplay[].

  SELECT * FROM zmsg_display
    WHERE ( uname = @uname OR ( ernam = @uname AND object_type = 'TASK' ) )
    AND ( ( status = 'C' AND stop = '' AND object_type <> 'TASK' ) OR ( status = 'B' AND object_type = 'TASK' ) )
    AND erdat >= @g_start_date
    AND erdat <= @g_end_date
     AND object_type IN @r_object_type
    INTO CORRESPONDING FIELDS OF TABLE @gt_sdisplay.

  PERFORM frm_set_status.
  SORT gt_sdisplay BY status clr aedati DESCENDING aetimi DESCENDING erdat erzet.
  DELETE ADJACENT DUPLICATES FROM gt_sdisplay COMPARING ALL FIELDS. "add by yuxiaosan 20210830删除重复行，收件人重复就会有重复数据

ENDFORM.


FORM frm_get_my USING uname.


  CLEAR gt_sdisplay[].
  SELECT * FROM zmsg_display
    WHERE ( ernam = @uname  )
    AND erdat >= @g_start_date
    AND erdat <= @g_end_date
    AND object_type IN @r_object_type
    AND status <> 'D'
    INTO CORRESPONDING FIELDS OF TABLE @gt_sdisplay.

  DELETE ADJACENT DUPLICATES FROM gt_sdisplay COMPARING msgno.

  PERFORM frm_set_status.
  SORT gt_sdisplay BY status clr aedati DESCENDING aetimi DESCENDING erdat erzet.
  DELETE ADJACENT DUPLICATES FROM gt_sdisplay COMPARING ALL FIELDS. "add by yuxiaosan 20210830删除重复行，收件人重复就会有重复数据

ENDFORM.


FORM frm_get_dis_todo USING uname.

  CLEAR gt_sdisplay[].

  SELECT * FROM zmsg_display
    WHERE uname = @uname
    AND todo = 'X'
    AND erdat >= @g_start_date
    AND erdat <= @g_end_date
     AND object_type IN @r_object_type
    INTO CORRESPONDING FIELDS OF TABLE @gt_sdisplay.


  PERFORM frm_set_status.
  SORT gt_sdisplay BY status clr aedati DESCENDING aetimi DESCENDING erdat erzet.
  DELETE ADJACENT DUPLICATES FROM gt_sdisplay COMPARING ALL FIELDS. "add by yuxiaosan 20210830删除重复行，收件人重复就会有重复数据

ENDFORM.


FORM frm_get_dis_stop USING uname.

  CLEAR gt_sdisplay[].

  SELECT * FROM zmsg_display
    WHERE uname = @uname
    AND stop = 'X'
    AND erdat >= @g_start_date
    AND erdat <= @g_end_date
     AND object_type IN @r_object_type
    INTO CORRESPONDING FIELDS OF TABLE @gt_sdisplay.

  PERFORM frm_set_status.
  SORT gt_sdisplay BY status clr aedati DESCENDING aetimi DESCENDING erdat erzet.
  DELETE ADJACENT DUPLICATES FROM gt_sdisplay COMPARING ALL FIELDS. "add by yuxiaosan 20210830删除重复行，收件人重复就会有重复数据

ENDFORM.
