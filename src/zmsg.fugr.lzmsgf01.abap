*&---------------------------------------------------------------------*
*& 包含               LZMSGF01
*&---------------------------------------------------------------------*



FORM frm_get_next_msgno USING nr CHANGING  ls_msgno.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'       " 锁定编码对象
    EXPORTING
      object           = 'ZMSGNO'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'            " 获取流水号
    EXPORTING
      nr_range_nr             = nr
      object                  = 'ZMSGNO'
    IMPORTING
      number                  = ls_msgno
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      internal_overflow       = 6
      OTHERS                  = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'       " 释放编码对象锁
    EXPORTING
      object = 'ZMSGNO'.
  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
*    WHEN 'INSR'.                      "insert row
*      PERFORM fcode_insert_row USING    p_tc_name
*                                        p_table_name.
*      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  DELETE <table> INDEX tc1-current_line.

  <tc>-lines = <tc>-lines - 1.


ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

FORM frm_reset_start_code USING uname.

  SELECT SINGLE *
     FROM agr_dateu
    WHERE id = 'START_TCD'
    AND uname = @uname
    AND reports = 'ZMSG'
    INTO @DATA(l_agr).

  IF sy-subrc EQ 0.

    DELETE FROM agr_dateu
      WHERE id = 'START_TCD'
      AND uname = uname
      AND reports = 'ZMSG'.

    COMMIT WORK AND WAIT.

  ENDIF.
ENDFORM.


FORM frm_set_start_tcode USING uname .
  DATA:ls_msg TYPE char40.

  CALL FUNCTION 'NAVIGATION_SET_START_TCODE'
    EXPORTING
      uname       = uname
      start_tcode = 'ZMSG'.
*  ls_msg = '已设置用户的登陆事务-' && uname.
  ls_msg = TEXT-019 && uname.
  WRITE: / ls_msg.

ENDFORM.

FORM frm_send_mail TABLES ct_data STRUCTURE zmsg_scollect USING us_data TYPE zmsg_scollect .
  DATA:ls_msg TYPE char40.
  DATA:i_header TYPE  so_obj_des.
  DATA:it_recipient LIKE TABLE OF  zafo_receivers WITH HEADER LINE.
  DATA: it_text LIKE TABLE OF  line WITH HEADER LINE.

  CLEAR it_recipient.
  CLEAR i_header.
  CLEAR it_text.
  CLEAR it_recipient[].


  SELECT SINGLE name_last2 INTO it_recipient-mail_addr
    FROM zapp_addr WHERE person = us_data-uname.

  IF it_recipient-mail_addr IS INITIAL.
*   ls_msg = '用户' && us_data-uname && '邮件地址获取失败'.
    ls_msg =  us_data-uname && TEXT-018.
    WRITE: / ls_msg.
    RETURN.
  ENDIF.
  APPEND it_recipient.


  i_header = TEXT-017 && sy-datum.

  it_text-line = '您好&nbsp;&nbsp;' && us_data-name1 && '<br/>'.
  APPEND it_text.

  PERFORM frm_add_tab_text TABLES it_text
                                  ct_data
                                  USING 'ZMSG_SCOLLECT_POP'.



  CALL FUNCTION 'ZAFO_BCS_MAIL'
    EXPORTING
      i_sender          = sy-uname
      i_header          = i_header
*     i_replyer         = i_replyer
    TABLES
      it_recipient      = it_recipient[]
      it_text           = it_text
*     IT_ATTCHMENT      =
    EXCEPTIONS
      file_upload_error = 1
      OTHERS            = 2.
  IF sy-subrc EQ 0.
*    ls_msg = '已向用户' && lt_data-name1 && '发送邮件'.
    ls_msg = TEXT-020 && lt_data-name1.
    WRITE: / ls_msg.
  ELSE.
*    ls_msg = '用户' && lt_data-name1 && '发送邮件失败'.
    ls_msg = TEXT-021 && lt_data-name1 .
    WRITE: / ls_msg.
  ENDIF.

ENDFORM.


FORM frm_add_tab_text TABLES ct_text STRUCTURE line
                              ut_item STRUCTURE zmsg_scollect
                              USING strname.
  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any.

  DATA:lt_html TYPE TABLE OF w3html WITH HEADER LINE.
  DATA:lt_fields TYPE TABLE OF w3fields WITH HEADER LINE.
  DATA:lt_row_header TYPE TABLE OF w3head WITH HEADER LINE.

  DATA: lt_text TYPE TABLE OF line WITH HEADER LINE.
*创建动态表结构
  CREATE DATA dyn_table TYPE TABLE OF (strname).
*创建动态内表
  ASSIGN dyn_table->* TO <dyn_table>.
*创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
*创建动态工作区
  ASSIGN dyn_wa->* TO <dyn_wa>.


  LOOP AT ut_item.
    MOVE-CORRESPONDING ut_item TO <dyn_wa>.
    APPEND <dyn_wa> TO <dyn_table>.
    CLEAR <dyn_wa>.
  ENDLOOP.



  CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS' "根据结构得到列抬头
    EXPORTING
      table_name = strname
    TABLES
      header     = lt_row_header.

  DATA:table_attributes LIKE  w3html.


  LOOP AT lt_row_header.
    CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT' "F每一列的格式
      EXPORTING
        field_nr  = sy-tabix
*       line_nr   = 3
        justified = 'center'
*       ICON      =
*       SYMBOL    =
*       size      = '8'
*       LINK      =
*       fgcolor   = 'BULE'
*       bgcolor   = 'GREEN'
*       FONT      =
      TABLES
        fields    = lt_fields.
  ENDLOOP.



  table_attributes = '" border="1" cellspacing="0" cellpadding="0"  align="left"'. "bgcolor="#cccccc" width="100%

  CALL FUNCTION 'WWW_ITAB_TO_HTML'
    EXPORTING
      table_attributes = table_attributes
    TABLES
      html             = lt_text
      fields           = lt_fields
      row_header       = lt_row_header
      itable           = <dyn_table>.

  lt_text-line = '<br/>'.
  APPEND lt_text TO ct_text.
  lt_text-line = TEXT-022 && '<br/>'.
*  '待办清单汇总'
  APPEND lt_text TO ct_text.

  LOOP AT lt_text.
    APPEND lt_text TO ct_text.
  ENDLOOP.



ENDFORM.


FORM frm_send_ding TABLES ct_data STRUCTURE zmsg_scollect
                    USING us_data TYPE zmsg_scollect .


  DATA:ls_ding_string TYPE string.
  DATA:lt_dinguser TYPE TABLE OF zhrs_dinguser WITH HEADER LINE .
  DATA:return TYPE bapireturn1.

  CLEAR ls_ding_string.
  CLEAR lt_dinguser[].
  CLEAR return.

  ls_ding_string = TEXT-017 && sy-datum && ':'."SAP系统待处理消息提醒-

  LOOP AT ct_data.
    ls_ding_string = ls_ding_string && '|' && ct_data-object_name && '| ' && ct_data-count && '.'.
  ENDLOOP.

  lt_dinguser-pernr = us_data-uname.
  APPEND lt_dinguser.

  CALL FUNCTION 'ZHRI_ASYNCSEND_TO_DING'
    EXPORTING
*     in_dctyp    = '0'
*     IN_TOKEN    =
*     IN_AGENTID  =
      in_message  = ls_ding_string
    IMPORTING
      return      = return
    TABLES
      it_dinguser = lt_dinguser.

  WRITE: /  us_data-name1  .
  WRITE: TEXT-012." '钉钉发送:'  .
  WRITE: return-message  .

ENDFORM.

FORM frm_get_datai USING datum us_data TYPE zmsg_scollect.

  DATA:lt_datai LIKE TABLE OF zmsg_data_i WITH HEADER LINE  .
  SELECT h~object,i~*
    FROM zmsg_data_h AS h
    INNER JOIN zmsg_data_i AS i
    ON h~msgno = i~msgno
    WHERE uname = @us_data-uname
    AND status = ''
    AND erdat >= @datum
    APPENDING CORRESPONDING FIELDS OF TABLE  @gt_datai_add.

ENDFORM.

FORM frm_update_itme.

  CHECK gt_datai_add[] IS NOT INITIAL.

  SELECT * FROM zmsg_object_con
    INTO TABLE gt_object.


  SORT gt_datai_add BY uname.
  DELETE ADJACENT DUPLICATES FROM gt_datai_add COMPARING uname.
  LOOP AT gt_datai_add.
    ADD 1 TO gt_datai_add-times.
    READ TABLE gt_object WITH KEY object = gt_datai_add-object.
    CHECK sy-subrc EQ 0.

    IF gt_datai_add-times >= gt_object-max_times.
      gt_datai_add-status = 'D'.
    ENDIF.

    MOVE-CORRESPONDING gt_datai_add TO gt_datai_modify.
    APPEND gt_datai_modify.
    CLEAR gt_datai_modify.
  ENDLOOP.

  IF gt_datai_modify[] IS NOT INITIAL.
    MODIFY zmsg_data_i FROM TABLE gt_datai_modify[].
  ENDIF.

ENDFORM.


FORM frm_send_mail_inter TABLES ct_data STRUCTURE zmsg_scollect USING us_data TYPE zmsg_scollect .
  DATA:ls_msg TYPE char40.
  DATA:i_header TYPE  so_obj_des.
  DATA:it_recipient LIKE TABLE OF  zafo_receivers WITH HEADER LINE.
  DATA: it_text LIKE TABLE OF  line WITH HEADER LINE.

  CLEAR it_recipient.
  CLEAR i_header.
  CLEAR it_text.
  CLEAR it_recipient[].



  i_header = TEXT-017 && sy-datum."SAP系统待处理消息提醒-

  it_text-line = TEXT-025 && '&nbsp;&nbsp;' && us_data-name1  .
  APPEND it_text.

  it_text-line = TEXT-024."以下为待处理的清单个数,请使用事务ZMSG处理。'  .
  APPEND it_text.

  LOOP AT ct_data.
    it_text-line = ct_data-object_name && '&nbsp;&nbsp;&nbsp;' && ct_data-count.
    APPEND it_text.
  ENDLOOP.

  CALL FUNCTION 'ZAFO_BCS_MAIL_INTER'
    EXPORTING
      uname      = us_data-uname
      title      = i_header
    TABLES
      email_text = it_text.


  IF sy-subrc EQ 0.
*    ls_msg = '已向用户' && lt_data-name1 && '发送内部邮件'.
    ls_msg = TEXT-026 && lt_data-name1 .
    WRITE: / ls_msg.
  ELSE.
*    ls_msg = '用户' && lt_data-name1 && '发送内部邮件失败'.
    ls_msg = TEXT-027 && lt_data-name1 .
    WRITE: / ls_msg.
  ENDIF.

ENDFORM.
