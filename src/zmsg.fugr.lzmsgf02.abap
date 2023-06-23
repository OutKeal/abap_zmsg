*&---------------------------------------------------------------------*
*& 包含               LZMSGF02
*&---------------------------------------------------------------------*

DATA:g_cumtom_container_300       TYPE REF TO cl_gui_custom_container,
     g_cumtom_container_300_left  TYPE REF TO cl_gui_container,
     g_cumtom_container_300_right TYPE REF TO cl_gui_container,
     g_splitter_300               TYPE REF TO cl_gui_splitter_container,
     g_splitter_300_left          TYPE REF TO cl_gui_splitter_container.

DATA list_container TYPE REF TO cl_gui_container.
DATA:list_control TYPE REF TO cl_gui_html_viewer.
DATA reply_container TYPE REF TO cl_gui_container.
DATA reply_control TYPE REF TO cl_gui_html_viewer.

MODULE create_object_0300 OUTPUT.

*  CHECK g_cumtom_container_300 IS NOT INITIAL.

  IF g_cumtom_container_300 IS INITIAL.
    CREATE OBJECT g_cumtom_container_300
      EXPORTING
        container_name = 'ITEM_300'.


    CREATE OBJECT g_splitter_300
      EXPORTING
        parent  = g_cumtom_container_300
        rows    = 1
        columns = 2.

    g_cumtom_container_300_left = g_splitter_300->get_container( row = 1 column = 1 ).


    CREATE OBJECT g_splitter_300_left
      EXPORTING
        parent  = g_cumtom_container_300_left
        rows    = 2
        columns = 1.

    list_container = g_splitter_300_left->get_container( row = 1 column = 1 ).
    reply_container = g_splitter_300_left->get_container( row = 2 column = 1 ).


    g_cumtom_container_300_right = g_splitter_300->get_container( row = 1 column = 2 ).
*    g_container_300_right  = g_splitter_300->get_container( row = 1 column = 2 ).

    CALL METHOD g_splitter_300->set_column_width
      EXPORTING
        id    = 1
        width = 80.
    IF g_model = 'R'.
      CALL METHOD g_splitter_300_left->set_row_height
        EXPORTING
          id     = 1
          height = 60.
    ELSEIF g_model = 'C'.
      CALL METHOD g_splitter_300_left->set_row_height
        EXPORTING
          id     = 1
          height = 50.
    ENDIF.


  ENDIF.


ENDMODULE.




MODULE set_list OUTPUT.

  PERFORM frm_set_list.

ENDMODULE.

FORM frm_set_list.
  DATA: ui_flag TYPE i.

  DATA:w_url_list(256) TYPE c.

  ui_flag = cl_gui_html_viewer=>uiflag_no3dborder.
  IF list_control IS  INITIAL.
*    CREATE OBJECT: list_container EXPORTING container_name = 'list'.

    CREATE OBJECT list_control
      EXPORTING
        parent             = list_container
        saphtmlp           = 'X'
        uiflag             = ui_flag
        lifetime           = cl_gui_html_viewer=>lifetime_dynpro
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
  ENDIF.

  CALL METHOD list_control->load_data
    EXPORTING
      type                 = 'text'
      subtype              = 'html'
    IMPORTING
      assigned_url         = w_url_list
    CHANGING
      data_table           = gt_list[]
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
  ENDIF.

  CALL METHOD list_control->show_url
    EXPORTING
      url                    = w_url_list
    EXCEPTIONS
      cntl_error             = 1
      cnht_error_not_allowed = 2
      cnht_error_parameter   = 3
      dp_error_general       = 4
      OTHERS                 = 5.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.

FORM frm_refresh_list.
  SELECT * FROM zmsg_data_r
    WHERE msgno = @gt_datah-msgno
    INTO TABLE @gt_datar[].

  PERFORM frm_html_start.

  LOOP AT gt_datar.

    PERFORM frm_add_list USING gt_datar .

  ENDLOOP.
  PERFORM frm_html_end.
  PERFORM frm_set_list.
ENDFORM.

FORM frm_pop_confirm USING text .
  DATA: l_answer(1) TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'N'
      textline1      = text
      titel          = '确认'
      start_column   = 15
      start_row      = 5
      cancel_display = ''
    IMPORTING
      answer         = l_answer.
  IF l_answer <> 'J'.

    g_error = 'X'.

    MESSAGE s002." '已取消操作'.
  ELSE.
  ENDIF.

ENDFORM.

FORM frm_delete_msg.
  CLEAR g_error.
  PERFORM frm_pop_confirm USING TEXT-028."是否确认删除消息?
  CHECK g_error IS INITIAL.

  UPDATE zmsg_data_i SET status = 'D'
    WHERE msgno = gt_datah-msgno.
  COMMIT WORK AND WAIT.

  PERFORM free_screen_300.
  LEAVE TO SCREEN 0.

ENDFORM.

CLASS cl_myevent_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_sapevent
      FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.

ENDCLASS.

CLASS cl_myevent_handler IMPLEMENTATION.

  METHOD on_sapevent.

    CLEAR edaction.
    CLEAR edframe.
    CLEAR edgetdata.
    CLEAR edpostdataline.

    edaction       = action.
    edframe        = frame.
    edgetdata      = getdata.
    postdata_tab   = postdata.
    IF NOT postdata_tab IS INITIAL.
      READ TABLE postdata_tab INDEX 1 INTO edpostdataline.
    ENDIF.
    edquery_table  = query_table.

    CASE action.
      WHEN 'SUBMIT_FORM_AS_GET_METHOD'.

        PERFORM frm_reply_msg TABLES postdata.

      WHEN 'SHOW_FRAMESET'.
*        PERFORM load_frame_set.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

FORM frm_reply_msg TABLES postdata  .
  gt_datar-msgno = gt_datah-msgno.

  CHECK postdata[] IS NOT INITIAL.


  CLEAR gt_datar-text.

  CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
    EXPORTING
      i_tabline_length = 256
    IMPORTING
      e_string         = gt_datar-text
    TABLES
      it_table         = postdata.

  SPLIT gt_datar-text AT '&files=' INTO gt_datar-text gt_datar-filename.
  REPLACE 'inputHtml=' IN gt_datar-text WITH ''.

  FIND '<p>' IN gt_datar-text.
  IF sy-subrc NE 0.
    gt_datar-text = '&nbsp;&nbsp;&nbsp;&nbsp;' && gt_datar-text && '<br/>'.
  ELSE.

    DO.
      REPLACE '<br></p>' WITH '<br/>' INTO gt_datar-text.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.

    DO.
      REPLACE '</p>' WITH '<br/>' INTO gt_datar-text.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.

    DO.
      REPLACE '<p>' WITH '&nbsp;&nbsp;&nbsp;&nbsp;' INTO gt_datar-text.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.


  IF g_model = 'R'.
    PERFORM frm_reply_update_db CHANGING gt_datar.

    APPEND gt_datar.

    PERFORM frm_add_list USING gt_datar.

    PERFORM frm_set_list.
  ELSEIF g_model = 'C'.
    PERFORM frm_add_list USING gt_datar.
    PERFORM frm_save_msg.
  ENDIF.
  CLEAR gt_datar.
  CLEAR edgetdata.


ENDFORM.

*****------------------------------------------------------------------------------*****
*add by at-yuxs 20211222 创建状态提交消息后自动发送邮件
*****------------------------------------------------------------------------------*****
FORM frm_save_msg.
  IF g_model = 'C'.

    IF gt_datah-object IS INITIAL.
      MESSAGE s009 DISPLAY LIKE 'E'."'请选择对象类型' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF gt_datah-text IS INITIAL.
      MESSAGE s010 DISPLAY LIKE 'E' ."'必须输入主题' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF gt_datah-zzpino IS NOT INITIAL. "add by at-yuxs 20220304 如果销售合同号不为空，需校验
      SELECT SINGLE zzpino INTO @DATA(lv_zzpino) FROM zsdsch WHERE zzpino = @gt_datah-zzpino.
      IF sy-subrc  NE 0.
        MESSAGE s017 DISPLAY LIKE 'E' ."销售订单号不存在.
        RETURN.
      ENDIF.
    ENDIF.

    IF gt_datai[] IS INITIAL.
      MESSAGE s011 DISPLAY LIKE 'E'."'必须选择通知人' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
*  add by at-yuxs 20220302 收件人去重
    SORT gt_datai BY uname.
    DELETE ADJACENT DUPLICATES FROM gt_datai COMPARING uname.
    CLEAR ls_string.
    LOOP AT gt_list.
      IF sy-tabix < 6.
        CONTINUE.
      ENDIF.
      ls_string = ls_string && gt_list-line.
    ENDLOOP.

    IF ls_string IS INITIAL.
      MESSAGE s012 DISPLAY LIKE 'E'."'必须填写正文' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    CLEAR lt_data[].
    LOOP AT gt_datai.
      MOVE-CORRESPONDING gt_datai TO lt_data.
      APPEND lt_data.
    ENDLOOP.

    CALL FUNCTION 'ZMSG_SAVE_DATA'
      EXPORTING
        is_datah  = gt_datah
        iv_string = ls_string
      IMPORTING
        es_return = gs_return
      TABLES
        it_data   = lt_data[]
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
    MESSAGE s013."保存成功

    PERFORM free_screen_300.
    LEAVE TO SCREEN 0.

  ELSEIF g_model = 'R'.
    CLEAR lt_data[].
    CLEAR lt_datai[].

*    LOOP AT gt_datai.
*      MOVE-CORRESPONDING gt_datai TO lt_data.
*      APPEND lt_data.
*    ENDLOOP.
*  add by at-yuxs 20220302 收件人去重
    SORT gt_datai BY uname.
    DELETE ADJACENT DUPLICATES FROM gt_datai COMPARING uname.
    DATA:ls_line_id TYPE zmsgnr.
    SELECT MAX( msgnr ) INTO ls_line_id FROM zmsg_data_i
      WHERE msgno = gt_datah-msgno.
    CLEAR lt_data[].

    CALL FUNCTION 'ZFM_GET_USERNAME'
      EXPORTING
        userid   = sy-uname
      IMPORTING
        username = lv_uname.
    LOOP AT gt_datai ASSIGNING FIELD-SYMBOL(<fs_datai>) WHERE msgno = ''.
      ADD 1 TO ls_line_id.
      <fs_datai>-msgno = gt_datah-msgno.
      <fs_datai>-msgnr = ls_line_id.
      MOVE-CORRESPONDING <fs_datai> TO lt_datai.
      lt_datai-msgno = gt_datah-msgno.
      lt_datai-msgnr = ls_line_id.
      lt_datai-fw_user = sy-uname.
      lt_datai-fw_uname = lv_uname.
      lt_datai-fw_dat = sy-datum.
      APPEND lt_datai.
    ENDLOOP.
    IF lt_datai[] IS NOT INITIAL.
      MODIFY zmsg_data_i FROM TABLE lt_datai.
      COMMIT WORK.
      MESSAGE s014." TYPE 'S'."转发成功
    ENDIF.

  ENDIF.
ENDFORM.

FORM frm_reply_update_db CHANGING gs_datar TYPE zmsg_data_r.

  SELECT MAX( line_id ) FROM zmsg_data_r
  WHERE msgno = @gs_datar-msgno
  INTO @DATA(ls_line_id).

  ADD 1 TO ls_line_id.
  gs_datar-line_id = ls_line_id.
  gs_datar-ernam = sy-uname.
  gs_datar-erdat = sy-datum.
  gs_datar-erzet = sy-uzeit.
  SELECT SINGLE name
    INTO gt_datar-name1
    FROM zapp_addr
    WHERE person = sy-uname.
  MODIFY zmsg_data_r FROM gt_datar.
  UPDATE zmsg_data_h SET aenam = sy-uname
                         aedat = sy-datum
                         aetim = sy-uzeit
                         WHERE msgno = gs_datar-msgno.
  UPDATE zmsg_data_i SET status = ''
                         text = '未读'
                          WHERE msgno = gs_datar-msgno
                          AND uname <> sy-uname.
  COMMIT WORK AND WAIT.

ENDFORM.

FORM convert_utf8 CHANGING data.

  DATA: xstr           TYPE xstring,
        str            TYPE string,
        l_codepage(4)  TYPE n,
        l_encoding(20).
**********字符集名与内码转换
  "将外部字符集名转换为内部编码
  CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
    EXPORTING
      external_name = 'UTF-8'
    IMPORTING
      sap_codepage  = l_codepage.
  WRITE: /  l_codepage.
  "等同于下面类方法
  l_codepage = cl_abap_codepage=>sap_codepage( 'UTF-8' ).
  WRITE: /  l_codepage.

  "编码
  xstr = cl_abap_codepage=>convert_to(
    source   = data
    codepage = `UTF-8` ).
  WRITE: / xstr.
  "解码
  str = cl_abap_codepage=>convert_from(
    source   = xstr
    codepage = `UTF-8` ).
  WRITE: / str.
ENDFORM.


DATA: evt_receiver TYPE REF TO cl_myevent_handler.

MODULE set_reply OUTPUT.

  PERFORM frm_set_reply.

ENDMODULE.



FORM frm_set_reply .
  DATA: ui_flag TYPE i.
  DATA:w_url_reply(256) TYPE c.

*  CREATE OBJECT: reply_container EXPORTING container_name = 'REPLY'.


  ui_flag = cl_gui_html_viewer=>uiflag_no3dborder.

  CREATE OBJECT reply_control
    EXPORTING
      parent               = reply_container
      saphtmlp             = 'X'
*     saphttp              = 'X'
      uiflag               = ui_flag
      query_table_disabled = 'X'
      lifetime             = cl_gui_html_viewer=>lifetime_dynpro
    EXCEPTIONS
      cntl_error           = 1
      cntl_install_error   = 2
      dp_install_error     = 3
      dp_error             = 4
      OTHERS               = 5.

  myevent-eventid = reply_control->m_id_sapevent.
  myevent-appl_event = 'x'.
  APPEND myevent TO myevent_tab.
  CALL METHOD reply_control->set_registered_events
    EXPORTING
      events = myevent_tab.

  CREATE OBJECT evt_receiver.

  SET HANDLER evt_receiver->on_sapevent
              FOR reply_control.

  w_url_reply = 'https://www.antexsoft.com/sap_mail/static/sap2.html'.
*  w_url_reply = 'http://192.168.6.75:1234'.

  CALL METHOD reply_control->show_url
    EXPORTING
      url = w_url_reply.

  CALL METHOD reply_control->do_refresh.

ENDFORM.

FORM free_screen_100.

  IF editor IS BOUND.
    CALL METHOD editor->free.
    FREE editor.
  ENDIF.

  IF msg_container IS BOUND.
    CALL METHOD msg_container->free.
    FREE msg_container.
  ENDIF.

ENDFORM.

FORM free_screen_300.


  IF g_grid IS BOUND.
    CALL METHOD g_grid->free.
    FREE g_grid.
  ENDIF.


  IF list_control IS BOUND.
    CALL METHOD list_control->free.
    FREE list_control.
  ENDIF.

  IF list_container IS BOUND.
    CALL METHOD list_container->free.
    FREE list_container.
  ENDIF.

  IF reply_control IS BOUND.
    CALL METHOD reply_control->free.
    FREE reply_control.
  ENDIF.

  IF reply_container IS BOUND.
    CALL METHOD reply_container->free.
    FREE reply_container.
  ENDIF.

  IF msg_container IS BOUND.
    CALL METHOD msg_container->free.
    FREE msg_container.
  ENDIF.

  IF g_splitter_300_left IS BOUND.
    CALL METHOD g_splitter_300_left->free.
    FREE g_splitter_300_left.
  ENDIF.

  IF g_cumtom_container_300_left IS BOUND.
    CALL METHOD g_cumtom_container_300_left->free.
    FREE g_cumtom_container_300_left.
  ENDIF.

  IF g_cumtom_container_300_right IS BOUND.
    CALL METHOD g_cumtom_container_300_right->free.
    FREE g_cumtom_container_300_right.
  ENDIF.

  IF g_splitter_300 IS BOUND.
    CALL METHOD g_splitter_300->free.
    FREE g_splitter_300.
  ENDIF.


  IF g_cumtom_container_300 IS BOUND.
    CALL METHOD g_cumtom_container_300->free.
    FREE g_cumtom_container_300.
  ENDIF.




ENDFORM.
