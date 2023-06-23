*&---------------------------------------------------------------------*
*& Include LZMSGO01
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc1_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_datai LINES tc1-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc1_get_lines OUTPUT.
  g_tc1_lines = sy-loopc.
ENDMODULE.

*&---------------------------------------------------------------------*
*& 包含               LZMSGPBO
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA:lt_okcode TYPE TABLE OF sy-ucomm .
  DATA:ls_title TYPE char40.

  IF readonly = 'X'.

    APPEND 'SAVE' TO lt_okcode.

*    ls_title = '显示消息' && gt_datah-msgno && '-' && gt_datah-ernam_name.
    ls_title = TEXT-034 && gt_datah-msgno && '-' && gt_datah-ernam_name.

  ELSE.

*    ls_title = '创建新消息'.
    ls_title = TEXT-035.

  ENDIF.

  SET TITLEBAR 'T100' WITH  ls_title.

  SET PF-STATUS 'S100' EXCLUDING lt_okcode.

ENDMODULE.


MODULE status_0300 OUTPUT.

  CLEAR lt_okcode[].
  IF g_model = 'R'.

*    APPEND 'SAVE' TO lt_okcode.

*    ls_title = '回复消息' && gt_datah-msgno && '-' && gt_datah-ernam_name.
    ls_title = TEXT-036 && gt_datah-msgno && '-' && gt_datah-ernam_name.

  ELSE.

*    ls_title = '创建新主题'.
    ls_title = TEXT-037.

  ENDIF.

  SET TITLEBAR 'T100' WITH  ls_title.

  IF g_model = 'C'.
    APPEND 'REFRESH' TO lt_okcode.
    APPEND 'TO_DO' TO lt_okcode.
    APPEND 'UNTO_DO' TO lt_okcode.
    APPEND 'STOP' TO lt_okcode.
    APPEND 'UNSTOP' TO lt_okcode.
  ELSEIF g_model = 'R'.
*    APPEND 'SAVE' TO lt_okcode.
    APPEND 'CLEAR' TO lt_okcode.

    IF gt_datai-stop = 'X'.
      APPEND 'STOP' TO lt_okcode.
    ELSE.
      APPEND 'UNSTOP' TO lt_okcode.
    ENDIF.

    IF gt_datai-todo = 'X'.
      APPEND 'TO_DO' TO lt_okcode.
    ELSE.
      APPEND 'UNTO_DO' TO lt_okcode.
    ENDIF.

  ENDIF.

  IF sy-uname <> gt_datah-ernam.
    APPEND 'DELETE' TO lt_okcode.
  ELSE.
    APPEND 'STOP' TO lt_okcode.
    APPEND 'UNSTOP' TO lt_okcode.
  ENDIF.





  SET PF-STATUS 'S300' EXCLUDING lt_okcode.

ENDMODULE.



MODULE set_screen_300 OUTPUT.
  IF g_model = 'R'.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.
MODULE status_0400 OUTPUT.




*  ls_title = '选择用户'.
  ls_title = TEXT-038.


  SET TITLEBAR 'T100' WITH  ls_title.


  SET PF-STATUS 'S400' EXCLUDING lt_okcode.

ENDMODULE.

MODULE tc_get_lines OUTPUT.
  g_tc_lines = sy-loopc.
ENDMODULE.                    "TC001_GET_LINES OUTPUT


MODULE set_screen OUTPUT.


  CHECK readonly = 'X'.

  DATA:lv_line(256) TYPE c.

  LOOP AT SCREEN .
    screen-input = 0.
    MODIFY SCREEN.
  ENDLOOP.

  LOOP AT tc1-cols INTO DATA(ls_col).

    ls_col-screen-input = 0.

    MODIFY tc1-cols FROM ls_col.

  ENDLOOP.



  CALL METHOD editor->set_readonly_mode
    EXPORTING
      readonly_mode = 1.


ENDMODULE. " STATUS_0100 OUTPUT

MODULE status_0200 OUTPUT.

*  ls_title = '个人消息'.
  ls_title = TEXT-039.

  SET PF-STATUS 'S200'.
  SET TITLEBAR 'T200' WITH ls_title.



ENDMODULE.

MODULE g_object_type INPUT.
  DATA:BEGIN OF lt_type OCCURS 0 ,
         name        TYPE name1,
         object_type TYPE zmsg_object_type,
       END OF lt_type.




  CHECK lt_type[] IS  INITIAL.


  lt_type-object_type = 'ALL'.
*  lt_type-name = '所有消息'.
  lt_type-name = TEXT-040.
  APPEND lt_type.

  lt_type-object_type = 'REPLY'.
*  lt_type-name = '会话消息'.
  lt_type-name = TEXT-041.
  APPEND lt_type.

  lt_type-object_type = 'SHORT'.
*  lt_type-name = '短消息'.
  lt_type-name = TEXT-042.
  APPEND lt_type.

  lt_type-object_type = 'TASK'.
*  lt_type-name = '任务消息'.
  lt_type-name = TEXT-043.
  APPEND lt_type.



  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'G_OBJECT_TYPE'
    TABLES
      value_tab = lt_type.

ENDMODULE.


MODULE init_timer OUTPUT.
  PERFORM init_timer.
ENDMODULE.

MODULE create_gos_service OUTPUT.

  DATA:obj TYPE borident.
  DATA:manager TYPE REF TO cl_gos_manager.
  DATA:it_gos_sels TYPE tgos_sels.
  DATA:is_gos_sels TYPE sgos_sels.

  CHECK gt_datah-msgno IS NOT INITIAL .
  DEFINE append_gos_sels  .
    is_gos_sels-sign = 'I'.
    is_gos_sels-option = 'EQ'..
    is_gos_sels-low = &1.
    APPEND is_gos_sels TO it_gos_sels.
    CLEAR is_gos_sels.
  END-OF-DEFINITION.

  append_gos_sels 'VIEW_ATTA'.     "附件清单
  append_gos_sels 'ZMSG'.          "创建通知消息
  append_gos_sels 'CREATE_ATTA'.   "创建...
  append_gos_sels 'DISP_ATTA'.     "附件清单
  append_gos_sels 'ARL_LINK'.      "存储业务凭证

  obj-objtype = 'ZMSG'.
  obj-objkey = gt_datah-msgno .

  IF manager IS INITIAL .

    CREATE OBJECT manager
      EXPORTING
*       IP_START_DIRECT      = 'X'
        it_service_selection = it_gos_sels
*       ip_no_instance       = 'X'
        is_object            = obj
*       ip_no_commit         = 'X'
      EXCEPTIONS
        OTHERS               = 1.

  ENDIF.

  cl_gui_cfw=>flush( ).

ENDMODULE.                    "CREATE_GOS_SERVICE


MODULE user_command INPUT.
  DATA:lt_text TYPE TABLE OF line .
  DATA:ls_string TYPE string.
  CASE sy-ucomm.
    WHEN 'OBJECT'.
    WHEN 'TC1_INSR'.
      CLEAR sy-ucomm.
      DATA:lt_user TYPE TABLE OF zapp_addr WITH HEADER LINE.

      CALL FUNCTION 'ZMSG_CHOISE_USER'
        TABLES
          et_user = lt_user[].
      CHECK lt_user[] IS NOT INITIAL.
      LOOP AT lt_user.
        READ TABLE gt_datai WITH KEY uname = lt_user-person.
        IF sy-subrc NE 0.
          gt_datai-uname = lt_user-person.
          gt_datai-name1 = lt_user-name.
          gt_datai-department = lt_user-department.
          APPEND gt_datai.
        ENDIF.
      ENDLOOP.


    WHEN 'SAVE'.
      CLEAR sy-ucomm.
      DATA:lt_data TYPE TABLE OF zmsg_suser WITH HEADER LINE.
      DATA:lt_datai TYPE TABLE OF zmsg_data_i WITH HEADER LINE.
      CLEAR lt_data[].
      CLEAR lt_datai[].

      IF gt_datah-object IS INITIAL.
        MESSAGE s005 DISPLAY LIKE 'E'."'请选择对象类型' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      DELETE gt_datai[] WHERE uname = ''.
      IF gt_datai[] IS INITIAL.
        MESSAGE s006 DISPLAY LIKE 'E' ."'请选择发送人' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT gt_datai.
        MOVE-CORRESPONDING gt_datai TO lt_data.
        APPEND lt_data.
      ENDLOOP.

      CLEAR lt_text[].
      CLEAR lv_line.
      CLEAR gt_datah-text.

      CALL METHOD editor->get_text_as_r3table "写数据
        IMPORTING
          table = lt_text.


      LOOP AT lt_text INTO lv_line.
        IF sy-tabix = 1.
          gt_datah-text = lv_line.
        ELSE.
          gt_datah-text = gt_datah-text && '//' && lv_line.
        ENDIF.
      ENDLOOP.

      IF gt_datah-text IS INITIAL.
        MESSAGE s007 DISPLAY LIKE 'E'."'请填写消息' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CALL FUNCTION 'ZMSG_SAVE_DATA'
        EXPORTING
          is_datah  = gt_datah
        IMPORTING
          es_return = gs_return
        TABLES
          it_data   = lt_data[]
        EXCEPTIONS
          error     = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CALL METHOD editor->free.
      CALL METHOD msg_container->free.
      FREE  editor.
      FREE  msg_container.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*      CALL METHOD cl_gui_cfw=>dispatch.
*      call METHOD reply_control->
*      CALL METHOD cl_gui_cfw=>flush
*        EXCEPTIONS
*          cntl_system_error = 1
*          cntl_error        = 2.



  ENDCASE.

ENDMODULE.

MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN '&TYPE'.

      PERFORM frm_get_todo_new USING g_uname.

*      PERFORM f_refresh_grid_alv USING g_grid_200.

    WHEN 'TODO'.

      PERFORM frm_get_todo_new USING g_uname.

*      PERFORM f_refresh_grid_alv USING g_grid_200.

    WHEN 'DO'.

      PERFORM frm_get_do USING g_uname.

*      PERFORM f_refresh_grid_alv USING g_grid_200.

    WHEN 'MY'.

      PERFORM frm_get_my USING g_uname.

    WHEN 'DIS_TODO'.

      PERFORM frm_get_dis_todo USING g_uname.


    WHEN 'DIS_STOP'.

      PERFORM frm_get_dis_stop USING g_uname.

    WHEN 'CLOSE'.
      DATA:lt_index_rows TYPE  lvc_t_row,
           lt_row_no     TYPE  lvc_t_roid.

      CALL METHOD g_grid_200->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.
        MESSAGE s008 DISPLAY LIKE 'E'."'请选中行' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT lt_index_rows INTO DATA(ls_index_row).
        READ TABLE gt_sdisplay ASSIGNING <fs_sdisplay>  INDEX ls_index_row-index.


        IF sy-subrc EQ 0.
          PERFORM frm_close_msg CHANGING <fs_sdisplay>.
        ENDIF.
      ENDLOOP.

      COMMIT WORK AND WAIT.

    WHEN 'ALL_CLOSE'.

      LOOP AT gt_sdisplay ASSIGNING <fs_sdisplay> WHERE status = ''.
        CHECK <fs_sdisplay>-uname = sy-uname.

        <fs_sdisplay>-status = 'C'.
        <fs_sdisplay>-icon = icon_complete.
        <fs_sdisplay>-icontext = TEXT-001."'已阅'.

        UPDATE zmsg_data_i SET status = 'C'
                               fr_dat = sy-datum
                               aedat = sy-datum
                               aetim = sy-uzeit
          WHERE msgno = <fs_sdisplay>-msgno
            AND uname = sy-uname.

      ENDLOOP.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

    WHEN 'CREATE_MSG'.
      CALL FUNCTION 'ZMSG_CREATE'
        EXPORTING
          object = 'MSG'
*         OBJECT_ID       =
*         TEXT   =
*         URGENT =
*       IMPORTING
*         ES_RETURN       =
*       TABLES
*         IT_DATA         =
        EXCEPTIONS
          error  = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    WHEN 'CREATE_REP'.
      CALL FUNCTION 'ZMSG_CREATE'
        EXPORTING
          object = 'ZSC'
*         OBJECT_ID       =
*         TEXT   =
*         URGENT =
*       IMPORTING
*         ES_RETURN       =
*       TABLES
*         IT_DATA         =
        EXCEPTIONS
          error  = 1
          OTHERS = 2.
  ENDCASE.

ENDMODULE.


FORM frm_close_msg CHANGING cs_sdisplay TYPE ty_zemp.
  CASE cs_sdisplay-object_type.
    WHEN 'SHORT' OR 'REPLY'.
      IF cs_sdisplay-status <> 'C' AND cs_sdisplay-uname = g_uname.
        cs_sdisplay-status = 'C'.
        PERFORM frm_set_status_single CHANGING cs_sdisplay.
        IF cs_sdisplay-fr_dat IS NOT INITIAL.
          UPDATE zmsg_data_i
            SET status = cs_sdisplay-status aedat = sy-datum aetim = sy-uzeit
            WHERE msgno = cs_sdisplay-msgno
            AND uname = cs_sdisplay-uname.
        ELSE.
          UPDATE zmsg_data_i
            SET status = cs_sdisplay-status fr_dat = sy-datum aedat = sy-datum aetim = sy-uzeit
            WHERE msgno = cs_sdisplay-msgno
            AND uname = cs_sdisplay-uname.
        ENDIF.
      ELSE.
*        RETURN.
      ENDIF.

    WHEN 'TASK'.
      IF ( cs_sdisplay-status = '' OR cs_sdisplay-status = 'A' ) AND cs_sdisplay-uname = g_uname.
        cs_sdisplay-status = 'B'.
        PERFORM frm_set_status_single CHANGING cs_sdisplay.
        UPDATE zmsg_data_i
          SET status = cs_sdisplay-status
          WHERE msgno = cs_sdisplay-msgno
          AND uname = cs_sdisplay-uname.
        COMMIT WORK.
        RETURN.
      ELSEIF cs_sdisplay-status = 'B' AND cs_sdisplay-ernam = g_uname.
*        cs_sdisplay-status = 'C'.
*        PERFORM frm_set_status_single CHANGING cs_sdisplay.
*        UPDATE zmsg_data_i
*          SET status = cs_sdisplay-status
*          WHERE msgno = cs_sdisplay-msgno
*          AND uname = cs_sdisplay-uname.
      ELSE.
*        RETURN.
      ENDIF.
  ENDCASE.

  IF cs_sdisplay-uname = g_uname.

    UPDATE zmsg_data_i
      SET aedat = sy-datum
          aetim = sy-uzeit
          text  = '已读'
      WHERE msgno = cs_sdisplay-msgno
      AND uname = cs_sdisplay-uname.

  ENDIF.
  COMMIT WORK AND WAIT.




ENDFORM.

MODULE set_editor OUTPUT.

  CHECK msg_container IS INITIAL.

  CREATE OBJECT: msg_container EXPORTING container_name = 'EDITOR'.


  CREATE OBJECT editor
    EXPORTING
      max_number_chars           = 180
      parent                     = msg_container
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 256
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  CALL METHOD editor->set_statusbar_mode "去掉状态栏
    EXPORTING
      statusbar_mode = 0.

  CALL METHOD editor->set_toolbar_mode  "去掉工具栏
    EXPORTING
      toolbar_mode = 0.



  CLEAR lt_text[].
  SPLIT gt_datah-text AT ';' INTO TABLE lt_text[].

  CALL METHOD editor->set_text_as_r3table "写数据
    EXPORTING
      table = lt_text.
ENDMODULE.


MODULE user_command_0300 INPUT.
  DATA:lv_uname TYPE ad_namtext.
  CASE sy-ucomm.
    WHEN 'DELETE'.

      PERFORM frm_delete_msg.

    WHEN 'REFRESH'.

      PERFORM frm_refresh_list.

    WHEN 'ADD_USER'.
*      DATA:lt_user TYPE TABLE OF zapp_addr WITH HEADER LINE.

      CLEAR lt_user[].
      CALL FUNCTION 'ZMSG_CHOISE_USER'
        TABLES
          et_user = lt_user[].
      IF lt_user[] IS NOT INITIAL.
        CLEAR gt_datai.
        LOOP AT lt_user.
          READ TABLE gt_datai WITH KEY uname = lt_user-person.
          IF sy-subrc NE 0.
            gt_datai-uname = lt_user-person.
            gt_datai-name1 = lt_user-name.
            gt_datai-department = lt_user-department.
            APPEND gt_datai.
            CLEAR gt_datai.
          ENDIF.
        ENDLOOP.
        SORT gt_datai BY uname.
        DELETE ADJACENT DUPLICATES FROM gt_datai COMPARING uname.
      ENDIF.

* add by at-yuxs 20211222 转发邮件后自动保存
      IF g_model = 'R'.
        PERFORM frm_save_msg.
      ENDIF.
      PERFORM f_refresh_grid_alv USING g_grid.

    WHEN 'CLEAR'.
      PERFORM frm_html_start.
      PERFORM frm_set_reply.

    WHEN 'SAVE'.
      PERFORM frm_save_msg.
*      IF g_model = 'C'.
*
*        IF gt_datah-object IS INITIAL.
*          MESSAGE s009 DISPLAY LIKE 'E'."'请选择对象类型' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.
*
*        IF gt_datah-text IS INITIAL.
*          MESSAGE s010 DISPLAY LIKE 'E' ."'必须输入主题' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.
*
*        IF gt_datai[] IS INITIAL.
*          MESSAGE s011 DISPLAY LIKE 'E'."'必须选择通知人' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.
*
*        CLEAR ls_string.
*        LOOP AT gt_list.
*          IF sy-tabix < 6.
*            CONTINUE.
*          ENDIF.
*          ls_string = ls_string && gt_list-line.
*        ENDLOOP.
*
*        IF ls_string IS INITIAL.
*          MESSAGE s012 DISPLAY LIKE 'E'."'必须填写正文' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.
*        CLEAR lt_data[].
*        LOOP AT gt_datai.
*          MOVE-CORRESPONDING gt_datai TO lt_data.
*          APPEND lt_data.
*        ENDLOOP.
*
*
*        CALL FUNCTION 'ZMSG_SAVE_DATA'
*          EXPORTING
*            is_datah  = gt_datah
*            IV_string = ls_string
*          IMPORTING
*            es_return = gs_return
*          TABLES
*            it_data   = lt_data[]
*          EXCEPTIONS
*            error     = 1
*            OTHERS    = 2.
*        MESSAGE s013."保存成功
*
*        PERFORM free_screen_300.
*        LEAVE TO SCREEN 0.
*
*      ELSEIF g_model = 'R'.
*        CLEAR lt_data[].
*        LOOP AT gt_datai.
*          MOVE-CORRESPONDING gt_datai TO lt_data.
*          APPEND lt_data.
*        ENDLOOP.
*        DATA:ls_line_id TYPE zmsgnr.
*        SELECT MAX( msgnr ) INTO ls_line_id FROM zmsg_data_i
*          WHERE msgno = gt_datah-msgno.
*        CLEAR lt_data[].
*
*        CALL FUNCTION 'ZFM_GET_USERNAME'
*          EXPORTING
*            userid   = sy-uname
*          IMPORTING
*            username = lv_uname.
*        LOOP AT gt_datai WHERE msgno = ''.
*          ADD 1 TO ls_line_id.
*          MOVE-CORRESPONDING gt_datai TO lt_datai.
*          lt_datai-msgno = gt_datah-msgno.
*          lt_datai-msgnr = ls_line_id.
*          lt_datai-fw_user = sy-uname.
*          lt_datai-fw_uname = lv_uname.
*          lt_datai-fw_dat = sy-datum.
*          APPEND lt_datai.
*        ENDLOOP.
*        IF lt_datai[] IS NOT INITIAL.
*          MODIFY zmsg_data_i FROM TABLE lt_datai.
*          COMMIT WORK.
*          MESSAGE s014." TYPE 'S'."转发成功
*        ENDIF.
*
*      ENDIF.

    WHEN 'TO_DO'.
      gt_datai-todo = 'X'.
      MODIFY zmsg_data_i FROM gt_datai.
      COMMIT WORK AND WAIT.
    WHEN 'UNTO_DO'.
      gt_datai-todo = ''.
      MODIFY zmsg_data_i FROM gt_datai.
      COMMIT WORK AND WAIT.
    WHEN 'STOP'.
      gt_datai-stop = 'X'.
      MODIFY zmsg_data_i FROM gt_datai.
      COMMIT WORK AND WAIT.
    WHEN 'UNSTOP'.
      gt_datai-stop = ''.
      MODIFY zmsg_data_i FROM gt_datai.
      COMMIT WORK AND WAIT.
    WHEN 'PRINT'.
      CALL FUNCTION 'ZMSG_PRINT'.

    WHEN OTHERS.
  ENDCASE.


ENDMODULE.
