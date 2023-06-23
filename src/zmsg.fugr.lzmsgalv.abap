*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:
  lcl_event_receiver_grid_200 DEFINITION DEFERRED.


DATA:
  g_event_receiver_grid_200   TYPE REF TO lcl_event_receiver_grid_200.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_200 DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click
      OF cl_gui_alv_grid
      IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click
      OF cl_gui_alv_grid
      IMPORTING
        e_row_id
        e_column_id
        es_row_no.

    METHODS handle_toolbar
      FOR EVENT toolbar
      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.


    METHODS: data_changed_finished
      FOR EVENT data_changed_finished
      OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.


ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_200 IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_200
      USING er_data_changed
            e_onf4.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_200 USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_200 USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_200 USING e_object->mt_toolbar.
  ENDMETHOD.
  METHOD handle_user_command.
    PERFORM f_user_command_200 USING e_ucomm.
  ENDMETHOD.


  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_200 USING e_modified et_good_cells.
  ENDMETHOD.


ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION




FORM f_handle_data_changed_200
 USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
   u_onf4    TYPE any.


  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS:
    <fs_changed> TYPE any,
    <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.

*    READ TABLE GT_ALV INDEX LS_MODI-ROW_ID
*                      ASSIGNING FIELD-SYMBOL(<FS_LS_DATA>) .
*    IF SY-SUBRC = 0.
**
*      IF <FS_CHANGED> IS ASSIGNED.
*        UNASSIGN: <FS_CHANGED>.
*      ENDIF.
*      ASSIGN COMPONENT LS_MODI-FIELDNAME OF STRUCTURE <FS_LS_DATA>
*                    TO <FS_CHANGED>.
*      <FS_CHANGED> = LS_MODI-VALUE.
*
*    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_handle_double_click_200 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

  READ TABLE gt_sdisplay ASSIGNING <fs_sdisplay> INDEX e_row_id-index.
  CHECK sy-subrc EQ 0.

  PERFORM frm_close_msg CHANGING <fs_sdisplay>.
  PERFORM frm_open_msg USING <fs_sdisplay>.

  PERFORM f_refresh_grid_alv USING g_grid_200.

ENDFORM.


FORM f_handle_hotspot_click_200 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

  CASE e_column_id-fieldname .
    WHEN 'OBJECT_ID'.

      READ TABLE gt_sdisplay ASSIGNING FIELD-SYMBOL(<fs_sdisplay>) INDEX e_row_id-index .
      CHECK <fs_sdisplay>-object_id IS NOT INITIAL.


      PERFORM frm_close_msg CHANGING <fs_sdisplay>.

      PERFORM f_refresh_grid_alv USING g_grid_200.
      SELECT SINGLE * FROM zmsg_object_con
        INTO gt_object WHERE object = <fs_sdisplay>-object.
      IF sy-subrc EQ 0 AND gt_object-tcode IS NOT INITIAL.

        IF gt_object-tcode = 'ZAFO'.
          DATA:ls_afono TYPE zafono.
          ls_afono = <fs_sdisplay>-object_id.

          CALL FUNCTION 'ZAFO_CALL_TRANSACTION'
            EXPORTING
              afono = ls_afono.
          RETURN.

          IF gt_object-tcode = 'ZAPP'.

          ENDIF.
        ENDIF.

        SET PARAMETER ID gt_object-memoryid FIELD <fs_sdisplay>-object_id.
        CALL TRANSACTION gt_object-tcode AND SKIP FIRST SCREEN.

      ELSE.
****************Add by H-Liuwk Start at 09.06.2021 16:53:39***************
        IF <fs_sdisplay>-object = 'ZBOM'.
          SELECT SINGLE * INTO @DATA(ls_msg_bom) FROM zmsg_data_r WHERE msgno = @<fs_sdisplay>-msgno.
          IF sy-subrc IS INITIAL AND ls_msg_bom-text IS NOT INITIAL.
            cl_abap_browser=>show_html( html_string = ls_msg_bom-text ).
          ENDIF.
        ENDIF.
****************Add by H-Liuwk End at 09.06.2021 16:53:39 ****************
      ENDIF.

    WHEN 'MSGNO'.

      READ TABLE gt_sdisplay ASSIGNING <fs_sdisplay> INDEX e_row_id-index .
      PERFORM frm_close_msg CHANGING <fs_sdisplay>.
      PERFORM frm_open_msg USING <fs_sdisplay>.
      PERFORM f_refresh_grid_alv USING g_grid_200.

    WHEN 'STOP_BTN'.
      DATA: lt_datai TYPE zmsg_data_i.
      READ TABLE gt_sdisplay ASSIGNING <fs_sdisplay> INDEX e_row_id-index .
      CHECK <fs_sdisplay>-msgno IS NOT INITIAL.

      SELECT SINGLE * INTO lt_datai
        FROM zmsg_data_i
        WHERE msgno = <fs_sdisplay>-msgno
        AND uname = <fs_sdisplay>-uname.
      IF sy-subrc = 0.
        IF lt_datai-stop = 'X'.
          CLEAR lt_datai-stop.
          CLEAR <fs_sdisplay>-stop.
        ELSE.
          lt_datai-stop = 'X'.
          <fs_sdisplay>-stop = 'X'.
        ENDIF.

        PERFORM frm_set_btn_single CHANGING <fs_sdisplay>.
        MODIFY zmsg_data_i FROM lt_datai.
        COMMIT WORK AND WAIT.

      ELSE.

      ENDIF.
      DELETE  gt_sdisplay WHERE msgno = <fs_sdisplay>-msgno.

  ENDCASE.

  PERFORM f_refresh_grid_alv USING g_grid_200.
ENDFORM.


FORM frm_open_msg USING ls_sdisplay TYPE ty_zemp.

  READ TABLE gt_object WITH KEY object = ls_sdisplay-object.
  IF sy-subrc EQ 0.
    IF gt_object-object_type = 'REPLY'.
      CALL FUNCTION 'ZMSG_REPLY'
        EXPORTING
          zmsgno = ls_sdisplay-msgno.

    ELSE.
      CALL FUNCTION 'ZMSG_DISPLAY'
        EXPORTING
          zmsgno = ls_sdisplay-msgno
        EXCEPTIONS
          error  = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


FORM f_handle_user_command_200 USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
*  CASE ok_code.
*    WHEN '&NEW'.
*      SUBMIT zmmr0050 WITH modify = ''
*                WITH create = 'X'
*                VIA SELECTION-SCREEN
*                AND RETURN.
*
*
*    WHEN '&DETAIL'.
*      CLEAR:gt_item_dis.
*
*      CALL METHOD g_grid_200->get_selected_rows
*        IMPORTING
*          et_index_rows = lt_index_rows
*          et_row_no     = lt_row_no.
*      IF lt_index_rows[] IS INITIAL.
*        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
*
*      LOOP AT lt_index_rows INTO ls_index_rows.
*
*        PERFORM get_dis_data USING ls_index_rows-index.
*
*      ENDLOOP.
*
*      CALL METHOD g_grid2->set_frontend_layout
*        EXPORTING
*          is_layout = gs_layout.
*
*  ENDCASE.

ENDFORM.

FORM f_toolbar_200 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.



ENDFORM.
FORM f_user_command_200 USING ok_code.

ENDFORM.


FORM f_data_changed_finished_200  USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.

*
  DATA:ls_refresh TYPE char1.
  CLEAR ls_refresh.
  CHECK NOT et_good_cells IS INITIAL.
*
  LOOP AT et_good_cells INTO DATA(ls_cell).


*    READ TABLE gt_item ASSIGNING <gs_item>
*                            INDEX ls_cell-row_id.
*
*    CHECK sy-subrc = 0.
*    IF <gs_item>-icon <> icon_led_yellow.
*      <gs_item>-icon = icon_led_yellow.
*      <gs_item>-text = '已维护'.
*
*
*      ls_refresh = 'X'.
*    ENDIF.



  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_200.
  ENDIF.
*  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
*    EXPORTING
*      NEW_CODE = 'ZDATA_CHANGE'.

ENDFORM.



CLASS lcl_event_handler DEFINITION DEFERRED.

DATA: go_timer    TYPE REF TO cl_gui_timer,

      go_evt_hndl TYPE REF TO lcl_event_handler,

      gv_datum    TYPE sy-datum,
      gv_uzeit    TYPE sy-uzeit.

CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:

      handle_timer FOR EVENT finished OF cl_gui_timer.

ENDCLASS.                    "lcl_event_handler DEFINITION

*&---------------------------------------------------------------------*

*&      CLASS lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_timer.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = 'TODO'.

    CALL METHOD go_timer->run "必须重新激活定时器
      EXCEPTIONS
        OTHERS = 9.

  ENDMETHOD.                 "handle_timer

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

FORM init_timer.

  CHECK go_timer IS INITIAL.

  CREATE OBJECT go_timer
    EXCEPTIONS
      OTHERS = 9.

  CREATE OBJECT go_evt_hndl.

  SET HANDLER go_evt_hndl->handle_timer FOR go_timer.


  go_timer->interval = 300.  "设置间隔为300秒

  CALL METHOD go_timer->run "激活定时器
    EXCEPTIONS
      OTHERS = 9.

ENDFORM.                    " INIT_TIMER





MODULE create_object_0200 OUTPUT.

  IF g_grid_200 IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_200.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_grid_field_catalog_200.
**-- LAYOUT
    PERFORM f_create_grid_layout_200.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_200  CHANGING gt_exclude_200[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_grid_handlers_200 CHANGING g_grid_200.
**-- REGISTER EVENT
    PERFORM f_register_grid_event_200 USING g_grid_200.
**--
    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_200.
*--
    CALL METHOD g_grid_200->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_200.
*    PERFORM f_refresh_grid_alv USING g_grid2.
  ENDIF.

ENDMODULE.



FORM f_create_container_200 .

*  IF g_docking_container_200 IS INITIAL.
*
*    CREATE OBJECT g_docking_container_200
*      EXPORTING
*        style     = cl_gui_control=>ws_child
*        repid     = sy-repid
*        dynnr     = sy-dynnr
*        side      = g_docking_container_200->dock_at_bottom
*        lifetime  = cl_gui_control=>lifetime_imode
*        extension = '3000'
*      EXCEPTIONS
*        OTHERS    = 1.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid
*            TYPE sy-msgty
*          NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDIF.
  CREATE OBJECT g_cumtom_container_200
    EXPORTING
      container_name = 'ITEM'.


* SPLITTER CONTAINER
  IF g_splitter_200 IS INITIAL.
    CREATE OBJECT g_splitter_200
      EXPORTING
        parent  = g_cumtom_container_200
        rows    = 1
        columns = 1.

    g_container_200  = g_splitter_200->get_container( row = 1 column = 1 ).
*    g_container_2  = g_splitter->get_container( row = 1 column = 1 ).

  ENDIF.

  CREATE OBJECT g_grid_200
    EXPORTING
      i_parent = g_container_200.

*  CREATE OBJECT g_grid2
*    EXPORTING
*      i_parent = g_container_2.

ENDFORM.


FORM f_set_grid_field_catalog_200 .

  REFRESH: gt_fcat_200.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.
  DATA: lt_fcat TYPE lvc_t_fcat.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.

  DATA:l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZMSG_SDISPLAY'.

* 取得字段的属性
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = l_struc_name
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  PERFORM f_transfer_slis_to_lvc CHANGING lt_fieldcat lt_fcat.

* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'ICON' .
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = TEXT-014."'图标'.
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-emphasize = 'C300'.

      WHEN 'ICONTEXT' .
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = TEXT-015."'状态'.
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-emphasize = 'C300'.

      WHEN 'MSGNO'.
        <ls_fcat>-hotspot = 'X'.
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-emphasize = 'C300'.

      WHEN 'OBJECT' OR 'OBJECT_NAME'.
        <ls_fcat>-emphasize = 'C500'.

      WHEN 'OBJECT_ID' .
        <ls_fcat>-hotspot = 'X'.

      WHEN 'ERNAM' OR 'ERDAT' OR 'ERZET'."创建信息
        <ls_fcat>-emphasize = 'C700'.

      WHEN 'AENAM' OR 'AEDAT' OR 'AETIM'."修改信息

      WHEN 'FW_UNAME' OR 'FW_DAT'."转发信息

      WHEN 'UNAME' OR 'NAME1' OR 'FR_DAT'." 通知人 / 收件日期

      WHEN 'AEDATI'." 查看信息
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = TEXT-029.

      WHEN 'AETIMI'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = TEXT-030.

      WHEN 'STOP_BTN'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '停看'.
        <ls_fcat>-hotspot = 'X'.

      WHEN 'ZZPINO' .
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '销售合同/订单'.

      WHEN 'TEXT' .
        <ls_fcat>-emphasize = 'C710'.

      WHEN OTHERS.
        <ls_fcat>-no_out = 'X'.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

  ENDLOOP.

  gt_fcat_200 = lt_fcat.

ENDFORM.


FORM f_create_grid_layout_200 .

  CLEAR: gs_layout_200.
  gs_layout_200-sel_mode   = 'A'.
  gs_layout_200-cwidth_opt = 'X'.
  gs_layout_200-zebra      = 'X'.
  gs_layout_200-info_fname = 'CLR'.  "行颜色代码的字段
*  GS_LAYOUT-NO_ROWMARK = 'X'.
*  GS_LAYOUT-BOX_FNAME = 'SEL'.

*  GS_LAYOUT-STYLEFNAME = 'CELLTAB'.

*  GS_LAYOUT-NUMC_TOTAL = CNS_CHAR_X.

*  GS_LAYOUT-SGL_CLK_HD    = 'X'.
*  GS_LAYOUT-TOTALS_BEF    = 'X'.             " 合计显示在上面
*  GS_LAYOUT-NO_HGRIDLN    = ' '.
*  GS_LAYOUT-NO_VGRIDLN    = ' '.
*  GS_LAYOUT-NO_TOOLBAR    = SPACE.
*  GS_LAYOUT-GRID_TITLE    = ' '.
*  GS_LAYOUT-SMALLTITLE    = ' '.
*  GS_LAYOUT-EXCP_FNAME    = 'ICON'.          " LED
*  GS_LAYOUT-INFO_FNAME    = 'COLOR'.         " LINE COLOR
*  GS_LAYOUT-CTAB_FNAME    = ' '.             " CELL COLOR
*  GS_LAYOUT-BOX_FNAME     = ' '.
*  GS_LAYOUT-DETAILINIT    = ' '.

ENDFORM.

FORM f_create_grid_toolbar_200
  CHANGING  c_t_toolbar TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

  CLEAR: c_t_toolbar[].

*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND  LS_EXCLUDE  TO C_T_TOOLBAR.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO c_t_toolbar.
ENDFORM.

FORM f_assign_grid_handlers_200
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_200.

  SET HANDLER g_event_receiver_grid_200->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_grid_200->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_200->data_changed_finished
        FOR c_grid.
  SET HANDLER g_event_receiver_grid_200->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid_200->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_200->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_register_grid_event_200
  USING u_grid TYPE REF TO cl_gui_alv_grid.

* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** MODIFY EVENT
*  CALL METHOD u_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.




FORM f_display_grid_alv_200 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  ls_variant-handle = 1.

  CALL METHOD g_grid_200->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_200
      it_toolbar_excluding = gt_exclude_200[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_sdisplay[]
      it_sort              = gt_sort_200[]
      it_fieldcatalog      = gt_fcat_200[].

ENDFORM.

FORM f_set_catalog_alv_200.

  PERFORM f_set_grid_field_catalog_200.

  CALL METHOD g_grid_200->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_200.

  CALL METHOD g_grid_200->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_200.


ENDFORM.

FORM f_refresh_grid_alv
   USING u_grid TYPE REF TO cl_gui_alv_grid.

  DATA: ls_scroll TYPE lvc_s_stbl.

  CLEAR: ls_scroll.
  ls_scroll-row = 'X'.
  ls_scroll-col = 'X'.

  CALL METHOD u_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_scroll
      i_soft_refresh = 'X'.

ENDFORM.


FORM f_transfer_slis_to_lvc
  CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv
           ct_fcat     TYPE lvc_t_fcat..

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
    EXPORTING
      it_fieldcat = ct_fieldcat
    IMPORTING
      et_fieldcat = lt_fieldcat.

  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
    EXPORTING
      it_fieldcat_kkblo = lt_fieldcat
    IMPORTING
      et_fieldcat_lvc   = ct_fcat.



ENDFORM.
