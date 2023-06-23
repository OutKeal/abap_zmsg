*&---------------------------------------------------------------------*
*& 包含               LZMSGF04
*&---------------------------------------------------------------------*




*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:
  lcl_event_receiver_grid_400_l DEFINITION DEFERRED.


DATA:
  g_event_receiver_grid_400_l   TYPE REF TO lcl_event_receiver_grid_400_l.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_400_l DEFINITION.

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

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_400_l IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_400_l
      USING er_data_changed
            e_onf4.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_400_l USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_400_l USING e_row_id e_column_id .
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

DATA: g_grid_400_l            TYPE REF TO cl_gui_alv_grid,
      g_grid_400_m            TYPE REF TO cl_gui_alv_grid,
      g_grid_400_r            TYPE REF TO cl_gui_alv_grid,
      gt_fcat_400_l           TYPE lvc_t_fcat,
      gt_fcat_400_m           TYPE lvc_t_fcat,
      gt_fcat_400_r           TYPE lvc_t_fcat,


      gs_layout_400           TYPE lvc_s_layo,
      gt_sort_400_l           TYPE lvc_t_sort,
      gt_sort_400_m           TYPE lvc_t_sort,
      gt_exclude_400          TYPE ui_functions,
      g_docking_container_400 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_400  TYPE REF TO cl_gui_custom_container,
      g_container_400_l       TYPE REF TO cl_gui_container,
      g_container_400_r       TYPE REF TO cl_gui_container,
      g_container_400_m       TYPE REF TO cl_gui_container,
      g_splitter_400          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_400_l         TYPE REF TO cl_gui_toolbar.





FORM f_handle_data_changed_400_l
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

FORM f_handle_double_click_400_l USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

*  CLEAR gt_item_front_dis[].
*  PERFORM frm_get_item_front_dis USING e_row_id-index.
*
*  PERFORM f_refresh_grid_alv USING g_grid_400_r.

ENDFORM.




FORM f_handle_hotspot_click_400_l USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

  CASE e_column_id-fieldname.

      READ TABLE gt_department INDEX e_row_id-index.

    WHEN 'NAME'.

      LOOP AT gt_user WHERE department = gt_department-department.
        READ TABLE gt_user_sel WITH KEY person = gt_user-person.
        IF sy-subrc NE 0.
          APPEND gt_user TO gt_user_sel.
        ENDIF.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM f_refresh_grid_alv USING g_grid_400_r.
      ENDIF.

    WHEN 'DEPARTMENT'.
      CLEAR gt_user_list[].
      LOOP AT gt_user WHERE department = gt_department-department.
        APPEND gt_user TO gt_user_list.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM f_refresh_grid_alv USING g_grid_400_m.
      ENDIF.
  ENDCASE.


*  CHECK e_column_id-fieldname = 'AFONO'.
*  READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX e_row_id-index .
*
*  IF sy-subrc EQ 0.
*    CALL FUNCTION 'ZAFO_MAINTAIN'
*      EXPORTING
*        i_bustyp = <gs_head_dis>-bustyp
*        i_afono  = <gs_head_dis>-afono
*      IMPORTING
*        es_head  = <gs_head_dis>
**     TABLES
**       ET_RETURN       =
**       ET_ITEM  =
**     EXCEPTIONS
**       ERROR    = 1
**       OTHERS   = 2
*      .
*
*    PERFORM f_refresh_grid_alv USING g_grid_400_l.
*    PERFORM f_refresh_grid_alv USING g_grid_400_r.
*
*  ENDIF.

ENDFORM.


FORM f_handle_user_command_400_l USING ok_code.
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
*      CALL METHOD g_grid_400_l->get_selected_rows
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


CLASS:
  lcl_event_receiver_grid_400_m DEFINITION DEFERRED.


DATA:
  g_event_receiver_grid_400_m   TYPE REF TO lcl_event_receiver_grid_400_m.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_400_m DEFINITION.

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

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_400_m IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_400_m
      USING er_data_changed
            e_onf4.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_400_m USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_400_m USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_handle_toolbar_400_m USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_handle_user_command_400_m USING e_ucomm.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION







FORM f_handle_data_changed_400_m
 USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
   u_onf4    TYPE any.


  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS:
    <fs_changed> TYPE any,
    <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.

*    READ TABLE GT_ALV INDEX LS_MODI-ROW_ID
*                      ASSIGNING FIELD-SYMBOL(<FS_mS_DATA>) .
*    IF SY-SUBRC = 0.
**
*      IF <FS_CHANGED> IS ASSIGNED.
*        UNASSIGN: <FS_CHANGED>.
*      ENDIF.
*      ASSIGN COMPONENT LS_MODI-FIELDNAME OF STRUCTURE <FS_mS_DATA>
*                    TO <FS_CHANGED>.
*      <FS_CHANGED> = LS_MODI-VALUE.
*
*    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_handle_double_click_400_m USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

  READ TABLE gt_user_list INDEX e_row_id-index.
  IF sy-subrc EQ 0.
    READ TABLE gt_user_sel WITH KEY person = gt_user_list-person.
    IF sy-subrc NE 0.
      APPEND gt_user_list TO gt_user_sel.
      PERFORM f_refresh_grid_alv USING g_grid_400_r.
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_handle_hotspot_click_400_m USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.


ENDFORM.

FORM f_handle_toolbar_400_m USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.


  MOVE '&SEL' TO ls_toolbar-function.
  MOVE icon_arrow_right TO ls_toolbar-icon.
  MOVE TEXT-a02 TO ls_toolbar-quickinfo. "插入行
  MOVE '' TO ls_toolbar-disabled.
  MOVE TEXT-a02 TO ls_toolbar-text. "插入行
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

ENDFORM.

FORM f_handle_user_command_400_m USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
  CASE ok_code.

    WHEN '&SEL'.

      CALL METHOD g_grid_400_m->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.
        MESSAGE '请至少选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT lt_index_rows INTO ls_index_rows.

        READ TABLE gt_user_list INDEX ls_index_rows-index.
        IF sy-subrc EQ 0.
          READ TABLE gt_user_sel WITH KEY person = gt_user_list-person.
          IF sy-subrc NE 0.
            APPEND gt_user_list TO gt_user_sel.

          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM f_refresh_grid_alv USING g_grid_400_r.
  ENDCASE.

ENDFORM.


CLASS:
  lcl_event_receiver_grid_400_r DEFINITION DEFERRED.


DATA:
  g_event_receiver_grid_400_r   TYPE REF TO lcl_event_receiver_grid_400_r.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_400_r DEFINITION.

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

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_400_r IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_400_r
      USING er_data_changed
            e_onf4.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_400_r USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_400_r USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_handle_toolbar_400_r USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_handle_user_command_400_r USING e_ucomm.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION







FORM f_handle_data_changed_400_r
 USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
   u_onf4    TYPE any.


  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS:
    <fs_changed> TYPE any,
    <fs_rod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.

*    READ TABLE GT_ALV INDEX LS_rODI-ROW_ID
*                      ASSIGNING FIELD-SYMBOL(<FS_rS_DATA>) .
*    IF SY-SUBRC = 0.
**
*      IF <FS_CHANGED> IS ASSIGNED.
*        UNASSIGN: <FS_CHANGED>.
*      ENDIF.
*      ASSIGN COMPONENT LS_rODI-FIELDNAME OF STRUCTURE <FS_rS_DATA>
*                    TO <FS_CHANGED>.
*      <FS_CHANGED> = LS_rODI-VALUE.
*
*    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_handle_double_click_400_r USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

  DELETE gt_user_sel INDEX e_row_id-index.
  PERFORM f_refresh_grid_alv USING g_grid_400_r.

ENDFORM.




FORM f_handle_hotspot_click_400_r USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

*  CHECK e_column_id-fieldname = 'AFONO'.
*  READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX e_row_id-index .
*
*  IF sy-subrc EQ 0.
*    CALL FUNCTION 'ZAFO_rAINTAIN'
*      EXPORTING
*        i_bustyp = <gs_head_dis>-bustyp
*        i_afono  = <gs_head_dis>-afono
*      IMPORTING
*        es_head  = <gs_head_dis>
**     TABLES
**       ET_RETURN       =
**       ET_ITEM  =
**     EXCEPTIONS
**       ERROR    = 1
**       OTHERS   = 2
*      .
*
*    PERFORM f_refresh_grid_alv USING g_grid_400_r.
*    PERFORM f_refresh_grid_alv USING g_grid_400_r.
*
*  ENDIF.

ENDFORM.


FORM f_handle_toolbar_400_r USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.

  MOVE '&DEL' TO ls_toolbar-function.
  MOVE icon_arrow_left TO ls_toolbar-icon.
  MOVE TEXT-a03 TO ls_toolbar-quickinfo."删除行
  MOVE '' TO ls_toolbar-disabled.
  MOVE TEXT-a03 TO ls_toolbar-text."删除行
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

ENDFORM.

FORM f_handle_user_command_400_r USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

  CASE ok_code.
    WHEN '&DEL'.

      CALL METHOD g_grid_400_r->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.
        MESSAGE '请至少选择一行数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      DATA(lt_user_sel)  = gt_user_sel[].
      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE lt_user_sel INTO DATA(ls_user_sel) INDEX ls_index_rows-index.
        LOOP AT gt_user_sel WHERE person = ls_user_sel-person.
          DELETE gt_user_sel INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.

      PERFORM f_refresh_grid_alv USING g_grid_400_r.

  ENDCASE.

ENDFORM.



MODULE create_object_0400 OUTPUT.

  IF g_grid_400_l IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_400.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_field_catalog_400_l.
    PERFORM f_set_field_catalog_400_m.
    PERFORM f_set_field_catalog_400_r.
**-- LAYOUT
    PERFORM f_create_grid_layout_400_l.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_400_l  CHANGING gt_exclude_400[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_grid_handlers_400_l CHANGING g_grid_400_l.
    PERFORM f_assign_grid_handlers_400_m CHANGING g_grid_400_m.
    PERFORM f_assign_grid_handlers_400_r CHANGING g_grid_400_r.
*    PERFORM F_ASSIGN_GRID_EVENT_HANDLERS CHANGING G_GRID2.
**-- REGISTER EVENT
*    PERFORM f_register_grid_event1 USING g_grid_400_l.
*    PERFORM F_REGISTER_GRID_EVENT2 USING G_GRID2.
**--
    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_400.
*--
    CALL METHOD g_grid_400_l->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_400_m.
    PERFORM f_refresh_grid_alv USING g_grid_400_l.
    PERFORM f_refresh_grid_alv USING g_grid_400_r.
  ENDIF.

ENDMODULE.

FORM f_create_container_400 .

*  IF g_docking_container_400 IS INITIAL.

*    CREATE OBJECT g_docking_container_400
*      EXPORTING
*        style     = cl_gui_control=>ws_child
*        repid     = sy-repid
*        dynnr     = sy-dynnr
*        side      = g_docking_container_400->dock_at_bottom
*        lifetime  = cl_gui_control=>lifetime_imode
*        extension = '4000'
*      EXCEPTIONS
*        OTHERS    = 1.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid
*            TYPE sy-msgty
*          NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDIF.
  CHECK g_cumtom_container_400 IS INITIAL.
  CREATE OBJECT g_cumtom_container_400
    EXPORTING
      container_name = 'ITEM'.

* SPLITTER CONTAINER
  IF g_splitter_400 IS INITIAL.
    CREATE OBJECT g_splitter_400
      EXPORTING
        parent  = g_cumtom_container_400
        rows    = 1
        columns = 3.

    g_container_400_l  = g_splitter_400->get_container( row = 1 column = 1 ).
    g_container_400_m  = g_splitter_400->get_container( row = 1 column = 2 ).
    g_container_400_r  = g_splitter_400->get_container( row = 1 column = 3 ).

  ENDIF.


  CALL METHOD g_splitter_400->set_column_width
    EXPORTING
      id    = 1
      width = 20.

  CALL METHOD g_splitter_400->set_column_width
    EXPORTING
      id    = 2
      width = 40.

  CALL METHOD g_splitter_400->set_column_width
    EXPORTING
      id    = 3
      width = 40.

  CREATE OBJECT g_grid_400_l
    EXPORTING
      i_parent = g_container_400_l.

  CREATE OBJECT g_grid_400_m
    EXPORTING
      i_parent = g_container_400_m.

  CREATE OBJECT g_grid_400_r
    EXPORTING
      i_parent = g_container_400_r.

ENDFORM.


FORM f_set_field_catalog_400_l .

  REFRESH: gt_fcat_400_l.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAPP_ADDR'.

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

  PERFORM f_transfer_slis_to_lvc
          CHANGING lt_fieldcat
                   lt_fcat.
* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.
    CASE <ls_fcat>-fieldname.
      WHEN 'DEPARTMENT'.
        <ls_fcat>-hotspot = 'X'.
        <ls_fcat>-outputlen = 8.
      WHEN 'NAME'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
          <ls_fcat>-scrtext_l = <ls_fcat>-reptext = TEXT-032."全选
        <ls_fcat>-hotspot = 'X'.
        <ls_fcat>-outputlen = 3.
      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

  ENDLOOP.


  gt_fcat_400_l = lt_fcat.

ENDFORM.


FORM f_set_field_catalog_400_m .

  REFRESH: gt_fcat_400_m.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAPP_ADDR'.

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

  PERFORM f_transfer_slis_to_lvc
          CHANGING lt_fieldcat
                   lt_fcat.
* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.
    CASE <ls_fcat>-fieldname.
      WHEN 'ZPOSITION'.
        <ls_fcat>-outputlen = 8.
      WHEN 'DEPARTMENT'.
        <ls_fcat>-outputlen = 10.
      WHEN 'NAME'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
  <ls_fcat>-scrtext_l = <ls_fcat>-reptext = TEXT-033."'名字'.
        <ls_fcat>-outputlen = 8.
      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

  ENDLOOP.


  gt_fcat_400_m = lt_fcat.

ENDFORM.
FORM f_set_field_catalog_400_r .

  REFRESH: gt_fcat_400_r.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAPP_ADDR'.

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

  PERFORM f_transfer_slis_to_lvc
          CHANGING lt_fieldcat
                   lt_fcat.
* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.
    CASE <ls_fcat>-fieldname.
      WHEN 'ZPOSITION'.
        <ls_fcat>-outputlen = 8.
      WHEN 'DEPARTMENT'.
        <ls_fcat>-outputlen = 10.
      WHEN 'NAME'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = TEXT-033." '名字'.
        <ls_fcat>-outputlen = 8.
      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.
  ENDLOOP.

  gt_fcat_400_r = lt_fcat.

ENDFORM.





FORM f_create_grid_layout_400_l .

  CLEAR: gs_layout_400.
  gs_layout_400-sel_mode   = 'A'.
*  gs_layout_400-cwidth_opt = 'X'.
  gs_layout_400-zebra      = 'X'.


ENDFORM.

FORM f_create_grid_toolbar_400_l
  CHANGING  c_t_toolbar TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

  CLEAR: c_t_toolbar[].
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND  LS_EXCLUDE  TO C_T_TOOLBAR.

  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO c_t_toolbar.
*  IF gs_head-execute_type <> 'PO'.
  IF g_model <> 'C'.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO c_t_toolbar.
  ENDIF.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO c_t_toolbar.
*  ENDIF.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_print  TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_print_prev  TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_info TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_save_variant TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_variant_admin TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_maximum TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_minimum TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_find TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_average TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_html TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_views TO c_t_toolbar.
ENDFORM.

FORM f_assign_grid_handlers_400_l
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_400_l.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOOLBAR
*          FOR C_GRID .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_USER_COMMAND
*          FOR C_GRID .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid_400_l->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_400_l->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_assign_grid_handlers_400_m
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_400_m.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_grid_400_m->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_400_m->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid_400_m->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_400_m->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_assign_grid_handlers_400_r
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_400_r.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_grid_400_r->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_400_r->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid_400_r->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_400_r->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_register_grid_event_400_l
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




FORM f_display_grid_alv_400 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  ls_variant-handle = 400.

  CALL METHOD g_grid_400_l->set_table_for_first_display
    EXPORTING
*     is_variant           = ls_variant
      i_save               = ''
      is_layout            = gs_layout_400
      it_toolbar_excluding = gt_exclude_400[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_department[]
      it_sort              = gt_sort_400_l[]
      it_fieldcatalog      = gt_fcat_400_l[].

  ls_variant-handle = 350.
*
  CALL METHOD g_grid_400_m->set_table_for_first_display
    EXPORTING
*     is_variant           = ls_variant
      i_save               = ''
      is_layout            = gs_layout_400
      it_toolbar_excluding = gt_exclude_400[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_user_list[]
*     it_sort              = gt_sort[]
      it_fieldcatalog      = gt_fcat_400_m[].
*
  ls_variant-handle = 350.
*
  CALL METHOD g_grid_400_r->set_table_for_first_display
    EXPORTING
*     is_variant           = ls_variant
      i_save               = ''
      is_layout            = gs_layout_400
      it_toolbar_excluding = gt_exclude_400[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_user_sel[]
*     it_sort              = gt_sort[]
      it_fieldcatalog      = gt_fcat_400_r[].

ENDFORM.
