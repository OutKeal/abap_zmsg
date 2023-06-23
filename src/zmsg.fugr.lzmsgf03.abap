*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:
  lcl_event_receiver_grid DEFINITION DEFERRED.

CONSTANTS:
  cns_extension TYPE i VALUE 3000.  "Docking size
DATA:
  g_event_receiver_grid   TYPE REF TO lcl_event_receiver_grid.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed
      USING er_data_changed
            e_onf4.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION


DATA:index TYPE sy-tabix.



FORM f_handle_data_changed
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


MODULE create_object_alv OUTPUT.

  IF g_grid IS INITIAL.
**-- Create Container
    PERFORM f_create_container.
**-- Field_Catalog Define
    PERFORM f_set_grid_field_catalog.
**-- Layout
    PERFORM f_create_grid_layout.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_exclude_toolbar  CHANGING gt_exclude[].
**-- Grid event handler Define
    PERFORM f_assign_grid_event_handlers CHANGING g_grid.
**-- REGISTER EVENT
    PERFORM f_register_grid_event USING g_grid.
**--
    CALL METHOD cl_gui_cfw=>flush.
**-- Display Grid Alv
    PERFORM f_display_grid_alv.
*--
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid.
  ENDIF.

ENDMODULE.

FORM f_create_container .

*  IF g_docking_container IS INITIAL.
*    CREATE OBJECT g_docking_container
*      EXPORTING
*        style     = cl_gui_control=>ws_child
*        repid     = sy-cprog
*        dynnr     = sy-dynnr
*        side      = g_docking_container->dock_at_left
*        lifetime  = cl_gui_control=>lifetime_imode
*        extension = cns_extension
*      EXCEPTIONS
*        OTHERS    = 1.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid
*            TYPE sy-msgty
*          NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDIF.

* SPLITTER CONTAINER
*  IF g_splitter IS INITIAL.
*    CREATE OBJECT g_splitter
*      EXPORTING
*        parent  = g_docking_container
*        rows    = 1
*        columns = 1.
*
*    g_container_1  = g_splitter->get_container( row = 1 column = 1 ).
**    g_container_2  = g_splitter->get_container( row = 2 column = 1 ).
*
*  ENDIF.

  CREATE OBJECT g_grid
    EXPORTING
      i_parent = g_cumtom_container_300_right.

ENDFORM.


FORM f_set_grid_field_catalog .

  REFRESH: gt_fcat.

  DATA:
    ls_fcat TYPE lvc_s_fcat,
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZMSG_DATA_I'.

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
  LOOP AT lt_fcat INTO ls_fcat.
    index = sy-tabix.

    CASE ls_fcat-fieldname.
      WHEN 'NAME1' .
        ls_fcat-col_pos = 2.
      WHEN 'DEPARTMENT' .
        ls_fcat-col_pos = 1.
      WHEN 'TEXT' .
        ls_fcat-scrtext_s = ls_fcat-scrtext_m =
        ls_fcat-scrtext_l = ls_fcat-reptext = TEXT-031.
        ls_fcat-col_pos = 3.
      WHEN 'FR_DAT' .
        ls_fcat-col_pos = 4.
      WHEN 'AEDAT' .
        ls_fcat-scrtext_s = ls_fcat-scrtext_m =
        ls_fcat-scrtext_l = ls_fcat-reptext = TEXT-029.
        ls_fcat-col_pos = 5.
      WHEN 'AETIM' .
        ls_fcat-scrtext_s = ls_fcat-scrtext_m =
        ls_fcat-scrtext_l = ls_fcat-reptext = TEXT-030.
        ls_fcat-col_pos = 6.
      WHEN 'FW_UNAME' .
        ls_fcat-col_pos = 7.
      WHEN 'FW_DAT' .
        ls_fcat-col_pos = 8.
      WHEN OTHERS.
        ls_fcat-tech = 'X'.
    ENDCASE.

    MODIFY lt_fcat FROM ls_fcat.
  ENDLOOP.

  gt_fcat = lt_fcat.

ENDFORM.

FORM f_create_grid_layout .

  CLEAR: gs_layout.
  gs_layout-sel_mode   = 'A'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-zebra      = 'X'.

ENDFORM.

FORM f_create_grid_exclude_toolbar
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

FORM f_assign_grid_event_handlers
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid.

  SET HANDLER g_event_receiver_grid->handle_data_changed
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOOLBAR
*          FOR C_GRID .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_USER_COMMAND
*          FOR C_GRID .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_HOTSPOT_CLICK
*          FOR C_GRID .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DOUBLE_CLICK
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_register_grid_event
  USING u_grid TYPE REF TO cl_gui_alv_grid.

* Enter event
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** Modify event
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.



FORM f_display_grid_alv .

*  DATA: ls_variant LIKE disvariant.
*  ls_variant-report = sy-repid.

  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
*     is_variant           = ls_variant
      i_save               = ' '
      is_layout            = gs_layout
      it_toolbar_excluding = gt_exclude[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_datai[]
      it_sort              = gt_sort[]
      it_fieldcatalog      = gt_fcat[].

ENDFORM.

*FORM f_refresh_grid_alv
*   USING u_grid TYPE REF TO cl_gui_alv_grid..
*
*  DATA: ls_scroll TYPE lvc_s_stbl.
*
*  CLEAR: ls_scroll.
*  ls_scroll-row = 'X'.
*  ls_scroll-col = 'X'.
*
*  CALL METHOD u_grid->refresh_table_display
*    EXPORTING
*      is_stable      = ls_scroll
*      i_soft_refresh = 'X'.
*
*ENDFORM.
*
*FORM f_transfer_slis_to_lvc
*  CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv
*           ct_fcat     TYPE lvc_t_fcat..
*
*  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.
*
*  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
*    EXPORTING
*      it_fieldcat = ct_fieldcat
*    IMPORTING
*      et_fieldcat = lt_fieldcat.
*
*  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
*    EXPORTING
*      it_fieldcat_kkblo = lt_fieldcat
*    IMPORTING
*      et_fieldcat_lvc   = ct_fcat.
*
*ENDFORM.

*MODULE exit INPUT.
*  LEAVE TO SCREEN 0.
*ENDMODULE.

*MODULE user_command_300 INPUT.
*  PERFORM handle_user_command CHANGING sy-ucomm.
*
*ENDMODULE.
