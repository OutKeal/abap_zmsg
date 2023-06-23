FUNCTION-POOL zmsg MESSAGE-ID zmsg. "MESSAGE-ID ..

* INCLUDE LZMSGD...                          " Local class definition

DATA:gt_object TYPE TABLE OF zmsg_object_con WITH HEADER LINE.
DATA:gt_datah TYPE TABLE OF zmsg_data_h WITH HEADER LINE.
DATA:gt_datai TYPE TABLE OF zmsg_data_i WITH HEADER LINE.
DATA:gt_datar TYPE TABLE OF zmsg_data_r WITH HEADER LINE.

DATA:g_tc_lines  LIKE sy-loopc.

*CONTROLS: tc TYPE TABLEVIEW USING SCREEN 100.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC1' ITSELF
CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'TC1'
DATA: g_tc1_lines  LIKE sy-loopc.

DATA: ok_code LIKE sy-ucomm.

TYPES: BEGIN OF ty_zemp.
         INCLUDE STRUCTURE zmsg_sdisplay.
TYPES: clr TYPE char4, "可以控制行颜色
       END OF ty_zemp.

DATA: gt_sdisplay TYPE TABLE OF ty_zemp WITH HEADER LINE.

FIELD-SYMBOLS <fs_sdisplay> TYPE ty_zemp.

DATA:gt_datai_modify TYPE TABLE OF zmsg_data_i WITH HEADER LINE .

DATA: BEGIN OF gt_datai_add  OCCURS 0,
        object TYPE zmsg_object.
        INCLUDE STRUCTURE zmsg_data_i.
DATA:END OF gt_datai_add.

DATA editor TYPE REF TO cl_gui_textedit.

DATA msg_container TYPE REF TO cl_gui_custom_container.


DATA: myevent_tab          TYPE cntl_simple_events,
      myevent              TYPE cntl_simple_event,
      edurl(2048),
      edframe(255),
      edaction(256),
      edgetdata            TYPE string,

      edpostdataline(1024),
      postdata_tab         TYPE cnht_post_data_tab,
      edquery_table        TYPE cnht_query_table.

DATA:readonly TYPE char1.

DATA:gs_return TYPE bapiret2.

DATA:gt_reply  TYPE TABLE OF line WITH HEADER LINE.
DATA:gt_list TYPE TABLE OF line WITH HEADER LINE.

DATA:g_model TYPE c.

DATA:gt_department TYPE TABLE OF zapp_addr WITH HEADER LINE.
DATA:gt_user TYPE TABLE OF zapp_addr WITH HEADER LINE.
DATA:gt_user_list TYPE TABLE OF zapp_addr WITH HEADER LINE.
DATA:gt_user_sel TYPE TABLE OF zapp_addr WITH HEADER LINE.
DATA:gt_msg_user_list TYPE TABLE OF zmsg_user_list WITH HEADER LINE.

DATA:gs_user TYPE zapp_addr .

DATA:g_field TYPE char30.

DATA: g_grid_200              TYPE REF TO cl_gui_alv_grid,
      gt_fcat_200             TYPE lvc_t_fcat,
      gs_layout_200           TYPE lvc_s_layo,
      gt_sort_200             TYPE lvc_t_sort,
      gt_exclude_200          TYPE ui_functions,
      g_docking_container_200 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_200  TYPE REF TO cl_gui_custom_container,
      g_container_200         TYPE REF TO cl_gui_container,
      g_splitter_200          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_200           TYPE REF TO cl_gui_toolbar.

DATA: g_grid     TYPE REF TO cl_gui_alv_grid,
      gt_fcat    TYPE lvc_t_fcat,
      gs_layout  TYPE lvc_s_layo,
      gt_sort    TYPE lvc_t_sort,
      gt_exclude TYPE ui_functions,
*      g_docking_container TYPE REF TO cl_gui_docking_container,
*      g_cumtom_container  TYPE REF TO cl_gui_custom_container,
*      g_container_1       TYPE REF TO cl_gui_container,
*      g_container_2       TYPE REF TO cl_gui_container,
*      g_splitter          TYPE REF TO cl_gui_splitter_container,
      g_toolbar  TYPE REF TO cl_gui_toolbar.
DATA:gt_color TYPE STANDARD TABLE OF line WITH HEADER LINE   .

DATA:g_start_date TYPE datum .
DATA:g_end_date TYPE datum.
DATA:g_uname TYPE sy-uname.
DATA:g_zzpino TYPE zzpino.
DATA:tt_zzpino TYPE zttppzzpino.
DATA:g_error TYPE char1.

DATA:g_object_type TYPE zmsg_object_type.
RANGES:r_object_type FOR zmsg_display-object_type.

DATA:gs_print_h TYPE  zafo_print_h  .
DATA:gt_print_h TYPE TABLE OF zafo_print_h WITH HEADER LINE .
DATA:gt_print_i TYPE TABLE OF zafo_print_i WITH HEADER LINE .

DATA: func_module_name   TYPE rs38l_fnam,
      control_parameters TYPE ssfctrlop.

DATA:gs_print TYPE zafo_print.
