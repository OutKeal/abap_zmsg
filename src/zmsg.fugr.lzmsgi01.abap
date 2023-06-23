*&---------------------------------------------------------------------*
*& Include LZMSGI01
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc1_modify INPUT.
*  MODIFY GT_DATAI
*    FROM GT_DATAI
*    INDEX TC1-CURRENT_LINE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc1_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC1'
                              'GT_DATAI'
                              ' '
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&---------------------------------------------------------------------*
*& 包含               LZMSGPAI
*&---------------------------------------------------------------------*

MODULE exit_command_0100 INPUT.

  IF editor IS BOUND.
    CALL METHOD editor->free.
  ENDIF.

  PERFORM free_screen_100.


  CASE sy-ucomm.
*    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.

*    WHEN 'CANCEL'.
*      CLEAR sy-ucomm.
*      LEAVE TO LIST-PROCESSING
*        AND RETURN TO SCREEN 0.
**      LEAVE TO SCREEN 0.
*    WHEN OTHERS.
**      LEAVE TO SCREEN 0.
*      LEAVE PROGRAM.
  ENDCASE.


  CLEAR sy-ucomm.

ENDMODULE.                 " EXIT_COMMAND  INPUT

MODULE exit_command_0300 INPUT.

  IF editor IS BOUND.
    CALL METHOD editor->free.
  ENDIF.

  PERFORM free_screen_300.


  CASE sy-ucomm.
*    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.

*    WHEN 'CANCEL'.
*      CLEAR sy-ucomm.
*      LEAVE TO LIST-PROCESSING
*        AND RETURN TO SCREEN 0.
**      LEAVE TO SCREEN 0.
*    WHEN OTHERS.
**      LEAVE TO SCREEN 0.
*      LEAVE PROGRAM.
  ENDCASE.


  CLEAR sy-ucomm.

ENDMODULE.                 " EXIT_COMMAND  INPUT

MODULE exit_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      CLEAR sy-ucomm.
      LEAVE TO LIST-PROCESSING
        AND RETURN TO SCREEN 0.
*      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*      LEAVE TO SCREEN 0.
      LEAVE PROGRAM.
  ENDCASE.
  CLEAR sy-ucomm.

ENDMODULE.                 " EXIT_COMMAND  INPUT

MODULE object INPUT.

  SELECT SINGLE * FROM zmsg_object_con
    WHERE object = @gt_datah-object
    INTO @gt_object.
  IF sy-subrc NE 0.
    CLEAR gt_object.
  ENDIF.

  gt_datah-object = gt_object-object.
  gt_datah-object_name = gt_object-object_name.
*  MOVE-CORRESPONDING gt_object TO gt_datah.
ENDMODULE.

MODULE tc_modify INPUT.
*  MODIFY gt_datai
*    INDEX tc-current_line.
ENDMODULE.                    "TC001_MODIFY INPUT

MODULE uname INPUT.
  DATA:text TYPE char20.
  DATA:lt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  DATA:lt_addr TYPE TABLE OF zapp_addr WITH HEADER LINE.


  IF gt_datai-uname IS INITIAL.
    CLEAR gt_datai.
    MODIFY gt_datai INDEX tc1-current_line.
    RETURN.
  ENDIF.


  SELECT SINGLE * FROM zapp_addr
   INTO lt_addr
   WHERE person EQ  gt_datai-uname.
  IF sy-subrc EQ 0.
    gt_datai-name1 = lt_addr-name.
    gt_datai-department = lt_addr-department.

    READ TABLE gt_datai INTO DATA(ls_datai) WITH KEY uname = gt_datai-uname.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.
    READ TABLE gt_datai TRANSPORTING NO FIELDS INDEX tc1-current_line.
    IF sy-subrc EQ 0.
      MODIFY gt_datai FROM gt_datai INDEX tc1-current_line.
    ELSE.
      APPEND gt_datai.
    ENDIF.
    RETURN.
  ENDIF.

  text = '%' && gt_datai-uname && '%'.
  SELECT * FROM zapp_addr
    INTO TABLE  lt_addr
    WHERE person LIKE text
    OR name LIKE text.

  IF sy-subrc NE 0.
    MESSAGE s003."无此用户
    RETURN.
  ENDIF.

  CLEAR lt_return_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'PERSON'
      pvalkey          = 'PERSON'
      dynprofield      = 'PERSON'
      value_org        = 'S'
*     multiple_choice  = 'X'
      callback_program = sy-repid
    TABLES
      value_tab        = lt_addr
      return_tab       = lt_return_tab[]
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.


  CHECK lt_return_tab[] IS NOT INITIAL.

  LOOP AT lt_return_tab.
    gt_datai-uname = lt_return_tab-fieldval.

    READ TABLE gt_datai INTO ls_datai WITH KEY uname = gt_datai-uname.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.

    READ TABLE lt_addr WITH KEY person = gt_datai-uname.
    IF sy-subrc EQ 0.
      gt_datai-name1 = lt_addr-name.
      gt_datai-department = lt_addr-department.
    ENDIF.


    READ TABLE gt_datai TRANSPORTING NO FIELDS INDEX tc1-current_line.
    IF sy-subrc EQ 0.
      MODIFY gt_datai FROM gt_datai INDEX tc1-current_line.
    ELSE.
      APPEND gt_datai.
    ENDIF.

  ENDLOOP.




ENDMODULE.


MODULE user_command_0400 INPUT.

  CASE sy-ucomm.
    WHEN 'ENTR'.
      GET CURSOR FIELD g_field.
      CASE g_field.

        WHEN 'GS_USER-NAME'  .
          CHECK gs_user-name IS NOT INITIAL.
          CLEAR gt_user_list[].
          LOOP AT gt_user.
            FIND gs_user-name IN gt_user-name.
            IF sy-subrc EQ 0.
              APPEND gt_user TO gt_user_list.
            ENDIF.
          ENDLOOP.
          CLEAR gs_user-name.
          IF gt_user_list[] IS INITIAL.
            MESSAGE s004."无数据
          ENDIF.

        WHEN 'GS_USER-DEPARTMENT'  .
          CHECK gs_user-department IS NOT INITIAL.
          CLEAR gt_user_list[].
          LOOP AT gt_user.
            FIND gs_user-department IN gt_user-department.
            IF sy-subrc EQ 0.
              APPEND gt_user TO gt_user_list.
            ENDIF.
          ENDLOOP.
          CLEAR gs_user-department.
          IF gt_user_list[] IS INITIAL.
            MESSAGE s004."无数据
          ENDIF.
        WHEN 'GS_USER-ZPOSITION'  .
          CHECK gs_user-zposition IS NOT INITIAL.
          CLEAR gt_user_list[].
          LOOP AT gt_user.
            FIND gs_user-zposition IN gt_user-zposition.
            IF sy-subrc EQ 0.
              APPEND gt_user TO gt_user_list.
            ENDIF.
          ENDLOOP.
          CLEAR gs_user-zposition.
          IF gt_user_list[] IS INITIAL.
            MESSAGE s004."无数据
          ENDIF.
      ENDCASE.

    WHEN 'OK'.


      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE_LISE'.

      PERFORM frm_save_list.

    WHEN 'LIST'.

      PERFORM frm_get_list.


  ENDCASE.

ENDMODULE.

FORM frm_save_list.
  DATA:ls_name TYPE char30.

  CLEAR gt_msg_user_list[].
  IF gt_user_sel[] IS INITIAL.
    MESSAGE s015 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR g_error.

  PERFORM frm_pop_get_name CHANGING ls_name.

  CHECK g_error IS INITIAL.

  LOOP AT gt_user_sel.
    gt_msg_user_list-crnam = sy-uname.
    gt_msg_user_list-name1 = ls_name.
    gt_msg_user_list-line_id = sy-tabix.
    gt_msg_user_list-person = gt_user_sel-person.
    gt_msg_user_list-name = gt_user_sel-name.
    gt_msg_user_list-department = gt_user_sel-department.
    gt_msg_user_list-zposition = gt_user_sel-zposition.
    APPEND gt_msg_user_list.
    CLEAR gt_msg_user_list.
  ENDLOOP.

  IF sy-subrc EQ 0.
    MODIFY zmsg_user_list  FROM TABLE  gt_msg_user_list[].
    MESSAGE s013.
    COMMIT WORK.
  ENDIF.

ENDFORM.

FORM frm_get_list.
  DATA: BEGIN OF lt_name1 OCCURS 0,
          name1 TYPE name1,
        END OF lt_name1.

  DATA:   lt_return      TYPE TABLE OF ddshretval WITH HEADER LINE.
  CLEAR gt_msg_user_list[].

  SELECT * FROM zmsg_user_list
    INTO TABLE gt_msg_user_list
    WHERE CRNAM = sy-uname."当前操作人创建的用户组才能选择
  CHECK sy-subrc EQ 0.

  LOOP AT gt_msg_user_list.
    lt_name1-name1 = gt_msg_user_list-name1.
    COLLECT lt_name1.
  ENDLOOP.

  CHECK lt_name1[] IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'NAME1'
      dynprofield      = 'NAME1'
      value_org        = 'S'
      callback_program = sy-repid
*     callback_form    = 'CB_FORM_301_IDNLF'
    TABLES
      value_tab        = lt_name1
      return_tab       = lt_return
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.

  IF sy-subrc EQ 0.
    READ TABLE lt_return INDEX 1.

    LOOP AT gt_msg_user_list WHERE name1 = lt_return-fieldval.
      READ TABLE gt_user_sel WITH KEY person = gt_msg_user_list-person.
      IF sy-subrc EQ 0.
        CONTINUE.
      ELSE.
        CLEAR gt_user_sel.
        gt_user_sel-person = gt_msg_user_list-person.
        gt_user_sel-name = gt_msg_user_list-name.
        gt_user_sel-department = gt_msg_user_list-department.
        gt_user_sel-zposition = gt_msg_user_list-zposition.
        APPEND gt_user_sel.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.




FORM frm_pop_get_name CHANGING name.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.

  CLEAR ls_flds.
  ls_flds-tabname = 'ZMSG_USER_LIST'.
  ls_flds-fieldname = 'NAME1'.
  ls_flds-field_obl = 'X'.
  APPEND ls_flds TO lt_flds.


  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = '请输入自定义用户组名称'
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0 AND p_gv_ret_code <> 'A'.

    READ TABLE lt_flds INTO ls_flds INDEX 1.
    name = ls_flds-value.
  ELSE.
    MESSAGE s029 DISPLAY LIKE 'E'."已取消操作
    g_error = 'X'.
    RETURN.
  ENDIF.

  SELECT SINGLE * INTO @DATA(ls_user_list) FROM zmsg_user_list
    WHERE name1 = @name.
  IF sy-subrc EQ 0.
    MESSAGE s016 DISPLAY LIKE 'E'.
    g_error = 'X'.
  ENDIF.


ENDFORM.
