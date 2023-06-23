FUNCTION zmsg_print.
*"----------------------------------------------------------------------
*"*"本地接口：
*"----------------------------------------------------------------------

  CLEAR gt_print_h[].
  CLEAR gt_print_i[].
  gs_print_h-afono = gt_datah-msgno.

  gs_print_h-title_txt1 = '主题'.
  gs_print_h-title_txt2 = gt_datah-text.

  gs_print_h-head_txt1 = '类型:'.
  gs_print_h-head_txt2 = gt_datah-object_name.

  gs_print_h-head_txt3 = '销售合同:'.
  gs_print_h-head_txt4 = gt_datah-zzpino.

  gs_print_h-foot_txt1 = '创建人:'.
  SELECT SINGLE name INTO gs_print_h-foot_txt2 FROM zapp_addr WHERE person = gt_datah-ernam.

  gs_print_h-foot_txt3 = '创建日期:'.
  PERFORM frm_get_date_name USING gt_datah-erdat  CHANGING gs_print_h-foot_txt4.

  APPEND gs_print_h TO gt_print_h.

  DATA: ls_text TYPE string.
  DATA: lt_text TYPE TABLE OF string WITH HEADER LINE.

  LOOP AT gt_datar.
    CHECK gt_datar-text IS NOT INITIAL.

    PERFORM frm_replace_html CHANGING gt_datar-text.

    FIND '<img' IN gt_datar-text .
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

*    DO .
*      REPLACE PCRE   '<a+.*?>.*/?</a>' IN gt_datar-text WITH ''.
*      IF sy-subrc NE 0.
*        EXIT.
*      ENDIF.
*    ENDDO.

    FIND '<table' IN gt_datar-text .
    IF sy-subrc EQ 0.
      DO .
        REPLACE '</tr>' IN gt_datar-text WITH '\n&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDDO.

      DO .
        REPLACE '</td>' IN gt_datar-text WITH '&nbsp;&nbsp;&nbsp;&nbsp;'.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    DO .
      REPLACE '<br>' IN gt_datar-text WITH '\n'.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '<br />' IN gt_datar-text WITH '\n'.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.

    DO .
      REPLACE '<br/>' IN gt_datar-text WITH '\n'.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE PCRE '<table.*?>' IN gt_datar-text WITH '表格:'.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

    ENDDO.

    DO .
      REPLACE PCRE '<[a-zA-Z]+.*?>|</[a-zA-Z]*?>|<[a-zA-Z]*/?>' IN gt_datar-text WITH ''.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.

    DO .
      REPLACE PCRE '[\ \r\t]+' IN gt_datar-text WITH ' '.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

    ENDDO.

    DO .
      REPLACE '&nbsp;' IN gt_datar-text WITH ' '.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.

    DO .
      REPLACE '&nbsp' IN gt_datar-text WITH ''.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.

    CHECK gt_datar-text IS NOT INITIAL.

    gt_print_i-afono = gt_datar-msgno.
    gt_print_i-col01 = gt_datar-name1.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = gt_datar-erdat
      IMPORTING
        date_external            = gt_print_i-col02
      EXCEPTIONS
        date_internal_is_invalid = 1
        OTHERS                   = 2.

*    gt_print_i-col02 = gt_print_i-col02 && |\n| && gt_datar-erzet+0(2) && ':' && gt_datar-erzet+2(2) && ':' && gt_datar-erzet+4(2).


    SPLIT gt_datar-text AT '\n' INTO TABLE lt_text.

    LOOP AT lt_text INTO ls_text .
      DO .

        DATA(len) = strlen( ls_text ) .
        IF ls_text IS NOT INITIAL.
          gt_print_i-col03 = ls_text.

          APPEND gt_print_i.
        ENDIF.


        IF len <= 100.
          EXIT.
        ELSE.
          ls_text = ls_text+100.
        ENDIF.
      ENDDO.
    ENDLOOP.

*    DO .
*
*      DATA(len) = strlen( gt_datar-text ) .
*      gt_print_i-col03 = gt_datar-text.
*
*      APPEND gt_print_i.
*
*      IF len <= 100.
*        EXIT.
*      ELSE.
*        gt_datar-text = gt_datar-text+100.
*      ENDIF.
*    ENDDO.

  ENDLOOP.

  PERFORM frm_initial_smartforms USING 'ZMSG_PRINT'.


  LOOP AT gt_print_h INTO gs_print_h .

    AT FIRST.

      control_parameters-no_close = 'X'.

    ENDAT.
    AT LAST.

      control_parameters-no_close = space.

    ENDAT.

    PERFORM frm_print_report.

    control_parameters-no_open = 'X'.

  ENDLOOP.
ENDFUNCTION.

FORM frm_initial_smartforms USING smartform TYPE tdsfname .
  CLEAR control_parameters.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = smartform "SMARTFORMS名字
    IMPORTING
      fm_name            = func_module_name "得到的函数名
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. "FRM_INITIAL_SMARTFORM

*---------------------------------------------------------------------*
FORM frm_print_report.
  DATA: l_ssfcrespd TYPE ssfcrespd.

  DATA:lt_print_i TYPE TABLE OF zafo_print_i.
  lt_print_i = gt_print_i[].
  DELETE lt_print_i WHERE afono <> gs_print_h-afono.

  CALL FUNCTION func_module_name
    EXPORTING
      control_parameters   = control_parameters
      gs_print_h           = gs_print_h
    IMPORTING
      document_output_info = l_ssfcrespd
    TABLES
      gt_print_i           = lt_print_i
    EXCEPTIONS
      user_cancled         = 4.

  IF sy-subrc = '4'.
  ELSEIF sy-subrc <> 0 .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. "FRM_PRINT_REPORT

FORM frm_get_date_name USING date CHANGING name1.
  DATA:year TYPE char4.
  DATA:month TYPE char2.
  DATA:day TYPE char2.
  year = date+0(4).
  month = date+4(2).
  day = date+6(2).
  IF month+0(1) = '0'.
    month+0(1) = ''.
  ENDIF.

  IF day+0(1) = '0'.
    day+0(1) = ''.
  ENDIF.

  name1 = year && '年' && month && '月' && day && '日'.
  CONDENSE name1 NO-GAPS.

ENDFORM.
