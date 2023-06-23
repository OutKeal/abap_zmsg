FUNCTION zmsg_single_pop.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(MSGNO) TYPE  ZMSGNO
*"  TABLES
*"      IT_DATA STRUCTURE  ZMSG_SUSER
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA:lv_dis_msg TYPE c.
  DATA:lt_UINFO TYPE TABLE OF uinfo WITH HEADER LINE  .

  SELECT SINGLE * FROM zmsg_data_h
    INTO @DATA(ls_datah)
    WHERE msgno = @msgno.

  GET PARAMETER ID 'Z_DIS_MSG' FIELD lv_dis_msg.
  IF lv_dis_msg  = abap_true.
    SELECT  * FROM zmsg_data_i
      INTO TABLE @DATA(lt_datai)
      WHERE msgno = @msgno.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.

    CALL FUNCTION 'THUSRINFO'
      TABLES
        usr_tabl = lt_uinfo.

    DATA:ls_msg TYPE char40.
    DATA:i_header TYPE  so_obj_des.
    DATA:it_recipient LIKE TABLE OF  zafo_receivers WITH HEADER LINE.
    DATA: it_text LIKE TABLE OF  line WITH HEADER LINE.

    LOOP AT lt_datai INTO DATA(ls_data).
      CLEAR it_recipient.
      CLEAR i_header.
      CLEAR it_text.
      CLEAR it_recipient[].
      READ TABLE lt_uinfo WITH KEY mandt = sy-mandt bname = ls_data-uname.
      IF sy-subrc EQ 0.
*      i_header = '通知消息' && ls_datah-object_name .
        i_header = TEXT-013 && ls_datah-object_name .
        IF ls_datah-object_id IS NOT INITIAL.
          i_header = i_header && '(' && ls_datah-object_id  && ')'.
        ENDIF.

        it_text-line = ls_datah-text.
        APPEND it_text.

        CALL FUNCTION 'ZAFO_BCS_MAIL_INTER'
          EXPORTING
            uname      = ls_data-uname
            title      = i_header
          TABLES
            email_text = it_text.
        "ELSE.
        " PERFORM frm_send_ding_single USING ls_data ls_datah-text.
      ENDIF.
      "  PERFORM frm_send_ding_single USING ls_data ls_datah-text .
    ENDLOOP.
  ENDIF.
  PERFORM frm_send_ding_multiuser TABLES it_data USING ls_datah-text .

ENDFUNCTION.

FORM frm_send_ding_multiuser TABLES  it_data STRUCTURE zmsg_suser USING msg.
  DATA:ls_ding_string TYPE string.
  DATA:lt_dinguser TYPE TABLE OF zhrs_dinguser WITH HEADER LINE .
  DATA:return TYPE bapireturn1.


  CLEAR ls_ding_string.
  CLEAR lt_dinguser[].
  CLEAR return.

*  ls_ding_string = 'SAP系统加急消息提醒-' && sy-datum && ':' && msg.
  ls_ding_string = TEXT-011 && sy-datum && ':' && msg.

  LOOP AT it_data.
    lt_dinguser-pernr = it_data-uname.
    APPEND lt_dinguser.
  ENDLOOP.
  CALL FUNCTION 'ZHRI_ASYNCSEND_TO_DING'
    EXPORTING
*     in_dctyp    = '0'
*     IN_TOKEN    =
*     IN_AGENTID  =
      in_message  = ls_ding_string
    IMPORTING
      return      = return
    TABLES
      it_dinguser = lt_dinguser
    EXCEPTIONS
      error       = 1.


ENDFORM.

FORM frm_send_ding_single USING us_data STRUCTURE zmsg_data_i msg.
  DATA:ls_ding_string TYPE string.
  DATA:lt_dinguser TYPE TABLE OF zhrs_dinguser WITH HEADER LINE .
  DATA:return TYPE bapireturn1.


  CLEAR ls_ding_string.
  CLEAR lt_dinguser[].
  CLEAR return.

*  ls_ding_string = 'SAP系统加急消息提醒-' && sy-datum && ':' && msg.
  ls_ding_string = TEXT-011 && sy-datum && ':' && msg.

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
      it_dinguser = lt_dinguser
    EXCEPTIONS
      error       = 1.


  WRITE: /  us_data-name1  .
  WRITE: TEXT-012." '钉钉发送:'  .
  WRITE: return-message  .




ENDFORM.
