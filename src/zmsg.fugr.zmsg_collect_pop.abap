FUNCTION zmsg_collect_pop.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(DATUM) TYPE  DATUM OPTIONAL
*"     VALUE(SET_START) TYPE  CHAR1 OPTIONAL
*"     VALUE(SEND_MAIL) TYPE  CHAR1 OPTIONAL
*"     VALUE(SEND_DING) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      RANGE_USER STRUCTURE  RANGEROW4USER OPTIONAL
*"----------------------------------------------------------------------
  DATA:lt_data TYPE TABLE OF zmsg_scollect WITH HEADER LINE.
  DATA:it_data TYPE TABLE OF zmsg_scollect WITH HEADER LINE.
  DATA:ct_data TYPE TABLE OF zmsg_scollect WITH HEADER LINE.
  DATA:lt_uinfo TYPE TABLE OF uinfo WITH HEADER LINE  .

*  CHECK set_start = 'X' OR send_mail = 'X' OR send_ding = 'X'.

  DATA:ls_msg TYPE char40.

  IF datum IS INITIAL.
    datum = sy-datum - 15.
  ENDIF.

  SELECT
    object,
    object_name,
    uname,
    name1,
    COUNT(*) AS count
    FROM zmsg_display
    WHERE status = ''
    AND erdat >= @datum
    GROUP BY object,object_name ,uname,name1
    INTO TABLE @it_data.

  CHECK sy-subrc EQ 0.

  LOOP AT it_data INTO DATA(ls_data).
    lt_data-uname = ls_data-uname.
    lt_data-name1 = ls_data-name1.
    APPEND lt_data.
    CLEAR lt_data.
  ENDLOOP.

  SORT lt_data BY uname.

  DELETE ADJACENT DUPLICATES FROM  lt_data COMPARING uname.

  SELECT h~object,i~*
    FROM zmsg_data_h AS h
    INNER JOIN zmsg_data_i AS i  ON h~msgno = i~msgno
    FOR ALL ENTRIES IN @lt_data
    WHERE uname = @lt_data-uname
    AND status = ''
    AND erdat >= @datum
  INTO CORRESPONDING FIELDS OF TABLE  @gt_datai_add.


  CLEAR gt_datai_add[].
  CLEAR gt_datai_modify[].


  CALL FUNCTION 'THUSRINFO'
    TABLES
      usr_tabl = lt_uinfo.


  LOOP AT lt_data.

    CLEAR ct_data[].

    LOOP AT it_data WHERE uname = lt_data-uname.
      APPEND it_data TO ct_data.
    ENDLOOP.

    READ TABLE lt_uinfo WITH KEY mandt = sy-mandt bname = lt_data-uname.
    IF sy-subrc EQ 0.
      PERFORM frm_send_mail_inter TABLES ct_data USING lt_data .
      CONTINUE.
    ELSE.

      IF set_start = 'X'.
        PERFORM frm_set_start_tcode USING lt_data-uname.
      ENDIF.

      IF send_mail = 'X'.
        PERFORM frm_send_mail TABLES ct_data USING lt_data .
      ENDIF.

*      IF send_ding = 'X'.
*        PERFORM frm_send_ding TABLES ct_data USING lt_data.
*      ENDIF.

    ENDIF.

    IF send_ding = 'X'.
      PERFORM frm_send_ding TABLES ct_data USING lt_data.
    ENDIF.
  ENDLOOP.

  PERFORM frm_update_itme.


ENDFUNCTION.
