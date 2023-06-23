FUNCTION zmsg_save_data.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IS_DATAH) TYPE  ZMSG_DATA_H
*"     VALUE(IV_STRING) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      IT_DATA STRUCTURE  ZMSG_SUSER
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CLEAR gt_datah.
  CLEAR gt_datah[].
  CLEAR gt_datai[].

  SELECT SINGLE * INTO gt_object
    FROM zmsg_object_con
    WHERE object = is_datah-object.

  IF sy-subrc NE 0 .
    RAISE error.
  ENDIF.

  IF it_data[] IS INITIAL.
    RAISE error.
  ENDIF.

  IF iv_string IS INITIAL.
    PERFORM frm_get_next_msgno USING '02' CHANGING gt_datah-msgno.
  ELSE.
    PERFORM frm_get_next_msgno USING '01' CHANGING gt_datah-msgno.
  ENDIF.

  IF gt_datah-msgno IS INITIAL.
    RAISE error.
  ENDIF.

  gt_datah-object_id = is_datah-object_id.
  gt_datah-text = is_datah-text.
  gt_datah-object = gt_object-object.
  gt_datah-object_name = gt_object-object_name.
  gt_datah-urgent = is_datah-urgent.
  IF gt_datah-object_id IS INITIAL.
    gt_datah-object_id = gt_datah-zzpino.
  ENDIF.
  gt_datah-zzpino = is_datah-zzpino.
  gt_datah-zkunnr_mat = is_datah-zkunnr_mat.
  gt_datah-erdat = sy-datum.
  gt_datah-erzet = sy-uzeit.
  gt_datah-ernam = sy-uname.

  SELECT SINGLE name_textc FROM user_addr
    WHERE bname = @sy-uname
    INTO @gt_datah-ernam_name.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO gt_datai.
    gt_datai-msgno = gt_datah-msgno.
    gt_datai-msgnr = sy-tabix.
    gt_datai-status = ''.
    gt_datai-text = TEXT-010."'未读'.

*    IF iv_string IS NOT INITIAL.
*      gt_datai-status = ''.
*    ENDIF.

    APPEND gt_datai.
    CLEAR gt_datai.
  ENDLOOP.


  IF iv_string IS NOT INITIAL.

    gt_datar-msgno = gt_datah-msgno.
    gt_datar-line_id = 0.
    gt_datar-name1 = gt_datah-ernam_name.
    gt_datar-ernam = sy-uname.
    gt_datar-erdat = sy-datum.
    gt_datar-erzet = sy-uzeit.
    gt_datar-text = iv_string.
    MODIFY zmsg_data_r FROM gt_datar.
    CLEAR gt_datar.
  ENDIF.

  SORT gt_datai BY UNAME.
  DELETE ADJACENT DUPLICATES FROM gt_datai COMPARING UNAME.

  MODIFY zmsg_data_h FROM gt_datah.
  MODIFY zmsg_data_i FROM TABLE gt_datai.

  COMMIT WORK AND WAIT.

  PERFORM frm_set_msg
      USING 'S' 'ZMSG' '001' gt_datah-msgno '' '' '' CHANGING es_return.


  IF is_datah-urgent IS NOT INITIAL.
    CALL FUNCTION 'ZMSG_SINGLE_POP'
      EXPORTING
        msgno   = gt_datah-msgno
      TABLES
        it_data = it_data[]
*     EXCEPTIONS
*       ERROR   = 1
*       OTHERS  = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  PERFORM binary_relation USING gt_datah.
  PERFORM binary_relation_dhd USING gt_datah.

ENDFUNCTION.

FORM frm_set_msg USING type id number  msgv1 msgv2 msgv3 msgv4 CHANGING es_return TYPE bapiret2..
  es_return-type = type.
  es_return-id = id.
  es_return-number = number.
  es_return-message_v1 = msgv1.
  es_return-message_v2 = msgv2.
  es_return-message_v3 = msgv3.
  es_return-message_v4 = msgv4.
ENDFORM.

FORM binary_relation USING ls_head TYPE zmsg_data_h.

  DATA:ls_borident1 TYPE borident.
  DATA:ls_borident2 TYPE borident.

  CHECK ls_head-object IS NOT INITIAL.
  CHECK ls_head-object_id IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(ls_tojtb)
    FROM tojtb
    WHERE name = @ls_head-object.

  CHECK sy-subrc EQ 0.

  ls_borident1-objtype = 'ZMSG'.
  ls_borident1-objkey = ls_head-msgno.

  ls_borident2-objtype = ls_head-object.
  ls_borident2-objkey = ls_head-object_id.


  CALL FUNCTION 'BINARY_RELATION_CREATE'
    EXPORTING
      obj_rolea      = ls_borident1
      obj_roleb      = ls_borident2
      relationtype   = 'VORL'
*     FIRE_EVENTS    = 'X'
*     IMPORTING
*     BINREL         =
*     TABLES
*     BINREL_ATTRIB  =
    EXCEPTIONS
      no_model       = 1
      internal_error = 2
      unknown        = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  COMMIT WORK AND WAIT.
ENDFORM.

FORM binary_relation_dhd USING ls_head TYPE zmsg_data_h.

  DATA:ls_borident1 TYPE borident.
  DATA:ls_borident2 TYPE borident.

  CHECK ls_head-object EQ 'ZSC'.
  CHECK ls_head-zzpino IS NOT INITIAL.


  SELECT SINGLE * INTO @DATA(ls_tojtb)
    FROM tojtb
    WHERE name = @ls_head-object.

  CHECK sy-subrc EQ 0.

  ls_borident1-objtype = 'ZMSG'.
  ls_borident1-objkey = ls_head-msgno.



  SELECT * FROM ztpp0089 WHERE zzpino = @ls_head-zzpino INTO TABLE @DATA(lt_table).
  CHECK sy-subrc EQ 0.

  LOOP AT lt_table INTO DATA(ls_table).
    ls_borident2-objtype = 'ZPPDHD'.
    ls_borident2-objkey = ls_table-zppdhd.
    CALL FUNCTION 'BINARY_RELATION_CREATE'
      EXPORTING
        obj_rolea      = ls_borident1
        obj_roleb      = ls_borident2
        relationtype   = 'VORL'
*       FIRE_EVENTS    = 'X'
*     IMPORTING
*       BINREL         =
*     TABLES
*       BINREL_ATTRIB  =
      EXCEPTIONS
        no_model       = 1
        internal_error = 2
        unknown        = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    COMMIT WORK AND WAIT.
  ENDLOOP.

  ls_borident2-objtype = 'ZSC'.
  ls_borident2-objkey = ls_head-zzpino.
  CALL FUNCTION 'BINARY_RELATION_CREATE'
    EXPORTING
      obj_rolea      = ls_borident1
      obj_roleb      = ls_borident2
      relationtype   = 'VORL'
*     FIRE_EVENTS    = 'X'
*     IMPORTING
*     BINREL         =
*     TABLES
*     BINREL_ATTRIB  =
    EXCEPTIONS
      no_model       = 1
      internal_error = 2
      unknown        = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  COMMIT WORK AND WAIT.
ENDFORM.
