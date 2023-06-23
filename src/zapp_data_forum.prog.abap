*&---------------------------------------------------------------------*
*& 包含               ZAPP_DATA_FORUM
*&---------------------------------------------------------------------*



FORM frm_forum_list USING p_action.
  CASE p_action.
    WHEN 'FIELD'.
      PERFORM frm_forum_list_field.
    WHEN 'DATA'.
      PERFORM frm_forum_list_data.
  ENDCASE.
ENDFORM.

FORM frm_forum_list_field.
  DATA: BEGIN OF ls_json,
          data TYPE TABLE OF zconf_app_fieldi,
        END OF ls_json.

  DATA:lt_data TYPE TABLE OF zconf_app_fieldi WITH HEADER LINE.

  REFRESH lt_data.

  SELECT * INTO TABLE @lt_data
    FROM zconf_app_fieldi
    WHERE app_id = 'FORUM'
    ORDER BY location.
  IF sy-subrc = 0.
    ls_json-data = lt_data[].

    gs_res_obj-code = '200'.
    gs_res_obj-msg = '查询成功' .
    PERFORM frm_json_in USING ls_json CHANGING gs_res_obj-data_json.
  ELSE.
    gs_res_obj-code = '400'.
    gs_res_obj-msg = '无数据' .
  ENDIF.

ENDFORM.


FORM frm_forum_list_data.
  DATA: BEGIN OF ls_json,
          data TYPE TABLE OF zmsg_sdisplay,
        END OF ls_json.

  DATA:lt_data TYPE TABLE OF zmsg_sdisplay WITH HEADER LINE.

  REFRESH lt_data.


  SELECT * FROM zmsg_display
    WHERE object_type = 'REPLY'
     AND ( uname = @gs_req_obj-ernam OR ernam = @gs_req_obj-ernam  )

*      AND status = '' AND stop = ''
    INTO CORRESPONDING FIELDS OF TABLE @lt_data.

  IF sy-subrc = 0.

    ls_json-data = lt_data[].

    gs_res_obj-code = '200'.
    gs_res_obj-msg = '查询成功' .
    PERFORM frm_json_in USING ls_json CHANGING gs_res_obj-data_json.
  ELSE.
    gs_res_obj-code = '400'.
    gs_res_obj-msg = '无数据' .
  ENDIF.

ENDFORM.


FORM frm_forum USING p_action.
  CASE p_action.
    WHEN 'ITEM'.
      PERFORM frm_forum_get_item.
    WHEN 'ITEMP'.
      PERFORM frm_forum_post_item.
    WHEN 'CY_D'.
      PERFORM frm_forum_get_members.
  ENDCASE.
ENDFORM.


FORM frm_forum_get_item.
  DATA: BEGIN OF ls_json_data,
          msgno TYPE zmsgno,
        END OF ls_json_data.
  DATA:lt_datar TYPE TABLE OF zmsg_data_r WITH HEADER LINE.

  DATA: BEGIN OF ls_json,
          data TYPE TABLE OF zmsg_data_r,
        END OF ls_json.

  PERFORM frm_json_out USING gs_req_obj-data_json CHANGING ls_json_data.

  SELECT * FROM zmsg_data_r
    WHERE msgno = @ls_json_data-msgno
    INTO TABLE @lt_datar[].

  IF sy-subrc = 0.

    ls_json-data = lt_datar[].

    gs_res_obj-code = '200'.
    gs_res_obj-msg = '查询成功' .
    PERFORM frm_json_in USING ls_json CHANGING gs_res_obj-data_json.
  ELSE.
    gs_res_obj-code = '400'.
    gs_res_obj-msg = '无数据' .
  ENDIF.


ENDFORM.

FORM frm_forum_post_item.

ENDFORM.

FORM frm_forum_get_members.
  DATA: BEGIN OF ls_json_data,
          msgno TYPE zmsgno,
        END OF ls_json_data.
  DATA:lt_datai TYPE TABLE OF zmsg_data_i WITH HEADER LINE.
  DATA: BEGIN OF ls_json,
          data TYPE TABLE OF zmsg_data_i  ,
        END OF ls_json.

  PERFORM frm_json_out USING gs_req_obj-data_json CHANGING ls_json_data.

  SELECT * FROM zmsg_data_i
  WHERE msgno = @ls_json_data-msgno
  INTO TABLE @lt_datai[].
  IF sy-subrc = 0.
    ls_json-data = lt_datai[].
    gs_res_obj-code = '200'.
    gs_res_obj-msg = '查询成功' .
    PERFORM frm_json_in USING ls_json CHANGING gs_res_obj-data_json.
  ELSE.
    gs_res_obj-code = '400'.
    gs_res_obj-msg = '无数据' .
  ENDIF.
ENDFORM.
