*&---------------------------------------------------------------------*
*& 包含               ZAPP_DATA_APPROVAL
*&---------------------------------------------------------------------*



FORM frm_approval USING p_action.
  CASE p_action.
    WHEN 'FIELD'.
      PERFORM frm_approval_field.
    WHEN 'DATA'.
      PERFORM frm_approval_data.
  ENDCASE.
ENDFORM.

FORM frm_approval_field.
  DATA: BEGIN OF ls_json,
          data TYPE TABLE OF zconf_app_fieldi,
        END OF ls_json.

  DATA:lt_data TYPE TABLE OF zconf_app_fieldi WITH HEADER LINE.

  REFRESH lt_data.

  SELECT * INTO TABLE @lt_data
    FROM zconf_app_fieldi
    WHERE app_id = 'APPROVAL'
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


FORM frm_approval_data.
  DATA: BEGIN OF ls_json,
          data TYPE TABLE OF zapp_flow_head,
        END OF ls_json.

  DATA:lt_data TYPE TABLE OF zapp_flow_head WITH HEADER LINE.

  REFRESH lt_data.

  SELECT h~* INTO TABLE @lt_data
    FROM zapp_flow_head AS h
    LEFT JOIN zapp_flow_item AS i ON h~appno = i~appno
    WHERE person = @gs_req_obj-ernam.
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
