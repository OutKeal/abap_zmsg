*&---------------------------------------------------------------------*
*& 包含               ZAPP_DATA_RESPONSE
*&---------------------------------------------------------------------*


FORM frm_response CHANGING p_o_code p_o_msg p_o_json.

  IF gs_res_obj-code IS INITIAL .
    gs_res_obj-code = '500'.
    gs_res_obj-msg = '查询失败！'.
  ENDIF.

  p_o_code = gs_res_obj-code.
  p_o_msg = gs_res_obj-msg.
  p_o_json = gs_res_obj-data_json.

ENDFORM.
