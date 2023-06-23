*&---------------------------------------------------------------------*
*& 包含               ZAPP_DATA_REQUEST
*&---------------------------------------------------------------------*

FORM frm_request USING p_i_bustyp
                        p_i_ernam
                        p_i_aedat
                        p_i_erzet
                        p_i_json.
  PERFORM frm_clear .

  gs_req_obj-bustyp = p_i_bustyp.
  gs_req_obj-ernam = p_i_ernam.
  gs_req_obj-erdat = p_i_aedat.
  gs_req_obj-erzet = p_i_erzet.
  gs_req_obj-data_json = p_i_json.

ENDFORM.


FORM frm_clear .
  CLEAR g_error.

  CLEAR:gs_req_obj,gs_res_obj.

ENDFORM.
