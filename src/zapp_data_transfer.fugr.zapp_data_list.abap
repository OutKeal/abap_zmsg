FUNCTION zapp_data_list.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_BUSTYP) TYPE  CHAR10
*"     VALUE(I_ACTION) TYPE  CHAR5
*"     VALUE(I_ERNAM) TYPE  ERNAM
*"     VALUE(I_ERDAT) TYPE  ERDAT
*"     VALUE(I_ERZET) TYPE  ERZET
*"     VALUE(I_JSON) TYPE  STRING
*"  EXPORTING
*"     VALUE(O_CODE) TYPE  CHAR3
*"     VALUE(O_MSG) TYPE  CHAR100
*"     VALUE(O_JSON) TYPE  STRING
*"----------------------------------------------------------------------

  PERFORM frm_request USING i_bustyp i_ernam i_erdat i_erzet i_json.

  CASE i_bustyp.
    WHEN 'APPROVAL'.
      PERFORM frm_approval USING i_action.
    WHEN 'FORUMLIST'.
      PERFORM frm_forum_list USING i_action.
    WHEN 'FORUM'.
      PERFORM frm_forum USING i_action.
  ENDCASE.

  PERFORM frm_response CHANGING o_code o_msg o_json.
ENDFUNCTION.
