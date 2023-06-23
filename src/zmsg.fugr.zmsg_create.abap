FUNCTION zmsg_create.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(OBJECT) TYPE  ZMSG_OBJECT DEFAULT 'MSG'
*"     VALUE(OBJECT_ID) TYPE  ZMSG_OBJECT_ID OPTIONAL
*"     VALUE(TEXT) TYPE  ZMSG_TEXT OPTIONAL
*"     VALUE(URGENT) TYPE  ZMSG_URGENT OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      IT_DATA STRUCTURE  ZMSG_SUSER OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

*
  CLEAR gt_datah.
  CLEAR gt_datah[].
  CLEAR gt_datai.
  CLEAR gt_datai[].
  CLEAR gt_object.
  CLEAR readonly.
  g_model = 'C'.

  SELECT SINGLE * INTO gt_object
    FROM zmsg_object_con
    WHERE object = object.
  IF sy-subrc NE 0.
    gt_object-object = 'MSG'.
    SELECT SINGLE * INTO gt_object
      FROM zmsg_object_con
      WHERE object = gt_object-object.
  ENDIF.

  MOVE-CORRESPONDING it_data[] TO gt_datai[].

  gt_datah-object_id = object_id.
  gt_datah-text = text.
  gt_datah-object = gt_object-object.
  gt_datah-urgent = urgent.
  gt_datah-object_name = gt_object-object_name.

  SELECT SINGLE name,department FROM zapp_addr
    WHERE person = @sy-uname
    INTO (@gt_datah-ernam_name,@gt_datai-department).

  PERFORM frm_html_start.


  CASE gt_object-object_type.

    WHEN 'REPLY'  .

      gt_datai-uname = sy-uname.
      gt_datai-name1 = gt_datah-ernam_name.
      append gt_datai.
      clear gt_datai.

      PERFORM free_screen_300.
      CALL SCREEN 300.
    WHEN OTHERS.
      CLEAR gt_datai.
      CALL SCREEN 100 STARTING AT 10 5
                        ENDING AT 75 25.

  ENDCASE.

  es_return = gs_return.

ENDFUNCTION.
