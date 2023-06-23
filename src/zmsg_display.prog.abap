*&---------------------------------------------------------------------*
*& Report ZMSG_DISPLAY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmsg_display.


PARAMETERS: p_msgno TYPE zmsgno MEMORY ID zmsgno.


START-OF-SELECTION.

  CHECK p_msgno IS NOT INITIAL.

  SELECT SINGLE * FROM zmsg_data_h WHERE msgno = @p_msgno INTO @DATA(ls_line).
  CHECK sy-subrc = 0.

  SELECT SINGLE * FROM zmsg_object_con WHERE object = @ls_line-object INTO @DATA(ls_object).
  CHECK sy-subrc = 0.

  CASE ls_object-object_type.
    WHEN 'REPLY'.
      CALL FUNCTION 'ZMSG_REPLY'
       EXPORTING
         ZMSGNO        = p_msgno
                .

    WHEN OTHERS.
      CALL FUNCTION 'ZMSG_DISPLAY'
        EXPORTING
          zmsgno = p_msgno
        EXCEPTIONS
          error  = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
  ENDCASE.
