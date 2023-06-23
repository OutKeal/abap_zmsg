FUNCTION zmsg_display.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(ZMSGNO) TYPE  ZMSGNO OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

*
  SELECT SINGLE * FROM zmsg_data_h
    WHERE msgno = @zmsgno
    INTO @gt_datah.
  IF sy-subrc NE 0.
    RAISE error.
  ENDIF.

  SELECT * FROM zmsg_data_i
    WHERE msgno = @zmsgno
    INTO TABLE @gt_datai[].

  SELECT SINGLE name_textc FROM user_addr
    WHERE bname = @sy-uname
    INTO @gt_datah-ernam_name.

  readonly = 'X'.

  CASE gt_object-object_type.

    WHEN 'REPLY'  .

      PERFORM free_screen_300.
      CALL SCREEN 300.
    WHEN OTHERS.
      CALL SCREEN 100 STARTING AT 10 5
                        ENDING AT 75 25.
  ENDCASE.


ENDFUNCTION.
