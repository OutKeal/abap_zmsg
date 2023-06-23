FUNCTION zmsg_reply.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(ZMSGNO) DEFAULT 1000000125
*"----------------------------------------------------------------------
  CLEAR   gt_reply[].
  CLEAR   gt_list[].

  g_model = 'R'.
  SELECT SINGLE * FROM zmsg_data_h
    WHERE msgno = @zmsgno
    INTO @gt_datah.
  IF sy-subrc NE 0.
    RAISE error.
  ENDIF.

  SELECT * FROM zmsg_data_i
    WHERE msgno = @zmsgno
    INTO TABLE @gt_datai[].

  READ TABLE gt_datai WITH KEY uname = sy-uname.

  SELECT * FROM zmsg_data_r
    WHERE msgno = @zmsgno
    INTO TABLE @gt_datar[].

  SORT gt_datar BY erdat erzet.
  SELECT SINGLE name_textc FROM user_addr
    WHERE bname = @sy-uname
    INTO @gt_datah-ernam_name.


  PERFORM frm_html_start.

  LOOP AT gt_datar.

    PERFORM frm_add_list USING gt_datar .

  ENDLOOP.

  PERFORM frm_html_end.

  PERFORM free_screen_300.
  CALL SCREEN '300'.


ENDFUNCTION.


FORM frm_html_start.

  CLEAR gt_list[].

  gt_list-line = '<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" />'.
  APPEND gt_list.

  gt_list-line =   '<link href="https://www.antexsoft.com/sap_mail/static/lib/bootstrap/3.4.1/css/bootstrap.min.css" rel="stylesheet">' .
  APPEND gt_list.

  gt_list-line = '<script src="https://www.antexsoft.com/sap_mail/static/lib/jquery/3.5.1/jquery.min.js"></script>'.
  APPEND gt_list.

  gt_list-line = '<script>$(document).ready(function() {setTimeout(function() {$(window).scrollTop($("#container")[0].scrollHeight);}, 10)      });  </script></head>'.
  APPEND gt_list.

  gt_list-line = '<body><div id = "container" style="overflow-x:hide;padding:1rem;padding-bottom:30px;">'.

  APPEND gt_list.

ENDFORM.


FORM frm_html_end.

*  gt_list-line = '</div></body></html>'.
*  APPEND gt_list.

ENDFORM.

FORM frm_add_list USING gt_datar TYPE zmsg_data_r .
  DATA:ls_mod(2) TYPE i.

  ls_mod = gt_datar-line_id MOD 6 + 1.


  DATA:lt_text TYPE TABLE OF line WITH HEADER LINE.
  CLEAR lt_text[].
  CASE g_model .
    WHEN 'C'.
      PERFORM frm_replace_html CHANGING gt_datar-text.

      CALL FUNCTION 'CONVERT_STRING_TO_TAB'
        EXPORTING
          i_string         = gt_datar-text
          i_tabline_length = 255
        TABLES
          et_table         = lt_text.

      LOOP AT lt_text.
        gt_list-line = lt_text-line.
        APPEND gt_list.
      ENDLOOP.

    WHEN 'R'.
      PERFORM frm_init_color.

      READ TABLE gt_color INDEX ls_mod.

      gt_list-line =
                '<span style="color:' && gt_color-line && '">' &&  gt_datar-name1
                 && '&nbsp;' && gt_datar-erdat+0(4) && '-' && gt_datar-erdat+4(2) && '-' && gt_datar-erdat+6(2)
                 && '&nbsp;' && gt_datar-erzet+0(2) && ':' && gt_datar-erzet+2(2) && ':' && gt_datar-erzet+4(2)
                 && '</span><br/>'.
      APPEND gt_list.

      PERFORM frm_replace_html CHANGING gt_datar-text.

      CALL FUNCTION 'CONVERT_STRING_TO_TAB'
        EXPORTING
          i_string         = gt_datar-text
          i_tabline_length = 255
        TABLES
          et_table         = lt_text.

      LOOP AT lt_text.
        gt_list-line = lt_text-line.
        APPEND gt_list.
      ENDLOOP.

  ENDCASE.

ENDFORM.

FORM frm_replace_html CHANGING data.

  DO.
    REPLACE '%26nbsp;' WITH '&nbsp;' INTO gt_datar-text.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.

  DO .
    REPLACE '%26amp;' WITH '&' INTO gt_datar-text .
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.

  DO.
    REPLACE '%3D' WITH '=' INTO gt_datar-text.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.
  DO.
    REPLACE '%3F' WITH '?' INTO gt_datar-text.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.

FORM frm_init_color.

  CHECK gt_color[] IS INITIAL.
  gt_color-line = '#0000FF'.APPEND gt_color.
  gt_color-line = '#8A2BE2'.APPEND gt_color.
  gt_color-line = '#A52A2A'.APPEND gt_color.
  gt_color-line = '#FF0000'.APPEND gt_color.
  gt_color-line = '#000000'.APPEND gt_color.
  gt_color-line = '#8B5A2B'.APPEND gt_color.

ENDFORM.
