FUNCTION zmsg_choise_user.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      ET_USER STRUCTURE  ZAPP_ADDR OPTIONAL
*"----------------------------------------------------------------------

  CLEAR gt_department[].
  CLEAR gt_user_sel[].
  CLEAR gt_user_list[].

  SELECT * FROM zapp_manager
    INTO TABLE @DATA(lt_manager).

  SELECT * FROM zapp_addr
    WHERE department IS NOT INITIAL
    INTO TABLE @gt_user.

  gt_department[] = gt_user[].

  SORT gt_department BY department.
  DELETE ADJACENT DUPLICATES FROM gt_department COMPARING department.

  LOOP AT gt_department.
    READ TABLE lt_manager INTO DATA(ls_manager) WITH KEY department = gt_department-department.
    IF sy-subrc EQ 0.
      gt_department-person = ls_manager-line_id.
    ELSE.
      gt_department-person = '9999999'.
    ENDIF.
    gt_department-name = icon_select_all.
    MODIFY gt_department.
  ENDLOOP.

  SORT gt_department BY person.

  CALL SCREEN 400 STARTING AT 10 5
                  ENDING AT 100 25.

  IF sy-ucomm = 'OK'.
    et_user[] = gt_user_sel[].
  ENDIF.

ENDFUNCTION.
