FUNCTION zmsg_report.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_SDISPLAY STRUCTURE  ZMSG_SDISPLAY
*"----------------------------------------------------------------------

  gt_sdisplay[] = it_sdisplay[].
  LOOP AT gt_sdisplay.
*    gt_sdisplay-reply = icon_delivery.
    WHILE 1 = 1.
      REPLACE '//' IN gt_sdisplay-text WITH ';'.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDWHILE.


    IF gt_sdisplay-status = 'C'  .
      gt_sdisplay-icon = icon_complete.
      gt_sdisplay-icontext = text-001."'已阅'.
    ENDIF.

    IF gt_sdisplay-status = 'D'  .
      gt_sdisplay-icon = icon_delete.
      gt_sdisplay-icontext = text-008." '已删除'.
    ENDIF.

    IF gt_sdisplay-status = '' OR  gt_sdisplay-status = 'A' .
      gt_sdisplay-icon = ICON_led_yellow.
      gt_sdisplay-icontext = text-003."'待阅'.
    ENDIF.
    MODIFY gt_sdisplay.
  ENDLOOP.

  CALL SCREEN 200.

ENDFUNCTION.
