*&---------------------------------------------------------------------*
*& Report ZMSG_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmsg_report.
TABLES:zmsg_sdisplay.
DATA:gt_sdisplay TYPE TABLE OF zmsg_sdisplay WITH HEADER LINE.

SELECT-OPTIONS:
  s_msgno FOR zmsg_sdisplay-msgno,
  s_object FOR zmsg_sdisplay-object,
  s_objid FOR zmsg_sdisplay-object_id,
  s_erdat FOR zmsg_sdisplay-erdat,
  s_uname FOR zmsg_sdisplay-uname,
  s_ernam FOR zmsg_sdisplay-ernam,
  s_status FOR zmsg_sdisplay-status.


START-OF-SELECTION.

  SELECT * FROM
    zmsg_data_h AS h
    INNER JOIN zmsg_data_i AS i ON h~msgno = i~msgno
    WHERE h~msgno IN @s_msgno
    AND object IN @s_object
    AND object_id IN @s_objid
    AND erdat IN @s_erdat
    AND ernam_name IN @s_uname
    AND status IN @s_status
    INTO CORRESPONDING FIELDS OF TABLE @gt_sdisplay.

  IF sy-subrc NE 0.
    MESSAGE s000(zmm) WITH 'NO DATA'.
  ENDIF.

  CALL FUNCTION 'ZMSG_REPORT'
    TABLES
      it_sdisplay       = gt_sdisplay[]
            .
