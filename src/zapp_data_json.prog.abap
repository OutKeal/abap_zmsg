*&---------------------------------------------------------------------*
*& 包含               ZAPP_DATA_JSON
*&---------------------------------------------------------------------*

"多层结构内表 格式化为JSON 格式
CLASS lcl_trex_json_serializer   DEFINITION DEFERRED.
"JSON 格式 转多层内表
CLASS lcl_trex_json_deserializer DEFINITION DEFERRED.

CLASS lcl_trex_json_serializer DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:class_constructor.

    METHODS:
      constructor
        IMPORTING !data TYPE data ,
      serialize,
      get_data RETURNING
                 VALUE(rval) TYPE string.
  PRIVATE SECTION.
    DATA:
      fragments TYPE trext_string,
      data_ref  TYPE REF TO data.
    CLASS-DATA: c_colon   TYPE string.
    CLASS-DATA: c_comma   TYPE string.

    METHODS:  recurse
      IMPORTING
        !data TYPE data .
ENDCLASS.


CLASS lcl_trex_json_deserializer DEFINITION.
  PUBLIC SECTION.
*"* public components of class CL_TREX_JSON_DESERIALIZER
*"* do not include other source files here!!!

    INTERFACES if_trex_serialization .

    METHODS deserialize
      IMPORTING
        !json TYPE string
      EXPORTING
        !abap TYPE any .
    METHODS deserialize_ref
      IMPORTING
        !json TYPE string
        !ref  TYPE REF TO object .

  PRIVATE SECTION.
*"* private components of class CL_TREX_JSON_DESERIALIZER
*"* do not include other source files here!!!

    METHODS deserialize_node
      IMPORTING
        !json   TYPE string
      CHANGING
        !offset TYPE i DEFAULT 0
        !node   TYPE any
      RAISING
        cx_trex_serialization .
    METHODS deserialize_object
      IMPORTING
        !json   TYPE string
      CHANGING
        !offset TYPE i DEFAULT 0
        !node   TYPE any
      RAISING
        cx_trex_serialization .
    METHODS deserialize_array
      IMPORTING
        !json   TYPE string
      CHANGING
        !offset TYPE i DEFAULT 0
        !node   TYPE any
      RAISING
        cx_trex_serialization .
ENDCLASS.

CLASS lcl_trex_json_deserializer IMPLEMENTATION.
  METHOD deserialize_array.
    DATA:
      l_done TYPE abap_bool,
      l_rec  TYPE REF TO data.
    FIELD-SYMBOLS:
      <itab> TYPE ANY TABLE,
      <rec>  TYPE data.

    ADD 1 TO offset . "skip [

    ASSIGN node TO <itab> .

* create record
    CREATE DATA l_rec LIKE LINE OF <itab> .
    ASSIGN l_rec->* TO <rec> .

    WHILE l_done = abap_false .
      CLEAR <rec> .
      deserialize_node(
        EXPORTING
          json = json
        CHANGING
          offset = offset
          node = <rec> ) .

      INSERT <rec> INTO TABLE <itab> .

      FIND REGEX ',|\]' IN SECTION OFFSET offset OF json MATCH OFFSET offset .
      IF sy-subrc <> 0 .
        RAISE EXCEPTION TYPE cx_trex_serialization .
      ENDIF .
      IF json+offset(1) = ']' .
        l_done = abap_true .
      ENDIF .
      ADD 1 TO offset .
    ENDWHILE .
  ENDMETHOD.

  METHOD deserialize.
    deserialize_node(
      EXPORTING
        json = json
      CHANGING
        node = abap ) .
  ENDMETHOD.

  METHOD deserialize_node.
    DATA:
      l_len    TYPE i,
      l_string TYPE string,
      l_number TYPE string.
    FIND REGEX '\{|\[|"([^":]*)"|(\d+)' IN SECTION OFFSET offset OF json "changed by jw
      MATCH OFFSET offset MATCH LENGTH l_len
      SUBMATCHES l_string l_number .

    IF sy-subrc <> 0 .
      RAISE EXCEPTION TYPE cx_trex_serialization .
    ENDIF .

    CASE json+offset(1) .
      WHEN '{' .
        deserialize_object(
          EXPORTING
            json = json
          CHANGING
            offset = offset
            node = node ) .

      WHEN '[' .
        deserialize_array(
          EXPORTING
            json = json
          CHANGING
            offset = offset
            node = node ) .

      WHEN '"' .
        node = l_string .
        ADD l_len TO offset .
      WHEN OTHERS . "0-9
        node = l_number .
        ADD l_len TO offset .
    ENDCASE .
  ENDMETHOD.

  METHOD deserialize_ref.
    DATA l_ref TYPE REF TO object .
    l_ref = ref .
    deserialize_node(
      EXPORTING
        json = json
      CHANGING
        node = l_ref ) .
  ENDMETHOD.

  METHOD deserialize_object.
    DATA:
      l_node_type TYPE REF TO cl_abap_typedescr,
      l_ref       TYPE REF TO object.

    ADD 1 TO offset . "skip {

    l_node_type = cl_abap_typedescr=>describe_by_data( node ) .

* prepare for dynamic access
    CASE l_node_type->kind .
      WHEN cl_abap_typedescr=>kind_ref .
        l_ref = node .
      WHEN cl_abap_typedescr=>kind_struct .

      WHEN OTHERS .
        RAISE EXCEPTION TYPE cx_trex_serialization .
    ENDCASE .

    DATA:
      l_done TYPE abap_bool,
      l_len  TYPE i,
      l_name TYPE string.

* handle each component
    WHILE l_done = abap_false .
      "find next key
      FIND REGEX '(\w+)([^":]*)\s*' IN SECTION OFFSET offset OF json "changed by jw
        MATCH OFFSET offset MATCH LENGTH l_len
        SUBMATCHES l_name .
      IF sy-subrc <> 0 .
        RAISE EXCEPTION TYPE cx_trex_serialization .
      ENDIF .
      ADD l_len TO offset .

      FIELD-SYMBOLS <comp> TYPE any .

*   dynamic binding to component
      TRANSLATE l_name TO UPPER CASE .
      CASE l_node_type->kind .
        WHEN cl_abap_typedescr=>kind_ref .
          ASSIGN l_ref->(l_name) TO <comp> .
        WHEN cl_abap_typedescr=>kind_struct .
          ASSIGN COMPONENT l_name OF STRUCTURE node TO <comp> .

        WHEN OTHERS .
          RAISE EXCEPTION TYPE cx_trex_serialization .
      ENDCASE .

      DATA:l_comp_type TYPE REF TO cl_abap_typedescr,
           l_ref_type  TYPE REF TO cl_abap_refdescr.

*   check component type
      l_comp_type = cl_abap_typedescr=>describe_by_data( <comp> ) .
      CASE l_comp_type->kind .
*     create instance if it's an oref
        WHEN cl_abap_typedescr=>kind_ref .
          l_ref_type ?= l_comp_type .
          l_comp_type = l_ref_type->get_referenced_type( ) .
          CREATE OBJECT <comp> TYPE (l_comp_type->absolute_name) .
      ENDCASE .

*   deserialize current component
      deserialize_node(
        EXPORTING
          json = json
        CHANGING
          offset = offset
          node = <comp> ) .

      FIND REGEX ',|\}' IN SECTION OFFSET offset OF json MATCH OFFSET offset .
      IF sy-subrc <> 0 .
        RAISE EXCEPTION TYPE cx_trex_serialization .
      ENDIF .

      IF json+offset(1) = '}' .
        l_done = abap_true .
      ENDIF .
      ADD 1 TO offset .
    ENDWHILE .

  ENDMETHOD.

ENDCLASS.


CLASS lcl_trex_json_serializer IMPLEMENTATION.
  "CLASS_CONSTRUCTOR
  METHOD class_constructor.
    cl_abap_string_utilities=>c2str_preserving_blanks(
                                                EXPORTING source = ': '
                                                IMPORTING dest   = c_colon ) .
    cl_abap_string_utilities=>c2str_preserving_blanks(
                                                EXPORTING source = ', '
                                                IMPORTING dest   = c_comma ) .
  ENDMETHOD.

  METHOD constructor.
    GET REFERENCE OF data INTO me->data_ref .
  ENDMETHOD.

  METHOD get_data.
    CONCATENATE LINES OF me->fragments INTO rval .
  ENDMETHOD.

  METHOD serialize.
    FIELD-SYMBOLS <data> TYPE data .

    ASSIGN me->data_ref->* TO <data> .
    recurse( <data> ).

  ENDMETHOD.

  METHOD recurse.
    DATA:      l_type
    TYPE c,      l_comps
    TYPE i,      l_lines
    TYPE i,      l_index
    TYPE i,      l_value
    TYPE string.
    FIELD-SYMBOLS:      <itab>
    TYPE ANY TABLE,      <comp>
    TYPE any.

    DESCRIBE FIELD data TYPE l_type COMPONENTS l_comps .

    IF l_type = cl_abap_typedescr=>typekind_table .
*   itab -> array
      APPEND '[' TO me->fragments .
      ASSIGN data TO <itab> .
      l_lines = lines( <itab> ) .
      LOOP AT <itab> ASSIGNING <comp> .
        ADD 1 TO l_index .
        recurse( <comp> ) .
        IF l_index < l_lines .
          APPEND c_comma TO me->fragments .
        ENDIF .
      ENDLOOP .
      APPEND ']' TO fragments .
    ELSE .
      IF l_comps IS INITIAL .

        l_value = data .
        REPLACE ALL OCCURRENCES OF '\' IN l_value WITH '\\' .
        REPLACE ALL OCCURRENCES OF '''' IN l_value WITH '\''' .
        REPLACE ALL OCCURRENCES OF '"' IN l_value WITH '\"' .
*        REPLACE ALL OCCURRENCES OF '&' IN l_value WITH '\&' .
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN l_value WITH '\r\n' .
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_value WITH '\n' .
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN l_value WITH '\t' .
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN l_value WITH '\b' .
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN l_value WITH '\f' .
        CONCATENATE '"' l_value '"' INTO l_value .
        APPEND l_value TO me->fragments .
      ELSE .
*     structure -> object
        DATA l_typedescr TYPE REF TO cl_abap_structdescr .
        FIELD-SYMBOLS <abapcomp> TYPE abap_compdescr .

        APPEND '{' TO me->fragments .
        l_typedescr ?= cl_abap_typedescr=>describe_by_data( data ).
        LOOP AT l_typedescr->components ASSIGNING <abapcomp> .
          l_index = sy-tabix .
          CONCATENATE '"' <abapcomp>-name '"' c_colon INTO l_value."changed by jw
          TRANSLATE l_value TO LOWER CASE .
          APPEND l_value TO me->fragments .
          ASSIGN COMPONENT <abapcomp>-name OF STRUCTURE data TO <comp> .
          recurse( <comp> ) .
          IF l_index < l_comps .
            APPEND c_comma TO me->fragments .
          ENDIF .
        ENDLOOP .
        APPEND '}' TO me->fragments .
      ENDIF .
    ENDIF .
  ENDMETHOD.

ENDCLASS.


FORM frm_json_out USING p_json CHANGING cs_json_data.

  DATA: json_des TYPE REF TO lcl_trex_json_deserializer.

  CHECK p_json IS NOT INITIAL.

  CREATE OBJECT json_des.

  CALL METHOD json_des->deserialize
    EXPORTING
      json = p_json
    IMPORTING
      abap = cs_json_data.
ENDFORM.


FORM frm_json_in USING ps_json_data CHANGING c_json .
  DATA: json_ser TYPE REF TO lcl_trex_json_serializer.

  CREATE OBJECT json_ser
    EXPORTING
      data = ps_json_data.

  CALL METHOD json_ser->serialize.

  CALL METHOD json_ser->get_data
    RECEIVING
      rval = c_json.
ENDFORM.
