*****           Implementation of object type ZAFO                 *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
    zmsgno LIKE zmsg_data_h-msgno,
  END OF key.
end_data object. " Do not change.. DATA is generated

begin_method gosaddobjects changing container.
DATA:
      service(255),
      busidentifs LIKE borident OCCURS 0.
DATA: ls_borident TYPE borident.

CLEAR ls_borident.

ls_borident-logsys = space.
ls_borident-objtype = 'ZMSG'.
ls_borident-objkey = object-key.
APPEND ls_borident TO BusIdentifs.

swc_get_element container 'Service' service.
swc_set_table container 'BusIdentifs' busidentifs.
end_method.

begin_method display changing container.

SET PARAMETER ID 'ZMSGNO' FIELD object-key-zmsgno.
CALL TRANSACTION 'ZMSG3' AND SKIP FIRST SCREEN.

end_method.

begin_method existencecheck changing container.

DATA:ls_msgno TYPE zmsgno.
SELECT SINGLE msgno  INTO ls_msgno FROM zmsg_data_h WHERE msgno = object-key-zmsgno.
IF sy-subrc NE 0.
  exit_object_not_found.
ENDIF.


end_method.
