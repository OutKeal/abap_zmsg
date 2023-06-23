*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMSG_OBJECT_CON.................................*
DATA:  BEGIN OF STATUS_ZMSG_OBJECT_CON               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMSG_OBJECT_CON               .
CONTROLS: TCTRL_ZMSG_OBJECT_CON
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZMSG_USER_CON...................................*
DATA:  BEGIN OF STATUS_ZMSG_USER_CON                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMSG_USER_CON                 .
CONTROLS: TCTRL_ZMSG_USER_CON
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMSG_OBJECT_CON               .
TABLES: *ZMSG_USER_CON                 .
TABLES: ZMSG_OBJECT_CON                .
TABLES: ZMSG_USER_CON                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
