*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHE_LGORT.......................................*
DATA:  BEGIN OF STATUS_ZHE_LGORT                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHE_LGORT                     .
CONTROLS: TCTRL_ZHE_LGORT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZHE_LGORT                     .
TABLES: ZHE_LGORT                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
