FUNCTION Z_GNVKDFS_ALV_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VKDFS_CALLBACK_PROGRAM) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VKDFS_CALLBACK_PF_STATUS_SET) TYPE  SLIS_FORMNAME
*"         OPTIONAL
*"     VALUE(I_VKDFS_CALLBACK_USER_COMMAND) TYPE  SLIS_FORMNAME
*"         OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_PROGNAME) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_INT_TABNAME) TYPE  SLIS_TABNAME
*"         OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_INCLNAME) LIKE  TRDIR-NAME OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_STRUCTURE_NAM) LIKE  DD02L-TABNAME
*"         OPTIONAL
*"     VALUE(I_DEFAULT_DATA) LIKE  RV60A STRUCTURE  RV60A
*"     VALUE(I_VARIANT) LIKE  DISVARIANT STRUCTURE  DISVARIANT
*"         OPTIONAL
*"     VALUE(I_GRID_CONTROL) DEFAULT 'X'
*"     VALUE(I_INVOICE_LIST) DEFAULT SPACE
*"  TABLES
*"      I_FVKDFI STRUCTURE  VKDFIFGN
*"--------------------------------------------------------------------
  DATA : PT_FIELDCAT               TYPE SLIS_T_FIELDCAT_ALV.
  DATA : PS_LAYOUT                 TYPE SLIS_LAYOUT_ALV.
  DATA : PT_OUTTAB                 TYPE SD_ALV .
  DATA : PT_EVENTS                 TYPE SLIS_T_EVENT.
  DATA : LD_CALLBACK_PROGRAM       LIKE SY-REPID
                                   VALUE 'SAPLV60P'.
  DATA : LD_CALLBACK_PF_STATUS_SET TYPE  SLIS_FORMNAME
                                   VALUE 'GNVKDFS_PF_STATUS_SET'.
  DATA : LD_CALLBACK_USER_COMMAND  TYPE  SLIS_FORMNAME
                                   VALUE 'GNVKDFS_USER_COMMAND'.
  DATA : LD_FIELDCAT_WA TYPE SLIS_FIELDCAT_ALV.

  GT_FVKDFIGN[] = I_FVKDFI[].
  GD_DEFAULT_DATA = I_DEFAULT_DATA.
  IF NOT I_VKDFS_CALLBACK_PROGRAM IS INITIAL.
    LD_CALLBACK_PROGRAM = I_VKDFS_CALLBACK_PROGRAM.
  ENDIF.
  IF NOT I_VKDFS_CALLBACK_PF_STATUS_SET IS INITIAL.
    LD_CALLBACK_PF_STATUS_SET = I_VKDFS_CALLBACK_PF_STATUS_SET.
  ENDIF.
  IF NOT I_VKDFS_CALLBACK_USER_COMMAND IS INITIAL.
    LD_CALLBACK_USER_COMMAND = I_VKDFS_CALLBACK_USER_COMMAND.
  ENDIF.
  if not i_invoice_list is initial.
    gd_invoice_list = i_invoice_list.
  endif.
  PS_LAYOUT-GET_SELINFOS      = 'X'.
  PS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  PS_LAYOUT-DETAIL_POPUP      = 'X'.
  PS_LAYOUT-BOX_FIELDNAME     = 'SELKZ'.
  PS_LAYOUT-NO_KEYFIX         = 'X'.
  PS_LAYOUT-KEY_HOTSPOT       = 'X'.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME         = 'SAPLV60P'
            I_INTERNAL_TABNAME     = 'GT_FVKDFIGN'
            I_INCLNAME             = 'SAPLV60P'
       CHANGING
            CT_FIELDCAT            = PT_FIELDCAT
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  READ TABLE PT_FIELDCAT INTO LD_FIELDCAT_WA
                         WITH KEY FIELDNAME = 'STATF'.
  IF SY-SUBRC IS INITIAL.
    LD_FIELDCAT_WA-ICON = 'X'.
    MODIFY PT_FIELDCAT FROM LD_FIELDCAT_WA INDEX SY-TABIX.
    GT_FVKDFIGN-STATF = '@0Y@'.
    MODIFY GT_FVKDFIGN TRANSPORTING STATF WHERE STATF IS INITIAL.
  ENDIF.
  if not i_grid_control is initial.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
*           I_INTERFACE_CHECK           = ' '
              I_CALLBACK_PROGRAM          = ld_callback_program
             I_CALLBACK_PF_STATUS_SET    = LD_CALLBACK_PF_STATUS_SET
             I_CALLBACK_USER_COMMAND     = LD_CALLBACK_USER_COMMAND
*           I_CALLBACK_TOP_OF_PAGE      = ' '
*           I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*           I_STRUCTURE_NAME            =
*           I_BACKGROUND_ID             = ' '
*           I_GRID_TITLE                =
             IS_LAYOUT                   = PS_LAYOUT
             IT_FIELDCAT                 = PT_FIELDCAT
*           IT_EXCLUDING                =
*           IT_SPECIAL_GROUPS           =
*           IT_SORT                     =
*           IT_FILTER                   =
*           IS_SEL_HIDE                 =
             I_DEFAULT                   = 'X'
             I_SAVE                      = 'A'
             IS_VARIANT                  = I_variant
*           IT_EVENTS                   =
*           IT_EVENT_EXIT               =
*           IS_PRINT                    =
*           IS_REPREP_ID                =
*           I_SCREEN_START_COLUMN       = 0
*           I_SCREEN_START_LINE         = 0
*           I_SCREEN_END_COLUMN         = 0
*           I_SCREEN_END_LINE           = 0
*      IMPORTING
*           E_EXIT_CAUSED_BY_CALLER     =
*           ES_EXIT_CAUSED_BY_USER      =
        TABLES
             T_OUTTAB                    = gt_fvkdfign
        EXCEPTIONS
             PROGRAM_ERROR               = 1
             OTHERS                      = 2
             .
  else.
*   if i_variant-variant = '1STANDGRID'.
*     i_variant-variant = '1STANDARD'.
*   ENDIF.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
         EXPORTING
              I_CALLBACK_PROGRAM       = LD_CALLBACK_PROGRAM
              I_CALLBACK_PF_STATUS_SET = LD_CALLBACK_PF_STATUS_SET
              I_CALLBACK_USER_COMMAND  = LD_CALLBACK_USER_COMMAND
              IS_LAYOUT                = PS_LAYOUT
              IT_FIELDCAT              = PT_FIELDCAT
              I_DEFAULT                = 'X'
              I_SAVE                   = 'A'
              IS_VARIANT               = I_VARIANT
         TABLES
              T_OUTTAB                 = GT_FVKDFIGN
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  endif.
ENDFUNCTION.
*---------------------------------------------------------------------*
*       FORM VKDFS_PF_STATUS_SET
*---------------------------------------------------------------------*
*       This form sets the PF-Status and is required by the           *
*       general list viewer to display another status                 *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM GNVKDFS_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  DATA: LT_EXTAB TYPE SLIS_T_EXTAB.
  DATA:    BEGIN OF EXCTAB OCCURS 1,
             OKCOD(4)          TYPE C,
           END   OF EXCTAB.

  REFRESH EXCTAB.

  APPEND LINES OF EXCTAB   TO LT_EXTAB.
  APPEND LINES OF RT_EXTAB TO LT_EXTAB.

  SET PF-STATUS 'GNVKDFS_ALV' EXCLUDING LT_EXTAB.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM VKDFS_USER_COMMAND
*---------------------------------------------------------------------*
FORM GNVKDFS_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA : LD_SAMMG LIKE VBSK-SAMMG.
  CASE R_UCOMM.
    WHEN 'SLEG'.
      PERFORM FCODE_SLEG.
    WHEN 'PROT'.
      PERFORM GNVKDFS_FCODE_PROT
                USING GD_SAMMG gd_invoice_list.
    WHEN 'NSEL'.
      PERFORM GNVKDFS_FCODE_NSEL
                USING R_UCOMM
                      RS_SELFIELD
                      GD_DEFAULT_DATA.
    WHEN 'SAMO'.
      PERFORM GNVKDFS_FCODE_SAMO
                TABLES
                  GT_FVKDFIGN
                USING
                      RS_SELFIELD
                      GD_DEFAULT_DATA
                      SPACE
                      gd_invoice_list     .
      R_UCOMM = 'BACK'.
      RS_SELFIELD-EXIT = 'X'.
    WHEN 'SAMD'.
      PERFORM GNVKDFS_FCODE_SAMD
                TABLES
                  GT_FVKDFIGN
                USING
                      RS_SELFIELD
                      GD_DEFAULT_DATA
                      SPACE
                      gd_invoice_list.
*     R_UCOMM = 'BACK'.
*     RS_SELFIELD-EXIT = 'X'.
    WHEN 'SAMH'.
      PERFORM GNVKDFS_FCODE_SAMH
                USING R_UCOMM
                      RS_SELFIELD
                      GD_DEFAULT_DATA
                      gd_invoice_list.
    WHEN 'NDE '.
      PERFORM GNVKDFS_FCODE_NSEL
                USING R_UCOMM
                      RS_SELFIELD
                      GD_DEFAULT_DATA.
    WHEN 'ANZB'.
      PERFORM GNVKDFS_FCODE_ANZB
                USING
                  RS_SELFIELD.
    WHEN 'SAMQ'.
      PERFORM GNVKDFS_FCODE_SAMQ
                USING R_UCOMM
                      RS_SELFIELD
                      GD_DEFAULT_DATA
                      gd_invoice_list.
    WHEN 'SAMS'.
      PERFORM GNVKDFS_FCODE_SAMS
                USING R_UCOMM
                      RS_SELFIELD
                      GD_DEFAULT_DATA
                      gd_invoice_list.
    WHEN '&NTE'.
  ENDCASE.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM VKDFS_USER_COMMAND
*---------------------------------------------------------------------*
FORM GNBATCH_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_NSEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_NSEL
       USING R_UCOMM LIKE SY-UCOMM
             RS_SELFIELD TYPE SLIS_SELFIELD
             GD_DEFAULT_DATA.
  DATA : ANSWER(1).
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            TITEL     = TEXT-PT2
            TEXTLINE1 = TEXT-054
            TEXTLINE2 = TEXT-055
       IMPORTING
            ANSWER    = ANSWER(1).
  IF ANSWER(1) = 'J'.
    R_UCOMM = 'BACK'.
    RS_SELFIELD-EXIT = 'X'.
  ENDIF.

ENDFORM.                               " VKDFS_FCODE_NSEL

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_SAMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_SAMO
       TABLES
         GT_FVKDFIGN STRUCTURE VKDFIFGN
       USING
         RS_SELFIELD TYPE SLIS_SELFIELD
         GD_DEFAULT_DATA STRUCTURE RV60A
         LD_NO_DIRECT_POSTING LIKE RV50S-BFORM
         gd_invoice_list.

  DATA : LT_VBFS LIKE VBFS OCCURS 10 WITH HEADER LINE.
  DATA : LT_VBSS LIKE VBSS OCCURS 10 WITH HEADER LINE.
  DATA : LT_VBSK LIKE VBSK OCCURS 1  WITH HEADER LINE.
  DATA : LT_FVKDFI LIKE GT_FVKDFIGN OCCURS 10 WITH HEADER LINE.
  DATA : LT_VBFS_SUBRC LIKE SY-SUBRC.
  DATA : GT_FVKDFI_SUBRC LIKE SY-SUBRC.
  DATA : GT_FVKDFI_TABIX LIKE SY-TABIX.
  DATA : LD_NO_NEW_RUN(1) VALUE ' '.
  data : ld_smart like vbsk-smart value 'F'.

  LT_FVKDFI[] = GT_FVKDFIGN[].
  LOOP AT LT_FVKDFI WHERE SELKZ NE GC_CHARX.
    DELETE LT_FVKDFI INDEX SY-TABIX.
  ENDLOOP.

  IF NOT GD_SAMMG IS INITIAL.
    LD_NO_NEW_RUN = 'X'.
  ENDIF.
  if gd_invoice_list is initial.
  SET PARAMETER ID 'VF' FIELD SPACE.
  else.
  SET PARAMETER ID 'VFL' FIELD SPACE.
  ld_smart = 'R'.
  endif.
  CALL FUNCTION 'GN_COLLECTIVE_RUN_EXECUTE'
       EXPORTING
            V60P_INPUT_RV60A  = GD_DEFAULT_DATA
            v60p_input_smart  = ld_smart
            id_invoice_list   = gd_invoice_list
            ID_NO_NEW_RUN     = LD_NO_NEW_RUN
       IMPORTING
            V60P_OUTPUT_VBSK  = LT_VBSK
       TABLES
            GN_INPUT_VKDFIF = LT_FVKDFI
            V60P_OUTPUT_VBFS  = LT_VBFS
            V60P_OUTPUT_VBSS  = LT_VBSS
       EXCEPTIONS
            OTHERS            = 1.

  IF LD_NO_NEW_RUN IS INITIAL.
    GD_SAMMG = LT_VBSK-SAMMG.
    APPEND LT_VBSK.
  ENDIF.

*  IF SY-BATCH IS INITIAL.
*    LOOP AT LT_FVKDFI.
*      READ TABLE LT_VBFS WITH KEY VBELN = LT_FVKDFI-VBELN.
*      LT_VBFS_SUBRC = SY-SUBRC.
*      READ TABLE GT_FVKDFIGN WITH KEY VBELN = LT_FVKDFI-VBELN.
*      GT_FVKDFI_SUBRC = SY-SUBRC.
*      IF GT_FVKDFI_SUBRC IS INITIAL.
*        GT_FVKDFI_TABIX = SY-TABIX.
*        IF LT_VBFS_SUBRC IS INITIAL.
**          CLEAR GT_FVKDFIGN-SELKZ.
*          GT_FVKDFIGN-STATF = '@0W@'.
*          GT_FVKDFIGN-SAMMG = LT_VBFS-SAMMG.
*        ELSE.
*          GT_FVKDFIGN-SELKZ = '1'.
*          GT_FVKDFIGN-STATF = '@0V@'.
*        ENDIF.
*        MODIFY GT_FVKDFIGN INDEX GT_FVKDFI_TABIX.
*      ENDIF.
*    ENDLOOP.
*    RS_SELFIELD-REFRESH = 'X'.
*  ELSE.
*    REFRESH LT_VBSK.
*    APPEND LT_VBSK.

  CALL FUNCTION 'VBSK_ALV_DISPLAY'
       TABLES
            I_VBSK = LT_VBSK
       EXCEPTIONS
            OTHERS = 1.

ENDFORM.                               " VKDFS_FCODE_SAMO

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_SAMD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_SAMD
       TABLES
         GT_FVKDFIGN STRUCTURE VKDFIFGN
       USING
         RS_SELFIELD TYPE SLIS_SELFIELD
         GD_DEFAULT_DATA STRUCTURE RV60A
         LD_NO_DIRECT_POSTING LIKE RV50S-BFORM
         ld_invoice_list                      .

  DATA : LT_VBFS LIKE VBFS OCCURS 10 WITH HEADER LINE.
  DATA : LT_VBSS LIKE VBSS OCCURS 10 WITH HEADER LINE.
  DATA : LT_VBSK LIKE VBSK OCCURS 1  WITH HEADER LINE.
  DATA : LT_FVKDFI LIKE GT_FVKDFIGN OCCURS 10 WITH HEADER LINE.
  DATA : LT_VBFS_SUBRC LIKE SY-SUBRC.
  DATA : GT_FVKDFI_SUBRC LIKE SY-SUBRC.
  DATA : GT_FVKDFI_TABIX LIKE SY-TABIX.
  DATA : LD_NO_NEW_RUN(1).
  data : ld_smart like vbsk-smart value 'F'.

  LT_FVKDFI[] = GT_FVKDFIGN[].
  LOOP AT LT_FVKDFI WHERE SELKZ NE GC_CHARX.
    DELETE LT_FVKDFI INDEX SY-TABIX.
  ENDLOOP.

  IF NOT GD_SAMMG IS INITIAL.
    LD_NO_NEW_RUN = GC_CHARX.
  ENDIF.
  if ld_invoice_list is initial.
  SET PARAMETER ID 'VF' FIELD SPACE.
  else.
  SET PARAMETER ID 'VFL' FIELD SPACE.
  ld_smart = 'R'.
  endif.
  CALL FUNCTION 'GN_COLLECTIVE_RUN_EXECUTE'
       EXPORTING
            V60P_INPUT_RV60A  = GD_DEFAULT_DATA
            v60p_input_smart  = ld_smart
            ID_NO_NEW_RUN     = LD_NO_NEW_RUN
            id_invoice_list   = ld_invoice_list
       IMPORTING
            V60P_OUTPUT_VBSK  = LT_VBSK
       TABLES
            GN_INPUT_VKDFIF = LT_FVKDFI
            V60P_OUTPUT_VBFS  = LT_VBFS
            V60P_OUTPUT_VBSS  = LT_VBSS
       EXCEPTIONS
            OTHERS            = 1.

  IF LD_NO_NEW_RUN IS INITIAL.
    GD_SAMMG = LT_VBSK-SAMMG.
  ENDIF.

  IF SY-BATCH IS INITIAL.
    LOOP AT LT_FVKDFI.
      READ TABLE LT_VBFS WITH KEY VBELN = LT_FVKDFI-VBELN.
      LT_VBFS_SUBRC = SY-SUBRC.
      READ TABLE GT_FVKDFIGN WITH KEY VBELN = LT_FVKDFI-VBELN.
      GT_FVKDFI_SUBRC = SY-SUBRC.
      IF GT_FVKDFI_SUBRC IS INITIAL.
        GT_FVKDFI_TABIX = SY-TABIX.
        IF LT_VBFS_SUBRC IS INITIAL.
*          CLEAR GT_FVKDFIGN-SELKZ.
          GT_FVKDFIGN-STATF = '@0W@'.
          GT_FVKDFIGN-SAMMG = LT_VBFS-SAMMG.
        ELSE.
          GT_FVKDFIGN-SELKZ = '1'.
          GT_FVKDFIGN-STATF = '@0V@'.
        ENDIF.
        MODIFY GT_FVKDFIGN INDEX GT_FVKDFI_TABIX.
      ENDIF.
    ENDLOOP.
    RS_SELFIELD-REFRESH = 'X'.
  ELSE.
    REFRESH LT_VBSK.
    APPEND LT_VBSK.

    CALL FUNCTION 'VBSK_ALV_DISPLAY'
         TABLES
              I_VBSK = LT_VBSK
         EXCEPTIONS
              OTHERS = 1.

    DATA : LD_VF_VBELN LIKE GT_FVKDFIGN-VBELN.

    if ld_invoice_list is initial.
    GET PARAMETER ID 'VF' FIELD LD_VF_VBELN.
    else.
    GET PARAMETER ID 'VF' FIELD LD_VF_VBELN.
    endif.
    IF NOT LD_VF_VBELN IS INITIAL.
      LOOP AT GT_FVKDFIGN WHERE SELKZ = 'X'.
        GT_FVKDFIGN-SELKZ = '-'.
        MODIFY GT_FVKDFIGN INDEX SY-TABIX.
      ENDLOOP.
      RS_SELFIELD-REFRESH = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                               " VKDFS_FCODE_SAMO

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_SAMH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_SAMH USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD
                            P_GD_DEFAULT_DATA LIKE RV60A
                            ld_invoice_list             .
  DATA : AC_CODE LIKE SY-SUBRC.
  DATA : RV60A LIKE RV60A.
  DATA : LD_FVKDFI_TABIX LIKE SY-TABIX.
  DATA : OK-CODE(5)          TYPE C.
  DATA:    BEGIN OF POSTAB OCCURS 50.
          INCLUDE STRUCTURE VKDFI.
  DATA:    V_FKDAT LIKE VKDFI-FKDAT,
           V_FKART LIKE VKDFI-FKART,
           ACTIV(1)     TYPE N.
  DATA:    END   OF POSTAB.

  PERFORM AUTHORITY_CHECK USING '19' 'X' CHANGING AC_CODE.
  OK-CODE = R_UCOMM.
  CLEAR XKOMFKGN.
  REFRESH XKOMFKGN.
  LOOP AT GT_FVKDFIGN WHERE SELKZ = GC_CHARX.
    LD_FVKDFI_TABIX = SY-TABIX.
    MOVE-CORRESPONDING GT_FVKDFIGN TO XKOMFKGN.
*   XKOMFKGN-SELDAT = GT_FVKDFIGN-FKDAT.
    XKOMFKGN-ERDAT = GT_FVKDFIGN-FKDAT.
     APPEND XKOMFKGN.
  ENDLOOP.
  RV60A-FKART  = P_GD_DEFAULT_DATA-FKART.
  RV60A-FKDAT  = P_GD_DEFAULT_DATA-FKDAT.
  RV60A-FBUDA  = P_GD_DEFAULT_DATA-FBUDA.
  RV60A-PRSDT  = P_GD_DEFAULT_DATA-PRSDT.
  if ld_invoice_list is initial.
  EXPORT OK-CODE
         RV60A-FKART
         RV60A-FKDAT
         RV60A-FBUDA
         RV60A-PRSDT
         XKOMFKGN
         TO MEMORY ID 'GNBILLDL'.
  CALL TRANSACTION 'VF01' AND SKIP FIRST SCREEN.
  IMPORT OK-CODE
         RV60A-FKART
         RV60A-FKDAT
         RV60A-FBUDA
         RV60A-PRSDT
         XKOMFKGN
         FROM MEMORY ID 'GNBILLDL'.
  FREE MEMORY ID 'GNBILLDL'.
  else.
  EXPORT OK-CODE
         RV60A-FKART
         RV60A-FKDAT
         RV60A-FBUDA
         RV60A-PRSDT
         XKOMFKGN
         TO MEMORY ID 'GNINVLDL'.
  CALL TRANSACTION 'VF21' AND SKIP FIRST SCREEN.
  IMPORT OK-CODE
         RV60A-FKART
         RV60A-FKDAT
         RV60A-FBUDA
         RV60A-PRSDT
         XKOMFKGN
         FROM MEMORY ID 'GNINVLDL'.
  FREE MEMORY ID 'GNINVLDL'.
  endif.
  LOOP AT GT_FVKDFIGN WHERE SELKZ = GC_CHARX.
    LD_FVKDFI_TABIX = SY-TABIX.
    READ TABLE XKOMFKGN WITH KEY VGBEL = GT_FVKDFIGN-VBELN.
    IF SY-SUBRC IS INITIAL.
      IF XKOMFKGN-FKSTK = 'C' . " OR XKOMFKGN-FKSAK = 'C'.
        GT_FVKDFIGN-SELKZ = '1'.
        GT_FVKDFIGN-STATF = '@5B@'.
      ELSE.
        GT_FVKDFIGN-STATF = '@5C@'.
      ENDIF.
      MODIFY GT_FVKDFIGN INDEX LD_FVKDFI_TABIX.
    ENDIF.
  ENDLOOP.
  RS_SELFIELD-REFRESH = 'X'.
ENDFORM.                               " VKDFS_FCODE_SAMH

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_ENDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_ENDE.

ENDFORM.                               " VKDFS_FCODE_ENDE

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_ANZB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_ANZB
       USING
         RS_SELFIELD TYPE SLIS_SELFIELD.
  IF NOT RS_SELFIELD-TABINDEX IS INITIAL.
    READ TABLE GT_FVKDFIGN INDEX RS_SELFIELD-TABINDEX.
    IF SY-SUBRC IS INITIAL.
      CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
           EXPORTING
                VBELN = GT_FVKDFIGN-VBELN.
    ELSE.
      MESSAGE I460(VR).
    ENDIF.
  ELSE.
    MESSAGE I460(VR).
  ENDIF.
ENDFORM.                               " VKDFS_FCODE_ANZB

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_SAMQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_SAMQ USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD
                            P_GD_DEFAULT_DATA LIKE RV60A
                            ld_invoice_list             .
  DATA : AC_CODE LIKE SY-SUBRC.
  DATA : RV60A LIKE RV60A.
  DATA : LD_FVKDFI_TABIX LIKE SY-TABIX.
  DATA : OK-CODE(5)          TYPE C.
  DATA:    BEGIN OF POSTAB OCCURS 50.
          INCLUDE STRUCTURE VKDFI.
  DATA:    V_FKDAT LIKE VKDFI-FKDAT,
           V_FKART LIKE VKDFI-FKART,
           ACTIV(1)     TYPE N.
  DATA:    END   OF POSTAB.
  PERFORM AUTHORITY_CHECK USING '01' 'X' CHANGING AC_CODE.
  LOOP AT GT_FVKDFIGN WHERE SELKZ = GC_CHARX.
    LD_FVKDFI_TABIX = SY-TABIX.
    OK-CODE = R_UCOMM.
    CLEAR POSTAB. REFRESH POSTAB.
    POSTAB-FKDAT = GT_FVKDFIGN-FKDAT.
    POSTAB-FKART = GT_FVKDFIGN-FKART.
    RV60A-FKART  = P_GD_DEFAULT_DATA-FKART.
    RV60A-FKDAT  = P_GD_DEFAULT_DATA-FKDAT.
    RV60A-FBUDA  = P_GD_DEFAULT_DATA-FBUDA.
    RV60A-PRSDT  = P_GD_DEFAULT_DATA-PRSDT.
    REFRESH POSTAB.
    MOVE-CORRESPONDING GT_FVKDFIGN TO POSTAB.
    APPEND POSTAB.
    SET PARAMETER ID 'VF' FIELD SPACE.
    if ld_invoice_list is initial.
    SET PARAMETER ID 'VFR' FIELD GT_FVKDFIGN-VBELN.
    EXPORT OK-CODE
           POSTAB-FKDAT
           POSTAB-FKART
           RV60A-FKART
           RV60A-FKDAT
           RV60A-FBUDA
           RV60A-PRSDT
           POSTAB
           TO MEMORY ID 'VF04'.
    CALL TRANSACTION 'VF01' AND SKIP FIRST SCREEN.
    DATA : LD_VF01_PARAMETER LIKE VBRK-VBELN.
    GET PARAMETER ID 'VF' FIELD LD_VF01_PARAMETER.
    IF LD_VF01_PARAMETER IS INITIAL.
      GT_FVKDFIGN-STATF = '@5C@'.
    ELSE.
      GT_FVKDFIGN-SELKZ = '1'.
      GT_FVKDFIGN-STATF = '@5B@'.
    ENDIF.
    else.
*    SET PARAMETER ID 'VFL' FIELD GT_FVKDFIGN-VBELN.
     SET PARAMETER ID 'VFN' FIELD GT_FVKDFIGN-VBELN.
    EXPORT OK-CODE
           POSTAB-FKDAT
           POSTAB-FKART
           RV60A-FKART
           RV60A-FKDAT
           RV60A-FBUDA
           RV60A-PRSDT
           POSTAB
           TO MEMORY ID 'VF04'.
    CALL TRANSACTION 'VF21' AND SKIP FIRST SCREEN.
    DATA : LD_VF21_PARAMETER LIKE VBRK-VBELN.
    GET PARAMETER ID 'VF' FIELD LD_VF21_PARAMETER.
    IF LD_VF21_PARAMETER IS INITIAL.
      GT_FVKDFIGN-STATF = '@5C@'.
    ELSE.
      GT_FVKDFIGN-SELKZ = '1'.
      GT_FVKDFIGN-STATF = '@5B@'.
    ENDIF.
    endif.
    MODIFY GT_FVKDFIGN INDEX LD_FVKDFI_TABIX.
  ENDLOOP.
  RS_SELFIELD-REFRESH = 'X'.
* DATA : LD_FVKDFI LIKE GT_FVKDFIGN.
* DATA : LD_MULTIPLE_SELECTION(1).
* DATA : AC_CODE LIKE SY-SUBRC.
* DATA : RV60A LIKE RV60A.
*
* DATA : LT_KOMFK LIKE KOMFK   OCCURS 10 WITH HEADER LINE.
* DATA : LT_KOMV  LIKE KOMV    OCCURS 10 WITH HEADER LINE.
* DATA : LT_THEAD LIKE THEADVB OCCURS 10 WITH HEADER LINE.
* DATA : LT_VBFS  LIKE VBFS    OCCURS 10 WITH HEADER LINE.
* DATA : LT_VBPA  LIKE VBPAVB  OCCURS 10 WITH HEADER LINE.
* DATA : LT_VBRK  LIKE VBRKVB  OCCURS 10 WITH HEADER LINE.
* DATA : LT_VBRP  LIKE VBRPVB  OCCURS 10 WITH HEADER LINE.
* DATA : LT_VBSS  LIKE VBSS    OCCURS 10 WITH HEADER LINE.
* DATA : LS_VBSK  LIKE VBSK.
* DATA : LD_FVKDFI_TABIX LIKE SY-TABIX.
*
ENDFORM.                               " VKDFS_FCODE_SAMQ

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_SAMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_SAMS USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD
                            P_GD_DEFAULT_DATA LIKE RV60A
                            ld_invoice_list             .

  DATA : AC_CODE LIKE SY-SUBRC.
  DATA : RV60A LIKE RV60A.

  PERFORM AUTHORITY_CHECK USING '19' 'X' CHANGING AC_CODE.
  DATA : OK-CODE(5)          TYPE C.
  DATA:    BEGIN OF POSTAB OCCURS 50.
          INCLUDE STRUCTURE VKDFI.
  DATA:    V_FKDAT LIKE VKDFI-FKDAT,
           V_FKART LIKE VKDFI-FKART,
           ACTIV(1)     TYPE N.
  DATA:    END   OF POSTAB.
  OK-CODE = R_UCOMM.
  CLEAR POSTAB. REFRESH POSTAB.
  POSTAB-FKDAT = GT_FVKDFIGN-FKDAT.
  POSTAB-FKART = GT_FVKDFIGN-FKART.
  RV60A-FKART  = P_GD_DEFAULT_DATA-FKART.
  RV60A-FKDAT  = P_GD_DEFAULT_DATA-FKDAT.
  RV60A-FBUDA  = P_GD_DEFAULT_DATA-FBUDA.
  RV60A-PRSDT  = P_GD_DEFAULT_DATA-PRSDT.
  REFRESH POSTAB.
  LOOP AT GT_FVKDFIGN WHERE SELKZ = 'X'.
    MOVE-CORRESPONDING GT_FVKDFIGN TO POSTAB.
    APPEND POSTAB.
  ENDLOOP.
  if ld_invoice_list is initial.
  SET PARAMETER ID 'VFR' FIELD GT_FVKDFIGN-VBELN.
  else.
*  SET PARAMETER ID 'VFL' FIELD GT_FVKDFIGN-VBELN.
   SET PARAMETER ID 'VFN' FIELD GT_FVKDFIGN-VBELN.
  endif.
  IF SY-SUBRC IS INITIAL.
    if ld_invoice_list is initial.
    EXPORT OK-CODE
           POSTAB-FKDAT
           POSTAB-FKART
           RV60A-FKART
           RV60A-FKDAT
           RV60A-FBUDA
           RV60A-PRSDT
           POSTAB
           TO MEMORY ID 'VF04'.
    CALL TRANSACTION 'VF01' AND SKIP FIRST SCREEN.
    else.
    EXPORT OK-CODE
           POSTAB-FKDAT
           POSTAB-FKART
           RV60A-FKART
           RV60A-FKDAT
           RV60A-FBUDA
           RV60A-PRSDT
           POSTAB
           TO MEMORY ID 'VF04'.
    CALL TRANSACTION 'VF21' AND SKIP FIRST SCREEN.
    endif.
  ELSE.
    MESSAGE S460(VR).
  ENDIF.
ENDFORM.                               " VKDFS_FCODE_SAMS
*&---------------------------------------------------------------------*
*&      Form  SAMMELGANGSNR_ERMITTELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GNSAMMELGANGSNR_ERMITTELN
       USING
         P_SMART LIKE TVSA-SMART
       CHANGING
         P_SAMMG LIKE VBSK-SAMMG.

*LOCAL : TVSA.
  CHECK : P_SAMMG IS INITIAL.
  SELECT SINGLE * FROM TVSA WHERE SMART = P_SMART.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
              NR_RANGE_NR = TVSA-NUMKI
              OBJECT      = 'RV_SAMMG'
         IMPORTING
              NUMBER      = P_SAMMG.
  ELSE.
    MESSAGE E417(VR) WITH P_SAMMG.
  ENDIF.
ENDFORM.                               " SAMMELGANGSNR_ERMITTELN
*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_PROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GD_SAMMG  text
*----------------------------------------------------------------------*
FORM GNVKDFS_FCODE_PROT
       USING
         GD_SAMMG LIKE VBSK-SAMMG
         gd_invoice_list.

data : ld_smart like vbsk-smart.


  IF NOT GD_SAMMG IS INITIAL.
    ld_smart = 'F'.
    if not gd_invoice_list is initial.
      ld_smart = 'R'.
    endif.
    SUBMIT SDSAMPRO
             WITH SAMNR =  GD_SAMMG
             WITH SMART =  ld_smart
             WITH erdat =  sy-datum
             AND RETURN.
  ELSE.
    MESSAGE S042(VF).
  ENDIF.
ENDFORM.                               " VKDFS_FCODE_PROT
*&---------------------------------------------------------------------*
*&      Form  FCODE_SLEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GD_SAMMG  text
*----------------------------------------------------------------------*
FORM GNFCODE_SLEG.

  DATA : LT_LEGEND LIKE GT_LEGEND OCCURS 10 WITH HEADER LINE.

  LT_LEGEND-STATF = ICON_CREATE.
  LT_LEGEND-TEXT  = TEXT-CL1.
  APPEND LT_LEGEND.
  LT_LEGEND-STATF = ICON_OKAY.
  LT_LEGEND-TEXT  = TEXT-CL2.
  APPEND LT_LEGEND.
  LT_LEGEND-STATF = ICON_CANCEL.
  LT_LEGEND-TEXT  = TEXT-CL3.
  APPEND LT_LEGEND.
  LT_LEGEND-STATF = ICON_LED_GREEN.
  LT_LEGEND-TEXT  = TEXT-CL4.
  APPEND LT_LEGEND.
  LT_LEGEND-STATF = ICON_LED_RED.
  LT_LEGEND-TEXT  = TEXT-CL5.
  APPEND LT_LEGEND.

  CALL FUNCTION 'SD_COLOR_LEGEND_DISPLAY'
       TABLES
            IT_LEGEND = LT_LEGEND.
ENDFORM.                               " VKDFS_FCODE_PROT
