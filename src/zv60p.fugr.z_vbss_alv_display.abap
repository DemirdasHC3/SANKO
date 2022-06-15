FUNCTION Z_VBSS_ALV_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SMART) LIKE  VBSK-SMART
*"     VALUE(I_GRID_CONTROL) DEFAULT SPACE
*"  TABLES
*"      I_VBSS STRUCTURE  VBSS
*"      I_FVBSS STRUCTURE  VBSSF
*"--------------------------------------------------------------------
DATA : PT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA : PS_LAYOUT   TYPE SLIS_LAYOUT_ALV.
DATA : PT_OUTTAB   TYPE SD_ALV .
DATA : PT_EVENTS   TYPE SLIS_T_EVENT.
DATA : SMART_COUNT TYPE I.

CASE SMART.
  WHEN 'F'.
    CLEAR GT_FVBSS.
    REFRESH GT_FVBSS.
    LOOP AT I_FVBSS.
      MOVE-CORRESPONDING I_FVBSS TO GT_FVBSS.
      APPEND GT_FVBSS.
    ENDLOOP.
    CALL CUSTOMER-FUNCTION '006'
         TABLES
              C_VBSS     = GT_FVBSS
         EXCEPTIONS
              OTHERS     = 1.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
         EXPORTING
              I_PROGRAM_NAME         = 'SAPLV60P'
              I_INTERNAL_TABNAME     = 'GT_FVBSS'
              I_INCLNAME             = 'SAPLV60P'
         CHANGING
              CT_FIELDCAT            = PT_FIELDCAT
         EXCEPTIONS
              INCONSISTENT_INTERFACE = 1
              PROGRAM_ERROR          = 2
              OTHERS                 = 3.
    if not i_grid_control is initial.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
         EXPORTING
              I_CALLBACK_PROGRAM       = 'SAPLV60P'
              I_CALLBACK_PF_STATUS_SET = 'VBSS_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'VBSS_USER_COMMAND'
              IS_LAYOUT                = PS_LAYOUT
              IT_FIELDCAT              = PT_FIELDCAT
              I_DEFAULT                = 'X'
              I_SAVE                   = 'A'
         TABLES
              T_OUTTAB                 = GT_FVBSS
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
    else.
      CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
           EXPORTING
                I_CALLBACK_PROGRAM       = 'SAPLV60P'
                I_CALLBACK_PF_STATUS_SET = 'VBSS_PF_STATUS_SET'
                I_CALLBACK_USER_COMMAND  = 'VBSS_USER_COMMAND'
                IS_LAYOUT                = PS_LAYOUT
                IT_FIELDCAT              = PT_FIELDCAT
                I_DEFAULT                = 'X'
                I_SAVE                   = 'A'
           TABLES
                T_OUTTAB                 = GT_FVBSS
           EXCEPTIONS
                PROGRAM_ERROR            = 1
                OTHERS                   = 2.
    endif.
  WHEN OTHERS.
    CLEAR GT_VBSS.
    REFRESH GT_VBSS.
    LOOP AT I_VBSS.
      MOVE-CORRESPONDING I_VBSS TO GT_VBSS.
      APPEND GT_VBSS.
    ENDLOOP.
    CALL CUSTOMER-FUNCTION '007'
         TABLES
              C_VBSS     = GT_VBSS
         EXCEPTIONS
              OTHERS     = 1.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
         EXPORTING
              I_PROGRAM_NAME         = 'SAPLV60P'
              I_INTERNAL_TABNAME     = 'GT_VBSS'
              I_INCLNAME             = 'SAPLV60P'
         CHANGING
              CT_FIELDCAT            = PT_FIELDCAT
         EXCEPTIONS
              INCONSISTENT_INTERFACE = 1
              PROGRAM_ERROR          = 2
              OTHERS                 = 3.
    if not i_grid_control is initial.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
         EXPORTING
              I_CALLBACK_PROGRAM       = 'SAPLV60P'
              I_CALLBACK_PF_STATUS_SET = 'VBSS_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'VBSS_USER_COMMAND'
              IS_LAYOUT                = PS_LAYOUT
              IT_FIELDCAT              = PT_FIELDCAT
              I_DEFAULT                = 'X'
              I_SAVE                   = 'A'
         TABLES
              T_OUTTAB                 = GT_VBSS
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  else.
    cALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
        EXPORTING
             I_CALLBACK_PROGRAM       = 'SAPLV60P'
             I_CALLBACK_PF_STATUS_SET = 'VBSS_PF_STATUS_SET'
             I_CALLBACK_USER_COMMAND  = 'VBSS_USER_COMMAND'
             IS_LAYOUT                = PS_LAYOUT
             IT_FIELDCAT              = PT_FIELDCAT
             I_DEFAULT                = 'X'
             I_SAVE                   = 'A'
        TABLES
             T_OUTTAB                 = GT_VBSS
        EXCEPTIONS
             PROGRAM_ERROR            = 1
             OTHERS                   = 2.
  endif.
  ENDCASE.
ENDFUNCTION.
*---------------------------------------------------------------------*
*       FORM VBSS_PF_STATUS_SET
*---------------------------------------------------------------------*
*       This form sets the PF-Status and is required by the           *
*       general list viewer to display another status                 *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM VBSS_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  DATA: LT_EXTAB TYPE SLIS_T_EXTAB.
  DATA:    BEGIN OF EXCTAB OCCURS 1,
             OKCOD(4)          TYPE C,
           END   OF EXCTAB.

  READ TABLE GT_FVBSK INDEX 1.
  IF NOT SY-SUBRC IS INITIAL.
    EXCTAB-OKCOD = 'STOR'.
    APPEND EXCTAB.
  ENDIF.
  APPEND LINES OF EXCTAB   TO LT_EXTAB.
  APPEND LINES OF RT_EXTAB TO LT_EXTAB.

  SET PF-STATUS 'VBSS_ALV' EXCLUDING LT_EXTAB.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM VBSS_USER_COMMAND
*---------------------------------------------------------------------*
FORM VBSS_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  TABLES : VBCO6.
  DATA: LF_FELDNAME(20).
  DATA: DA_RC LIKE SY-SUBRC.
  DATA: LT_VBFS  LIKE VBFS  OCCURS 10 WITH HEADER LINE.
  DATA: LT_VBSS  LIKE VBSS  OCCURS 10 WITH HEADER LINE.
  CASE R_UCOMM.
    WHEN '&IC1'.
    WHEN 'STOR'.
      AUTHORITY-CHECK OBJECT 'V_VBSK_STO'
               ID 'SMART' FIELD 'S'
               ID 'ACTVT' FIELD '01'.
      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE S166(VF).
      ELSE.
        DATA : CANCELLATION_CRN LIKE GT_FVBSS-SAMMG.
        CALL FUNCTION 'SD_COLLECTIVE_RUN_CANCELLATION'
             EXPORTING
                  I_SAMMG                  = GT_FVBSS-SAMMG
             IMPORTING
                  O_SAMMG                  = CANCELLATION_CRN
             EXCEPTIONS
                  NO_COLLECTIVE_RUN_NUMBER = 1
                  OTHERS                   = 2.
        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE E417(VR) WITH SY-MSGV1 SY-MSGV2.
        ELSE.
            MESSAGE S025(VR) WITH GT_FVBSS-SAMMG CANCELLATION_CRN.
            RS_SELFIELD-EXIT = 'X'.
            R_UCOMM = 'BACK'.
        ENDIF.
      ENDIF.
    WHEN 'EPRO'.
      PERFORM VBFS_DATA_COLLECT
              TABLES
                LT_VBFS
              USING
                  RS_SELFIELD-TABNAME
                  RS_SELFIELD-TABINDEX
              CHANGING
                  DA_RC.
      CASE DA_RC.
        WHEN 4.
          MESSAGE S022(VR).
        WHEN 8.
          MESSAGE S415(VR).
        WHEN 0.
          CALL FUNCTION 'VBFS_TREE_LIST_DISPLAY'
               TABLES
                    I_VBFS  = LT_VBFS
               EXCEPTIONS
                    OTHERS  = 1.
      ENDCASE.
    WHEN 'BELE'.
      PERFORM VBSS_DATA_COLLECT
              TABLES
                GT_VBSS
                GT_FVBSS
              USING
                  RS_SELFIELD-TABNAME
                  RS_SELFIELD-TABINDEX
              CHANGING
                  DA_RC.
      IF NOT DA_RC IS INITIAL.
        MESSAGE S022(VR).
      ENDIF.
    WHEN 'BELA'.
      CASE RS_SELFIELD-TABNAME.
        WHEN 'GT_FVBSS'.
          READ TABLE GT_FVBSS INDEX RS_SELFIELD-TABINDEX.
          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE S022(VR).
          ELSE.
            CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
                 EXPORTING
                      VBELN = GT_FVBSS-VBELN.
          ENDIF.
        WHEN OTHERS.
          READ TABLE GT_VBSS INDEX RS_SELFIELD-TABINDEX.
          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE S022(VR).
          ELSE.
            CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
                 EXPORTING
                      VBELN = GT_VBSS-VBELN.
          ENDIF.
      ENDCASE.
    WHEN 'FLUS'.
      CASE RS_SELFIELD-TABNAME.
        WHEN 'GT_FVBSS'.
          READ TABLE GT_FVBSS INDEX RS_SELFIELD-TABINDEX.
          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE S022(VR).
          ELSE.
            VBCO6-VBELN = GT_FVBSS-VBELN.
            CALL DIALOG 'RV_DOCUMENT_FLOW'
                 EXPORTING
                      VBCO6 FROM VBCO6.
          ENDIF.
        WHEN OTHERS.
          READ TABLE GT_VBSS INDEX RS_SELFIELD-TABINDEX.
          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE S022(VR).
          ELSE.
            VBCO6-VBELN = GT_VBSS-VBELN.
            CALL DIALOG 'RV_DOCUMENT_FLOW'
                 EXPORTING
                      VBCO6 FROM VBCO6.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.
