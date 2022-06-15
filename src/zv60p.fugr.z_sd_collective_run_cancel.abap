FUNCTION Z_SD_COLLECTIVE_RUN_CANCEL.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_SAMMG) LIKE  VBSS-SAMMG
*"  EXPORTING
*"     VALUE(O_SAMMG) LIKE  VBSS-SAMMG
*"  EXCEPTIONS
*"      NO_COLLECTIVE_RUN_NUMBER
*"--------------------------------------------------------------------
  DATA : LT_KOMFK LIKE KOMFK    OCCURS 5 WITH HEADER LINE.
  DATA : LT_KOMV  LIKE KOMV     OCCURS 5 WITH HEADER LINE.
  DATA : LT_THEAD LIKE THEADVB  OCCURS 5 WITH HEADER LINE.
  DATA : LT_VBFS  LIKE VBFS     OCCURS 5 WITH HEADER LINE.
  DATA : LT_VBPA  LIKE VBPAVB   OCCURS 5 WITH HEADER LINE.
  DATA : LT_VBRK  LIKE VBRKVB   OCCURS 5 WITH HEADER LINE.
  DATA : LT_VBRP  LIKE VBRPVB   OCCURS 5 WITH HEADER LINE.
  DATA : LT_VBSS  LIKE VBSS     OCCURS 5 WITH HEADER LINE.
  DATA : LT_VBSS_RIC  LIKE VBSS     OCCURS 5 WITH HEADER LINE.
  DATA : LT_VBSK  LIKE VBSK     OCCURS 5 WITH HEADER LINE.
  DATA : LD_CONFIRM TYPE C.
  DATA : LD_TEXT(60).
  DATA : LD_SAMMG_TEXT(10).
  DATA : LD_RC LIKE SY-SUBRC.
  MOVE I_SAMMG TO LD_SAMMG_TEXT.
  CONCATENATE TEXT-C01 LD_SAMMG_TEXT TEXT-C02
              INTO LD_TEXT SEPARATED BY SPACE.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR              = TEXT-C05
*           DIAGNOSE_OBJECT       = ' '
            TEXT_QUESTION   = LD_TEXT
            TEXT_BUTTON_1         = TEXT-C03
            TEXT_BUTTON_2         = TEXT-C04
            DEFAULT_BUTTON        = '2'
            DISPLAY_CANCEL_BUTTON = 'X'
       IMPORTING
            ANSWER                =  LD_CONFIRM
       EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.
  IF LD_CONFIRM = '1'.
    SELECT * INTO TABLE LT_VBSS FROM VBSS WHERE SAMMG = I_SAMMG.
    READ TABLE LT_VBSS INDEX 1.
    IF SY-SUBRC IS INITIAL.
*   fill collective run directory
      LT_VBSK-MANDT = SY-MANDT.
      LT_VBSK-ERNAM = SY-UNAME.
      LT_VBSK-ERDAT = SY-DATUM.
      LT_VBSK-UZEIT = SY-UZEIT.
      LT_VBSK-SMART = 'S'.
      PERFORM COLLECTIVE_RUN_NUMBER_DETERMIN
              USING
                  LT_VBSK-SMART
              CHANGING
                  LT_VBSK-SAMMG
                  LD_RC.
      IF NOT LD_RC IS INITIAL.
        MESSAGE E417(VR) WITH LT_VBSK-SMART
                RAISING NO_COLLECTIVE_RUN_NUMBER.
      ENDIF.
      O_SAMMG = LT_VBSK-SAMMG.
      LOOP AT LT_VBSS.
*       Fill communication table
        CLEAR LT_KOMFK.
        REFRESH LT_KOMFK.
        CLEAR LT_VBFS.
        REFRESH LT_VBFS.
        CLEAR LT_VBSS_RIC.
        REFRESH LT_VBSS_RIC.
        MOVE-CORRESPONDING LT_VBSS TO LT_KOMFK.
        APPEND LT_KOMFK.
        CALL FUNCTION 'RV_INVOICE_CREATE'
             EXPORTING
                  VBSK_I       = LT_VBSK
                  WITH_POSTING = 'D'
             TABLES
                  XKOMFK       = LT_KOMFK
                  XKOMV        = LT_KOMV
                  XTHEAD       = LT_THEAD
                  XVBFS        = LT_VBFS
                  XVBPA        = LT_VBPA
                  XVBRK        = LT_VBRK
                  XVBRP        = LT_VBRP
                  XVBSS        = LT_VBSS_RIC
             EXCEPTIONS
                  OTHERS       = 1.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  COLLECTIVE_RUN_NUMBER_DETERMIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_VBSK_SMART  text                                        *
*      <--P_LT_VBSK_SAMMG  text                                        *
*----------------------------------------------------------------------*
FORM COLLECTIVE_RUN_NUMBER_DETERMIN
       USING
           P_LT_VBSK_SMART LIKE VBSK-SMART
       CHANGING
           P_LT_VBSK_SAMMG LIKE VBSK-SAMMG
           P_RC LIKE SY-SUBRC.

*TABLES : TVSA.
  P_RC = 0.
  SELECT SINGLE * FROM TVSA WHERE SMART = P_LT_VBSK_SMART.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
              NR_RANGE_NR = TVSA-NUMKI
              OBJECT      = 'RV_SAMMG'
         IMPORTING
              NUMBER      = P_LT_VBSK_SAMMG.
  ELSE.
    P_RC = 4.
  ENDIF.
ENDFORM.                               " COLLECTIVE_RUN_NUMBER_DETERMIN
