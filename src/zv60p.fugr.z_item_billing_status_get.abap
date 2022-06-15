FUNCTION Z_ITEM_BILLING_STATUS_GET.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ID_VOR_WA) LIKE  RVSEL-XFELD OPTIONAL
*"  TABLES
*"      IT_BILLSTAT STRUCTURE  ITEMBILLSTATUS
*"  EXCEPTIONS
*"      ERROR_01
*"--------------------------------------------------------------------

  DATA: RC LIKE SY-SUBRC.
  DATA: LS_BILLSTAT LIKE ITEMBILLSTATUS.
  DATA: LT_VBRP LIKE VBRP OCCURS 5.
  DATA: LS_VBRP LIKE VBRP,
        LS_VBRK LIKE VBRK.

  LOOP AT IT_BILLSTAT INTO LS_BILLSTAT.

    SELECT * FROM VBRP INTO TABLE LT_VBRP
             WHERE VGBEL_EX = LS_BILLSTAT-VBELN
             AND   VGPOS_EX = LS_BILLSTAT-POSNR
             AND   LOGSYS   = LS_BILLSTAT-LOGSYS.
    IF SY-SUBRC NE 0.
      PERFORM BELEGFLUSS_CHECK USING
                                 LS_BILLSTAT-VBELN
                                 LS_BILLSTAT-POSNR
                                 LS_BILLSTAT-LOGSYS
                                 ID_VOR_WA
                               CHANGING
                                 RC.
      IF RC = 4.
        LS_BILLSTAT-FKSTA = ' '.
      ELSE.
        LS_BILLSTAT-FKSTA = 'A'.
      ENDIF.
    ELSE.
      SORT LT_VBRP BY ERDAT ERZET.
      LOOP AT LT_VBRP INTO LS_VBRP.
        SELECT SINGLE * FROM VBRK INTO LS_VBRK
                        WHERE VBELN = LS_VBRP-VBELN
                        AND   FKSTO = SPACE.
        IF SY-SUBRC = 0 .
          IF cl_sd_doc_category_util=>is_invoice_or_credit_memo_canc( iv_vbtyp = LS_VBRK-VBTYP ).
            LS_BILLSTAT-FKSTA = 'A'.
          ELSE.
            LS_BILLSTAT-FKSTA = 'C'.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    MODIFY IT_BILLSTAT FROM LS_BILLSTAT.

  ENDLOOP.

ENDFUNCTION.
