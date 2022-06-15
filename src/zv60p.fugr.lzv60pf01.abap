*----------------------------------------------------------------------*
***INCLUDE LV60PF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BELEGFLUSS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_BILLSTAT_VBELN  text
*      -->P_LS_BILLSTAT_POSNR  text
*      -->P_LS_BILLSTAT_LOGSYS  text
*      <--P_RC  text
*----------------------------------------------------------------------*
FORM BELEGFLUSS_CHECK USING    P_LS_BILLSTAT_VBELN
                               P_LS_BILLSTAT_POSNR
                               P_LS_BILLSTAT_LOGSYS
                               P_ID_VOR_WA
                      CHANGING P_RC.

  DATA: LS_VBFA LIKE VBFA.

  IF P_ID_VOR_WA IS INITIAL.

    SELECT SINGLE * FROM VBFA INTO LS_VBFA
                            WHERE VBELN = P_LS_BILLSTAT_VBELN
                            AND   POSNN = P_LS_BILLSTAT_POSNR
                            AND  LOGSYS = P_LS_BILLSTAT_LOGSYS
                            AND  WBSTA  = 'C'.
    IF SY-SUBRC NE 0.
      P_RC = 4.
    ELSE.
      P_RC = 0.
    ENDIF.
  ELSE.
    SELECT SINGLE * FROM VBFA INTO LS_VBFA
                            WHERE VBELN = P_LS_BILLSTAT_VBELN
                            AND   POSNN = P_LS_BILLSTAT_POSNR
                            AND  LOGSYS = P_LS_BILLSTAT_LOGSYS.

    IF SY-SUBRC NE 0.
      P_RC = 4.
    ELSE.
      P_RC = 0.
    ENDIF.
 ENDIF.


ENDFORM.                              " BELEGFLUSS_CHECK
