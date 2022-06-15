FUNCTION Z_SD_DOCUMENT_ITEM_SCREEN_SELE.
*"--------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(ID_VBELN) LIKE  VBRK-VBELN
*"     VALUE(IS_RV60A) LIKE  RV60A STRUCTURE  RV60A OPTIONAL
*"     VALUE(ID_MARK_ALL_ITEMS) DEFAULT ' '
*"  TABLES
*"      GT_VDICS STRUCTURE  VDICS
*"      OT_VBRP STRUCTURE  VBRP OPTIONAL
*"  EXCEPTIONS
*"      SD_DOCUMENT_NOT_FOUND
*"      NO_BILLING_RELEVANT_ITEM_FOUND
*"--------------------------------------------------------------------
  DATA : ld_first_call(1) VALUE 'X'.
  DATA : lt_diff_vdics LIKE gt_vdics OCCURS 10 WITH HEADER LINE.

  LOOP AT gt_vdics.
    IF gt_vdics-vbeln NE id_vbeln.
      APPEND gt_vdics TO lt_diff_vdics.
      DELETE gt_vdics.

    ENDIF.
  ENDLOOP.

  READ TABLE gt_old_vdics INDEX 1.
  IF sy-subrc IS INITIAL.
    CLEAR ld_first_call.
  ELSE.
    CLEAR gd_old_vbeln.
  ENDIF.

  gt_old_vdics[] = gt_vdics[].
  READ TABLE gt_vdics WITH KEY vbeln = id_vbeln BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
*   REFRESH GT_VDICS.
    PERFORM vbtyp_ermitteln
              USING
                id_vbeln
              CHANGING
                gd_vbtyp.
    IF cl_sd_doc_category_util=>is_any_Sales( iv_vbtyp = gd_vbtyp ).
      PERFORM vdics_auftragsbezogen
                TABLES
                  gt_vdics
                USING
                  id_vbeln
                  gd_vbtyp
                  is_rv60a.
    ENDIF.
    IF cl_sd_doc_category_util=>is_delivery_outgoing( iv_vbtyp = gd_vbtyp ) OR
      ( cl_ops_switch_check=>ops_sfws_sc_advret1( ) eq gc_charx AND gd_vbtyp CA IF_SD_DOC_CATEGORY=>DELIVERY_SHIPPING_NOTIF ).
      PERFORM vdics_lieferbezogen
                TABLES
                  gt_vdics
                USING
                  id_vbeln
                  gd_vbtyp
                  is_rv60a.
    ENDIF.
    IF cl_sd_doc_category_util=>is_any_invoice( iv_vbtyp = gd_vbtyp ).
      PERFORM vdics_fakturabezogen
                TABLES
                  gt_vdics
                USING
                  id_vbeln.
    ENDIF.
  ENDIF.
  IF NOT id_mark_all_items IS INITIAL.
    IF NOT ld_first_call IS INITIAL.
      gt_vdics-selkz = 'X'.
      MODIFY gt_vdics TRANSPORTING selkz WHERE selkz = ' '.
    ENDIF.
  ENDIF.
  CALL SCREEN 4413.
  LOOP AT lt_diff_vdics.
    READ TABLE gt_vdics WITH KEY vbeln = lt_diff_vdics-vbeln
                                 posnr = lt_diff_vdics-posnr
                                 BINARY SEARCH.

    IF sy-subrc = 4.
      INSERT lt_diff_vdics INTO gt_vdics INDEX sy-tabix.
    ELSE.
      APPEND lt_diff_vdics TO gt_vdics.
    ENDIF.
  ENDLOOP.
  REFRESH lt_diff_vdics.

ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Module  vdics_AUFSETZEN100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE vdics_aufsetzen_o_100 OUTPUT.
  PERFORM vdics_aufsetzen_o_100.
ENDMODULE.                 " vdics_AUFSETZEN_o_100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  vdics_AUFSETZEN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_aufsetzen_o_100.
  IF gd_old_vbeln NE gt_vdics-vbeln.
    gd_old_vbeln = gt_vdics-vbeln.
    vdics_tabix_zeile_1 = 1.
  ENDIF.
  vdics_tabix_aktuell = vdics_tabix_zeile_1.
  IF vdics_tabix_aktuell LE 0.
    vdics_tabix_aktuell = 1.
  ENDIF.
*  CLEAR: GT_VDICS.
  CLEAR: fcode.
ENDFORM.                    " vdics_AUFSETZEN_o_100
*&---------------------------------------------------------------------*
*&      Module  TCTRL_KOPIEREN_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tctrl_kopieren_init100 OUTPUT.
  PERFORM tctrl_kopieren_init100.
ENDMODULE.                 " TCTRL_KOPIEREN_INIT100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  TCTRL_KOPIEREN_INIT100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tctrl_kopieren_init100.
  DATA: da_tfill LIKE sy-tabix.

  DESCRIBE TABLE gt_vdics LINES da_tfill.
  tctrl_kopieren-lines    = da_tfill.
  tctrl_kopieren-top_line = vdics_tabix_zeile_1.
ENDFORM.                    " TCTRL_KOPIEREN_INIT100
*&---------------------------------------------------------------------*
*&      Module  vdics_LESEN100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE vdics_lesen100 OUTPUT.
  PERFORM vdics_lesen100.
ENDMODULE.                 " vdics_LESEN100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  vdics_LESEN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_lesen100.

  vdics_loopc = sy-loopc.
  READ TABLE gt_vdics INDEX vdics_tabix_aktuell.
  IF sy-subrc NE 0.
*    CLEAR: GT_VDICS.
    rv60a-zeile_leer = gc_charx.
    EXIT FROM STEP-LOOP.
  ELSE.
    CLEAR: rv60a-zeile_leer.
    vdics_tabix_aktuell = vdics_tabix_aktuell + 1.
    vdics = gt_vdics.
*   VDICS-FKIMG = GT_VDICS-GESMNG.
    rv60a-selkz = gt_vdics-selkz.
*   IF GT_VDICS-UVPRS = 'X' AND SY-MSGID NE 'V1'.
*     MESSAGE S073 WITH GT_VDICS-POSNR.
*   ELSEIF gt_VDICS-UVALL = 'X'.
*     MESSAGE S074 WITH VDICS-POSNR.
*   ENDIF.
  ENDIF.
ENDFORM.                    " vdics_LESEN100
*&---------------------------------------------------------------------*
*&      Module  FUNKTION_AUSFUEHREN100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE funktion_ausfuehren100 INPUT.
  PERFORM funktion_ausfuehren100.
ENDMODULE.                 " FUNKTION_AUSFUEHREN100  INPUT

*&---------------------------------------------------------------------*
*&      Form  FUNKTION_AUSFUEHREN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM funktion_ausfuehren100.
  CASE fcode.
    WHEN 'ABBR'.
      CLEAR gt_vdics.
      REFRESH gt_vdics.
*    LOOP AT GT_OLD_VDICS WHERE NOT SELKZ IS INITIAL.
      gt_vdics[] = gt_old_vdics[].
*      APPEND GT_VDICS.
*    ENDLOOP.
      LEAVE TO SCREEN 0.
    WHEN 'POPO'.





  ENDCASE.
ENDFORM.                    " FUNKTION_AUSFUEHREN100
*&---------------------------------------------------------------------*
*&      Module  TCTRL_KOPIEREN_BLAETTERN100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tctrl_kopieren_blaettern100 INPUT.
  vdics_tabix_zeile_1 = tctrl_kopieren-top_line.

ENDMODULE.                 " TCTRL_KOPIEREN_BLAETTERN100  INPUT
*&---------------------------------------------------------------------*
*&      Module  VDICS_BEARBEITEN100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE vdics_bearbeiten100 INPUT.
  PERFORM vdics_bearbeiten100.
ENDMODULE.                 " VDICS_BEARBEITEN100  INPUT

*&---------------------------------------------------------------------*
*&      Form  VDICS_BEARBEITEN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_bearbeiten100.
  gt_vdics-selkz = rv60a-selkz.
  MODIFY gt_vdics INDEX vdics_tabix.
ENDFORM.                    " VDICS_BEARBEITEN100
*&---------------------------------------------------------------------*
*&      Module  VDICS_UNTERLEGEN100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE vdics_unterlegen100 INPUT.
  PERFORM vdics_unterlegen100.
ENDMODULE.                 " VDICS_UNTERLEGEN100  INPUT

*&---------------------------------------------------------------------*
*&      Form  VDICS_UNTERLEGEN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_unterlegen100.
  vdics_loopc = sy-loopc.
  READ TABLE gt_vdics INDEX vdics_tabix_aktuell.
  IF sy-subrc NE 0.
*    CLEAR VDICS.
    vdics_tabix = 0.
  ELSE.
    vdics = gt_vdics.
    vdics_tabix = sy-tabix.
  ENDIF.
  vdics_tabix_aktuell = vdics_tabix_aktuell + 1.
ENDFORM.                    " VDICS_UNTERLEGEN100
*&---------------------------------------------------------------------*
*&      Module  vdics_AUFSETZEN100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE vdics_aufsetzen_i_100.
  PERFORM vdics_aufsetzen_i_100.
ENDMODULE.                 " vdics_AUFSETZEN_I_100

*&---------------------------------------------------------------------*
*&      Form  vdics_AUFSETZEN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_aufsetzen_i_100.
  IF vdics_tabix_zeile_1 = 0.
    vdics_tabix_zeile_1 = 1.
  ENDIF.
  vdics_tabix_aktuell = vdics_tabix_zeile_1.
*  CLEAR: GT_VDICS.
ENDFORM.                    " vdics_AUFSETZEN_I_100
*&---------------------------------------------------------------------*
*&      Module  CUA_SETZEN100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cua_setzen100 OUTPUT.
  PERFORM cua_setzen100.
ENDMODULE.                 " CUA_SETZEN100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CUA_SETZEN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cua_setzen100.
  SET PF-STATUS 'CP'.
ENDFORM.                    " CUA_SETZEN100
*&---------------------------------------------------------------------*
*&      Module  FCODE_BEARBEITEN100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_bearbeiten100 INPUT.
  PERFORM fcode_bearbeiten100.
ENDMODULE.                 " FCODE_BEARBEITEN100  INPUT

*&---------------------------------------------------------------------*
*&      Form  FCODE_BEARBEITEN100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fcode_bearbeiten100.
  DATA : ld_tfill LIKE sy-tabix.
  DATA: dummy LIKE sy-tabix VALUE 2.
  DESCRIBE TABLE gt_vdics LINES ld_tfill.
* LD_TFILL = LD_TFILL + 1.
  IF fcode = 'P++' OR fcode = 'P+' OR fcode = 'P-' OR fcode = 'P--'.
    DESCRIBE TABLE gt_vdics LINES ld_tfill.
    ld_tfill = ld_tfill + 1.
    IF vdics_tabix_zeile_1 = 0.
      vdics_tabix_zeile_1 = 1.
    ENDIF.

    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = vdics_tabix_zeile_1
        entry_to       = ld_tfill
        loops          = vdics_loopc
        ok_code        = fcode
        page_go        = dummy
        page_act       = dummy
        overlapping    = space
        last_page_full = gc_charx
      IMPORTING
        entry_new      = vdics_tabix_zeile_1.
    CLEAR fcode.
  ENDIF.
  IF fcode = 'RUEB'.
    DATA : ls_vdics LIKE vdics.
    LOOP AT gt_vdics INTO ls_vdics WHERE selkz NE space.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS INITIAL.
      IF cl_sd_doc_category_util=>is_any_invoice( iv_vbtyp = gd_vbtyp ).
        PERFORM ot_vbrp_erzeugen
                  TABLES
                    gt_vdics
                    ot_vbrp.
      ENDIF.
      LEAVE TO SCREEN 0.
    ELSE.
      MESSAGE s205(vr).
    ENDIF.
  ENDIF.
  IF fcode = 'MKOL'.
    gt_vdics-selkz = gc_charx.
    MODIFY gt_vdics TRANSPORTING selkz WHERE selkz = space.
  ENDIF.
  IF fcode = 'MKLO'.
    gt_vdics-selkz = space.
    MODIFY gt_vdics TRANSPORTING selkz WHERE selkz NE space.
  ENDIF.
  IF fcode = 'POPO'.
    CALL FUNCTION 'SD_DOCUMENT_ITEM_SCREEN_SETPOS'
      IMPORTING
*       ITEM_NUMBER   =
*       MATERIAL_TEXT =
        item_tabix = vdics_tabix_zeile_1
      TABLES
        gt_vdics   = gt_vdics.
  ENDIF.
  IF fcode = 'MARK'.
    IF vdics_tabix_zeile_1 = 0.
      vdics_tabix_zeile_1 = 1.
      MESSAGE s310(v1).
      EXIT.
    ENDIF.
    READ TABLE gt_vdics INDEX vdics_tabix_zeile_1.
    IF sy-subrc IS INITIAL.
      IF gt_vdics-selkz IS INITIAL.
        gt_vdics-selkz = 'X'.
      ELSE.
        CLEAR gt_vdics-selkz.
      ENDIF.
      MODIFY gt_vdics INDEX vdics_tabix_zeile_1.
    ELSE.
      MESSAGE s310(v1).
      EXIT.
    ENDIF.
  ENDIF.


ENDFORM.                    " FCODE_BEARBEITEN100
*&---------------------------------------------------------------------*
*&      Module  VDICS_ORFMNG_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE vdics_orfmng_value INPUT.
  PERFORM vdics_orfmng_value.
ENDMODULE.                 " VDICS_ORFMNG_VALUE  INPUT

*&---------------------------------------------------------------------*
*&      Form  VDICS_ORFMNG_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_orfmng_value.

  DATA : ld_difference LIKE vdics-gesmng.
* IF NOT GT_VDICS-SELKZ IS INITIAL.
  IF cl_sd_doc_category_util=>is_any_invoice( iv_vbtyp = gd_vbtyp ).
    ld_difference = gt_vdics-gesmng.
  ELSE.
    ld_difference = gt_vdics-gesmng - gt_vdics-fkimg.
  ENDIF.
  IF vdics-orfmng GT ld_difference.
    MESSAGE e170(vf) WITH ld_difference
                          vdics-orfmng gt_vdics-vrkme.
  ENDIF.

* ENDIF.
*ENHANCEMENT-SECTION     SD_DOCUMENT_ITEM_SCREEN_SEL_01 SPOTS ES_SAPLV60P.
  IF gt_vdics-orfmng NE vdics-orfmng.
    gt_vdics-orfmng = vdics-orfmng.
    MODIFY gt_vdics INDEX vdics_tabix.
  ENDIF.
*END-ENHANCEMENT-SECTION.
ENDFORM.                    " VDICS_ORFMNG_VALUE
*&---------------------------------------------------------------------*
*&      Form  VDICS_AUFTRAGSBEZOGEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_auftragsbezogen
       TABLES
         gt_vdics STRUCTURE vdics
       USING
         id_vbeln LIKE vbrk-vbeln
         gd_vbtyp LIKE vbrk-vbtyp
         id_rv60a LIKE rv60a.

  DATA : lt_vbap  LIKE vbap    OCCURS 10 WITH HEADER LINE.
  DATA : ls_vbak  LIKE vbak.
  DATA : ls_tvak  LIKE tvak.
  DATA : ls_tvakt LIKE tvakt.
  DATA : ls_vbsk  LIKE vbsk.
  DATA : lt_komfk LIKE komfk   OCCURS 10 WITH HEADER LINE.
  DATA : lt_komv  LIKE komv    OCCURS 10 WITH HEADER LINE.
  DATA : lt_thead LIKE theadvb OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbfs  LIKE vbfs    OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbpa  LIKE vbpavb  OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbrk  LIKE vbrkvb  OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbrp  LIKE vbrpvb  OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbss  LIKE vbss    OCCURS 10 WITH HEADER LINE.

*IF GD_VBELN NE ID_VBELN.
  SELECT SINGLE * FROM vbak INTO ls_vbak WHERE vbeln = id_vbeln.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE * FROM tvak INTO ls_tvak WHERE auart = ls_vbak-auart.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE * FROM tvakt
                      INTO ls_tvakt
                      WHERE auart = ls_tvak-auart AND
                            spras = sy-langu.
      IF sy-subrc IS INITIAL.
        rv60a-txvbtyp = ls_tvakt-bezei.
      ENDIF.
    ENDIF.
*    GD_VBELN = ID_VBELN.
    CLEAR lt_komfk.
    REFRESH lt_komfk.
    lt_komfk-vbeln = id_vbeln.
    lt_komfk-vbtyp = gd_vbtyp.
    APPEND lt_komfk.
    CALL FUNCTION 'RV_INVOICE_CREATE'
      EXPORTING
        vbsk_i        = ls_vbsk
        delivery_date = is_rv60a-fbuda
        pricing_date  = is_rv60a-prsdt
        invoice_date  = is_rv60a-fkdat
        invoice_type  = is_rv60a-fkart
      TABLES
        xkomfk        = lt_komfk
        xkomv         = lt_komv
        xthead        = lt_thead
        xvbfs         = lt_vbfs
        xvbpa         = lt_vbpa
        xvbrk         = lt_vbrk
        xvbrp         = lt_vbrp
        xvbss         = lt_vbss.
    CLEAR gt_vdics.
    REFRESH gt_vdics.
    CLEAR gt_fkrelk_tab.
    REFRESH gt_fkrelk_tab.
    READ TABLE lt_vbrp INDEX 1.
    IF sy-subrc IS INITIAL.
      SELECT pstyv INTO CORRESPONDING FIELDS OF TABLE gt_fkrelk_tab
                   FROM tvap
                   FOR ALL ENTRIES IN lt_vbrp WHERE
                             pstyv = lt_vbrp-pstyv AND
                             fkrel = gc_chari.
      SORT gt_fkrelk_tab BY pstyv.
    ENDIF.
    LOOP AT lt_vbrp.
      MOVE-CORRESPONDING lt_vbrp TO gt_vdics.
      IF NOT lt_vbrp-abrbg IS INITIAL.
        PERFORM fkdat_aus_fplt_ermitteln
                  USING lt_vbrp-fplnr lt_vbrp-fpltr
                  CHANGING gt_vdics-fkdat.
      ELSE.
        DATA : loc_vbrk LIKE vbrk.
        READ TABLE lt_vbrk INTO loc_vbrk WITH KEY vbeln = lt_vbrp-vbeln
                                              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gt_vdics-fkdat = loc_vbrk-fkdat.
        ELSE.
          gt_vdics-fkdat = sy-datum.
        ENDIF.
      ENDIF.
      gt_vdics-vbeln = id_vbeln.
      gt_vdics-chgam = 'X'.
      gt_vdics-fkimg  = gt_vdics-gesmng - gt_vdics-fkimg.
      gt_vdics-orfmng = gt_vdics-gesmng - gt_vdics-fkimg.
      gt_vdics-posnr = lt_vbrp-vgpos.
      APPEND gt_vdics.
    ENDLOOP.
    READ TABLE gt_vdics INDEX 1.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE e171(vf) WITH id_vbeln
                       RAISING no_billing_relevant_item_found.
    ENDIF.
    CALL FUNCTION 'RV_INVOICE_REFRESH'
      TABLES
        xkomfk = lt_komfk
        xkomv  = lt_komv
        xthead = lt_thead
        xvbfs  = lt_vbfs
        xvbpa  = lt_vbpa
        xvbrk  = lt_vbrk
        xvbrp  = lt_vbrp
        xvbss  = lt_vbss.
  ENDIF.
*ENDIF.
ENDFORM.                    " VDICS_AUFTRAGSBEZOGEN

*&---------------------------------------------------------------------*
*&      Form  VDICS_LIEFERBEZOGEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_lieferbezogen
       TABLES
         gt_vdics STRUCTURE vdics
       USING
         id_vbeln LIKE vbrk-vbeln
         gd_vbtyp LIKE vbrk-vbtyp
         id_rv60a LIKE rv60a.

  DATA : lt_lips  LIKE lips    OCCURS 10 WITH HEADER LINE.
  DATA : ls_likp  LIKE likp.
  DATA : ls_tvlk  LIKE tvlk.
  DATA : ls_tvlkt LIKE tvlkt.
  DATA : ls_vbsk  LIKE vbsk.
  DATA : lt_komfk LIKE komfk   OCCURS 10 WITH HEADER LINE.
  DATA : lt_komv  LIKE komv    OCCURS 10 WITH HEADER LINE.
  DATA : lt_thead LIKE theadvb OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbfs  LIKE vbfs    OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbpa  LIKE vbpavb  OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbrk  LIKE vbrkvb  OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbrp  LIKE vbrpvb  OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbss  LIKE vbss    OCCURS 10 WITH HEADER LINE.

*IF GD_VBELN NE ID_VBELN.
  SELECT SINGLE * FROM likp INTO ls_likp WHERE vbeln = id_vbeln.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE * FROM tvlk INTO ls_tvlk WHERE lfart = ls_likp-lfart.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE * FROM tvlkt
                      INTO ls_tvlkt
                      WHERE lfart = ls_tvlk-lfart AND
                            spras = sy-langu.
      IF sy-subrc IS INITIAL.
        rv60a-txvbtyp = ls_tvlkt-vtext.
      ENDIF.
    ENDIF.
*    GD_VBELN = ID_VBELN.
    CLEAR lt_komfk.
    REFRESH lt_komfk.
    lt_komfk-vbeln = id_vbeln.
    lt_komfk-vbtyp = gd_vbtyp.
    APPEND lt_komfk.
    CALL FUNCTION 'RV_INVOICE_CREATE'
      EXPORTING
        vbsk_i        = ls_vbsk
        delivery_date = is_rv60a-fbuda
        pricing_date  = is_rv60a-prsdt
        invoice_date  = is_rv60a-fkdat
        invoice_type  = is_rv60a-fkart
      TABLES
        xkomfk        = lt_komfk
        xkomv         = lt_komv
        xthead        = lt_thead
        xvbfs         = lt_vbfs
        xvbpa         = lt_vbpa
        xvbrk         = lt_vbrk
        xvbrp         = lt_vbrp
        xvbss         = lt_vbss.
    CLEAR gt_vdics.
    REFRESH gt_vdics.
    CLEAR gt_fkrelk_tab.
    REFRESH gt_fkrelk_tab.
    READ TABLE lt_vbrp INDEX 1.
    IF sy-subrc IS INITIAL.
      SELECT pstyv INTO CORRESPONDING FIELDS OF TABLE gt_fkrelk_tab
                   FROM tvap
                   FOR ALL ENTRIES IN lt_vbrp WHERE
                             pstyv = lt_vbrp-pstyv AND
                             ( fkrel = gc_chark OR
                               fkrel = gc_charo ).
      SORT gt_fkrelk_tab BY pstyv.
    ENDIF.
    LOOP AT lt_vbrp.
      MOVE-CORRESPONDING lt_vbrp TO gt_vdics.
      DATA : loc_vbrk LIKE vbrk.
      READ TABLE lt_vbrk INTO loc_vbrk WITH KEY vbeln = lt_vbrp-vbeln
                                            BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        gt_vdics-fkdat = loc_vbrk-fkdat.
      ELSE.
        gt_vdics-fkdat = sy-datum.
      ENDIF.
      gt_vdics-vbeln = id_vbeln.
      gt_vdics-chgam = 'X'.
      gt_vdics-fkimg  = gt_vdics-gesmng - gt_vdics-fkimg.
      gt_vdics-orfmng = lt_vbrp-fkimg.
      gt_vdics-posnr = lt_vbrp-vgpos.
      gt_vdics-vbtyp = lt_vbrk-vbtyp.
      APPEND gt_vdics.
    ENDLOOP.
    READ TABLE gt_vdics INDEX 1.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE e171(vf) WITH id_vbeln
                       RAISING no_billing_relevant_item_found.
    ENDIF.
    CALL FUNCTION 'RV_INVOICE_REFRESH'
      TABLES
        xkomfk = lt_komfk
        xkomv  = lt_komv
        xthead = lt_thead
        xvbfs  = lt_vbfs
        xvbpa  = lt_vbpa
        xvbrk  = lt_vbrk
        xvbrp  = lt_vbrp
        xvbss  = lt_vbss.
*ENHANCEMENT-POINT SD_DOCUMENT_ITEM_SCR_SEL_01 SPOTS ES_SAPLV60P.
  ENDIF.
*ENDIF.
ENDFORM.                    " VDICS_LIEFERBEZOGEN
*&---------------------------------------------------------------------*
*&      Form  VDICS_FAKTURABEZOGEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vdics_fakturabezogen
       TABLES
         gt_vdics STRUCTURE vdics
       USING
         id_vbeln LIKE vbrk-vbeln.

  DATA : lt_vbrp  LIKE vbrp OCCURS 10 WITH HEADER LINE.
  DATA : ls_vbrk  LIKE vbrk.
  DATA : ls_tvfk  LIKE tvfk.
  DATA : ls_tvfkt LIKE tvfkt.
  DATA : da_archived TYPE xflag.
  DATA : switch_active TYPE xfeld.

  switch_active = cl_ops_switch_check=>sd_sfws_sc2( ).

  IF switch_active = 'X'.
* for billing documents: check if it's archived
    IF NOT id_vbeln IS INITIAL.
      CALL FUNCTION 'INVOICE_ARCHIVED'
        EXPORTING
          i_vbeln    = id_vbeln
        IMPORTING
          e_archived = da_archived.
    ENDIF.
  ENDIF.

  IF da_archived = 'X' AND switch_active = 'X'.
* Invoice is in archive if we reach this point
    CALL FUNCTION 'RETURN_VBRK'
      EXPORTING
        i_vbeln = id_vbeln
      IMPORTING
        e_vbrk  = ls_vbrk.

    IF ls_vbrk IS NOT INITIAL.
      SELECT SINGLE * FROM tvfk INTO ls_tvfk WHERE fkart = ls_vbrk-fkart.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE * FROM tvfkt
                        INTO ls_tvfkt
                        WHERE fkart = ls_tvfk-fkart AND
                              spras = sy-langu.
        IF sy-subrc IS INITIAL.
          rv60a-txvbtyp = ls_tvfkt-vtext.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'RETURN_VBRP'
        EXPORTING
          i_vbeln = id_vbeln
        CHANGING
          ch_vbrp = lt_vbrp[].
    ENDIF.

  ELSE.
* document is on DB
*   IF GD_VBELN NE ID_VBELN.
    SELECT SINGLE * FROM vbrk INTO ls_vbrk WHERE vbeln = id_vbeln.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE * FROM tvfk INTO ls_tvfk WHERE fkart = ls_vbrk-fkart.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE * FROM tvfkt
                        INTO ls_tvfkt
                        WHERE fkart = ls_tvfk-fkart AND
                              spras = sy-langu.
        IF sy-subrc IS INITIAL.
          rv60a-txvbtyp = ls_tvfkt-vtext.
        ENDIF.
      ENDIF.
*    GD_VBELN = ID_VBELN.
      SELECT * INTO TABLE lt_vbrp FROM vbrp WHERE vbeln = id_vbeln.
    ENDIF.
  ENDIF.

  READ TABLE lt_vbrp INDEX 1.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_vbrp.
      READ TABLE gt_vdics WITH KEY vbeln = lt_vbrp-vbeln
                                   posnr = lt_vbrp-posnr.
      IF sy-subrc NE 0.
        CLEAR gt_vdics.
        MOVE-CORRESPONDING lt_vbrp TO gt_vdics.
        gt_vdics-gesmng = lt_vbrp-fkimg.
        gt_vdics-orfmng = lt_vbrp-fkimg.
        gt_vdics-fkdat  = ls_vbrk-fkdat.
        gt_vdics-chgam = 'X'.
        APPEND gt_vdics.
      ENDIF.
    ENDLOOP.
    SORT gt_vdics BY vbeln posnr.
  ENDIF.
ENDFORM.                    " VDICS_FAKTURABEZOGEN
*&---------------------------------------------------------------------*
*&      Module  TCTRL_KOPIEREN_BLAETTERN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tctrl_kopieren_blaettern INPUT.
  vdics_tabix_zeile_1 = tctrl_kopieren-top_line.
  GET CURSOR LINE tc_selline.
ENDMODULE.                 " TCTRL_KOPIEREN_BLAETTERN  INPUT
*&---------------------------------------------------------------------*
*&      Module  FELDAUSWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE feldauswahl OUTPUT.
  PERFORM feldauswahl.
ENDMODULE.                 " FELDAUSWAHL  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FELDAUSWAHL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM feldauswahl.
  LOOP AT SCREEN.
    IF not cl_sd_doc_category_util=>is_any_Sales( iv_vbtyp = gd_vbtyp ).
      IF screen-name = 'VDICS-ORFMNG' OR screen-name = 'VDICS-/CWM/MENGE'.
        IF cl_sd_doc_category_util=>is_delivery_outgoing( iv_vbtyp = gd_vbtyp ) OR
           ( cl_ops_switch_check=>ops_sfws_sc_advret1( ) eq gc_charx AND gd_vbtyp CA IF_SD_DOC_CATEGORY=>DELIVERY_SHIPPING_NOTIF ).
          READ TABLE gt_fkrelk_tab WITH KEY pstyv = vdics-pstyv
                                            BINARY SEARCH.
          IF NOT sy-subrc IS INITIAL OR cl_sd_doc_category_util=>is_any_intercompany( iv_vbtyp = gt_vdics-vbtyp ).  "  CA '56'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
        IF cl_sd_doc_category_util=>is_any_invoice( iv_vbtyp = gd_vbtyp ).
          IF is_rv60a-fkart IS INITIAL.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF screen-name = 'VDICS-ORFMNG' OR screen-name = 'VDICS-/CWM/MENGE'.
        READ TABLE gt_fkrelk_tab WITH KEY pstyv = vdics-pstyv
                                          BINARY SEARCH.
* no partial payment for billing plan items.
        IF sy-subrc IS INITIAL.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FELDAUSWAHL
*&---------------------------------------------------------------------*
*&      Form  VBTYP_ERMITTELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ID_VBELN  text
*      <--P_GD_VBTYP  text
*----------------------------------------------------------------------*
FORM vbtyp_ermitteln USING    id_vbeln LIKE vbrk-vbeln
                     CHANGING gd_vbtyp LIKE vbrk-vbtyp.

  DATA : ls_vbuk LIKE vbuk.
  DATA : da_archived TYPE xflag.
  DATA : switch_active TYPE xfeld.

  switch_active = cl_ops_switch_check=>sd_sfws_sc2( ).

  IF switch_active = 'X'.
* for billing documents: check if it's archived
* Invoice is in archive if we reach this point
    CALL FUNCTION 'RETURN_VBUK'
      EXPORTING
        i_vbeln           = id_vbeln
      IMPORTING
        e_vbuk            = ls_vbuk
      EXCEPTIONS
        invoice_not_found = 4.
  ELSE.
    sy-subrc = 4.
  ENDIF.
  IF sy-subrc NE 0.
* document is on DB
    CALL FUNCTION 'SD_VBUK_READ_FROM_DOC'
      EXPORTING
        i_vbeln             = id_vbeln
      IMPORTING
        es_vbuk             = ls_vbuk
      EXCEPTIONS
        vbeln_not_found     = 1
        vbtyp_not_supported = 2
        vbobj_not_supported = 3
        OTHERS              = 4.
  ENDIF.

  IF sy-subrc IS INITIAL.
    gd_vbtyp = ls_vbuk-vbtyp.
  ELSE.
    MESSAGE e001(vf) RAISING sd_document_not_found.
  ENDIF.
ENDFORM.                    " VBTYP_ERMITTELN
*&---------------------------------------------------------------------*
*&      Form  OT_VBRP_ERZEUGEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_VDICS  text
*      -->P_OT_VBRP  text
*----------------------------------------------------------------------*
FORM ot_vbrp_erzeugen
       TABLES
         gt_vdics STRUCTURE vdics
         ot_vbrp  STRUCTURE vbrp.

  DATA: lt_vbrp TYPE TABLE OF vbrp.

  DATA : lt_vdics LIKE vdics OCCURS 10 WITH HEADER LINE.
  DATA : da_archived TYPE xflag.
  DATA : switch_active TYPE xfeld.

  lt_vdics[] = gt_vdics[].
  LOOP AT lt_vdics WHERE selkz IS INITIAL.
    DELETE lt_vdics INDEX sy-tabix.
  ENDLOOP.
  READ TABLE lt_vdics INDEX 1.
  IF sy-subrc IS INITIAL.

    switch_active = cl_ops_switch_check=>sd_sfws_sc2( ).

    IF switch_active = 'X'.
* for billing documents: check if it's archived
      IF NOT id_vbeln IS INITIAL.
        CALL FUNCTION 'INVOICE_ARCHIVED'
          EXPORTING
            i_vbeln    = id_vbeln
          IMPORTING
            e_archived = da_archived.
      ENDIF.
    ENDIF.

    IF da_archived = 'X' AND switch_active = 'X'.
* Invoice is in archive if we reach this point
      CALL FUNCTION 'RETURN_VBRP'
        EXPORTING
          i_vbeln = lt_vdics-vbeln
        CHANGING
          ch_vbrp = lt_vbrp.

      ot_vbrp[] = lt_vbrp[].
      LOOP AT ot_vbrp.
* delete not selected VBRPS
        READ TABLE lt_vdics WITH KEY vbeln = ot_vbrp-vbeln
                                     posnr = ot_vbrp-posnr.
        IF sy-subrc NE 0.
          DELETE ot_vbrp.
        ENDIF.
      ENDLOOP.
    ELSE.
* document is on DB
      SELECT * FROM            vbrp
             INTO TABLE      ot_vbrp
             FOR ALL ENTRIES IN lt_vdics
             WHERE           vbeln = lt_vdics-vbeln
             AND             posnr = lt_vdics-posnr
             ORDER BY PRIMARY KEY.
    ENDIF.

    LOOP AT lt_vdics.
      READ TABLE ot_vbrp WITH KEY vbeln = lt_vdics-vbeln
                                  posnr = lt_vdics-posnr
                                          BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ot_vbrp-fkimg = lt_vdics-orfmng.
        MODIFY ot_vbrp INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ELSE.
    CLEAR ot_vbrp.
    REFRESH ot_vbrp.
  ENDIF.
ENDFORM.                    " OT_VBRP_ERZEUGEN
*&---------------------------------------------------------------------*
*&      Form  FKDAT_AUS_FPLT_ERMITTELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_VDICS  text
*      -->P_OT_VBRP  text
*----------------------------------------------------------------------*
FORM fkdat_aus_fplt_ermitteln
       USING
         ld_fplnr LIKE vbrp-fplnr
         ld_fpltr LIKE vbrp-fpltr
       CHANGING
         ld_fkdat LIKE vbrk-fkdat.

  READ TABLE gt_fplt WITH KEY fplnr = ld_fplnr
                              fpltr = ld_fpltr
                              BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    SELECT * INTO TABLE gt_fplt FROM fplt
                                WHERE fplnr = ld_fplnr
                                ORDER BY PRIMARY KEY.
    READ TABLE gt_fplt WITH KEY fplnr = ld_fplnr
                                fpltr = ld_fpltr
                                BINARY SEARCH.
  ENDIF.
  IF sy-subrc IS INITIAL.
    ld_fkdat = gt_fplt-afdat.
  ELSE.
    ld_fkdat = sy-datum.
  ENDIF.

ENDFORM.                    "FKDAT_AUS_FPLT_ERMITTELN
