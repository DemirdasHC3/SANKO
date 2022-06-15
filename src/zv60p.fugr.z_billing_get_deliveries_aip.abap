FUNCTION Z_BILLING_GET_DELIVERIES_AIP.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_DELIVERY_KEYS STRUCTURE  LESHP_DELIVERY_KEY
*"      ET_LIKP STRUCTURE  LIKPVB
*"      ET_VBUK STRUCTURE  VBUKVB
*"      ET_VBPA STRUCTURE  VBPAVB
*"      ET_SADR STRUCTURE  SADRVB
*"      ET_LIPS STRUCTURE  LIPSVB
*"      ET_VBUP STRUCTURE  VBUPVB
*"      ET_VBFA STRUCTURE  VBFAVB
*"      ET_VBUV STRUCTURE  VBUVVB
*"      ET_XKOMFKKO STRUCTURE  KOMV
*"      ET_CHAR_VALUES STRUCTURE  BAPI_PAOBJNR_FIELD_VALUES
*"      ET_VBFS STRUCTURE  VBFS
*"      ET_TEXT STRUCTURE  AIP_KOMTXT OPTIONAL
*"  EXCEPTIONS
*"      OTHERS
*"--------------------------------------------------------------------

  TYPE-POOLS coast .

  DATA:
    ls_vbrk        TYPE vbrk,
    ls_vbrp        TYPE vbrp,
    ls_komfk       TYPE komfk,
    ls_xkomv       TYPE komv,
    ls_lips        TYPE lipsvb,
    ls_likp        TYPE likpvb,
    ls_vbup        TYPE vbup,
    ls_tpgi_values TYPE tpgi_values,
    ls_data        TYPE leshp_data,
    ls_vbsk        TYPE vbsk,
    ls_vbfa        TYPE vbfa,
    lt_tpgi_values TYPE tpgi_values OCCURS 0,
    lt_vbfa        TYPE vbfa OCCURS 0,
    lx_delivery    TYPE leshp_delivery_s, "complex structure 1 delivery
    lx_deliveries  TYPE leshp_delivery_t,  "table for many deliveries
    lt_likp        TYPE shp_vl10_likp_t,
    lt_lips        TYPE shp_vl10_lips_t.

  TYPE-POOLS: rkea1.

  DATA:
    repname(8)      VALUE 'RK2L',
    l_subrc         LIKE sy-subrc,
    tabname         LIKE dd02v-tabname VALUE 'CE0',
    l_erkrs         LIKE tkeb-erkrs,
    l_erkrs_old     LIKE tkeb-erkrs,
    l_bukrs         LIKE t001-bukrs,
    l_kokrs_old     LIKE lipsvb-kokrs,
    lt_copadata     LIKE copadata OCCURS 0,
    lt_copadata_old LIKE copadata OCCURS 0,
    ls_copadata     LIKE LINE OF lt_copadata,
    lt_fieldtab     TYPE rkea1_fieldtab WITH HEADER LINE,
    lt_fieldtab_old TYPE rkea1_fieldtab WITH HEADER LINE,
    ls_char_values  TYPE bapi_paobjnr_field_values.

  FIELD-SYMBOLS:
    <lx_delivery> TYPE leshp_delivery_s
    , <ls_item>         TYPE lipsvb .

  DATA:
    lt_xkomfk1 LIKE komfk OCCURS 10,
    lt_xkomfk2 LIKE komfk OCCURS 10,
    lt_xkomfk3 LIKE komfk OCCURS 10,
    lt_xkomfk4 LIKE komfk OCCURS 1,
    lt_xkomv   LIKE komv   OCCURS 10,
    lt_xthead  LIKE theadvb OCCURS 10,
    lt_xvbfs   LIKE vbfs OCCURS 10,
    lt_xvbpa   LIKE vbpavb OCCURS 10,
    lt_xvbrk   LIKE vbrkvb OCCURS 10,
    lt_xvbrp   LIKE vbrpvb OCCURS 10,
    lt_xvbss   LIKE vbss OCCURS 10.

  DATA: lt_delivery_keys TYPE leshp_delivery_key_t.

  DATA: l_lines LIKE sy-tabix.


  lt_delivery_keys[] = it_delivery_keys[].

  ls_data-head_status = 'X'.
  ls_data-head_partner = 'X'.
  ls_data-item     = 'X'.
  ls_data-item_status = 'X'.
  ls_data-doc_flow = 'X'.

  CALL FUNCTION 'LE_DELIVERY_GET_BUFFERED'
    EXPORTING
      if_requested_version   = 0
*     IF_HANDLE              =
*     IF_VBELN               =
*     IS_DELIVERY_KEY        =
      it_delivery_keys       = lt_delivery_keys
      is_data                = ls_data
*     IF_FILTER_BY_ENQUE     =
*     IF_PREDECESSOR_ENQ     =
*     IF_BYPASSING_BUFFER    =
* IMPORTING
*     EX_ADD_DATA            =
    CHANGING
      cx_deliveries          = lx_deliveries
*     CX_UNLOCKABLE          =
    EXCEPTIONS
      no_item_selected       = 1
      selected_item_enqueued = 2
      no_key_specified       = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING others.
  ENDIF.


  LOOP AT lx_deliveries ASSIGNING <lx_delivery>.

    APPEND <lx_delivery>-head TO et_likp.
    APPEND <lx_delivery>-head_status TO et_vbuk.
    APPEND LINES OF <lx_delivery>-head_partner TO et_vbpa.
    APPEND LINES OF <lx_delivery>-head_address TO et_sadr.
    APPEND LINES OF <lx_delivery>-item TO et_lips.
    APPEND LINES OF <lx_delivery>-item_status TO et_vbup.
    APPEND LINES OF <lx_delivery>-doc_flow TO et_vbfa.
    APPEND LINES OF <lx_delivery>-incomplete TO et_vbuv.

    SORT et_likp BY vbeln.
    SORT et_lips BY vbeln posnr.
    SORT et_vbfa BY vbeln posnn.

    ls_komfk-vbeln = <lx_delivery>-head-vbeln.
    ls_komfk-vbtyp = <lx_delivery>-head-vbtyp.
    ls_komfk-fkart = <lx_delivery>-head-fkaiv.
    ls_komfk-mandt = sy-mandt.

    IF <lx_delivery>-head_status-fkivk CA 'AB'.
      APPEND ls_komfk TO lt_xkomfk1.
    ELSEIF <lx_delivery>-head_status-fkivk CA 'C'.
      LOOP AT <lx_delivery>-doc_flow INTO ls_vbfa
              WHERE vbelv = <lx_delivery>-head-vbeln
              AND   vbtyp_n = if_sd_doc_category=>intercompany_invoice.
* Das Feld für Stornonummer wird für IV-Belegnummer benutzt.
        ls_komfk-sfakn = ls_vbfa-vbeln.
        APPEND ls_komfk TO lt_xkomfk2.
      ENDLOOP.
    ELSE.                       " fkivk = '', d.h. nicht IV relevant
      LOOP AT et_vbfa INTO ls_vbfa
                      WHERE   vbelv = <lx_delivery>-head-vbeln
                      AND     vbtyp_n = if_sd_doc_category=>goods_movement
                      AND NOT bwart IS INITIAL .
        READ TABLE et_lips INTO ls_lips
                           WITH KEY vbeln = ls_vbfa-vbelv
                                    posnr = ls_vbfa-posnv
                           BINARY SEARCH.
        CLEAR ls_tpgi_values.
        ls_tpgi_values-bwtar = ls_lips-bwtar.
        ls_tpgi_values-matnr = ls_lips-matnr.
        ls_tpgi_values-menge = ls_lips-lfimg.
        ls_tpgi_values-mtart = ls_lips-mtart.
        ls_tpgi_values-werks = ls_lips-werks.
        ls_tpgi_values-meins = ls_lips-meins.
        ls_tpgi_values-mblnr = ls_vbfa-vbeln.
        ls_tpgi_values-zeile = ls_vbfa-posnn.
        ls_tpgi_values-erdat = ls_vbfa-erdat.

        APPEND ls_tpgi_values TO lt_tpgi_values.

        CLEAR ls_xkomv.
        ls_xkomv-knumv = ls_vbfa-vbelv.
        ls_xkomv-kposn = ls_vbfa-posnv.
        ls_xkomv-kntyp = 'G'.
        ls_xkomv-kwert = ls_vbfa-rfwrt.
        ls_xkomv-waers = ls_vbfa-waers.
        ls_xkomv-kmein = ls_vbfa-meins.
        APPEND ls_xkomv TO et_xkomfkko.
      ENDLOOP.
    ENDIF.

* Falls Vor Warenausgang im Auftragssystem fakturiert wird, werden die
* betroffenen Lieferungen und deren Positionen gesammelt.

    IF <lx_delivery>-head_status-wbstk NE 'C'.
      LOOP AT <lx_delivery>-item_status
              INTO ls_vbup
              WHERE wbsta NE 'C'.
        READ TABLE et_likp INTO ls_likp
                           WITH KEY vbeln = ls_vbup-vbeln
                           BINARY SEARCH.
        IF sy-subrc = 0.
          APPEND ls_likp TO lt_likp.
        ENDIF.
        READ TABLE et_lips INTO ls_lips
                           WITH KEY vbeln = ls_vbup-vbeln
                                    posnr = ls_vbup-posnr
                           BINARY SEARCH.
        IF sy-subrc = 0.
          APPEND ls_lips TO lt_lips.
        ENDIF.
      ENDLOOP.
      APPEND ls_komfk TO lt_xkomfk3.
    ENDIF.
  ENDLOOP.

* Für nicht WA-gebuchte Lieferungen kann man mit dem BADI Kosten,
* Chargendaten etc. holen.

  IF NOT lt_xkomfk3[] IS INITIAL.
    CLASS cl_exithandler DEFINITION LOAD.         "Vorwärtsdeklaration
    DATA exit TYPE REF TO if_ex_vor_wa_faktura.      "Interfacereferenz

    CALL METHOD cl_exithandler=>get_instance      "Aufruf der Factory-
      CHANGING
        instance = exit.               "Methode
    REFRESH lt_xkomv.
    CALL METHOD exit->vor_wa_data_get             "Aufruf des Add-Ins
      EXPORTING
        it_komfk = lt_xkomfk3[]
      CHANGING
        ct_likp  = lt_likp[]
        ct_lips  = lt_lips[]
        ct_komv  = lt_xkomv[].

    LOOP AT lt_likp INTO ls_likp.
      READ TABLE et_likp WITH KEY vbeln = ls_likp-vbeln
                         TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MODIFY et_likp FROM ls_likp INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_lips INTO ls_lips.
      READ TABLE et_lips WITH KEY vbeln = ls_lips-vbeln
                                  posnr = ls_lips-posnr
                         TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MODIFY et_lips FROM ls_lips INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    APPEND LINES OF lt_xkomv TO et_xkomfkko.

*** Auflösen der alten Ergebnisobjekte + Füllen von ET_CHAR_VALUES mit
*** zugehörigen Merkmalen
    LOOP AT et_lips INTO ls_lips
                    WHERE NOT paobjnr IS INITIAL.
      IF ls_lips-kokrs NE l_kokrs_old.
* Bestimmung des Ergebnisbereichs aus dem Kostenrechnungskreis
        l_kokrs_old = ls_lips-kokrs.
        CALL FUNCTION 'COPA_ERKRS_FIND'
          EXPORTING
            bukrs  = l_bukrs
            kokrs  = ls_lips-kokrs
          IMPORTING
            erkrs  = l_erkrs
          EXCEPTIONS
            OTHERS = 1.
        IF sy-subrc NE 0.
          MESSAGE x001(ke).
        ENDIF.
        IF l_erkrs NE l_erkrs_old.
          CLEAR: lt_fieldtab_old[], lt_copadata_old[].
* Bestimmung des CO-PA-Feldkatalogs
          l_erkrs_old  = l_erkrs.
          tabname+3(4) = l_erkrs.
          CALL FUNCTION 'RKE_FILL_FIELD_TABLE'
            EXPORTING
              erkrs    = l_erkrs
              tabname  = tabname
            TABLES
              fieldtab = lt_fieldtab_old.
          SORT lt_fieldtab_old BY tabname fieldname.
* Tabelle mit CO-PA-Merkmalen erzeugen (bis auf Mengeneinheiten)
          LOOP AT lt_fieldtab_old WHERE pos_ce4  > 0
                                  AND   keyflag  IS INITIAL
                                  AND   datatype NE 'UNIT'
                                  AND   usgfl    NE 'T'.
            CLEAR ls_copadata.
            ls_copadata-fnam = lt_fieldtab_old-fieldname.
            APPEND ls_copadata TO lt_copadata_old.
          ENDLOOP.
        ENDIF.             " l_erkrs ne l_erkrs_old
      ENDIF.               " ls_lips-kokrs ne l_kokrs_old

      lt_copadata[] = lt_copadata_old[].
      repname+4(4) = l_erkrs_old.
      tabname+3(4) = l_erkrs_old.
* Tabelle COPADATA mit Werten des alten Erg.Objekts füllen
      PERFORM fill_copadata_with_criteria IN PROGRAM (repname)
                                          TABLES lt_copadata
                                                 lt_fieldtab_old
                                          USING  ls_lips-paobjnr
                                                 l_subrc.
      IF l_subrc NE 0.
        MESSAGE ID 'KE' TYPE 'E' NUMBER '499'
                WITH ls_lips-paobjnr l_erkrs_old.
      ENDIF.
* (nicht initiale) Merkmalswerte des alten Erg.Objekts in's IDoc stellen
      LOOP AT lt_copadata INTO ls_copadata
                          WHERE NOT fval IS INITIAL.
        CLEAR ls_char_values.
        READ TABLE lt_fieldtab_old WITH KEY tabname   = tabname
                                            fieldname = ls_copadata-fnam
                                            BINARY SEARCH.
        IF sy-subrc NE 0.
          MESSAGE x001(ke).
        ENDIF.
* Konvertierung in internes Format
        CALL FUNCTION 'G_CONVERT_INPUT'
          EXPORTING
            converted_length = lt_fieldtab_old-leng
            convexit         = lt_fieldtab_old-convexit
            datatype         = lt_fieldtab_old-datatype
            input_length     = lt_fieldtab_old-outputlen
            input_value      = ls_copadata-fval
          IMPORTING
            converted_value  = ls_copadata-fval
          EXCEPTIONS
            OTHERS           = 1.
        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
* Einfügen in Tabelle ET_CHAR_VALUES
        ls_char_values-paobjnr    = ls_lips-paobjnr.
        ls_char_values-fieldname  = ls_copadata-fnam.
        ls_char_values-fieldvalue = ls_copadata-fval.
        APPEND ls_char_values TO et_char_values.
      ENDLOOP.                       " at lt_copadata
    ENDLOOP.                         " at et_lips

    EXIT.
  ENDIF.

  CLEAR: l_lines.
  DESCRIBE TABLE lt_tpgi_values LINES l_lines.

  IF l_lines > 0.
    CALL FUNCTION 'TP_GI_VALUES_GET'
      TABLES
        t_tp_values  = lt_tpgi_values
      EXCEPTIONS
        no_tp_active = 1
        error_01     = 2
        OTHERS       = 3.
    IF sy-subrc > 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING others.
    ENDIF.

    LOOP AT lt_tpgi_values INTO ls_tpgi_values
                           WHERE valutyp CA '12'.
      CLEAR ls_xkomv.
      IF ls_tpgi_values-valutyp = '1'.
        ls_xkomv-kntyp = 'b'.
      ELSE.
        ls_xkomv-kntyp = 'c'.
      ENDIF.
      READ TABLE et_vbfa INTO ls_vbfa
                         WITH KEY vbeln = ls_tpgi_values-mblnr
                                  posnn =  ls_tpgi_values-zeile
                         BINARY SEARCH.
      ls_xkomv-knumv = ls_vbfa-vbelv.
      ls_xkomv-kposn = ls_vbfa-posnv.
      ls_xkomv-kwert = ls_tpgi_values-prs.
      ls_xkomv-waers = ls_tpgi_values-waers.
      ls_xkomv-kmein = ls_tpgi_values-meins.

      APPEND ls_xkomv TO et_xkomfkko.
    ENDLOOP.

  ENDIF.


*** Auflösen der alten Ergebnisobjekte + Füllen von ET_CHAR_VALUES mit
*** zugehörigen Merkmalen
  LOOP AT et_lips INTO ls_lips
                  WHERE NOT paobjnr IS INITIAL.
    IF ls_lips-kokrs NE l_kokrs_old.
* Bestimmung des Ergebnisbereichs aus dem Kostenrechnungskreis
      l_kokrs_old = ls_lips-kokrs.
      CALL FUNCTION 'COPA_ERKRS_FIND'
        EXPORTING
          bukrs  = l_bukrs
          kokrs  = ls_lips-kokrs
        IMPORTING
          erkrs  = l_erkrs
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc NE 0.
        MESSAGE x001(ke).
      ENDIF.
      IF l_erkrs NE l_erkrs_old.
        CLEAR: lt_fieldtab_old[], lt_copadata_old[].
* Bestimmung des CO-PA-Feldkatalogs
        l_erkrs_old  = l_erkrs.
        tabname+3(4) = l_erkrs.
        CALL FUNCTION 'RKE_FILL_FIELD_TABLE'
          EXPORTING
            erkrs    = l_erkrs
            tabname  = tabname
          TABLES
            fieldtab = lt_fieldtab_old.
        SORT lt_fieldtab_old BY tabname fieldname.
* Tabelle mit CO-PA-Merkmalen erzeugen (bis auf Mengeneinheiten)
        LOOP AT lt_fieldtab_old WHERE pos_ce4  > 0
                                AND   keyflag  IS INITIAL
                                AND   datatype NE 'UNIT'
                                AND   usgfl    NE 'T'.
          CLEAR ls_copadata.
          ls_copadata-fnam = lt_fieldtab_old-fieldname.
          APPEND ls_copadata TO lt_copadata_old.
        ENDLOOP.
      ENDIF.             " l_erkrs ne l_erkrs_old
    ENDIF.               " ls_lips-kokrs ne l_kokrs_old

    lt_copadata[] = lt_copadata_old[].
    repname+4(4) = l_erkrs_old.
    tabname+3(4) = l_erkrs_old.
* Tabelle COPADATA mit Werten des alten Erg.Objekts füllen
    PERFORM fill_copadata_with_criteria IN PROGRAM (repname)
                                        TABLES lt_copadata
                                               lt_fieldtab_old
                                        USING  ls_lips-paobjnr
                                               l_subrc.
    IF l_subrc NE 0.
      MESSAGE ID 'KE' TYPE 'E' NUMBER '499'
              WITH ls_lips-paobjnr l_erkrs_old.
    ENDIF.
* (nicht initiale) Merkmalswerte des alten Erg.Objekts in's IDoc stellen
    LOOP AT lt_copadata INTO ls_copadata
                        WHERE NOT fval IS INITIAL.
      CLEAR ls_char_values.
      READ TABLE lt_fieldtab_old WITH KEY tabname   = tabname
                                          fieldname = ls_copadata-fnam
                                          BINARY SEARCH.
      IF sy-subrc NE 0.
        MESSAGE x001(ke).
      ENDIF.
* Konvertierung in internes Format
      CALL FUNCTION 'G_CONVERT_INPUT'
        EXPORTING
          converted_length = lt_fieldtab_old-leng
          convexit         = lt_fieldtab_old-convexit
          datatype         = lt_fieldtab_old-datatype
          input_length     = lt_fieldtab_old-outputlen
          input_value      = ls_copadata-fval
        IMPORTING
          converted_value  = ls_copadata-fval
        EXCEPTIONS
          OTHERS           = 1.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
* Einfügen in Tabelle ET_CHAR_VALUES
      ls_char_values-paobjnr    = ls_lips-paobjnr.
      ls_char_values-fieldname  = ls_copadata-fnam.
      ls_char_values-fieldvalue = ls_copadata-fval.
      APPEND ls_char_values TO et_char_values.
    ENDLOOP.                       " at lt_copadata
  ENDLOOP.                         " at et_lips


  CLEAR: ls_komfk, l_lines.
  DESCRIBE TABLE lt_xkomfk1 LINES l_lines.

  IF l_lines > 0.
    lt_xkomfk4[] = lt_xkomfk1[].
    CALL FUNCTION 'RV_INVOICE_REFRESH'
      EXPORTING
        with_posting = ' '
      TABLES
        xkomfk       = lt_xkomfk4
        xkomv        = lt_xkomv
        xthead       = lt_xthead
        xvbfs        = lt_xvbfs
        xvbpa        = lt_xvbpa
        xvbrk        = lt_xvbrk
        xvbrp        = lt_xvbrp
        xvbss        = lt_xvbss.

    REFRESH: lt_xkomv, lt_xkomfk4.
    LOOP AT lt_xkomfk1 INTO ls_komfk.
      APPEND ls_komfk TO lt_xkomfk4.
      CALL FUNCTION 'RV_INVOICE_CREATE'
        EXPORTING
          vbsk_i       = ls_vbsk
          with_posting = ' '
        TABLES
          xkomfk       = lt_xkomfk4
          xkomv        = lt_xkomv
          xthead       = lt_xthead
          xvbfs        = lt_xvbfs
          xvbpa        = lt_xvbpa
          xvbrk        = lt_xvbrk
          xvbrp        = lt_xvbrp
          xvbss        = lt_xvbss.

* Fehlerprotokoll mitnehmen
      APPEND LINES OF lt_xvbfs TO et_vbfs.
* Nettowert der Internen Verrechnung mitnehmen, später als
* Verrechnungspreis der Kundenfaktura benutzen.
      READ TABLE lt_xvbrk INTO ls_vbrk INDEX 1.
      CHECK sy-subrc IS INITIAL.
      LOOP AT lt_xvbrp INTO ls_vbrp.
        CLEAR ls_xkomv.
        ls_xkomv-knumv = ls_vbrp-vgbel.
        ls_xkomv-kposn = ls_vbrp-vgpos.
        ls_xkomv-kwert = ls_vbrp-netwr.
        ls_xkomv-kntyp = 'G'.
        APPEND ls_xkomv TO et_xkomfkko.
* Transferpreise mitnehmen.
        LOOP AT lt_xkomv INTO ls_xkomv
                         WHERE knumv = ls_vbrk-vbeln
                         AND   kposn = ls_vbrp-posnr
                         AND   kntyp CA 'bc'
                         AND   kinak =   ' '      .
          ls_xkomv-knumv = ls_vbrp-vgbel.
          ls_xkomv-kposn = ls_vbrp-vgpos.
          APPEND ls_xkomv TO et_xkomfkko.
        ENDLOOP.
      ENDLOOP.
      REFRESH lt_xkomfk4.
    ENDLOOP.

  ENDIF.

  CLEAR: l_lines.
  DESCRIBE TABLE lt_xkomfk2 LINES l_lines.

  IF l_lines > 0.
    lt_xkomfk4[] = lt_xkomfk2[].
    CALL FUNCTION 'RV_INVOICE_REFRESH'
      EXPORTING
        with_posting = ' '
      TABLES
        xkomfk       = lt_xkomfk4
        xkomv        = lt_xkomv
        xthead       = lt_xthead
        xvbfs        = lt_xvbfs
        xvbpa        = lt_xvbpa
        xvbrk        = lt_xvbrk
        xvbrp        = lt_xvbrp
        xvbss        = lt_xvbss.

    LOOP AT lt_xkomfk2 INTO ls_komfk.
      MOVE ls_komfk-sfakn TO ls_vbrk-vbeln.
      REFRESH lt_xkomv.

      CALL FUNCTION 'RV_INVOICE_DOCUMENT_READ'
        EXPORTING
          konv_read            = 'X'
          i_no_authority_check = 'X'
          vbrk_i               = ls_vbrk
        TABLES
          xkomv                = lt_xkomv
          xvbpa                = lt_xvbpa
          xvbrk                = lt_xvbrk
          xvbrp                = lt_xvbrp
          xvbfs                = lt_xvbfs
        EXCEPTIONS
          no_authority         = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING others.
      ENDIF.
* Fehlerprotocol mitnehmen
      APPEND LINES OF lt_xvbfs TO et_vbfs.

      READ TABLE lt_xvbrk INTO ls_vbrk INDEX 1.
      CHECK sy-subrc IS INITIAL.
      LOOP AT lt_xvbrp INTO ls_vbrp.
        CLEAR ls_xkomv.
        ls_xkomv-knumv = ls_vbrp-vgbel.
        ls_xkomv-kposn = ls_vbrp-vgpos.
        ls_xkomv-kwert = ls_vbrp-netwr.
        ls_xkomv-kntyp = 'G'.
        APPEND ls_xkomv TO et_xkomfkko.
        LOOP AT lt_xkomv INTO ls_xkomv
                         WHERE knumv = ls_vbrk-knumv
                         AND   kposn = ls_vbrp-posnr
                         AND   kntyp CA 'bc'
                         AND   kinak =   ' '      .
          ls_xkomv-knumv = ls_vbrp-vgbel.
          ls_xkomv-kposn = ls_vbrp-vgpos.
          APPEND ls_xkomv TO et_xkomfkko.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    SORT et_xkomfkko BY knumv kposn kntyp.
    DELETE ADJACENT DUPLICATES FROM et_xkomfkko
                               COMPARING knumv kposn kntyp.
  ENDIF.

* Texte zu den Lieferungen ermitteln und uebergeben
  DATA: l_tdname LIKE thead-tdname.
  DATA: BEGIN OF ls_rvbeln,
          vbeln_vl LIKE leshp_delivery_key-vbeln,
          stern    VALUE '*',
        END   OF ls_rvbeln.
  DATA: BEGIN OF lt_thead OCCURS 10.
          INCLUDE STRUCTURE thead.
  DATA: END   OF lt_thead.
  DATA: BEGIN OF lt_thead_vbbk OCCURS 10.
          INCLUDE STRUCTURE thead.
  DATA: END   OF lt_thead_vbbk.
  DATA: BEGIN OF lt_thead_vbbp OCCURS 10.
          INCLUDE STRUCTURE thead.
  DATA: END   OF lt_thead_vbbp.
  DATA: BEGIN OF lt_tline OCCURS 50.
          INCLUDE STRUCTURE tline.
  DATA: END   OF lt_tline.

  LOOP AT it_delivery_keys.
    REFRESH lt_thead.
    REFRESH lt_thead_vbbk.
    REFRESH lt_thead_vbbp.
    ls_rvbeln-vbeln_vl = it_delivery_keys-vbeln.
    ls_rvbeln-stern    = '*'.
    l_tdname  = ls_rvbeln.
* zugehörige Texte zur Lieferung besorgen
* VBBK
    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        object     = 'VBBK'
        name       = l_tdname
      TABLES
        selections = lt_thead_vbbk.
* VBBP
    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        object     = 'VBBP'
        name       = l_tdname
      TABLES
        selections = lt_thead_vbbp.

    INSERT LINES OF lt_thead_vbbk INTO TABLE lt_thead.
    INSERT LINES OF lt_thead_vbbp INTO TABLE lt_thead.

    LOOP AT lt_thead.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT   = SY-MANDT
          id       = lt_thead-tdid
          language = lt_thead-tdspras
          name     = lt_thead-tdname
          object   = lt_thead-tdobject
*         ARCHIVE_HANDLE          = 0
*          IMPORTING
*         HEADER   =
        TABLES
          lines    = lt_tline
*          EXCEPTIONS
*         ID       = 1
*         LANGUAGE = 2
*         NAME     = 3
*         NOT_FOUND               = 4
*         OBJECT   = 5
*         REFERENCE_CHECK         = 6
*         WRONG_ACCESS_TO_ARCHIVE = 7
*         OTHERS   = 8
        .
      IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      LOOP AT lt_tline.
        MOVE-CORRESPONDING lt_thead  TO et_text.
        MOVE-CORRESPONDING lt_tline  TO et_text.
        APPEND et_text.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDFUNCTION.
