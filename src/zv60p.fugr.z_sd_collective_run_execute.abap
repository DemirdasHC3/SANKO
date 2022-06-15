FUNCTION Z_SD_COLLECTIVE_RUN_EXECUTE.
*"--------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(V60P_INPUT_RV60A) LIKE  RV60A STRUCTURE  RV60A
*"     VALUE(V60P_INPUT_SMART) LIKE  VBSK-SMART DEFAULT 'F'
*"     VALUE(V60P_INPUT_SAMMG) LIKE  VBSK-SAMMG OPTIONAL
*"     VALUE(ID_UTASY) DEFAULT ' '
*"     VALUE(ID_UTSWL) DEFAULT ' '
*"     VALUE(ID_UTSNL) DEFAULT ' '
*"     VALUE(ID_NO_NEW_RUN) DEFAULT ' '
*"     VALUE(ID_INVOICE_LIST) DEFAULT SPACE
*"     VALUE(IV_OPT_ENABLED) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"  EXPORTING
*"     VALUE(V60P_OUTPUT_VBSK) LIKE  VBSK STRUCTURE  VBSK
*"  TABLES
*"      V60P_INPUT_VKDFIF STRUCTURE  VKDFIF
*"      V60P_OUTPUT_VBFS STRUCTURE  VBFS
*"      V60P_OUTPUT_VBSS STRUCTURE  VBSS
*"--------------------------------------------------------------------
  DATA : max_documents LIKE sy-tabix.
  DATA : counter LIKE sy-tabix.
  DATA : rcode(2) TYPE p.
  DATA : xkomfk LIKE komfk OCCURS 10 WITH HEADER LINE.
  DATA : xvbfs  LIKE vbfs OCCURS 10 WITH HEADER LINE.
  DATA : xvbss  LIKE vbss OCCURS 10 WITH HEADER LINE.
  DATA : xvbsk  LIKE vbsk OCCURS 10 WITH HEADER LINE.
  DATA : epro_flag TYPE c.
  DATA : ld_sammg LIKE vbsk-sammg.

  CHECK rcode = 0.
  max_documents = 1000.
  CALL CUSTOMER-FUNCTION '009'
       TABLES
            ct_vkdfif = v60p_input_vkdfif
       CHANGING
            cd_max_documents = max_documents
       EXCEPTIONS
            OTHERS     = 1.
  CLEAR counter.
  REFRESH xkomfk.
  REFRESH xvbfs.
  REFRESH xvbss.
  CLEAR epro_flag.
  xvbsk-mandt = sy-mandt.
  xvbsk-ernam = sy-uname.
  xvbsk-erdat = sy-datum.
  xvbsk-uzeit = sy-uzeit.
  xvbsk-smart = v60p_input_smart.
  xvbsk-sammg = v60p_input_sammg.
  SORT v60p_input_vkdfif BY kunnr vkorg v_fkart v_fkdat sortkri
                            fkdat fkart vbeln.
  CALL CUSTOMER-FUNCTION '010'
       TABLES
            ct_vkdfif = v60p_input_vkdfif
       EXCEPTIONS
            OTHERS     = 1.
*ENHANCEMENT-POINT SD_COLLECTIVE_RUN_EXECUTE_01 SPOTS ES_SAPLV60P.
*IF NOT V60P_INPUT_NO_DIRECT_POSTING IS INITIAL.
  LOOP AT v60p_input_vkdfif WHERE selkz NE space.
    ON CHANGE OF v60p_input_vkdfif-kunnr OR
                 v60p_input_vkdfif-vkorg OR
                 v60p_input_vkdfif-v_fkart.
      IF counter > 0.
        CLEAR sy-calld.
        DELETE ADJACENT DUPLICATES FROM xkomfk.
        PERFORM rv_invoice_create
          TABLES
            xkomfk
            xvbfs
            xvbss
          USING
            v60p_input_rv60a
            id_utasy
            id_utswl
            id_utsnl
            id_invoice_list
          CHANGING
            xvbsk
            ld_sammg.
        CLEAR counter.
        REFRESH xkomfk.
      ENDIF.
    ENDON.

    ADD 1 TO counter.
    CLEAR xkomfk.
    MOVE v60p_input_vkdfif-mandt TO xkomfk-mandt.
    MOVE v60p_input_vkdfif-vbeln TO xkomfk-vbeln.
    MOVE v60p_input_vkdfif-vbtyp TO xkomfk-vbtyp.
    MOVE v60p_input_vkdfif-fkdat TO xkomfk-seldat.
    MOVE v60p_input_vkdfif-fkart TO xkomfk-fkart.
    APPEND xkomfk.

    IF counter > max_documents.        "maximale Anzahl Belege
      CLEAR sy-calld.
      DELETE ADJACENT DUPLICATES FROM xkomfk.
      PERFORM rv_invoice_create
        TABLES
          xkomfk
          xvbfs
          xvbss
        USING
          v60p_input_rv60a
          id_utasy
          id_utswl
          id_utsnl
          id_invoice_list
        CHANGING
          xvbsk
          ld_sammg.
      CLEAR counter.
      REFRESH xkomfk.
    ENDIF.
  ENDLOOP.

  IF counter <> 0.
    CLEAR sy-calld.
    DELETE ADJACENT DUPLICATES FROM xkomfk.
    PERFORM rv_invoice_create
              TABLES
                xkomfk
                xvbfs
                xvbss
              USING
                v60p_input_rv60a
                id_utasy
                id_utswl
                id_utsnl
                id_invoice_list
              CHANGING
                xvbsk
                ld_sammg.
    CLEAR counter.
    REFRESH xkomfk.
  ENDIF.

  REFRESH v60p_output_vbfs.
  REFRESH v60p_output_vbss.
  v60p_output_vbfs[] = xvbfs[].
  v60p_output_vbss[] = xvbss[].
  v60p_output_vbsk   = xvbsk.

ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  RV_INVOICE_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rv_invoice_create
       TABLES
         xkomfk STRUCTURE komfk
         xvbfs  STRUCTURE vbfs
         xvbss  STRUCTURE vbss
       USING
         gd_default_data LIKE rv60a
         id_utasy
         id_utswl
         id_utsnl
         id_invoice_list
       CHANGING
         xvbsk LIKE vbsk
         ld_sammg LIKE vbsk-sammg.

  DATA : yvbfs LIKE vbfs OCCURS 10 WITH HEADER LINE.
  DATA : yvbss LIKE vbss OCCURS 10 WITH HEADER LINE.
  DATA : ld_vbsk LIKE vbsk.
  DATA : xkomv LIKE komv OCCURS 10 WITH HEADER LINE.
  DATA : xthead LIKE theadvb OCCURS 10 WITH HEADER LINE.
  DATA : xvbpa LIKE vbpavb OCCURS 10 WITH HEADER LINE.
  DATA : xvbrk LIKE vbrkvb OCCURS 10 WITH HEADER LINE.
  DATA : xvbrl LIKE vbrlvb OCCURS 10 WITH HEADER LINE.
  DATA : xvbrp LIKE vbrpvb OCCURS 10 WITH HEADER LINE.
  DATA : ld_posting TYPE c.
  DATA : ld_no_vblog TYPE c.
  DATA : xkomfk_dummy LIKE komfk OCCURS 0 WITH HEADER LINE.


  REFRESH yvbfs.
  REFRESH yvbss.
  IF id_invoice_list IS INITIAL.
    CALL FUNCTION 'RV_INVOICE_REFRESH'
      EXPORTING
        with_posting = 'B'
      TABLES
        xkomfk       = xkomfk_dummy
        xkomv        = xkomv
        xthead       = xthead
        xvbfs        = xvbfs
        xvbss        = xvbss
        xvbpa        = xvbpa
        xvbrk        = xvbrk
        xvbrp        = xvbrp.
  ELSE.
    CALL FUNCTION 'RV_INVOICE_LIST_REFRESH'
      EXPORTING
        with_posting = 'B'
      TABLES
        xkomfk       = xkomfk_dummy
        xkomv        = xkomv
        xthead       = xthead
        xvbfs        = xvbfs
        xvbss        = xvbss
        xvbpa        = xvbpa
        xvbrk        = xvbrk
        xvbrl        = xvbrl.
  ENDIF.

  IF id_no_new_run IS INITIAL.
    IF id_invoice_list IS INITIAL.
      PERFORM sammelgangsnr_ermitteln USING 'F' CHANGING ld_sammg.
      xvbsk-sammg = ld_sammg.
    ELSE.
      PERFORM sammelgangsnr_ermitteln USING 'R' CHANGING ld_sammg.
      xvbsk-sammg = ld_sammg.
    ENDIF.
  ENDIF.

  ld_vbsk = xvbsk.
  ld_posting = 'B'.            "default for UTASY = 'X'
  IF NOT id_utswl IS INITIAL.
    ld_posting = 'D'.
  ELSEIF NOT id_utsnl IS INITIAL.
    ld_posting = 'D'.
    ld_no_vblog = 'X'.
  ENDIF.

  IF id_invoice_list IS INITIAL.
    CALL FUNCTION 'RV_INVOICE_CREATE'
      EXPORTING
        with_posting  = ld_posting
        i_no_vblog    = ld_no_vblog
        vbsk_i        = xvbsk
        delivery_date = gd_default_data-fbuda
        pricing_date  = gd_default_data-prsdt
        invoice_date  = gd_default_data-fkdat
        invoice_type  = gd_default_data-fkart
        iv_rfbfk      = gd_default_data-rfbfk
      IMPORTING
        vbsk_e        = xvbsk
      TABLES
        xkomv         = xkomv
        xvbpa         = xvbpa
        xvbrp         = xvbrp
        xvbrk         = xvbrk
        xkomfk        = xkomfk
        xthead        = xthead
        xvbfs         = yvbfs
        xvbss         = yvbss.
  ELSE.
    CALL FUNCTION 'RV_INVOICE_LIST_CREATE'
      EXPORTING
        with_posting    = ld_posting
        i_no_vblog      = ld_no_vblog
        vbsk_i          = xvbsk
        delivery_date   = gd_default_data-fbuda
        pricing_date    = gd_default_data-prsdt
        invoice_date    = gd_default_data-fkdat
        invoice_type    = gd_default_data-fkart
        iv_opt_enabled  = iv_opt_enabled
        iv_rfbfk        = gd_default_data-rfbfk
      IMPORTING
        vbsk_e          = xvbsk
      TABLES
        xkomv           = xkomv
        xvbpa           = xvbpa
        xvbrl           = xvbrl
        xvbrk           = xvbrk
        xkomfk          = xkomfk
        xthead          = xthead
        xvbfs           = yvbfs
        xvbss           = yvbss.
  ENDIF.

  LOOP AT yvbfs.
    xvbfs = yvbfs.
    APPEND xvbfs.
  ENDLOOP.
  LOOP AT yvbss.
    xvbss = yvbss.
    APPEND xvbss.
  ENDLOOP.
  ADD ld_vbsk-vbnum TO xvbsk-vbnum.
  ADD ld_vbsk-ernum TO xvbsk-ernum.
  COMMIT WORK.
*ENHANCEMENT-POINT SD_COLLECTIVE_RUN_EXECUTE_02 SPOTS ES_SAPLV60P.

ENDFORM.                               " RV_INVOICE_CREATE
