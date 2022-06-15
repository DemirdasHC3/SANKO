FUNCTION Z_VKDFS_ALV_DISPLAY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VKDFS_CALLBACK_PROGRAM) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VKDFS_CALLBACK_PF_STATUS_SET) TYPE  SLIS_FORMNAME
*"       OPTIONAL
*"     VALUE(I_VKDFS_CALLBACK_USER_COMMAND) TYPE  SLIS_FORMNAME
*"       OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_PROGNAME) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_INT_TABNAME) TYPE  SLIS_TABNAME OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_INCLNAME) LIKE  TRDIR-NAME OPTIONAL
*"     VALUE(I_VKDFS_FIELDCAT_STRUCTURE_NAM) LIKE  DD02L-TABNAME
*"       OPTIONAL
*"     VALUE(I_DEFAULT_DATA) LIKE  RV60A STRUCTURE  RV60A
*"     VALUE(I_VARIANT) LIKE  DISVARIANT STRUCTURE  DISVARIANT OPTIONAL
*"     VALUE(I_GRID_CONTROL) DEFAULT 'X'
*"     VALUE(I_INVOICE_LIST) DEFAULT SPACE
*"     VALUE(I_SAVE) DEFAULT 'A'
*"     VALUE(IV_OPT_ENABLED) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"  TABLES
*"      I_FVKDFI STRUCTURE  ZVKDFIF
*"----------------------------------------------------------------------
  DATA : pt_fieldcat               TYPE slis_t_fieldcat_alv.
  DATA : ps_layout                 TYPE slis_layout_alv.
  DATA : pt_outtab                 TYPE sd_alv .
  DATA : pt_events                 TYPE slis_t_event.
  DATA : ld_callback_program       LIKE sy-repid
                                   VALUE 'SAPLZV60P'.
  DATA : ld_callback_pf_status_set TYPE  slis_formname
                                   VALUE 'Z_VKDFS_PF_STATUS_SET'.
  DATA : ld_callback_user_command  TYPE  slis_formname
                                   VALUE 'Z_VKDFS_USER_COMMAND'."'VKDFS_USER_COMMAND'.
  DATA : ld_fieldcat_wa TYPE slis_fieldcat_alv.
  DATA : lv_dd04v       TYPE dd04v.

  gt_fvkdfi[] = i_fvkdfi[].
  gd_default_data = i_default_data.
  IF NOT i_vkdfs_callback_program IS INITIAL.
    ld_callback_program = i_vkdfs_callback_program.
  ENDIF.
  IF NOT i_vkdfs_callback_pf_status_set IS INITIAL.
    ld_callback_pf_status_set = i_vkdfs_callback_pf_status_set.
  ENDIF.
  IF NOT i_vkdfs_callback_user_command IS INITIAL.
    ld_callback_user_command = i_vkdfs_callback_user_command.
  ENDIF.
  IF NOT i_invoice_list IS INITIAL.
    gd_invoice_list = i_invoice_list.
  ENDIF.
  IF NOT iv_opt_enabled IS INITIAL.
    gd_opt_enabled = iv_opt_enabled.
  ENDIF.
  ps_layout-get_selinfos      = 'X'.
  ps_layout-colwidth_optimize = 'X'.
  ps_layout-detail_popup      = 'X'.
  ps_layout-box_fieldname     = 'SELKZ'.
  ps_layout-no_keyfix         = 'X'.
  ps_layout-key_hotspot       = 'X'.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = 'SAPLZV60P'
      i_internal_tabname     = 'GT_FVKDFI'
      i_inclname             = 'SAPLZV60P'
    CHANGING
      ct_fieldcat            = pt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF i_vkdfs_fieldcat_progname = 'SDINVLDL'.
    " KUNNR Texte
    READ TABLE pt_fieldcat REFERENCE INTO DATA(lr_fieldcat)
                           WITH KEY fieldname = 'KUNNR'.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name                = 'KUNRG'   " Payer
          state               = 'A'
          langu               = sy-langu
       IMPORTING
         dd04v_wa            = lv_dd04v.
      lr_fieldcat->seltext_l    = lv_dd04v-scrtext_l.
      lr_fieldcat->seltext_m    = lv_dd04v-scrtext_m.
      lr_fieldcat->seltext_s    = lv_dd04v-scrtext_s.
      lr_fieldcat->reptext_ddic = lv_dd04v-reptext.
    ENDIF.
    " NAME1 Texte
    READ TABLE pt_fieldcat REFERENCE INTO lr_fieldcat
                           WITH KEY fieldname = 'NAME1'.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name                = 'NAME_RG'   " Name Payer
          state               = 'A'
          langu               = sy-langu
       IMPORTING
         dd04v_wa            = lv_dd04v.
      lr_fieldcat->seltext_l    = lv_dd04v-scrtext_l.
      lr_fieldcat->seltext_m    = lv_dd04v-scrtext_m.
      lr_fieldcat->seltext_s    = lv_dd04v-scrtext_s.
      lr_fieldcat->reptext_ddic = lv_dd04v-reptext.
    ENDIF.
    " ORT01 Texte
    READ TABLE pt_fieldcat REFERENCE INTO lr_fieldcat
                           WITH KEY fieldname = 'ORT01'.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name                = 'ORT01'   " City
          state               = 'A'
          langu               = sy-langu
       IMPORTING
         dd04v_wa            = lv_dd04v.
      lr_fieldcat->seltext_l    = lv_dd04v-scrtext_l.
      lr_fieldcat->seltext_m    = lv_dd04v-scrtext_m.
      lr_fieldcat->seltext_s    = lv_dd04v-scrtext_s.
      lr_fieldcat->reptext_ddic = lv_dd04v-reptext.
    ENDIF.
  ENDIF.

  READ TABLE pt_fieldcat INTO ld_fieldcat_wa
                         WITH KEY fieldname = 'STATF'.
  IF sy-subrc IS INITIAL.
    ld_fieldcat_wa-icon = 'X'.
    MODIFY pt_fieldcat FROM ld_fieldcat_wa INDEX sy-tabix.
    gt_fvkdfi-statf = '@0Y@'.
    MODIFY gt_fvkdfi TRANSPORTING statf WHERE statf IS INITIAL.
  ENDIF.

  PERFORM set_tooltip.
  IF NOT i_grid_control IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
*           I_INTERFACE_CHECK           = ' '
             i_callback_program          = ld_callback_program
             i_callback_pf_status_set    = ld_callback_pf_status_set
             i_callback_user_command     = ld_callback_user_command
*           I_CALLBACK_TOP_OF_PAGE      = ' '
*           I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*           I_STRUCTURE_NAME            =
*           I_BACKGROUND_ID             = ' '
*           I_GRID_TITLE                =
             is_layout                   = ps_layout
             it_fieldcat                 = pt_fieldcat
*           IT_EXCLUDING                =
*           IT_SPECIAL_GROUPS           =
*           IT_SORT                     =
*           IT_FILTER                   =
*           IS_SEL_HIDE                 =
             i_default                   = 'X'
             i_save                      = i_save
             is_variant                  = i_variant
*           IT_EVENTS                   =
*           IT_EVENT_EXIT               =
*           IS_PRINT                    =
*           IS_REPREP_ID                =
*           I_SCREEN_START_COLUMN       = 0
*           I_SCREEN_START_LINE         = 0
*           I_SCREEN_END_COLUMN         = 0
*           I_SCREEN_END_LINE           = 0
            it_except_qinfo             = gt_exc
*      IMPORTING
*           E_EXIT_CAUSED_BY_CALLER     =
*           ES_EXIT_CAUSED_BY_USER      =
        TABLES
             t_outtab                    = gt_fvkdfi
        EXCEPTIONS
             program_error               = 1
             OTHERS                      = 2
             .
  ELSE.
*   if i_variant-variant = '1STANDGRID'.
*     i_variant-variant = '1STANDARD'.
*   ENDIF.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = ld_callback_program
        i_callback_pf_status_set = ld_callback_pf_status_set
        i_callback_user_command  = ld_callback_user_command
        is_layout                = ps_layout
        it_fieldcat              = pt_fieldcat
        i_default                = 'X'
        i_save                   = 'A'
        is_variant               = i_variant
      TABLES
        t_outtab                 = gt_fvkdfi
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ENDIF.

ENDFUNCTION.
*---------------------------------------------------------------------*
*       FORM VKDFS_PF_STATUS_SET
*---------------------------------------------------------------------*
*       This form sets the PF-Status and is required by the           *
*       general list viewer to display another status                 *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM z_vkdfs_pf_status_set
       USING
         rt_extab TYPE slis_t_extab.

  DATA : lt_extab TYPE slis_t_extab.
  DATA : BEGIN OF exctab OCCURS 1,
           okcod(4) TYPE c,
         END   OF exctab.

  REFRESH exctab.
  APPEND '&XPA' TO exctab.
  APPEND '&OMP' TO exctab.

  APPEND LINES OF exctab   TO lt_extab.
  APPEND LINES OF rt_extab TO lt_extab.

  SET PF-STATUS 'VKDFS_ALV' EXCLUDING lt_extab.

ENDFORM.                    "VKDFS_PF_STATUS_SET

*---------------------------------------------------------------------*
*       FORM VKDFS_USER_COMMAND
*---------------------------------------------------------------------*
FORM z_vkdfs_user_command
       USING
         r_ucomm     LIKE sy-ucomm
         rs_selfield TYPE slis_selfield.

  DATA : ld_sammg LIKE vbsk-sammg.

  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.

  CASE r_ucomm.
    WHEN 'SLEG'.
      PERFORM fcode_sleg.
    WHEN 'PROT'.
      PERFORM vkdfs_fcode_prot
                USING
                  gd_sammg
                  gd_invoice_list.
    WHEN 'NSEL'.
      PERFORM vkdfs_fcode_nsel
                USING
                  r_ucomm
                  rs_selfield
                  gd_default_data.
    WHEN 'SAMO'.
      PERFORM vkdfs_fcode_samo
                TABLES
                  gt_fvkdfi
                USING
                  rs_selfield
                  gd_default_data
                  space
                  gd_invoice_list     .
      r_ucomm = 'BACK'.
      rs_selfield-exit = 'X'.
    WHEN 'SAMD'.
      PERFORM vkdfs_fcode_samd
                TABLES
                  gt_fvkdfi
                USING
                  rs_selfield
                  gd_default_data
                  space
                  gd_invoice_list
                  gd_opt_enabled.
*     R_UCOMM = 'BACK'.
*     RS_SELFIELD-EXIT = 'X'.
    WHEN 'SAMH'.
      PERFORM vkdfs_fcode_samh
                USING
                  r_ucomm
                  rs_selfield
                  gd_default_data
                  gd_invoice_list.
    WHEN 'NDE '.
      PERFORM vkdfs_fcode_nsel
                USING
                  r_ucomm
                  rs_selfield
                  gd_default_data.
    WHEN 'ANZB'.
      PERFORM vkdfs_fcode_anzb
                USING
                  rs_selfield.
    WHEN 'SAMQ'.
      PERFORM vkdfs_fcode_samq
                USING
                  r_ucomm
                  rs_selfield
                  gd_default_data
                  gd_invoice_list.
    WHEN 'SAMS'.
      PERFORM vkdfs_fcode_sams
                USING
                  r_ucomm
                  rs_selfield
                  gd_default_data
                  gd_invoice_list.
    WHEN '&NTE'.

    WHEN OTHERS.
*ENHANCEMENT-POINT vkdfs_user_command_02 SPOTS es_saplv60p.

  ENDCASE.
ENDFORM.                    "VKDFS_USER_COMMAND

*---------------------------------------------------------------------*
*       FORM VKDFS_USER_COMMAND
*---------------------------------------------------------------------*
FORM batch_user_command
       USING
         r_ucomm     LIKE sy-ucomm
         rs_selfield TYPE slis_selfield.

ENDFORM.                    "BATCH_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_NSEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vkdfs_fcode_nsel
       USING
         r_ucomm     LIKE sy-ucomm
         rs_selfield TYPE slis_selfield
         gd_default_data.

  DATA : answer(1).

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = TEXT-pt2
      textline1 = TEXT-054
      textline2 = TEXT-055
    IMPORTING
      answer    = answer(1).
  IF answer(1) = 'J'.
    r_ucomm = 'BACK'.
    rs_selfield-exit = 'X'.
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
FORM vkdfs_fcode_samo
       TABLES
         gt_fvkdfi            STRUCTURE zvkdfif
       USING
         rs_selfield          TYPE slis_selfield
         gd_default_data      STRUCTURE rv60a
         ld_no_direct_posting LIKE rv50s-bform
         gd_invoice_list.

  DATA : ac_code LIKE sy-subrc.
  DATA : lt_vbfs LIKE vbfs OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbss LIKE vbss OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbsk LIKE vbsk OCCURS 1  WITH HEADER LINE.
  DATA : lt_fvkdfi LIKE gt_fvkdfi OCCURS 10 WITH HEADER LINE.
*  DATA : lt_fvkdfi LIKE gt_fvkdfi OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbfs_subrc LIKE sy-subrc.
  DATA : gt_fvkdfi_subrc LIKE sy-subrc.
  DATA : gt_fvkdfi_tabix LIKE sy-tabix.
  DATA : ld_no_new_run(1) VALUE ' '.
  DATA : ld_smart LIKE vbsk-smart VALUE 'F'.

  PERFORM authority_check USING '19' 'X' CHANGING ac_code.
  lt_fvkdfi[] = gt_fvkdfi[].
  LOOP AT lt_fvkdfi WHERE selkz NE gc_charx.
    DELETE lt_fvkdfi INDEX sy-tabix.
  ENDLOOP.

  IF NOT gd_sammg IS INITIAL.
    ld_no_new_run = 'X'.
  ENDIF.
  IF gd_invoice_list IS INITIAL.
    SET PARAMETER ID 'VF' FIELD space.
  ELSE.
    SET PARAMETER ID 'VFL' FIELD space.
    ld_smart = 'R'.
  ENDIF.
  CALL FUNCTION 'SD_COLLECTIVE_RUN_EXECUTE'
    EXPORTING
      v60p_input_rv60a  = gd_default_data
      v60p_input_smart  = ld_smart
      id_invoice_list   = gd_invoice_list
      id_no_new_run     = ld_no_new_run
    IMPORTING
      v60p_output_vbsk  = lt_vbsk
    TABLES
      v60p_input_vkdfif = lt_fvkdfi
      v60p_output_vbfs  = lt_vbfs
      v60p_output_vbss  = lt_vbss
    EXCEPTIONS
      OTHERS            = 1.

  IF ld_no_new_run IS INITIAL.
    gd_sammg = lt_vbsk-sammg.
    APPEND lt_vbsk.
  ENDIF.

  CALL FUNCTION 'VBSK_ALV_DISPLAY'
    TABLES
      i_vbsk = lt_vbsk
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
FORM vkdfs_fcode_samd
       TABLES
         gt_fvkdfi            STRUCTURE zvkdfif
       USING
         rs_selfield          TYPE slis_selfield
         gd_default_data      STRUCTURE rv60a
         ld_no_direct_posting LIKE rv50s-bform
         ld_invoice_list
         ld_opt_enabled TYPE abap_bool.

  DATA : ac_code LIKE sy-subrc.
  DATA : lt_vbfs LIKE vbfs OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbss LIKE vbss OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbsk LIKE vbsk OCCURS 1  WITH HEADER LINE.
  DATA : lt_fvkdfi LIKE gt_fvkdfi OCCURS 10 WITH HEADER LINE.
  DATA : lt_vbfs_subrc LIKE sy-subrc.
  DATA : gt_fvkdfi_subrc LIKE sy-subrc.
  DATA : gt_fvkdfi_tabix LIKE sy-tabix.
  DATA : ld_no_new_run(1).
  DATA : ld_smart LIKE vbsk-smart VALUE 'F'.
  DATA : ld_sammg LIKE vbsk-sammg.

  PERFORM authority_check USING '19' 'X' CHANGING ac_code.
  lt_fvkdfi[] = gt_fvkdfi[].
  LOOP AT lt_fvkdfi WHERE selkz NE gc_charx.
    DELETE lt_fvkdfi INDEX sy-tabix.
  ENDLOOP.

  IF NOT gd_sammg IS INITIAL.
    ld_no_new_run = gc_charx.
    ld_sammg      = gd_sammg.
  ENDIF.
  IF ld_invoice_list IS INITIAL.
    SET PARAMETER ID 'VF' FIELD space.
  ELSE.
    SET PARAMETER ID 'VFL' FIELD space.
    ld_smart = 'R'.
  ENDIF.
  CALL FUNCTION 'SD_COLLECTIVE_RUN_EXECUTE'
    EXPORTING
      v60p_input_rv60a  = gd_default_data
      v60p_input_smart  = ld_smart
      v60p_input_sammg  = ld_sammg
      id_no_new_run     = ld_no_new_run
      id_invoice_list   = ld_invoice_list
      iv_opt_enabled    = ld_opt_enabled
    IMPORTING
      v60p_output_vbsk  = lt_vbsk
    TABLES
      v60p_input_vkdfif = lt_fvkdfi
      v60p_output_vbfs  = lt_vbfs
      v60p_output_vbss  = lt_vbss
    EXCEPTIONS
      OTHERS            = 1.

  IF ld_no_new_run IS INITIAL.
    gd_sammg = lt_vbsk-sammg.
  ENDIF.

  IF sy-batch IS INITIAL.
    LOOP AT lt_fvkdfi.
      READ TABLE lt_vbfs WITH KEY vbeln = lt_fvkdfi-vbeln.
      lt_vbfs_subrc = sy-subrc.

      IF lt_vbfs_subrc = 0 AND lt_vbfs-msgty NA ' XAE '.
        lt_vbfs_subrc = 4.
        LOOP AT lt_vbfs FROM sy-tabix.
          IF lt_vbfs-vbeln NE lt_fvkdfi-vbeln.
            lt_vbfs_subrc = 4.
            EXIT.
          ENDIF.
          IF lt_vbfs-msgty CA 'XAE'.
            lt_vbfs_subrc = 0.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      READ TABLE gt_fvkdfi WITH KEY vbeln = lt_fvkdfi-vbeln
                                    fkdat = lt_fvkdfi-fkdat.
      gt_fvkdfi_subrc = sy-subrc.
      IF gt_fvkdfi_subrc IS INITIAL.
        gt_fvkdfi_tabix = sy-tabix.
        IF lt_vbfs_subrc IS INITIAL.
          gt_fvkdfi-statf = '@0W@'.
        ELSE.
          gt_fvkdfi-selkz = '1'.
          gt_fvkdfi-statf = '@0V@'.
        ENDIF.
        gt_fvkdfi-sammg = lt_vbsk-sammg.
        MODIFY gt_fvkdfi INDEX gt_fvkdfi_tabix.
      ENDIF.
    ENDLOOP.
    rs_selfield-refresh = 'X'.
  ELSE.
    REFRESH lt_vbsk.
    APPEND lt_vbsk.

    CALL FUNCTION 'VBSK_ALV_DISPLAY'
      TABLES
        i_vbsk = lt_vbsk
      EXCEPTIONS
        OTHERS = 1.

    DATA : ld_vf_vbeln LIKE gt_fvkdfi-vbeln.

    IF ld_invoice_list IS INITIAL.
      GET PARAMETER ID 'VF' FIELD ld_vf_vbeln.
    ELSE.
      GET PARAMETER ID 'VF' FIELD ld_vf_vbeln.
    ENDIF.
    IF NOT ld_vf_vbeln IS INITIAL.
      LOOP AT gt_fvkdfi WHERE selkz = 'X'.
        gt_fvkdfi-selkz = '-'.
        MODIFY gt_fvkdfi INDEX sy-tabix.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                               " VKDFS_FCODE_SAMD

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_SAMH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vkdfs_fcode_samh
       USING
         r_ucomm           LIKE sy-ucomm
         rs_selfield       TYPE slis_selfield
         p_gd_default_data LIKE rv60a
         ld_invoice_list
         .
  DATA : ac_code LIKE sy-subrc.
  DATA : rv60a LIKE rv60a.
  DATA : ld_fvkdfi_tabix LIKE sy-tabix.
  DATA : ok-code(5)          TYPE c.
  DATA : BEGIN OF postab OCCURS 50.
           INCLUDE STRUCTURE vkdfi.
  DATA :   v_fkdat  LIKE vkdfi-fkdat,
           v_fkart  LIKE vkdfi-fkart,
           activ(1) TYPE n.
  DATA : END   OF postab.

  PERFORM authority_check USING '19' 'X' CHANGING ac_code.
  ok-code = r_ucomm.
  CLEAR xkomfk.
  REFRESH xkomfk.
  LOOP AT gt_fvkdfi WHERE selkz = gc_charx.
    ld_fvkdfi_tabix = sy-tabix.
    MOVE-CORRESPONDING gt_fvkdfi TO xkomfk.
    xkomfk-seldat = gt_fvkdfi-fkdat.
    APPEND xkomfk.
  ENDLOOP.
  rv60a-fkart  = p_gd_default_data-fkart.
  rv60a-fkdat  = p_gd_default_data-fkdat.
  rv60a-fbuda  = p_gd_default_data-fbuda.
  rv60a-prsdt  = p_gd_default_data-prsdt.
  rv60a-rfbfk  = p_gd_default_data-rfbfk.
  IF ld_invoice_list IS INITIAL.
    EXPORT ok-code
           rv60a-fkart
           rv60a-fkdat
           rv60a-fbuda
           rv60a-prsdt
           rv60a-rfbfk
           xkomfk
           TO MEMORY ID 'SDBILLDL'.
    CALL TRANSACTION 'VF01' AND SKIP FIRST SCREEN.
    IMPORT ok-code
           rv60a-fkart
           rv60a-fkdat
           rv60a-fbuda
           rv60a-prsdt
           rv60a-rfbfk
           xkomfk
           FROM MEMORY ID 'SDBILLDL'.
    FREE MEMORY ID 'SDBILLDL'.
  ELSE.
    EXPORT ok-code
           rv60a-fkart
           rv60a-fkdat
           rv60a-fbuda
           rv60a-prsdt
           rv60a-rfbfk
           xkomfk
           TO MEMORY ID 'SDINVLDL'.
    CALL TRANSACTION 'VF21' AND SKIP FIRST SCREEN.
    IMPORT ok-code
           rv60a-fkart
           rv60a-fkdat
           rv60a-fbuda
           rv60a-prsdt
           rv60a-rfbfk
           xkomfk
           FROM MEMORY ID 'SDINVLDL'.
    FREE MEMORY ID 'SDINVLDL'.
  ENDIF.
  LOOP AT gt_fvkdfi WHERE selkz = gc_charx.
    ld_fvkdfi_tabix = sy-tabix.
    READ TABLE xkomfk WITH KEY vbeln = gt_fvkdfi-vbeln.
    IF sy-subrc IS INITIAL.
      IF xkomfk-fkstk = 'C' OR
         xkomfk-fksak = 'C' OR
         ( xkomfk-fxmsg IS INITIAL AND xkomfk-updkz = 'X' ).
        gt_fvkdfi-selkz = '1'.
        gt_fvkdfi-statf = '@0V@'.
      ELSE.
        gt_fvkdfi-statf = '@0W@'.
      ENDIF.
      MODIFY gt_fvkdfi INDEX ld_fvkdfi_tabix.
    ENDIF.
  ENDLOOP.

  rs_selfield-refresh = 'X'.

ENDFORM.                               " VKDFS_FCODE_SAMH

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_ENDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vkdfs_fcode_ende.

ENDFORM.                               " VKDFS_FCODE_ENDE

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_ANZB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vkdfs_fcode_anzb
       USING
         rs_selfield TYPE slis_selfield.

  IF NOT rs_selfield-tabindex IS INITIAL.
    READ TABLE gt_fvkdfi INDEX rs_selfield-tabindex.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
        EXPORTING
          vbeln = gt_fvkdfi-vbeln.
    ELSE.
      MESSAGE i460(vr).
    ENDIF.
  ELSE.
    MESSAGE i460(vr).
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
FORM vkdfs_fcode_samq
       USING
         r_ucomm           LIKE sy-ucomm
         rs_selfield       TYPE slis_selfield
         p_gd_default_data LIKE rv60a
         ld_invoice_list
         .
  DATA : ac_code         LIKE sy-subrc.
  DATA : rv60a           LIKE rv60a.
  DATA : ld_fvkdfi_tabix LIKE sy-tabix.
  DATA : ok-code(5)      TYPE c.
  DATA : BEGIN OF postab OCCURS 50.
           INCLUDE STRUCTURE vkdfi.
  DATA :   v_fkdat  LIKE vkdfi-fkdat,
           v_fkart  LIKE vkdfi-fkart,
           activ(1) TYPE n.
  DATA : END   OF postab.

  PERFORM authority_check USING '01' 'X' CHANGING ac_code.
  LOOP AT gt_fvkdfi WHERE selkz = gc_charx.
    ld_fvkdfi_tabix = sy-tabix.
    ok-code = r_ucomm.
    CLEAR postab. REFRESH postab.
*ENHANCEMENT-POINT vkdfs_fcode_samq_01 SPOTS es_saplv60p.
    postab-fkdat = gt_fvkdfi-fkdat.
    postab-fkart = gt_fvkdfi-fkart.
    rv60a-fkart  = p_gd_default_data-fkart.
    rv60a-fkdat  = p_gd_default_data-fkdat.
    rv60a-fbuda  = p_gd_default_data-fbuda.
    rv60a-prsdt  = p_gd_default_data-prsdt.
    rv60a-rfbfk  = p_gd_default_data-rfbfk.
    REFRESH postab.
    MOVE-CORRESPONDING gt_fvkdfi TO postab.
    APPEND postab.
    IF ld_invoice_list IS INITIAL.
      SET PARAMETER ID 'VF' FIELD space.
      SET PARAMETER ID 'VFR' FIELD gt_fvkdfi-vbeln.
      EXPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             TO MEMORY ID 'VF04'.
      CALL TRANSACTION 'VF01' AND SKIP FIRST SCREEN.
      IMPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             FROM MEMORY ID 'VF04'.
      FREE MEMORY ID 'VF04'.

      DATA : ld_vf01_parameter LIKE vbrk-vbeln.
      GET PARAMETER ID 'VF' FIELD ld_vf01_parameter.
      IF ld_vf01_parameter IS INITIAL.
        gt_fvkdfi-statf = '@0W@'.
      ELSE.
        gt_fvkdfi-selkz = '1'.
        gt_fvkdfi-statf = '@0V@'.
      ENDIF.
    ELSE.
      SET PARAMETER ID 'VFL' FIELD space.
      SET PARAMETER ID 'VFN' FIELD gt_fvkdfi-vbeln.
      EXPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             TO MEMORY ID 'VF04'.
      CALL TRANSACTION 'VF21' AND SKIP FIRST SCREEN.
      IMPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             FROM MEMORY ID 'VF04'.
      FREE MEMORY ID 'VF04'.

      DATA : ld_vf21_parameter LIKE vbrk-vbeln.
      GET PARAMETER ID 'VFL' FIELD ld_vf21_parameter.
      IF ld_vf21_parameter IS INITIAL.
        gt_fvkdfi-statf = '@0W@'.
      ELSE.
        gt_fvkdfi-selkz = '1'.
        gt_fvkdfi-statf = '@0V@'.
      ENDIF.
    ENDIF.
    MODIFY gt_fvkdfi INDEX ld_fvkdfi_tabix.
  ENDLOOP.

  rs_selfield-refresh = 'X'.

ENDFORM.                               " VKDFS_FCODE_SAMQ

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_SAMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vkdfs_fcode_sams
       USING
         r_ucomm           LIKE sy-ucomm
         rs_selfield       TYPE slis_selfield
         p_gd_default_data LIKE rv60a
         ld_invoice_list             .

  DATA : ac_code    LIKE sy-subrc.
  DATA : rv60a      LIKE rv60a.
  DATA : ok-code(5) TYPE c.
  DATA : BEGIN OF postab OCCURS 50.
           INCLUDE STRUCTURE vkdfi.
  DATA :   v_fkdat  LIKE vkdfi-fkdat,
           v_fkart  LIKE vkdfi-fkart,
           activ(1) TYPE n.
  DATA : END   OF postab.

  PERFORM authority_check USING '19' 'X' CHANGING ac_code.
  ok-code = r_ucomm.
  CLEAR postab.
  REFRESH postab.
*ENHANCEMENT-POINT vkdfs_fcode_sams_01 SPOTS es_saplv60p.
  postab-fkdat = gt_fvkdfi-fkdat.
  postab-fkart = gt_fvkdfi-fkart.
  rv60a-fkart  = p_gd_default_data-fkart.
  rv60a-fkdat  = p_gd_default_data-fkdat.
  rv60a-fbuda  = p_gd_default_data-fbuda.
  rv60a-prsdt  = p_gd_default_data-prsdt.
  rv60a-rfbfk  = p_gd_default_data-rfbfk.
  REFRESH postab.
  LOOP AT gt_fvkdfi WHERE selkz = 'X'.
    MOVE-CORRESPONDING gt_fvkdfi TO postab.
    APPEND postab.
  ENDLOOP.

  IF ld_invoice_list IS INITIAL.
    SET PARAMETER ID 'VFR' FIELD gt_fvkdfi-vbeln.
  ELSE.
    SET PARAMETER ID 'VFN' FIELD gt_fvkdfi-vbeln.
  ENDIF.

  IF sy-subrc IS INITIAL.
    IF ld_invoice_list IS INITIAL.
      EXPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             TO MEMORY ID 'VF04'.
      CALL TRANSACTION 'VF01' AND SKIP FIRST SCREEN.
      IMPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             FROM MEMORY ID 'VF04'.
      FREE MEMORY ID 'VF04'.
    ELSE.
      EXPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             TO MEMORY ID 'VF04'.
      CALL TRANSACTION 'VF21' AND SKIP FIRST SCREEN.
      IMPORT ok-code
             postab-fkdat
             postab-fkart
             rv60a-fkart
             rv60a-fkdat
             rv60a-fbuda
             rv60a-prsdt
             rv60a-rfbfk
             postab
             FROM MEMORY ID 'VF04'.
      FREE MEMORY ID 'VF04'.
    ENDIF.
  ELSE.
    MESSAGE s460(vr).
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
FORM sammelgangsnr_ermitteln
       USING
         p_smart LIKE tvsa-smart
       CHANGING
         p_sammg LIKE vbsk-sammg.

*LOCAL : TVSA.
  CHECK : p_sammg IS INITIAL.
  SELECT SINGLE * FROM tvsa WHERE smart = p_smart.
  IF sy-subrc = 0.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = tvsa-numki
        object      = 'RV_SAMMG'
      IMPORTING
        number      = p_sammg.
  ELSE.
    MESSAGE e417(vr) WITH p_sammg.
  ENDIF.

ENDFORM.                               " SAMMELGANGSNR_ERMITTELN

*&---------------------------------------------------------------------*
*&      Form  VKDFS_FCODE_PROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->P_GD_SAMMG      text
*----------------------------------------------------------------------*
FORM vkdfs_fcode_prot
       USING
         gd_sammg LIKE vbsk-sammg
         gd_invoice_list.

  DATA : ld_smart LIKE vbsk-smart.

  IF NOT gd_sammg IS INITIAL.
    ld_smart = 'F'.
    IF NOT gd_invoice_list IS INITIAL.
      ld_smart = 'R'.
    ENDIF.
    SUBMIT sdsampro
             WITH samnr =  gd_sammg
             WITH smart =  ld_smart
             WITH erdat =  sy-datum
             AND RETURN.
  ELSE.
    MESSAGE s042(vf).
  ENDIF.

ENDFORM.                               " VKDFS_FCODE_PROT

*&---------------------------------------------------------------------*
*&      Form  FCODE_SLEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->P_GD_SAMMG      text
*----------------------------------------------------------------------*
FORM fcode_sleg.

  DATA : lt_legend LIKE gt_legend OCCURS 10 WITH HEADER LINE.

  lt_legend-statf = icon_create.
  lt_legend-text  = TEXT-cl1.
  APPEND lt_legend.
  lt_legend-statf = icon_okay.
  lt_legend-text  = TEXT-cl2.
  APPEND lt_legend.
  lt_legend-statf = icon_cancel.
  lt_legend-text  = TEXT-cl3.
  APPEND lt_legend.
  lt_legend-statf = icon_led_green.
  lt_legend-text  = TEXT-cl4.
  APPEND lt_legend.
  lt_legend-statf = icon_led_red.
  lt_legend-text  = TEXT-cl5.
  APPEND lt_legend.

  CALL FUNCTION 'SD_COLOR_LEGEND_DISPLAY'
    TABLES
      it_legend = lt_legend.

ENDFORM.                               " FCODE_SLEG
