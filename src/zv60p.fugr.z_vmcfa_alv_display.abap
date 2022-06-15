FUNCTION Z_VMCFA_ALV_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VMCFA_CB_PROGRAM) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VMCFA_CB_USER_COMMAND) OPTIONAL
*"     VALUE(I_VMCFA_CB_STATUS) OPTIONAL
*"     VALUE(I_VARIANT) LIKE  DISVARIANT STRUCTURE  DISVARIANT
*"         OPTIONAL
*"     VALUE(I_GRID_CONTROL) DEFAULT 'X'
*"  TABLES
*"      I_VMCFA STRUCTURE  VMCFAO
*"--------------------------------------------------------------------
  DATA : ld_callback_program       LIKE sy-repid
                                   VALUE 'SAPLV60P'.
  DATA : ld_callback_pf_status_set TYPE  slis_formname
                                   VALUE 'VMCFA_PF_STATUS_SET'.
  DATA : ld_callback_user_command  TYPE  slis_formname
                                   VALUE 'VMCFA_USER_COMMAND'.
  DATA : pt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA : ps_layout   TYPE slis_layout_alv.
  DATA : pt_outtab   TYPE sd_alv .
  DATA : pt_events   TYPE slis_t_event.
  DATA : ld_vmcfa    LIKE vmcfao.
  DATA : ld_fieldcat_wa TYPE slis_fieldcat_alv.
  gd_vmcfa_cb_program      = i_vmcfa_cb_program.
  gd_vmcfa_cb_user_command = i_vmcfa_cb_user_command.
  gd_vmcfa_cb_status       = i_vmcfa_cb_status.
  CLEAR gt_vmcfa.
  READ TABLE i_vmcfa INDEX 1.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE e113(0k).
  ELSE.
    LOOP AT i_vmcfa INTO ld_vmcfa.
      MOVE-CORRESPONDING ld_vmcfa TO gt_vmcfa.
      APPEND gt_vmcfa.
    ENDLOOP.
    ps_layout-get_selinfos      = 'X'.
    ps_layout-colwidth_optimize = 'X'.
    ps_layout-detail_popup      = 'X'.
    ps_layout-box_fieldname     = 'SELKZ'.
    ps_layout-no_keyfix         = 'X'.
    ps_layout-key_hotspot       = 'X'.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = 'SAPLV60P'
        i_internal_tabname     = 'GT_VMCFA'
        i_inclname             = 'SAPLV60P'
      CHANGING
        ct_fieldcat            = pt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    READ TABLE pt_fieldcat INTO ld_fieldcat_wa
                   WITH KEY fieldname = 'STATF'.
    IF sy-subrc IS INITIAL.
      ld_fieldcat_wa-icon = 'X'.
      MODIFY pt_fieldcat FROM ld_fieldcat_wa INDEX sy-tabix.
      gt_vmcfa-statf = '@0Y@'.
      MODIFY gt_vmcfa TRANSPORTING statf WHERE statf IS INITIAL.
    ENDIF.
    IF i_grid_control IS INITIAL.
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
          t_outtab                 = gt_vmcfa
        EXCEPTIONS
          program_error            = 1
          OTHERS                   = 2.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
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
          t_outtab                 = gt_vmcfa
        EXCEPTIONS
          program_error            = 1
          OTHERS                   = 2.


    ENDIF.

  ENDIF.
ENDFUNCTION.
*---------------------------------------------------------------------*
*       FORM VMCFA_PF_STATUS_SET
*---------------------------------------------------------------------*
*       This form sets the PF-Status and is required by the           *
*       general list viewer to display another status                 *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM vmcfa_pf_status_set USING rt_extab TYPE slis_t_extab.

  DATA: lt_extab TYPE slis_t_extab.
  DATA:    BEGIN OF exctab OCCURS 1,
             okcod(4)          TYPE c,
           END   OF exctab.

  REFRESH exctab.

  APPEND LINES OF exctab   TO lt_extab.
  APPEND LINES OF rt_extab TO lt_extab.

*ENHANCEMENT-POINT VMCFA_PF_STATUS_SET_01 SPOTS ES_SAPLV60P.
  IF NOT gd_vmcfa_cb_status IS INITIAL.
    PERFORM (gd_vmcfa_cb_status)
             IN PROGRAM (gd_vmcfa_cb_program) USING lt_extab
                                             IF FOUND.
  ELSE.
    SET PF-STATUS 'VMCFA_ALV' EXCLUDING lt_extab.
  ENDIF.


ENDFORM.                    "VMCFA_PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM VBSK_USER_COMMAND
*---------------------------------------------------------------------*
FORM vmcfa_user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  DATA : ld_vbtyp LIKE vbrk-vbtyp.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.

  CASE r_ucomm.
    WHEN 'BEAN'.
      LOOP AT gt_vmcfa WHERE selkz  = gc_charx.
        PERFORM belegtyp_ermitteln
                  USING    gt_vmcfa-vbeln
                  CHANGING ld_vbtyp.
*ENHANCEMENT-SECTION     VMCFA_USER_COMMAND_11 SPOTS ES_SAPLV60P.
        CALL FUNCTION 'RV_CALL_CHANGE_TRANSACTION'
          EXPORTING
            vbeln = gt_vmcfa-vbeln
            vbtyp = vbrk-vbtyp.
*END-ENHANCEMENT-SECTION.
      ENDLOOP.
    WHEN 'FKFR'.
*      perform freigabe_via_vf02.
      DATA : lt_xkomfk LIKE  komfk   OCCURS 0 WITH HEADER LINE.
      DATA : lt_xkomv  LIKE  komv    OCCURS 0 WITH HEADER LINE.
      DATA : lt_xthead LIKE  theadvb OCCURS 0 WITH HEADER LINE.
      DATA : lt_xvbpa  LIKE  vbpavb  OCCURS 0 WITH HEADER LINE.
      DATA : lt_xvbrk  LIKE  vbrkvb  OCCURS 0 WITH HEADER LINE.
      DATA : lt_xvbrp  LIKE  vbrpvb  OCCURS 0 WITH HEADER LINE.
      DATA : lt_xvbss  LIKE  vbss    OCCURS 0 WITH HEADER LINE.
      DATA : it_vbrk   LIKE  vbrk    OCCURS 0 WITH HEADER LINE.
      DATA : lt_xvbrl  LIKE  vbrlvb  OCCURS 0 WITH HEADER LINE.
      LOOP AT gt_vmcfa WHERE selkz = gc_charx.
        MOVE-CORRESPONDING gt_vmcfa TO it_vbrk.
        APPEND it_vbrk.
      ENDLOOP.
      REFRESH gt_vbfs.
*ENHANCEMENT-POINT VMCFA_USER_COMMAND_12 SPOTS ES_SAPLV60P.

      CALL FUNCTION 'SD_INVOICE_RELEASE_TO_ACCOUNT'
        EXPORTING
          with_posting = 'B'
        TABLES
          it_vbrk      = it_vbrk
          xkomfk       = lt_xkomfk
          xkomv        = lt_xkomv
          xthead       = lt_xthead
          xvbfs        = gt_vbfs
          xvbpa        = lt_xvbpa
          xvbrk        = lt_xvbrk
          xvbrp        = lt_xvbrp
          xvbrl        = lt_xvbrl
          xvbss        = lt_xvbss.

*ENHANCEMENT-POINT VMCFA_USER_COMMAND_13 SPOTS ES_SAPLV60P.
*  set status for the list dynpro
      LOOP AT gt_vmcfa WHERE selkz = gc_charx.
        READ TABLE it_vbrk WITH KEY vbeln = gt_vmcfa-vbeln.
        IF sy-subrc IS INITIAL.
          IF it_vbrk-rfbsk = 'C'.
            gt_vmcfa-selkz = '1'.
            gt_vmcfa-statf = '@0V@'.
          ELSE.
            gt_vmcfa-statf = '@0W@'.
          ENDIF.
          MODIFY gt_vmcfa.
        ENDIF.
      ENDLOOP.
      rs_selfield-refresh = 'X'.

      READ TABLE gt_vbfs INDEX 1.
      IF sy-subrc IS INITIAL.
        MESSAGE s012(vr).
      ENDIF.
    WHEN 'EPRO'.
      CALL FUNCTION 'VBFS_TREE_LIST_DISPLAY'
        TABLES
          i_vbfs = gt_vbfs
        EXCEPTIONS
          OTHERS = 1.
    WHEN OTHERS.

*ENHANCEMENT-SECTION     VMCFA_USER_COMMAND_14 SPOTS ES_SAPLV60P.
      IF NOT gd_vmcfa_cb_user_command IS INITIAL AND
         NOT gd_vmcfa_cb_program      IS INITIAL.
        PERFORM (gd_vmcfa_cb_user_command)
                 IN PROGRAM (gd_vmcfa_cb_program)
                      TABLES gt_vmcfa
                      USING r_ucomm rs_selfield
                      IF FOUND.
      ENDIF.
*END-ENHANCEMENT-SECTION.
  ENDCASE.
ENDFORM.                    "VMCFA_USER_COMMAND
*---------------------------------------------------------------------*
*       FORM BELEGTYP_ERMITTELN                                       *
*---------------------------------------------------------------------*
*       Belegtyp ermitteln.                                           *
*---------------------------------------------------------------------*
FORM belegtyp_ermitteln
       USING    ld_vbeln
       CHANGING ld_vbtyp.
  SELECT SINGLE * FROM vbrk WHERE vbeln = ld_vbeln.
  IF sy-subrc IS INITIAL.
    ld_vbtyp = vbrk-vbtyp.
  ENDIF.
ENDFORM.                    "BELEGTYP_ERMITTELN
*---------------------------------------------------------------------*
*       FORM FREIGABE_VIA_VF02                                        *
*---------------------------------------------------------------------*
*       Freigabe an Buchhaltung                                       *
*---------------------------------------------------------------------*
FORM freigabe_via_vf02.
  DATA: BEGIN OF bdc_tab OCCURS 10.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF bdc_tab.
  DATA: modus_x(1)  VALUE 'N'.
  DATA: update_x(1) VALUE 'A'.
  LOOP AT gt_vmcfa WHERE selkz = gc_charx.
    REFRESH bdc_tab.
*  start transaction
    CLEAR bdc_tab.
    bdc_tab-program  = 'SAPMV60A'.
    IF gt_vmcfa-fktyp = 'R'.           "Rechnungsliste
      bdc_tab-dynpro   = '0201'.
    ELSE.
      bdc_tab-dynpro   = '0101'.       "Sonstige Fakturen
    ENDIF.
    bdc_tab-dynbegin = 'X'.
    APPEND bdc_tab.
*  request dynpro
    CLEAR bdc_tab.
    bdc_tab-fnam = 'VBRK-VBELN'.
    bdc_tab-fval = gt_vmcfa-vbeln.
    APPEND bdc_tab.
*  release
    CLEAR bdc_tab.
    bdc_tab-program  = 'SAPMV60A'.
    IF gt_vmcfa-fktyp = 'R'.           "Rechnungsliste
      bdc_tab-dynpro   = '0204'.
    ELSE.
      bdc_tab-dynpro   = '0104'.       "Sonstige Fakturen
    ENDIF.
    bdc_tab-dynbegin = 'X'.
    APPEND bdc_tab.
*  save
    CLEAR bdc_tab.
    bdc_tab-fnam = 'BDC_OKCODE'.
    bdc_tab-fval = '=SICH'.
    APPEND bdc_tab.
    IF gt_vmcfa-fktyp = 'R'.           "Rechnungsliste
      CALL TRANSACTION 'VF22'
           USING bdc_tab
           MODE   modus_x
           UPDATE update_x.
    ELSE.
      CALL TRANSACTION 'VF02'          "Sonstige Fakturen.
           USING bdc_tab
           MODE   modus_x
           UPDATE update_x.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "FREIGABE_VIA_VF02
