***INCLUDE RVREUSE0 .

form reuse_alv_fieldcatalog_merge
     using pt_fieldcat type slis_t_fieldcat_alv
     value(pi_structure) like dd02l-tabname
     value(pi_tablename) like dd02l-tabname
     value(pi_sp_group).

  data: ls_fieldcat type slis_fieldcat_alv.
  FIELD-SYMBOLS: <ls_fieldcat> type slis_fieldcat_alv.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
*         I_PROGRAM_NAME         =
*         I_INTERNAL_TABNAME     =
            i_structure_name       = pi_structure
            i_client_never_display = 'X'
       changing
            ct_fieldcat            = pt_fieldcat
       exceptions
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  if not pi_tablename is initial or
     not pi_sp_group  is initial.


    ls_fieldcat-tabname  = pi_tablename.
    ls_fieldcat-sp_group = pi_sp_group.

    modify pt_fieldcat from ls_fieldcat transporting tabname sp_group
                       where tabname is initial or
                             sp_group is initial.

  endif.

  IF pi_structure = 'FAMTV'.
* set fields ACTIVE and DUMMY as technical fields, so they do not appear on UI
    READ TABLE pt_fieldcat ASSIGNING <ls_fieldcat>
    WITH KEY FIELDNAME = 'ACTIV'.
    IF sy-subrc is INITIAL.
      <ls_fieldcat>-tech = 'X'.
    ENDIF.
    READ TABLE pt_fieldcat ASSIGNING <ls_fieldcat>
    WITH KEY FIELDNAME = 'DUMMY'.
    IF sy-subrc is INITIAL.
      <ls_fieldcat>-tech = 'X'.
    ENDIF.

  ENDIF.

endform.                    "REUSE_ALV_FIELDCATALOG_MERGE

*---------------------------------------------------------------------*
*       FORM REUSE_ALV_VARIANT_DEFAULT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
form reuse_alv_variant_default
     using ps_sd_alv         type sd_alv.

  data: ls_fieldcat type slis_fieldcat_alv.
  data: ls_variant  like disvariant.
  data: ls_import_flag.
  statics: ls_counter type i.

  import ls_import_flag from memory id ps_sd_alv-variant(48).
  if not ls_import_flag is initial.
    import ps_sd_alv-variant from memory id ps_sd_alv-variant(48).
  endif.

  if ps_sd_alv-variant-variant is initial.
    ps_sd_alv-default = gc_charx.
  else.
    clear ps_sd_alv-default.
  endif.
  ls_variant = ps_sd_alv-variant.

  call function 'REUSE_ALV_VARIANT_SELECT'
    exporting
      i_dialog            = 'N'
      i_user_specific     = gc_chara
      i_default           = ps_sd_alv-default
      i_tabname_header    = ps_sd_alv-tabname_header
      i_tabname_item      = ps_sd_alv-tabname_item
      it_default_fieldcat = ps_sd_alv-fieldcat
      i_layout            = ps_sd_alv-layout
    importing
      e_exit              = ps_sd_alv-exit
      et_fieldcat         = ps_sd_alv-fieldcat
      et_sort             = ps_sd_alv-sort
      et_filter           = ps_sd_alv-filter
    changing
      cs_variant          = ps_sd_alv-variant
    exceptions
      wrong_input         = 1
      fc_not_complete     = 2
      not_found           = 3
      program_error       = 4
      others              = 5.

  if ps_sd_alv-variant is initial.
    ps_sd_alv-variant = ls_variant.
  endif.
  ps_sd_alv-default = gc_charx.
  clear ls_counter.
endform.                    "REUSE_ALV_VARIANT_DEFAULT

*---------------------------------------------------------------------*
*       FORM REUSE_ALV_EVENTS_GET                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PT_EVENTS                                                     *
*  -->  PI_LIST_TYPE                                                  *
*---------------------------------------------------------------------*
form reuse_alv_events_get tables pt_events   type slis_t_event
                          using pi_list_type.

  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type     = pi_list_type
    importing
      et_events       = pt_events[]
    exceptions
      list_type_wrong = 1
      others          = 2.

endform.                    "REUSE_ALV_EVENTS_GET
*&---------------------------------------------------------------------*
*&      Form  REUSE_PARTNERANSCHRIFT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_SELFIELD-VALUE  text                                    *
*      -->P_LIST1-VBKA-PARVW  text                                     *
*      -->P_LIST1-VBPA-ARDNR  text                                     *
*----------------------------------------------------------------------*
form reuse_partneranschrift using    pi_kunde
                                     pi_parvw
                                     pi_ardnr.
  data: ls_vbpa  like vbpa,
        ls_vbadr like vbadr,
        ls_adrs  like adrs,
        da_temp(60) type c.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = pi_kunde
    importing
      output = ls_vbpa-kunnr.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = pi_kunde
    importing
      output = ls_vbpa-lifnr.

  move pi_kunde to da_temp.
  condense da_temp.
  if da_temp+8(2) co ' '.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = pi_kunde
      importing
        output = ls_vbpa-pernr.
  else.
    ls_vbpa-pernr = '00000000'.
  endif.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = pi_kunde
    importing
      output = ls_vbpa-parnr.
  move pi_parvw to ls_vbpa-parvw.
  move pi_ardnr to ls_vbpa-adrnr.

  call function 'VIEW_VBADR'
    exporting
      input   = ls_vbpa
    importing
      adresse = ls_vbadr.

  move-corresponding ls_vbadr to ls_adrs.
  call function 'RV_ADDRESS_WINDOW_DISPLAY'
    exporting
      adrswa_in = ls_adrs
      fadrtype  = space.

endform.                               " REUSE_PARTNERANSCHRIFT

*&---------------------------------------------------------------------*
*&      Form  REUSE_KONTAKT_BERECHTIGUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_RCODE  text                                             *
*----------------------------------------------------------------------*
type-pools: v43.
*---------------------------------------------------------------------*
*       FORM REUSE_KONTAKT_BERECHTIGUNG                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PI_VBELN                                                      *
*  -->  PO_RCODE                                                      *
*---------------------------------------------------------------------*
form reuse_kontakt_berechtigung using pi_vbeln
                                      po_rcode.
  data: lt_vbka like vbka occurs 1 with header line,
        lt_vbpa like vbpavb occurs 1 with header line.

  data: lt_vbeln type  v43_t_vbeln.
  ranges: lr_vbeln for vbka-vbeln.

  clear po_rcode.
  append pi_vbeln to lt_vbeln.
  call function 'SDCAS_SALES_ACTIVITY_READ_MANY'
    exporting
      fi_vbuk_read         = space
      fi_vbka_read         = gc_charx
      fi_vbpa_read         = space
      fi_sadr_read         = space
      fi_stxh_tlines_read  = space
      fi_vbfa_read         = space
    tables
      fi_vbeln             = lt_vbeln
      fe_vbka              = lt_vbka
    exceptions
      not_all_docs_in_vbuk = 1
      not_all_docs_in_vbka = 2
      others               = 3.

  call function 'SD_AUTHORITY_SALES_ACTIVITY'
    exporting
      activity     = '1'
    tables
      fxvbka       = lt_vbka
      fxvbpa       = lt_vbpa
      no_authority = lr_vbeln.
  describe table lr_vbeln lines sy-tfill.
  if sy-tfill > 0.
    po_rcode = 4.
  endif.

endform.                               " REUSE_KONTAKT_BERECHTIGUNG
*&---------------------------------------------------------------------*
*&      Form  REUSE_CHANGE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_SELFIELD-VALUE  text                                    *
*      -->P_CHAR1  text                                                *
*      -->P_SPACE  text                                                *
*----------------------------------------------------------------------*
form reuse_change_document using  pi_vbeln like vbak-vbeln
                                  pi_vbtyp like vbak-vbtyp
                                  pi_fcode type c.

  call function 'RV_CALL_CHANGE_TRANSACTION'
    exporting
      vbeln = pi_vbeln
      vbtyp = pi_vbtyp
      fcode = pi_fcode.

endform.                               " REUSE_CHANGE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  REUSE_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_SELFIELD-VALUE  text                                    *
*      -->P_CHAR1  text                                                *
*      -->P_SPACE  text                                                *
*----------------------------------------------------------------------*
form reuse_display_document using    pi_vbeln like vbak-vbeln
                                     pi_vbtyp like vbak-vbtyp
                                     pi_fcode type c.

  call function 'RV_CALL_DISPLAY_TRANSACTION'
    exporting
      vbeln = pi_vbeln
      vbtyp = pi_vbtyp
      fcode = pi_fcode.
endform.                               " REUSE_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  REUSE_STATUS_ANZEIGEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LIST1-VBELN  text                                       *
*      -->P_NULL  text                                                 *
*----------------------------------------------------------------------*
form reuse_status_anzeigen using pi_vbeln like vbak-vbeln
                                 pi_posnr type c
                                 pe_fcode like sy-ucomm.

  data: ls_vbuk like vbuk,
        ls_vbup like vbup.

  if pi_posnr is initial.
    ls_vbuk-vbeln = pi_vbeln.
    call function 'RV_DOCUMENT_HEAD_STATUS_TEXTS'
      exporting
        vbuk_in       = ls_vbuk
        window_senden = gc_charx
      importing
        fcode         = pe_fcode.
  else.
    ls_vbup-vbeln = pi_vbeln.
    ls_vbup-posnr = pi_posnr.
    call function 'RV_DOCUMENT_POS_STATUS_TEXTS'
      exporting
        vbup_in       = ls_vbup
        window_senden = gc_charx
      importing
        fcode         = pe_fcode.
  endif.
endform.                               " REUSE_STATUS_ANZEIGEN
*&---------------------------------------------------------------------*
*&      Form  REUSE_FLUSS_ANZEIGEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_VBKA-VBELN  text                                        *
*      -->P_NULL  text                                                 *
*----------------------------------------------------------------------*
form reuse_fluss_anzeigen using    pi_vbeln like vbak-vbeln
                                   pi_posnr like vbap-posnr.
  data: ls_vbco6 like vbco6.

  ls_vbco6-mandt = sy-mandt.
  ls_vbco6-vbeln = pi_vbeln.
  ls_vbco6-posnr = pi_posnr.

  call dialog 'RV_DOCUMENT_FLOW'
    exporting
      vbco6 from ls_vbco6.

endform.                               " REUSE_FLUSS_ANZEIGEN
*&---------------------------------------------------------------------*
*&      Form  REUSE_EVENTS_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_EVENTS_EXIT  text                                       *
*----------------------------------------------------------------------*
form reuse_events_exit tables   pt_events_exit type slis_t_event_exit.

  clear pt_events_exit.
  pt_events_exit-ucomm = '&ERW'.
  pt_events_exit-after = gc_charx.
  append pt_events_exit.

  clear pt_events_exit.
  pt_events_exit-ucomm = '&OLX'.
  pt_events_exit-after = gc_charx.
  append pt_events_exit.

  pt_events_exit-ucomm = '&OL0'.
  pt_events_exit-after = gc_charx.
  append pt_events_exit.

  clear pt_events_exit.
  pt_events_exit-ucomm = '&OAD'.
  pt_events_exit-after = gc_charx.
  append pt_events_exit.

  clear pt_events_exit.
  pt_events_exit-ucomm = '&OUP'.
  pt_events_exit-after  = gc_charx.
  append pt_events_exit.

  clear pt_events_exit.
  pt_events_exit-ucomm = '&ODN'.
  pt_events_exit-after  = gc_charx.
  append pt_events_exit.

  clear pt_events_exit.
  pt_events_exit-ucomm = '&ILT'.
  pt_events_exit-after = gc_charx.
  append pt_events_exit.

  clear pt_events_exit.
  pt_events_exit-ucomm = '&ILD'.
  pt_events_exit-after = gc_charx.
  append pt_events_exit.
endform.                               " REUSE_EVENTS_EXIT
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_LIST_LAYOUT_INFO_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text                                          *
*      -->P_GT_LAYOUT  text                                            *
*      -->P_GT_SORT  text                                              *
*      -->P_GT_FILTER  text                                            *
*----------------------------------------------------------------------*
form reuse_alv_list_layout_info_get using    pt_fieldcat
                                             ps_layout
                                             pt_sort
                                             pt_filter.

  call function 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
       importing
          es_layout      = ps_layout
          et_fieldcat    = pt_fieldcat
          et_sort        = pt_sort
          et_filter      = pt_filter
*         ES_LIST_SCROLL =
       exceptions
            no_infos       = 1
            program_error  = 2
            others         = 3.

endform.                               " REUSE_ALV_LIST_LAYOUT_INFO_GET
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_LIST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LIST1  text                                             *
*      -->P_GS_SD_ALV  text                                            *
*      -->P_PI_DEFAULT  text                                           *
*      -->P_PI_TABNAME_HEADER  text                                    *
*----------------------------------------------------------------------*
form reuse_alv_list_display
*    USING    pt_outtab         TYPE ANY TABLE
     using    pt_outtab         type standard table
              ps_sd_alv         type sd_alv.
  data: ls_fieldcat like line of ps_sd_alv-fieldcat.
  data: lv_number_outfields type int4.
  if ps_sd_alv-variant-variant is initial.
    loop at ps_sd_alv-fieldcat  transporting
            no fields where no_out = 'O'.
      lv_number_outfields = lv_number_outfields + 1.
    endloop.
    loop at ps_sd_alv-fieldcat into ls_fieldcat
            where no_out is initial and tech is initial.
      lv_number_outfields = lv_number_outfields + 1.
      if lv_number_outfields > 99.
        ls_fieldcat-no_out = 'X'.
        modify ps_sd_alv-fieldcat from ls_fieldcat.
      endif.
    endloop.
  endif.

  if not ps_sd_alv-grid_display is initial.
    call function 'REUSE_ALV_GRID_DISPLAY'
          exporting
*           i_interface_check        =
              i_callback_program       = ps_sd_alv-program
              i_callback_pf_status_set = ps_sd_alv-pf_status_set
              i_callback_user_command  = ps_sd_alv-user_command
              i_structure_name         = ps_sd_alv-structure
              is_layout                = ps_sd_alv-layout
              it_fieldcat              = ps_sd_alv-fieldcat
              it_excluding             = ps_sd_alv-excluding
              it_special_groups        = ps_sd_alv-special_groups
              it_sort                  = ps_sd_alv-sort
              it_filter                = ps_sd_alv-filter
              is_sel_hide              = ps_sd_alv-sel_hide
              i_default                = 'X'
              i_save                   = ps_sd_alv-save
              is_variant               = ps_sd_alv-variant
              it_events                = ps_sd_alv-events
              it_event_exit            = ps_sd_alv-event_exit
              is_print                 = ps_sd_alv-print
*         IS_REPREP_ID             =
              i_screen_start_column    = ps_sd_alv-start_column
              i_screen_start_line      = ps_sd_alv-start_line
              i_screen_end_column      = ps_sd_alv-end_column
              i_screen_end_line        = ps_sd_alv-end_line
         importing
              e_exit_caused_by_caller  = ps_sd_alv-exit
              es_exit_caused_by_user   = ps_sd_alv-user_exit
         tables
              t_outtab                 = pt_outtab
         exceptions
              program_error            = 1
              others                   = 2.


    export ps_sd_alv-variant to memory id ps_sd_alv-variant(48).
  else.
    call function 'REUSE_ALV_LIST_DISPLAY'
          exporting
*           i_interface_check        =
              i_callback_program       = ps_sd_alv-program
              i_callback_pf_status_set = ps_sd_alv-pf_status_set
              i_callback_user_command  = ps_sd_alv-user_command
              i_structure_name         = ps_sd_alv-structure
              is_layout                = ps_sd_alv-layout
              it_fieldcat              = ps_sd_alv-fieldcat
              it_excluding             = ps_sd_alv-excluding
              it_special_groups        = ps_sd_alv-special_groups
              it_sort                  = ps_sd_alv-sort
              it_filter                = ps_sd_alv-filter
              is_sel_hide              = ps_sd_alv-sel_hide
              i_default                = 'X'
              i_save                   = ps_sd_alv-save
              is_variant               = ps_sd_alv-variant
              it_events                = ps_sd_alv-events
              it_event_exit            = ps_sd_alv-event_exit
              is_print                 = ps_sd_alv-print
*         IS_REPREP_ID             =
              i_screen_start_column    = ps_sd_alv-start_column
              i_screen_start_line      = ps_sd_alv-start_line
              i_screen_end_column      = ps_sd_alv-end_column
              i_screen_end_line        = ps_sd_alv-end_line
         importing
              e_exit_caused_by_caller  = ps_sd_alv-exit
              es_exit_caused_by_user   = ps_sd_alv-user_exit
         tables
              t_outtab                 = pt_outtab
         exceptions
              program_error            = 1
              others                   = 2.


    export ps_sd_alv-variant to memory id ps_sd_alv-variant(48).
  endif.

endform.                               " REUSE_ALV_LIST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_HIERSEQ_LIST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LIST2  text                                             *
*      -->P_GT_LIST1  text                                             *
*      -->P_GS_SD_ALV  text                                            *
*----------------------------------------------------------------------*
form reuse_alv_hierseq_list_display tables   pt_outtab1
                                             pt_outtab2
                                    using    ps_sd_alv type sd_alv.
  data: ls_fieldcat like line of ps_sd_alv-fieldcat.
  data: lv_number_outfields type int4.
  if ps_sd_alv-variant-variant is initial.
    loop at ps_sd_alv-fieldcat  transporting
            no fields where no_out = 'O'.
      lv_number_outfields = lv_number_outfields + 1.
    endloop.
    loop at ps_sd_alv-fieldcat into ls_fieldcat
            where no_out is initial and tech is initial and
             tabname = ps_sd_alv-tabname_header.
      if lv_number_outfields > 98.
        ls_fieldcat-no_out = 'X'.
        modify ps_sd_alv-fieldcat from ls_fieldcat.
      else.
        lv_number_outfields = lv_number_outfields + 1.
      endif.
    endloop.
  endif.
*  clear lv_number_outfields.
  if ps_sd_alv-variant-variant is initial.
    loop at ps_sd_alv-fieldcat into ls_fieldcat
            where no_out is initial and tech is initial and
            tabname = ps_sd_alv-tabname_item.
      lv_number_outfields = lv_number_outfields + 1.
      if lv_number_outfields > 99.
        ls_fieldcat-no_out = 'X'.
        modify ps_sd_alv-fieldcat from ls_fieldcat.
      endif.
    endloop.
  endif.

  call function 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    exporting
      i_callback_program       = ps_sd_alv-program
      i_callback_pf_status_set = ps_sd_alv-pf_status_set
      i_callback_user_command  = ps_sd_alv-user_command
      is_layout                = ps_sd_alv-layout
      it_fieldcat              = ps_sd_alv-fieldcat
      it_excluding             = ps_sd_alv-excluding
      it_special_groups        = ps_sd_alv-special_groups
      it_sort                  = ps_sd_alv-sort
      it_filter                = ps_sd_alv-filter
      is_sel_hide              = ps_sd_alv-sel_hide
      i_screen_start_column    = ps_sd_alv-start_column
      i_screen_start_line      = ps_sd_alv-start_line
      i_screen_end_column      = ps_sd_alv-end_column
      i_screen_end_line        = ps_sd_alv-end_line
      i_default                = ps_sd_alv-default
      i_save                   = ps_sd_alv-save
      is_variant               = ps_sd_alv-variant
      it_events                = ps_sd_alv-events
      it_event_exit            = ps_sd_alv-event_exit
      i_tabname_header         = ps_sd_alv-tabname_header
      i_tabname_item           = ps_sd_alv-tabname_item
      i_structure_name_header  = ps_sd_alv-structure
      i_structure_name_item    = ps_sd_alv-structure_item
      is_keyinfo               = ps_sd_alv-keyinfo
      is_print                 = ps_sd_alv-print
    importing
      e_exit_caused_by_caller  = ps_sd_alv-exit
    tables
      t_outtab_header          = pt_outtab1
      t_outtab_item            = pt_outtab2
    exceptions
      program_error            = 1
      others                   = 2.

endform.                               " REUSE_ALV_HIERSEQ_LIST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_POPUP_TO_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LIST_TYPES  text                                        *
*      -->P_LS_SD_ALV  text                                            *
*----------------------------------------------------------------------*
form reuse_alv_popup_to_select tables   pt_outtab
                               using    ps_sd_alv type sd_alv.

  call function 'REUSE_ALV_POPUP_TO_SELECT'
       exporting
        i_title                 = ps_sd_alv-title
*         I_SELECTION             = 'X'
*         I_ZEBRA                 = ' '
        i_screen_start_column   = ps_sd_alv-start_column
        i_screen_start_line     = ps_sd_alv-start_line
        i_screen_end_column     = ps_sd_alv-end_column
        i_screen_end_line       = ps_sd_alv-end_line
        i_checkbox_fieldname    = ps_sd_alv-checkbox
        i_linemark_fieldname    = ps_sd_alv-linemark
*         I_SCROLL_TO_SEL_LINE    = 'X'
          i_tabname             = ps_sd_alv-tabname
          i_structure_name      = ps_sd_alv-structure
        it_fieldcat             = ps_sd_alv-fieldcat
        i_callback_program      = ps_sd_alv-program
        i_callback_user_command = ps_sd_alv-user_command
       importing
        es_selfield             = ps_sd_alv-selfield
        e_exit                  = ps_sd_alv-exit
       tables
        t_outtab                = pt_outtab
       exceptions
        program_error           = 1
        others                  = 2.
endform.                               " REUSE_ALV_POPUP_TO_SELECT
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SD_ALV  text                                            *
*----------------------------------------------------------------------*
form reuse_alv_variant_f4 using    ps_sd_alv type sd_alv.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant          = ps_sd_alv-variant
      i_tabname_header    = ps_sd_alv-tabname_header
      i_tabname_item      = ps_sd_alv-tabname_item
      it_default_fieldcat = ps_sd_alv-fieldcat
      i_save              = ps_sd_alv-save
    importing
      e_exit              = ps_sd_alv-exit
      es_variant          = ps_sd_alv-variant
    exceptions
      not_found           = 1
      program_error       = 2
      others              = 3.

  ps_sd_alv-subrc = sy-subrc.

endform.                               " REUSE_ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_VARIANT_EXISTENCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PC_DVARI  text                                             *
*      -->P_CHARA  text                                                *
*      -->P_LV_SUBRC  text                                             *
*----------------------------------------------------------------------*
form reuse_alv_variant_existence using    ps_variant like disvariant
                                          pi_save  type c
                                          pe_subrc like sy-subrc.

  call function 'REUSE_ALV_VARIANT_EXISTENCE'
    exporting
      i_save        = pi_save
    changing
      cs_variant    = ps_variant
    exceptions
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      others        = 4.

  pe_subrc = sy-subrc.

endform.                               " REUSE_ALV_VARIANT_EXISTENCE

*&---------------------------------------------------------------------*
*&      Form  RVREUSE_FORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form reuse_alv_variant_fill using
                               pi_report    like disvariant-report
                               pi_handle    type c
                               pi_loggroup  type c
                               pi_username  like disvariant-username
                               ps_variant   like disvariant.

  ps_variant-report   = pi_report  .
  ps_variant-handle   = pi_handle  .
  ps_variant-log_group = pi_loggroup.
* ps_variant-username = pi_username.

endform.                               " RVREUSE_FORMS
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_FIELDCATALOG_SELKZ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SD_ALV-FIELDCAT[]  text                                 *
*----------------------------------------------------------------------*
form reuse_alv_fieldcatalog_selkz tables pt_fieldcat
                                  type slis_t_fieldcat_alv.
  pt_fieldcat-tabname   = 'LIST1'.
  pt_fieldcat-sp_group  = gc_char5.
  pt_fieldcat-fieldname =  'SELKZ'.
  pt_fieldcat-ref_fieldname = 'SELKZ'.
  pt_fieldcat-ref_tabname   = 'VBMTV'.
  append pt_fieldcat.
endform.                               " REUSE_ALV_FIELDCATALOG_SELKZ
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SD_ALV-EVENTS  text                                     *
*----------------------------------------------------------------------*
form reuse_alv_events tables pt_events type slis_t_event
                      using  pe_program like sy-repid.

  pt_events-form = pt_events-name = slis_ev_user_command.
  append pt_events.
  pt_events-form = pt_events-name = slis_ev_pf_status_set.
  append pt_events.
  pt_events-form = pt_events-name = slis_ev_top_of_page.
  append pt_events.
  pt_events-form = pt_events-name = slis_ev_foreign_top_of_page.
  append pt_events.
  pe_program = sy-repid.

endform.                               " REUSE_ALV_EVENTS_USER_COMMAND

*---------------------------------------------------------------------*
*       FORM REUSE_ALV_EVENTS_LINE_OUTPUT                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PT_EVENTS                                                     *
*  -->  PI_BEFORE                                                     *
*  -->  PI_AFTER                                                      *
*---------------------------------------------------------------------*
form reuse_alv_events_line_output tables pt_events type slis_t_event
                                  using  pi_before
                                         pi_after.
  if not pi_before is initial.
    pt_events-form = pt_events-name = slis_ev_before_line_output.
    append pt_events.
  endif.

  if not pi_after is initial.
    pt_events-form = pt_events-name = slis_ev_after_line_output.
    append pt_events.
  endif.

  pt_events-form = pt_events-name = slis_ev_end_of_list.
  append pt_events.
endform.                               " REUSE_ALV_EVENTS_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_FIELDCATALOG_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SD_ALV-FIELDCAT  text                                   *
*      -->P_3585   text                                                *
*      -->P_3587   text                                                *
*----------------------------------------------------------------------*
form reuse_alv_fieldcatalog_field tables pt_fieldcat
                                  type slis_t_fieldcat_alv
                                  using pi_field type c
                                        pi_table type c.

  pt_fieldcat-fieldname =  pi_field.
  pt_fieldcat-ref_fieldname = pi_field.
  pt_fieldcat-ref_tabname   = pi_table.
  append pt_fieldcat.

endform.                               " REUSE_ALV_FIELDCATALOG_FIELD
*&---------------------------------------------------------------------*
*&      Form  REUSE_DDIF_FIELDINFO_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DFIES  text                                             *
*      -->P_LV_FIELDNAME  text                                         *
*      -->P_LV_TABNAME  text                                           *
*----------------------------------------------------------------------*
form reuse_ddif_fieldinfo_get tables   pt_dfies structure dfies
                              using    pi_fieldname like dfies-fieldname
                                       pi_tabname   like dcobjdef-name.

  call function 'DDIF_FIELDINFO_GET'
       exporting
            tabname        = pi_tabname
            fieldname      = pi_fieldname
*         LANGU          = SY-LANGU
*    IMPORTING
*         X030L_WA       =
     tables
            dfies_tab      =  pt_dfies
       exceptions
            not_found      = 1
            internal_error = 2
            others         = 3.
endform.                               " REUSE_DDIF_FIELDINFO_GET
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_FIELDCATALOG_NO_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SD_ALV-FIELDCAT  text                                   *
*----------------------------------------------------------------------*
form reuse_alv_fieldcatalog_no_key tables pt_fieldcat
                                          type slis_t_fieldcat_alv.

  pt_fieldcat-key_sel = gc_charx.
  modify pt_fieldcat from pt_fieldcat transporting key_sel
                     where key = gc_charx.
endform.                               " REUSE_ALV_FIELDCATALOG_NO_KEY
*&---------------------------------------------------------------------*
*&      Form  REUSE_VIEW_GET_FIELDINFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DD27P  text                                             *
*      -->P_FIELDCAT-REF_TABNAME  text                                 *
*----------------------------------------------------------------------*
form reuse_view_get_fieldinfo tables   pt_dd27p structure dd27p
                              using    pi_tabname type c.

  call function 'DDIF_VIEW_GET'
       exporting
            name          = pi_tabname
*         STATE         = 'A'
*         LANGU         = ' '
*    IMPORTING
*         GOTSTATE      =
*         DD25V_WA      =
*         DD09L_WA      =
       tables
*         DD26V_TAB     =
            dd27p_tab     =  pt_dd27p
*         DD28J_TAB     =
*         DD28V_TAB     =
       exceptions
            illegal_input = 1
            others        = 2.

endform.                               " REUSE_VIEW_GET_FIELDINFO
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_TEXTE_MERGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_FIELDCAT[]  text                                        *
*      -->P_0037   text                                                *
*      -->P_0038   text                                                *
*      -->P_CHAR8  text                                                *
*----------------------------------------------------------------------*
form reuse_alv_texte_merge
     tables pt_fieldcat type slis_t_fieldcat_alv
     using
     value(pi_structure) like dd02l-tabname
     value(pi_tablename) like dd02l-tabname
     value(pi_sp_group).

  data: lt_dfies_text like dfies occurs 1 with header line,
        lt_dfies      like dfies occurs 1 with header line,
        lv_tabname    like dcobjdef-name,
        lv_fieldname  like dfies-fieldname.

  lv_tabname =  pi_structure.

  perform reuse_ddif_fieldinfo_get tables   lt_dfies_text
                                   using    space
                                            lv_tabname .

  replace '_RTEXTE' with space into lv_tabname.

  perform reuse_ddif_fieldinfo_get tables   lt_dfies
                                   using    space
                                            lv_tabname .

  sort lt_dfies_text by position.

  loop at lt_dfies_text.
    pt_fieldcat-fieldname     = lt_dfies_text-fieldname.
    pt_fieldcat-tabname       = pi_tablename.
    pt_fieldcat-ref_fieldname = lt_dfies_text-fieldname.
    pt_fieldcat-ref_tabname   = pi_structure.
    pt_fieldcat-sp_group      = pi_sp_group.

    lv_fieldname = lt_dfies_text-fieldname.

    if lv_fieldname cs '_'.
      write space to lv_fieldname+sy-fdpos.
    endif.
    read table lt_dfies with key fieldname = lv_fieldname.
    if sy-subrc = 0.
      pt_fieldcat-seltext_l  =  lt_dfies-scrtext_l.
      pt_fieldcat-seltext_m  =  lt_dfies-scrtext_m.
      pt_fieldcat-seltext_s  =  lt_dfies-scrtext_s.
      pt_fieldcat-reptext_ddic = lt_dfies-scrtext_m.
      pt_fieldcat-rollname   =  lt_dfies-rollname.
    endif.
    if pi_structure = 'VBKA_RTEXTE'.
      pt_fieldcat-lowercase = 'X'.
    endif.
    append pt_fieldcat.
  endloop.

endform.                               " REUSE_ALV_TEXTE_MERGE
*&---------------------------------------------------------------------*
*&      Form  REUSE_KKB_LIST_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*        PS_SD_ALV TYPE SD_ALV.             *
*----------------------------------------------------------------------*
form reuse_kkb_list_layout using    ps_sd_alv type sd_alv.

  call function 'K_KKB_LIST_LAYOUT_INFO_GET'
       importing
*         ES_LAYOUT                =
*         ET_FIELDCAT              =
*         ET_SORT                  =
*         ET_FILTER                =
            et_filtered_entries      = ps_sd_alv-filtered_entries
            et_filtered_entries_item = ps_sd_alv-filtered_entries_item
            es_list_scroll           = ps_sd_alv-list_scroll
*         E_TABNAME                =
*         E_TABNAME_SLAVE          =
       exceptions
            no_infos                 = 1
            others                   = 2.

endform.                               " REUSE_KKB_LIST_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_LAYOUT_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SD_ALV-LAYOUT  text                                     *
*      -->P_LV_DUMMY  text                                             *
*      -->P_LV_DUMMY  text                                             *
*----------------------------------------------------------------------*
form reuse_alv_layout_fill using    ps_layout type slis_layout_alv
                                    pi_box_fieldname
                                    pi_box_tabname.

  ps_layout-get_selinfos      = gc_charx.
  ps_layout-colwidth_optimize = gc_charx.
  ps_layout-detail_popup      = gc_charx.
  ps_layout-box_tabname       = pi_box_tabname.
  ps_layout-box_fieldname     = pi_box_fieldname.
  ps_layout-no_keyfix         = gc_charx.
  ps_layout-key_hotspot       = gc_charx.
  ps_layout-group_change_edit = gc_charx.
  ps_layout-totals_before_items = gc_charx.

endform.                               " REUSE_ALV_LAYOUT_FILL
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_LIST_LAYOUT_INFO_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_SD_ALV_FIELDCAT  text                                   *
*      -->P_GS_SD_ALV_LAYOUT  text                                     *
*      -->P_GS_SD_ALV_SORT  text                                       *
*      -->P_GS_SD_ALV_FILTER  text                                     *
*----------------------------------------------------------------------*
form reuse_alv_list_layout_info_set using    pt_fieldcat
                                             ps_layout
                                             pt_sort
                                             pt_filter.

  call function 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
       exporting
            is_layout      = ps_layout
            it_fieldcat    = pt_fieldcat
            it_sort        = pt_sort
            it_filter      = pt_filter
*         IS_LIST_SCROLL =
       exceptions
            others         = 0.

endform.                               " REUSE_ALV_LIST_LAYOUT_INFO_SET

*---------------------------------------------------------------------*
*       FORM REUSE_ALV_LIST_REFRESH                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PS_SELFIELD                                                   *
*---------------------------------------------------------------------*
form reuse_alv_list_refresh using ps_selfield type slis_selfield.

  ps_selfield-refresh = gc_charx.
  ps_selfield-col_stable = gc_charx.
  ps_selfield-row_stable = gc_charx.

endform.                    "REUSE_ALV_LIST_REFRESH
*&---------------------------------------------------------------------*
*&      Form  REUSE_SUBMIT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SUBINFO  text                                           *
*----------------------------------------------------------------------*
form reuse_submit_info using  ps_subinfo structure rssubinfo.

  call function 'RS_SUBMIT_INFO'
    importing
      p_submit_info = ps_subinfo
    exceptions
      others        = 1.

endform.                               " REUSE_SUBMIT_INFO

*---------------------------------------------------------------------*
*       FORM REUSE_EXPORT_VARIANT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PS_SD_ALV-VARIANT                                             *
*---------------------------------------------------------------------*
form reuse_export_variant_to_memory
     using ps_sd_alv-variant like disvariant.

  data: ls_import_flag value 'X'.
  data: ls_variant like disvariant.
  ls_variant = ps_sd_alv-variant.
  export ls_import_flag
         ps_sd_alv-variant to memory id ls_variant(48).
endform.                    "REUSE_EXPORT_VARIANT_TO_MEMORY

*---------------------------------------------------------------------*
*       FORM REUSE_REFRESH_SD_ALV                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
form reuse_refresh_sd_alv changing gs_sd_alv type sd_alv.
  clear gs_sd_alv.
endform.                               "REUSE_REFRESH_SD_ALV
*&---------------------------------------------------------------------*
*&      Form  REUSE_LENGTH_WIDTH_POPUP
*&---------------------------------------------------------------------*
* to use the list-viewer in a popup-window it is necessary to
* set the height and width. To do this somehow dynamically
* we supply this function.
*----------------------------------------------------------------------*
form reuse_length_width_popup
                        tables
                         pt_fieldcat type slis_t_fieldcat_alv
                         pt_outtab
                        changing
                                 p_popup_start_column
                                 p_popup_start_line
                                 p_popup_end_line
                                 p_popup_end_column.
  data: lin type i, laenge type i.
  data: lv_breite type i, lv_anz_display type i.
  if p_popup_start_column is initial.
    p_popup_start_column = 1.
  endif.
  if p_popup_start_line is initial.
    p_popup_start_line = 1.
  endif.

  lv_breite = 11.
  lv_anz_display = 0.
  loop at pt_fieldcat where no_out is initial and tech is initial.
    lv_anz_display = lv_anz_display + 1.
    lv_breite = lv_breite + pt_fieldcat-outputlen.
  endloop.
  lv_breite = lv_breite + lv_anz_display.
  if lv_breite < 50.
    lv_breite = 50.
  endif.
  if lv_breite > 84.
    lv_breite = 84.
  endif.

  p_popup_end_column = p_popup_start_column + lv_breite.
  laenge = 3.
  describe table pt_outtab lines lin.
  laenge = laenge + ( ( lin * 8 ) div 10 ).
  if laenge > 22.
    laenge = 22.
  endif.
  p_popup_end_line = p_popup_start_line + laenge.
endform.                               " LAENGE_BREITE_BESTIMMEN

*&---------------------------------------------------------------------*
*&      Form  REUSE_BERECHTIGUNG_SETZEN
*&---------------------------------------------------------------------*
* Only priviliged users should change the global variants
*----------------------------------------------------------------------*
form reuse_berechtigung_setzen
                    changing p_save like gs_sd_alv-save.
  get parameter id 'SD_VARIANT_MAINTAIN' field p_save.
  if p_save ne 'X' and p_save ne 'A' and p_save ne 'U'.
    clear p_save.
  endif.
endform.                               "REUSE_BERECHTIGUNG_SETZEN
*&---------------------------------------------------------------------*
*&      Form  REUSE_WARNING_OBSOLETE
*&---------------------------------------------------------------------*
* We want to show a warning for a report which will be disposed
*----------------------------------------------------------------------*
form reuse_warning_obsolete
                using value(p_last_release) type c
                      value(p_reportname) type programm.

  data: gvf_last_release(5), gvf_first_release_without(5),
        gvf_reportname type programm.
  data: begin of lvt_parameters occurs 2.
          include structure  spar.
  data: end of lvt_parameters.

  lvt_parameters-param = 'GVF_LAST'.
  lvt_parameters-value = p_last_release.
  append lvt_parameters.
  lvt_parameters-param = 'GVF_REP'.
  lvt_parameters-value = p_reportname.
  append lvt_parameters.




  call function 'POPUP_DISPLAY_TEXT_WITH_PARAMS'
       exporting
           language       = sy-langu
            popup_title    = space                          "text-001
*         START_COLUMN   = 10
*         START_ROW      = 3
            text_object    = 'SDCAS_WARNING_OBSOLETE'
*         HELP_MODAL     = 'X'
*    IMPORTING
*         CANCELLED      =
       tables
            parameters     = lvt_parameters
       exceptions
            error_in_text  = 1
            text_not_found = 2
            others         = 3.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                               "REUSE_BERECHTIGUNG_SETZEN
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_GRID_LAYOUT_INFO_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  text
*      -->PS_LAYOUT    text
*      -->PT_SORT  text
*      -->PT_FILTER  text
*----------------------------------------------------------------------*
form reuse_alv_grid_layout_info_get using  pt_fieldcat
                                           ps_layout
                                           pt_sort
                                           pt_filter.

  call function 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
    importing
      es_layout     = ps_layout
      et_fieldcat   = pt_fieldcat
      et_sort       = pt_sort
      et_filter     = pt_filter
    exceptions
      no_infos      = 1
      program_error = 2
      others        = 3.

endform.                               " REUSE_ALV_GRID_LAYOUT_INFO_GET
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_GRID_LAYOUT_INFO_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  text
*      -->PS_LAYOUT  text
*      -->PT_SORT  text
*      -->PT_FILTER  text
*----------------------------------------------------------------------*
form reuse_alv_grid_layout_info_set using    pt_fieldcat
                                             ps_layout
                                             pt_sort
                                             pt_filter.

  call function 'REUSE_ALV_GRID_LAYOUT_INFO_SET'
       exporting
            is_layout      = ps_layout
            it_fieldcat    = pt_fieldcat
            it_sort        = pt_sort
            it_filter      = pt_filter
*         IS_LIST_SCROLL =
       exceptions
            others         = 0.


endform.                               " REUSE_ALV_GRID_LAYOUT_INFO_SET
