FUNCTION Z_VBSK_ALV_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VBSK_CB_PROGRAM) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VBSK_CB_USER_COMMAND) OPTIONAL
*"     VALUE(I_VBSK_CB_STATUS) OPTIONAL
*"     VALUE(I_GRID_CONTROL) DEFAULT SPACE
*"  TABLES
*"      I_VBSK STRUCTURE  VBSK
*"--------------------------------------------------------------------
  DATA : pt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA : ps_layout   TYPE slis_layout_alv.
  DATA : pt_outtab   TYPE sd_alv .
  DATA : pt_events   TYPE slis_t_event.
  DATA : smart_count TYPE i.
  DATA : BEGIN OF lt_smart OCCURS 4,
           ld_smart TYPE c.
  DATA : ld_tabname  TYPE slis_tabname.
  DATA : END OF lt_smart.
  DATA : ld_vbsk LIKE vbsk.
  gd_vbsk_cb_program      = i_vbsk_cb_program.
  gd_vbsk_cb_user_command = i_vbsk_cb_user_command.
  gd_vbsk_cb_status       = i_vbsk_cb_status.
  CLEAR gt_fvbsk.
  CLEAR gt_rvbsk.
  CLEAR gt_lvbsk.
  CLEAR gt_cvbsk.                                                  "AIP
  CLEAR gt_wvbsk.
  CLEAR gt_svbsk.
  CLEAR gt_gvbsk.
  CLEAR gt_tvbsk.
  REFRESH gt_fvbsk.
  REFRESH gt_rvbsk.
  REFRESH gt_lvbsk.
  REFRESH gt_cvbsk.                                                "AIP
  REFRESH gt_wvbsk.
  REFRESH gt_svbsk.
  REFRESH gt_gvbsk.
  REFRESH gt_tvbsk.
  READ TABLE i_vbsk INDEX 1.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE e113(0k).
  ELSE.
    LOOP AT i_vbsk INTO ld_vbsk.
      CASE ld_vbsk-smart.
        WHEN 'F'.
          MOVE-CORRESPONDING ld_vbsk TO gt_fvbsk.
          APPEND gt_fvbsk.
        WHEN 'R'.
          MOVE-CORRESPONDING ld_vbsk TO gt_rvbsk.
          APPEND gt_rvbsk.
        WHEN 'L'.
          MOVE-CORRESPONDING ld_vbsk TO gt_lvbsk.
          APPEND gt_lvbsk.
        WHEN 'C'.                                                  "AIP
          MOVE-CORRESPONDING ld_vbsk TO gt_cvbsk.                  "AIP
          APPEND gt_cvbsk.                                         "AIP
        WHEN 'W'.
          MOVE-CORRESPONDING ld_vbsk TO gt_wvbsk.
          APPEND gt_wvbsk.
        WHEN 'S'.
          MOVE-CORRESPONDING ld_vbsk TO gt_svbsk.
          APPEND gt_svbsk.
        WHEN 'G'.
          MOVE-CORRESPONDING ld_vbsk TO gt_gvbsk.
          APPEND gt_gvbsk.
        WHEN 'T'.
          MOVE-CORRESPONDING ld_vbsk TO gt_tvbsk.
          APPEND gt_tvbsk.
      ENDCASE.
    ENDLOOP.
    READ TABLE gt_fvbsk INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'F'.
      lt_smart-ld_tabname = 'GT_FVBSK'.
      APPEND lt_smart.
      CALL CUSTOMER-FUNCTION '001'
           TABLES
                c_vbsk     = gt_fvbsk
           EXCEPTIONS
                OTHERS     = 1.
    ENDIF.
    READ TABLE gt_rvbsk INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'R'.
      lt_smart-ld_tabname = 'GT_RVBSK'.
      APPEND lt_smart.
      CALL CUSTOMER-FUNCTION '002'
           TABLES
                c_vbsk     = gt_rvbsk
           EXCEPTIONS
                OTHERS     = 1.
    ENDIF.
    READ TABLE gt_lvbsk INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'L'.
      lt_smart-ld_tabname = 'GT_LVBSK'.
      APPEND lt_smart.
      CALL CUSTOMER-FUNCTION '003'
           TABLES
                c_vbsk     = gt_lvbsk
           EXCEPTIONS
                OTHERS     = 1.
    ENDIF.
    READ TABLE gt_cvbsk INDEX 1.                            "AIP begin
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'C'.
      lt_smart-ld_tabname = 'GT_CVBSK'.
      APPEND lt_smart.
    ENDIF.                                                  "AIP end
    READ TABLE gt_wvbsk INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'W'.
      lt_smart-ld_tabname = 'GT_WVBSK'.
      APPEND lt_smart.
      CALL CUSTOMER-FUNCTION '004'
           TABLES
                c_vbsk     = gt_wvbsk
           EXCEPTIONS
                OTHERS     = 1.
    ENDIF.
    READ TABLE gt_svbsk INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'S'.
      lt_smart-ld_tabname = 'GT_SVBSK'.
      APPEND lt_smart.
      CALL CUSTOMER-FUNCTION '005'
           TABLES
                c_vbsk     = gt_svbsk
           EXCEPTIONS
                OTHERS     = 1.
    ENDIF.
    READ TABLE gt_gvbsk INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'G'.
      lt_smart-ld_tabname = 'GT_GVBSK'.
      APPEND lt_smart.
      CALL CUSTOMER-FUNCTION '011'
           TABLES
                c_vbsk     = gt_gvbsk
           EXCEPTIONS
                OTHERS     = 1.
    ENDIF.
    READ TABLE gt_tvbsk INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO smart_count.
      lt_smart-ld_smart = 'T'.
      lt_smart-ld_tabname = 'GT_TVBSK'.
      APPEND lt_smart.
*      CALL CUSTOMER-FUNCTION '012'
*           TABLES
*                c_vbsk     = gt_tvbsk
*           EXCEPTIONS
*                OTHERS     = 1.
    ENDIF.
    gd_vbsk_tabname = lt_smart-ld_tabname.
    gd_vbsk_smart   = lt_smart-ld_smart.
    ps_layout-colwidth_optimize = 'X'.
    ps_layout-detail_popup      = 'X'.
    ps_layout-no_keyfix         = 'X'.
    ps_layout-key_hotspot       = 'X'.
    LOOP AT lt_smart.
      REFRESH pt_fieldcat.
      CASE lt_smart-ld_smart.
        WHEN 'F'.
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_FVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_fvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
                 EXPORTING
                      i_callback_program       = 'SAPLV60P'
                      i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                      i_callback_user_command  = 'VBSK_USER_COMMAND'
                      is_layout                = ps_layout
                      it_fieldcat              = pt_fieldcat
                      i_default                = 'X'
                      i_save                   = 'A'
*                 is_variant
                 TABLES
                      t_outtab                 = gt_fvbsk
                 EXCEPTIONS
                      program_error            = 1
                      OTHERS                   = 2.


          ENDIF.
        WHEN 'R'.
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_RVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_rvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_rvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.

          ENDIF.
        WHEN 'L'.
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_LVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_lvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_lvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.

          ENDIF.
        WHEN 'C'.                                             "AIP begin
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_CVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_cvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_cvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
         ENDIF.                                                 "AIP end
        WHEN 'W'.
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_WVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_wvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_wvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.

          ENDIF.
        WHEN 'S'.
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_SVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_svbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_svbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.

          ENDIF.
        WHEN 'G'.
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_GVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_gvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_gvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.

          ENDIF.
        WHEN 'T'.
          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_program_name         = 'SAPLV60P'
              i_internal_tabname     = 'GT_TVBSK'
              i_inclname             = 'SAPLV60P'
            CHANGING
              ct_fieldcat            = pt_fieldcat
            EXCEPTIONS
              inconsistent_interface = 1
              program_error          = 2
              OTHERS                 = 3.
          IF i_grid_control IS INITIAL.
            CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_tvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.
          ELSE.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                i_callback_program       = 'SAPLV60P'
                i_callback_pf_status_set = 'VBSK_PF_STATUS_SET'
                i_callback_user_command  = 'VBSK_USER_COMMAND'
                is_layout                = ps_layout
                it_fieldcat              = pt_fieldcat
                i_default                = 'X'
                i_save                   = 'A'
              TABLES
                t_outtab                 = gt_tvbsk
              EXCEPTIONS
                program_error            = 1
                OTHERS                   = 2.

          ENDIF.
      ENDCASE.

    ENDLOOP.

  ENDIF.
ENDFUNCTION.
*---------------------------------------------------------------------*
*       FORM VBSK_PF_STATUS_SET
*---------------------------------------------------------------------*
*       This form sets the PF-Status and is required by the           *
*       general list viewer to display another status                 *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM vbsk_pf_status_set USING rt_extab TYPE slis_t_extab.

  DATA: lt_extab TYPE slis_t_extab.
  DATA:    BEGIN OF exctab OCCURS 1,
             okcod(4)          TYPE c,
           END   OF exctab.

  REFRESH exctab.

  APPEND LINES OF exctab   TO lt_extab.
  APPEND LINES OF rt_extab TO lt_extab.

  IF NOT gd_vbsk_cb_status IS INITIAL.
    PERFORM (gd_vbsk_cb_status)
             IN PROGRAM (gd_vbsk_cb_program) USING lt_extab
                                             IF FOUND.
  ELSE.
    SET PF-STATUS 'VBSK_ALV' EXCLUDING lt_extab.
  ENDIF.


ENDFORM.                    "VBSK_PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM VBSK_USER_COMMAND
*---------------------------------------------------------------------*
FORM vbsk_user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  DATA: lf_feldname(20).
  DATA: da_rc LIKE sy-subrc.
  DATA: lt_vbfs  LIKE vbfs  OCCURS 10 WITH HEADER LINE.
  DATA: lt_vbss  LIKE vbss  OCCURS 10 WITH HEADER LINE.
  DATA: lt_fvbss LIKE vbssf OCCURS 10 WITH HEADER LINE.
  CASE r_ucomm.
    WHEN 'EPRO'.
      PERFORM vbfs_data_collect
              TABLES
                lt_vbfs
              USING
                gd_vbsk_tabname
                rs_selfield-tabindex
              CHANGING
                da_rc.
      CASE da_rc.
        WHEN 4.
          MESSAGE s022(vr).
        WHEN 8.
          MESSAGE s415(vr).
        WHEN 0.
          CALL FUNCTION 'VBFS_TREE_LIST_DISPLAY'
            TABLES
              i_vbfs = lt_vbfs
            EXCEPTIONS
              OTHERS = 1.
      ENDCASE.
    WHEN 'BELE'.
      PERFORM vbss_data_collect
              TABLES
                lt_vbss
                lt_fvbss
              USING
                  gd_vbsk_tabname
                  rs_selfield-tabindex
              CHANGING
                  da_rc.
      IF NOT da_rc IS INITIAL.
        MESSAGE s022(vr).
      ELSE.
        CASE gd_vbsk_tabname.
          WHEN 'GT_FVBSK'.
            CALL FUNCTION 'VBSS_ALV_DISPLAY'
              EXPORTING
                smart          = 'F'
                i_grid_control = 'X'
              TABLES
                i_vbss         = lt_vbss
                i_fvbss        = lt_fvbss
              EXCEPTIONS
                OTHERS         = 1.
          WHEN OTHERS.
            CALL FUNCTION 'VBSS_ALV_DISPLAY'
              EXPORTING
                smart   = 'X'
              TABLES
                i_vbss  = lt_vbss
                i_fvbss = lt_fvbss
              EXCEPTIONS
                OTHERS  = 1.
        ENDCASE.
      ENDIF.
    WHEN OTHERS.

*ENHANCEMENT-POINT VBSK_USER_COMMAND_02 SPOTS ES_SAPLV60P.

      IF NOT gd_vbsk_cb_user_command IS INITIAL AND
         NOT gd_vbsk_cb_program      IS INITIAL.
        CASE gd_vbsk_smart.
          WHEN 'F'.
            PERFORM (gd_vbsk_cb_user_command)
                     IN PROGRAM (gd_vbsk_cb_program)
                          TABLES gt_fvbsk
                          USING r_ucomm gd_vbsk_smart rs_selfield
                          IF FOUND.
          WHEN 'R'.
            PERFORM (gd_vbsk_cb_user_command)
                     IN PROGRAM (gd_vbsk_cb_program)
                          TABLES gt_rvbsk
                          USING r_ucomm gd_vbsk_smart rs_selfield
                          IF FOUND.
          WHEN 'L'.
            PERFORM (gd_vbsk_cb_user_command)
                     IN PROGRAM (gd_vbsk_cb_program)
                          TABLES gt_lvbsk
                          USING r_ucomm gd_vbsk_smart rs_selfield
                          IF FOUND.
          WHEN 'C'.                                          "AIP begin
            PERFORM (gd_vbsk_cb_user_command)
                     IN PROGRAM (gd_vbsk_cb_program)
                          TABLES gt_cvbsk
                          USING r_ucomm gd_vbsk_smart rs_selfield
                          IF FOUND.                          " AIP end
          WHEN 'W'.
            PERFORM (gd_vbsk_cb_user_command)
                     IN PROGRAM (gd_vbsk_cb_program)
                          TABLES gt_wvbsk
                          USING r_ucomm gd_vbsk_smart rs_selfield
                          IF FOUND.
          WHEN 'S'.
            PERFORM (gd_vbsk_cb_user_command)
                     IN PROGRAM (gd_vbsk_cb_program)
                          TABLES gt_svbsk
                          USING r_ucomm gd_vbsk_smart rs_selfield
                          IF FOUND.
          WHEN 'G'.
            PERFORM (gd_vbsk_cb_user_command)
                     IN PROGRAM (gd_vbsk_cb_program)
                          TABLES gt_gvbsk
                          USING r_ucomm gd_vbsk_smart rs_selfield
                          IF FOUND.
        ENDCASE.
      ENDIF.
  ENDCASE.
"$$
ENDFORM.                    "VBSK_USER_COMMAND
