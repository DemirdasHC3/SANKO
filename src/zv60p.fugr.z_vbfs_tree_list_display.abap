FUNCTION Z_VBFS_TREE_LIST_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VBFS_S_CB_PROGRAM) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VBFS_CB_PROGRAM) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_VBFS_S_CB_USER_COMMAND) OPTIONAL
*"     VALUE(I_VBFS_CB_USER_COMMAND) OPTIONAL
*"     VALUE(I_VBFS_S_CB_GUI_STATUS) OPTIONAL
*"     VALUE(I_VBFS_CB_GUI_STATUS) OPTIONAL
*"     VALUE(I_VBFS_CALLED_BY) DEFAULT ' '
*"     VALUE(I_VBFS_CB_TOP_OF_PAGE) OPTIONAL
*"     VALUE(I_VBFS_S_CB_TOP_OF_PAGE) OPTIONAL
*"     VALUE(I_HOTSPOT_MODE) DEFAULT '3'
*"     VALUE(I_TREE_TITLE) LIKE  SY-LISEL DEFAULT SPACE
*"     VALUE(I_VBTYP) DEFAULT ' '
*"  TABLES
*"      I_VBFS STRUCTURE  VBFS
*"--------------------------------------------------------------------
  DATA : xvbfs_tabix LIKE sy-tabix.

  gt_vbfs[] = i_vbfs[].
  gt_vbfs = i_vbfs.
  READ TABLE gt_vbfs INDEX 1.
  IF sy-subrc IS INITIAL.
    gd_vbfs_cb_program        = i_vbfs_cb_program.
    gd_vbfs_cb_user_command   = i_vbfs_cb_user_command.
    gd_vbfs_cb_gui_status     = i_vbfs_cb_gui_status.
    gd_vbfs_s_cb_program      = i_vbfs_s_cb_program.
    gd_vbfs_s_cb_user_command = i_vbfs_s_cb_user_command.
    gd_vbfs_s_cb_gui_status   = i_vbfs_s_cb_gui_status.
    gd_vbfs_called_by         = i_vbfs_called_by.
    gd_vbfs_cb_top_of_page    = i_vbfs_cb_top_of_page.
    gd_vbfs_s_cb_top_of_page  = i_vbfs_s_cb_top_of_page.
    gd_hotspot_mode           = i_hotspot_mode.
    gd_vbtyp                  = i_vbtyp.

    CLEAR treelist.
    REFRESH treelist.
    CLEAR gt_longtext.
    REFRESH gt_longtext.
    CLEAR gt_shorttext.
    REFRESH gt_shorttext.

    PERFORM vbfs_header_node_prepare
              TABLES
                treelist
              USING
                i_tree_title.
    LOOP AT i_vbfs.
      xvbfs_tabix = sy-tabix.
      PERFORM vbfs_message_compose
         TABLES
           gt_longtext
           gt_shorttext
         USING
           xvbfs_tabix
           i_vbfs.
      PERFORM vbfs_st_node_prepare
                USING
                  i_vbfs
                  xvbfs_tabix.
      LOOP AT gt_longtext WHERE msgid = i_vbfs-msgid
                          AND   msgno = i_vbfs-msgno
                          AND   index = xvbfs_tabix.
        EXIT.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        PERFORM vbfs_lt_node_prepare
                  USING
                    i_vbfs
                    xvbfs_tabix.
        PERFORM vbfs_lt_sub_node_prepare
                  USING
                    i_vbfs
                    xvbfs_tabix.
      ENDIF.
      PERFORM vbfs_tv_node_prepare
                USING
                  i_vbfs
                  xvbfs_tabix.
      PERFORM vbfs_tv_sub_node_prepare
                USING
                  i_vbfs
                  xvbfs_tabix.
    ENDLOOP.
    PERFORM vbfs_tree_list_display.
  ENDIF.

ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  VBFS_HEADER_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbfs_header_node_prepare
       TABLES
         treelist STRUCTURE snodetext
       USING
         i_tree_title LIKE sy-lisel.
  DATA : ld_tabix2char(10).
  DATA : ld_name(20).
  CLEAR treelist.
  treelist-name = 'HEADER_NODE'.
  treelist-color = 0.
  treelist-intensiv = '0'.
  treelist-text = text-100.
  IF NOT i_tree_title IS INITIAL.
    treelist-text = i_tree_title.
  ENDIF.
  IF NOT gt_vbfs-sammg IS INITIAL.
    CONCATENATE treelist-text text-099 gt_vbfs-sammg INTO treelist-text
                SEPARATED BY space.
  ENDIF.
  treelist-tlength = strlen( treelist-text ).
  treelist-tlevel = 1.
  treelist-tcolor = 0.
  treelist-tintensiv = '0'.
  treelist-text1 = ''.
  treelist-tlength1 = 30.
  treelist-tcolor1 = 0.
  treelist-tintensiv1 = '0'.
  APPEND treelist.
ENDFORM.                               " VBFS_HEADER_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_LT_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbfs_lt_node_prepare
       USING
         xvbfs LIKE vbfs
         xvbfs_tabix LIKE sy-tabix.

  DATA : ld_tabix2char(10).
  DATA : ld_name(30).
  DATA : rep_msgid LIKE vbfs-msgid.

  CLEAR treelist.
  ld_tabix2char = xvbfs_tabix.
  SHIFT ld_tabix2char LEFT DELETING LEADING ' '.
  rep_msgid = xvbfs-msgid.
  TRANSLATE rep_msgid USING ' .'.
  CONCATENATE rep_msgid xvbfs-msgno ld_tabix2char INTO treelist-name.
  treelist-color = 0.
  treelist-intensiv = '0'.
  treelist-text = text-t04.
  treelist-tlength = 20.
  treelist-tlevel = 3.
  treelist-tcolor = 0.
  treelist-tintensiv = '0'.
  treelist-text1 = ''.
  treelist-tlength1 = 30.
  treelist-tcolor1 = 0.
  treelist-tintensiv1 = '0'.
  APPEND treelist.
ENDFORM.                               " VBFS_LT_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_LT_SUB_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbfs_lt_sub_node_prepare
       USING
         xvbfs LIKE vbfs
         xvbfs_tabix LIKE sy-tabix.
  DATA : ld_tabix2char(10).
  DATA : ld_name(30).
  DATA : rep_msgid LIKE vbfs-msgid.
  CLEAR treelist.
  ld_tabix2char = xvbfs_tabix.
  SHIFT ld_tabix2char LEFT DELETING LEADING ' '.
  rep_msgid = xvbfs-msgid.
  TRANSLATE rep_msgid USING ' .'.
  CONCATENATE rep_msgid xvbfs-msgno ld_tabix2char INTO treelist-name.
  treelist-name+29(1) = 'L'.
  treelist-color = 0.
  treelist-intensiv = '0'.
  treelist-text = text-t03.
  treelist-tlength = 6.
  treelist-tlevel = 4.
  treelist-tcolor = 0.
  treelist-tintensiv = '0'.
  treelist-text1 = ''.
  treelist-tlength1 = 30.
  treelist-tcolor1 = 0.
  treelist-tintensiv1 = '0'.
  treelist-moreinfo = 'X'.
  APPEND treelist.
ENDFORM.                               " VBFS_LT_SUB_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_ST_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbfs_st_node_prepare
       USING
         xvbfs LIKE vbfs
         xvbfs_tabix LIKE sy-tabix.
  DATA : ld_tabix2char(10).
  DATA : ld_name(30).
  DATA : rep_msgid LIKE vbfs-msgid.
  DATA : ld_msgid LIKE xvbfs-msgid.
  DATA : ld_length TYPE i.
  DATA : ld_shorttext_line LIKE gt_shorttext-line.
  DATA : ld_max_length TYPE i VALUE 75.
  DATA : ld_pos LIKE sy-fdpos.
  CLEAR treelist.
  ld_tabix2char = xvbfs_tabix.
  SHIFT ld_tabix2char LEFT DELETING LEADING ' '.
  rep_msgid = xvbfs-msgid.
  TRANSLATE rep_msgid USING ' .'.
  CONCATENATE rep_msgid xvbfs-msgno ld_tabix2char INTO treelist-name.
  READ TABLE gt_shorttext INDEX xvbfs_tabix.
  IF sy-subrc IS INITIAL.
    ld_length = strlen( gt_shorttext-line ).
    IF ld_length <= 75.
      treelist-text2 = gt_shorttext-line.
    ELSE.
      ld_pos = sy-fdpos.
      IF sy-langu CA '1JM3'.
        CALL FUNCTION 'STRING_SPLIT_AT_POSITION'
          EXPORTING
            string            = gt_shorttext-line
            pos               = 75
          IMPORTING
            string1           = treelist-text2
            string2           = treelist-text3
          EXCEPTIONS
            string1_too_small = 1
            string2_too_small = 2
            pos_not_valid     = 3
            OTHERS            = 4.
      ELSE.
        TREELIST-TEXT2 = GT_SHORTTEXT-LINE(75).             "n_692203
        TREELIST-TEXT3 = GT_SHORTTEXT-LINE+75(57).          "n_692203
      ENDIF.
      treelist-kind3 = 'N'.
    ENDIF.
  ENDIF.
  treelist-color = 0.
  treelist-intensiv = '0'.

**** begin tooltip for icon in tree
  DATA: wa_icont TYPE icont.

  CLEAR wa_icont.
**** end tooltip for icon in tree

  IF xvbfs-msgty CA'XAE'.
*    treelist-text = 'ICON_RED_LIGHT'.
**** begin tooltip for icon in tree
*    ICON_RED_LIGHT
    SELECT * FROM icont INTO wa_icont
                     WHERE langu = sy-langu
                       and id    = '@0A@'.
      CONCATENATE '@0A@' wa_icont-quickinfo INTO treelist-text.
    ENDSELECT.
**** end tooltip for icon in tree
  ENDIF.
  IF xvbfs-msgty CA'WI'.
*    treelist-text = 'ICON_YELLOW_LIGHT'.
**** begin tooltip for icon in tree
*    ICON_YELLOW_LIGHT
    SELECT * FROM icont INTO wa_icont
                     WHERE langu = sy-langu
                       and id    = '@09@'.
      CONCATENATE '@09@' wa_icont-quickinfo INTO treelist-text.
    ENDSELECT.
**** end tooltip for icon in tree
  ENDIF.
  IF xvbfs-msgty CA'S'.
*    treelist-text = 'ICON_GREEN_LIGHT'.
**** begin tooltip for icon in tree
*    ICON_GREEN_LIGHT
    SELECT * FROM icont INTO wa_icont
                     WHERE langu = sy-langu
                       and id    = '@08@'.
      CONCATENATE '@08@' wa_icont-quickinfo INTO treelist-text.
    ENDSELECT.
**** end tooltip for icon in tree
  ENDIF.
**** begin tooltip for icon in tree
*      treelist-kind = 'I'.
  treelist-kind = 'L'.    "Icon with tooltip
**** end tooltip for icon in tree
  treelist-tlength = 4.
  treelist-tlevel = 2.
  treelist-tcolor = 1.
  treelist-tintensiv = '1'.
  treelist-tlength2 = strlen( treelist-text2 ).
  treelist-tlength3 = 75.
  treelist-tcolor1 = 0.
  treelist-tcolor1 = 0.
  treelist-tintensiv1 = '0'.
  CONCATENATE xvbfs-vbeln xvbfs-posnr
              INTO treelist-text1 SEPARATED BY space.
  treelist-tlength1 = 17.
  treelist-hotspot1 = 'X'.
  APPEND treelist.
ENDFORM.                               " VBFS_ST_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_TV_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbfs_tv_node_prepare
       USING
         xvbfs LIKE vbfs
         xvbfs_tabix LIKE sy-tabix.

  DATA : ld_tabix2char(10).
  DATA : ld_name(30).
  DATA : rep_msgid LIKE vbfs-msgid.
  CLEAR treelist.
  ld_tabix2char = xvbfs_tabix.
  SHIFT ld_tabix2char LEFT DELETING LEADING ' '.
  rep_msgid = xvbfs-msgid.
  TRANSLATE rep_msgid USING ' .'.
  CONCATENATE rep_msgid xvbfs-msgno ld_tabix2char INTO treelist-name.
  treelist-color = 0.
  treelist-intensiv = '0'.
  treelist-text = text-t01.
  treelist-tlength = 20.
  treelist-tlevel = 3.
  treelist-tcolor = 0.
  treelist-tintensiv = '0'.
  treelist-text1 = ''.
  treelist-tlength1 = 30.
  treelist-tcolor1 = 0.
  treelist-tintensiv1 = '0'.
  APPEND treelist.
ENDFORM.                               " VBFS_TV_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_TV_SUB_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbfs_tv_sub_node_prepare
       USING
         xvbfs LIKE vbfs
         xvbfs_tabix LIKE sy-tabix.

  DATA : ld_tabix2char(10).
  DATA : ld_name(30).
  DATA : rep_msgid LIKE vbfs-msgid.
  CLEAR treelist.
  ld_tabix2char = xvbfs_tabix.
  SHIFT ld_tabix2char LEFT DELETING LEADING ' '.
  rep_msgid = xvbfs-msgid.
  TRANSLATE rep_msgid USING ' .'.
  CONCATENATE rep_msgid xvbfs-msgno ld_tabix2char INTO treelist-name.
  treelist-name+29(1) = 'T'.
*treelist-NAME = TEXT-A10.
  treelist-color = 0.
  treelist-intensiv = '0'.
  treelist-text = text-t02.
  treelist-tlength = 20.
  treelist-tlevel = 4.
  treelist-tcolor = 0.
  treelist-tintensiv = '0'.
  treelist-text1 = ''.
  treelist-tlength1 = 30.
  treelist-tcolor1 = 0.
  treelist-tintensiv1 = '0'.
  treelist-moreinfo = 'X'.
  APPEND treelist.
ENDFORM.                               " VBFS_TV_SUB_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_TREE_LIST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbfs_tree_list_display.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    TABLES
      nodetab      = treelist
    EXCEPTIONS
      tree_failure = 1.

  sy-lsind = 0.
  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
    EXPORTING
      callback_program          = 'SAPLV60P'
      callback_gui_status       = 'VBFS_SET_STATUS'
      callback_user_command     = 'VBFS_USER_COMMAND'
      callback_moreinfo_display = 'VBFS_HANDLE_MOREINFO'
      callback_top_of_page      = 'VBFS_TOP_OF_PAGE'.
*           use_control               = 'F'.
ENDFORM.                               " VBFS_TREE_LIST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  VBFS_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->                                                             *
*      -->                                                             *
*----------------------------------------------------------------------*
FORM vbfs_user_command TABLES   node STRUCTURE seucomm
                       USING    command
                       CHANGING exit
                                list_refresh.
  CASE command.
    WHEN 'KUMU'.
      CALL FUNCTION 'VBFS_SUMMED_TREE_LIST_DISPLAY'
        EXPORTING
          i_vbfs_called_by         = 'E'
          i_vbfs_s_cb_program      = gd_vbfs_s_cb_program
          i_vbfs_s_cb_user_command = gd_vbfs_s_cb_user_command
          i_vbfs_s_cb_gui_status   = gd_vbfs_s_cb_gui_status
          i_vbfs_cb_program        = gd_vbfs_cb_program
          i_vbfs_cb_user_command   = gd_vbfs_cb_user_command
          i_vbfs_cb_gui_status     = gd_vbfs_cb_gui_status
          i_vbfs_cb_top_of_page    = gd_vbfs_cb_top_of_page
          i_vbfs_s_cb_top_of_page  = gd_vbfs_s_cb_top_of_page
        TABLES
          i_vbfs                   = gt_vbfs.
      IF ( sy-ucomm = 'BACK' OR
           sy-ucomm = 'EXIT' OR
           sy-ucomm = 'CANC'    ) AND NOT gd_vbfs_called_by IS INITIAL
                                  AND command = 'KUMU'.
        exit = gc_charx.
      ENDIF.
    WHEN 'TRPI'.
      IF node-selfield = 'TEXT1' AND NOT node-text1 IS INITIAL.
        DATA : ld_vbeln LIKE gt_vbfs-vbeln.
        ld_vbeln = node-text1(10).
        CASE gd_hotspot_mode.
          WHEN 2.
            CALL FUNCTION 'RV_CALL_CHANGE_TRANSACTION'
              EXPORTING
                vbeln = ld_vbeln.
          WHEN 3.
            CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
              EXPORTING
                vbeln = ld_vbeln
                vbtyp = gd_vbtyp.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    WHEN OTHERS.
      IF NOT gd_vbfs_cb_user_command IS INITIAL AND
         NOT gd_vbfs_cb_program      IS INITIAL.
        DATA : ld_index LIKE sy-tabix.
        DATA : ls_vbfs LIKE vbfs.
        MOVE node-name+23(6) TO ld_index.
        READ TABLE gt_vbfs INTO ls_vbfs INDEX ld_index.
        PERFORM (gd_vbfs_cb_user_command)
                 IN PROGRAM (gd_vbfs_cb_program)
                      TABLES node
                      USING command ls_vbfs
                      CHANGING exit list_refresh
                      IF FOUND.
      ENDIF.
  ENDCASE.
ENDFORM.                    "VBFS_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  HANDLE_MOREINFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MOREINFO  text                                             *
*      -->P_STRUCTURE  text                                            *
*      -->P_STREEATTR  text                                            *
*      -->P_P_NODE  text                                               *
*      -->P_LIKE  text                                                 *
*      -->P_STREENODE  text                                            *
*----------------------------------------------------------------------*
FORM vbfs_handle_moreinfo
       TABLES
         moreinfo STRUCTURE streeattr
       USING
         p_node LIKE streenode.

  DATA : ld_msgid LIKE vbfs-msgid.
  DATA : ld_msgno LIKE vbfs-msgno.
  DATA : ld_index LIKE sy-tabix.
  DATA : ld_art(1).
  DATA : ld_text LIKE gt_shorttext-line.
  DATA : vbfs_wa LIKE vbfs.
  DATA : ld_first_line(1).
  DATA : ld_longtext_tabix LIKE sy-tabix.
  DATA : ld_max_length TYPE i VALUE 75.
  DATA : ld_lt_max_length TYPE i.
  DATA : ld_lt_strlen TYPE i.
  DATA F2 TYPE STRING.

  ld_msgid = p_node-name(20).
  TRANSLATE ld_msgid USING '. '.
  ld_msgno = p_node-name+20(3).
  ld_index = p_node-name+23(6).
  ld_art   = p_node-name+29(1).
  CLEAR moreinfo.
  REFRESH moreinfo.

  CASE ld_art.
    WHEN 'L'.
      CLEAR moreinfo.
      REFRESH moreinfo.
      moreinfo-tcolor = 0.
      moreinfo-tintensiv = 0.
      moreinfo-tlength = 75.
      moreinfo-text(1) = sy-vline.
      LOOP AT gt_longtext WHERE msgid = ld_msgid
                          AND   msgno = ld_msgno
                          AND   index = ld_index.
        ld_lt_strlen = strlen( gt_longtext-line ).
        IF ld_lt_strlen > ld_lt_max_length.
          ld_lt_max_length = ld_lt_strlen.
        ENDIF.
        IF gt_longtext-line IS INITIAL.
          DELETE gt_longtext INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      IF sy-langu CA '1JM3'.
        ld_lt_max_length = 75.
        PERFORM vbfs_handle_moreinfo_langu_l
           TABLES moreinfo
           USING ld_msgid
                 ld_msgno
                 ld_index
                 ld_text.
      ELSE.

        IF ld_lt_max_length <= 73.
          ld_lt_max_length = 73 + 1.
        ELSE.
          ld_lt_max_length = ld_lt_max_length + 1.
        ENDIF.
        moreinfo-text+ld_lt_max_length(1) = sy-vline.

        LOOP AT gt_longtext WHERE msgid = ld_msgid
                            AND   msgno = ld_msgno
                            AND   index = ld_index.

          IF NOT gt_longtext-line(1) IS INITIAL.
            IF ld_lt_max_length <= 74.
              moreinfo-text+1(73) = sy-uline(73).
              moreinfo-text+74(1) = sy-vline.
            ELSE.
              moreinfo-text+1(74) = sy-uline(74).
              DATA : ld_length_diff TYPE i.
              ld_length_diff = ld_lt_max_length - 74.
              moreinfo-text1 = sy-uline(ld_length_diff).
              moreinfo-text1+ld_length_diff(1) = sy-vline.
              moreinfo-kind1 = 'N'.
              moreinfo-tlength1 = ld_length_diff + 1.
              moreinfo-tcolor1 = 0.
            ENDIF.
            moreinfo-tcolor  = 0.
            APPEND moreinfo.
            IF ld_lt_max_length <= 74.
              moreinfo-text+1(73) = gt_longtext-line(73).
            ELSE.
              moreinfo-text+1(74) = gt_longtext-line(74).
              moreinfo-text1      = gt_longtext-line+74(ld_length_diff).
              moreinfo-tcolor1 = 3.
            ENDIF.
            moreinfo-tcolor = 3.
            APPEND moreinfo.
            IF ld_lt_max_length <= 74.
              moreinfo-text+1(73) = sy-uline(73).
            ELSE.
              moreinfo-text+1(74) = sy-uline(74).
              ld_length_diff = ld_lt_max_length - 74.
              moreinfo-text1 = sy-uline(ld_length_diff).
              moreinfo-tcolor1 = 0.
            ENDIF.
            moreinfo-tcolor = 0.
            APPEND moreinfo.
          ELSE.
            IF ld_lt_max_length <= 74.
              moreinfo-text+1(73) = gt_longtext-line(73).
            ELSE.
              moreinfo-text+1(74) = gt_longtext-line(74).
              moreinfo-text1      = gt_longtext-line+74(ld_length_diff).
              moreinfo-tcolor1 = 0.
            ENDIF.
            moreinfo-tcolor = 0.
            APPEND moreinfo.
          ENDIF.
        ENDLOOP.

        IF SY-SUBRC NE 0.
          moreinfo-text(1)    = sy-vline.
          IF ld_lt_max_length <= 74.
            moreinfo-text+1(73) = sy-uline.
          ELSE.
            moreinfo-text+1(74) = sy-uline.
            ld_length_diff = ld_lt_max_length - 74.
            moreinfo-text1 = sy-uline(ld_length_diff).
          ENDIF.
          APPEND moreinfo.
        ENDIF.
      ENDIF.           "sy-langu CA '1JM3"

      READ TABLE gt_vbfs INDEX ld_index INTO vbfs_wa.
      IF NOT sy-subrc IS INITIAL.
        moreinfo-text = text-m02.
        APPEND moreinfo.
      ENDIF.
    WHEN 'T'.
      FIELD-SYMBOLS <FS1> type any.
      READ TABLE gt_vbfs INDEX ld_index INTO vbfs_wa.
      IF sy-subrc IS INITIAL.
        ld_text(1)    = sy-vline.
        ld_text+74(1) = sy-vline.
        ld_text+1(73) = sy-uline(73).
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        APPEND moreinfo.
        ld_text+1(38) = text-101.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MANDT'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-mandt.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tcolor = 2.
        moreinfo-tintensiv = 0.
        APPEND moreinfo.

        ld_text+1(38) = text-102.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-SAMMG'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-sammg.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 1.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-103.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-VBELN'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-vbeln.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 0.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-104.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-POSNR'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-posnr.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 1.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-105.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-ETENR'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-etenr.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 0.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-106.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-ZAEHL'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-zaehl.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 1.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-107.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MSGID'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-msgid.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 0.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-108.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MSGNO'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-msgno.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 1.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-109.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MSGTY'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-msgty.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 0.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-110.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MSGV1'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-msgv1.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 1.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-111.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MSGV2'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-msgv2.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 0.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-112.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MSGV3'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-msgv3.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 1.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-113.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-MSGV4'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-msgv4.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 0.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text+1(38) = text-114.
        IF sy-langu CA '1JM3'.
          F2 = 'VBFS_WA-SMART'.
          ASSIGN (F2) to <fs1>.
          PERFORM vbfs_handle_moreinfo_langu_t USING ld_text <fs1>.
        ELSE.
          ld_text+39(1) = sy-vline.
          ld_text+40(34) = vbfs_wa-smart.
        ENDIF.
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        moreinfo-tintensiv = 1.
        moreinfo-tcolor = 2.
        APPEND moreinfo.

        ld_text(1)    = sy-vline.
        ld_text+74(1) = sy-vline.
        ld_text+1(73) = sy-uline(73).
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        APPEND moreinfo.
      ENDIF.
    WHEN 'K'.
      READ TABLE gt_vbfs_summed INDEX ld_index INTO vbfs_wa.
      IF sy-subrc IS INITIAL.
        ld_text(1)    = sy-vline.
        ld_text+74(1) = sy-vline.
        ld_text+1(73) = sy-uline(73).
        moreinfo-text = ld_text.
        moreinfo-tlength = 75.
        APPEND moreinfo.

        ld_text+1(38) = text-101.
        ld_text+39(1) = sy-vline.
        ld_text+40(34) = vbfs_wa-mandt.
        moreinfo-text = ld_text.
        moreinfo-tcolor = 2.
        moreinfo-tintensiv = 0.
        APPEND moreinfo.

        ld_text+1(38) = text-102.
        ld_text+40(34) = vbfs_wa-sammg.
        moreinfo-text = ld_text.
        moreinfo-tintensiv = 1.
        APPEND moreinfo.

        ld_text+1(38) = text-105.
        ld_text+40(34) = vbfs_wa-etenr.
        moreinfo-text = ld_text.
        moreinfo-tintensiv = 0.
        APPEND moreinfo.

        ld_text+1(38) = text-106.
        ld_text+40(34) = vbfs_wa-zaehl.
        moreinfo-text = ld_text.
        moreinfo-tintensiv = 1.
        APPEND moreinfo.

        ld_text+1(38) = text-107.
        ld_text+40(34) = vbfs_wa-msgid.
        moreinfo-text = ld_text.
        moreinfo-tintensiv = 0.
        APPEND moreinfo.

        ld_text+1(38) = text-108.
        ld_text+40(34) = vbfs_wa-msgno.
        moreinfo-text = ld_text.
        moreinfo-tintensiv = 1.
        APPEND moreinfo.

        ld_text+1(38) = text-109.
        ld_text+40(34) = vbfs_wa-msgty.
        moreinfo-text = ld_text.
        moreinfo-tintensiv = 0.
        APPEND moreinfo.

        ld_text+1(38) = text-114.
        ld_text+40(34) = vbfs_wa-smart.
        moreinfo-text = ld_text.
        moreinfo-tintensiv = 1.
        APPEND moreinfo.

        ld_text(1)    = sy-vline.
        ld_text+74(1) = sy-vline.
        ld_text+1(73) = sy-uline(73).
        moreinfo-text = ld_text.
        APPEND moreinfo.

        ld_text(1)    = sy-vline.
        ld_text+11(1) = sy-vline.
        ld_text+18(1) = sy-vline.
        ld_text+29(1) = sy-vline.
        ld_text+40(1) = sy-vline.
        ld_text+51(1) = sy-vline.
        ld_text+62(1) = sy-vline.
*       LD_TEXT+1(63) = SY-ULINE(63).
        moreinfo-text = sy-uline(63).
        moreinfo-tlength = 63.
        APPEND moreinfo.
        ld_text+1(10)   = text-115.
        ld_text+12(6)   = text-116.
        ld_text+19(10)  = text-117.
        ld_text+30(10)  = text-118.
        ld_text+41(10)  = text-119.
        ld_text+52(10)  = text-120.
        moreinfo-text = ld_text.
        moreinfo-tlength = 63.
        moreinfo-tcolor = 2.
        moreinfo-tintensiv = 1.
        APPEND moreinfo.
        moreinfo-text = sy-uline(63).
        moreinfo-tlength = 63.
        APPEND moreinfo.
        DATA : vbfs_summed_wa LIKE vbfs.
        DATA : ld_intensiv LIKE moreinfo-tintensiv.
        LOOP AT gt_vbfs_summed INTO vbfs_summed_wa
                               WHERE msgid = vbfs_wa-msgid AND
                                     msgno = vbfs_wa-msgno.
          moreinfo-text = ld_text.
          ld_text+1(10)   = vbfs_summed_wa-vbeln.
          ld_text+12(6)   = vbfs_summed_wa-posnr.
          ld_text+19(10)  = vbfs_summed_wa-msgv1.
          ld_text+30(10)  = vbfs_summed_wa-msgv2.
          ld_text+41(10)  = vbfs_summed_wa-msgv3.
          ld_text+52(10)  = vbfs_summed_wa-msgv4.
          moreinfo-text = ld_text.
          moreinfo-tlength = 63.
          moreinfo-tcolor = 2.
          moreinfo-tintensiv = ld_intensiv.
          IF ld_intensiv = 0.
            ld_intensiv = 1.
          ELSE.
            ld_intensiv = 0.
          ENDIF.
          APPEND moreinfo.
        ENDLOOP.
        moreinfo-text = sy-uline(63).
        moreinfo-tlength = 63.
        APPEND moreinfo.
      ENDIF.
  ENDCASE.
ENDFORM.                               " HANDLE_MOREINFO
*&---------------------------------------------------------------------*
*&      Form  VBFS_SET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM vbfs_set_status.
  IF NOT gd_vbfs_cb_gui_status IS INITIAL.
    PERFORM (gd_vbfs_cb_gui_status)
             IN PROGRAM (gd_vbfs_cb_program) IF FOUND.
  ELSE.
    SET PF-STATUS 'VBFS_TREE'.
  ENDIF.
ENDFORM.                    "VBFS_SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  VBFS_MESSAGE_COMPOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LONGTEXT  text
*      -->P_I_VBFS  text
*      <--P_GT_SHORTTEXT  text
*----------------------------------------------------------------------*
FORM vbfs_message_compose
       TABLES
         gt_longtext  STRUCTURE gt_longtext
         gt_shorttext STRUCTURE gt_shorttext
       USING
         xvbfs_tabix LIKE sy-tabix
         i_vbfs LIKE vbfs.

  DATA : lt_longtext LIKE tline OCCURS 10 WITH HEADER LINE.
  DATA : ld_msgno LIKE sy-msgno.
  DATA : ld_shorttext LIKE sy-lisel.
  ld_msgno = i_vbfs-msgno.
  CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
    EXPORTING
      message_id        = i_vbfs-msgid
      message_number    = ld_msgno
      message_var1      = i_vbfs-msgv1
      message_var2      = i_vbfs-msgv2
      message_var3      = i_vbfs-msgv3
      message_var4      = i_vbfs-msgv4
    IMPORTING
      message_text      = ld_shorttext
    TABLES
      longtext          = lt_longtext
    EXCEPTIONS
      message_not_found = 1
      OTHERS            = 2.

  gt_shorttext = ld_shorttext.
  APPEND gt_shorttext.
  READ TABLE lt_longtext INDEX 1.
*  IF NOT SY-SUBRC IS INITIAL.
*    LT_LONGTEXT-TDFORMAT = '/='.
*    LT_LONGTEXT-TDLINE+10(122)   = TEXT-M01.
*    APPEND LT_LONGTEXT.
*  ENDIF.

  DATA : ld_prev_line_not_needed(1) VALUE 'X'.
  DATA : ld_prev_line_line(10) VALUE '__________'.
  DATA : ld_prev_line_sap(3) VALUE 'SAP'.
  LOOP AT lt_longtext.
    gt_longtext-msgid = i_vbfs-msgid.
    gt_longtext-msgno = i_vbfs-msgno.
    gt_longtext-index = xvbfs_tabix.
    gt_longtext-line  = lt_longtext-tdline+10(122).
    IF NOT ( gt_longtext-line(10) IS INITIAL            OR
             gt_longtext-line(10) EQ ld_prev_line_line  OR
             gt_longtext-line(3)  EQ ld_prev_line_sap      ).
      APPEND gt_longtext.
      CLEAR ld_prev_line_not_needed.
    ELSE.
      IF ld_prev_line_not_needed IS INITIAL.
        APPEND gt_longtext.
        ld_prev_line_not_needed = gc_charx.
      ENDIF.
    ENDIF.
  ENDLOOP.





ENDFORM.                               " VBFS_MESSAGE_COMPOSE
*&---------------------------------------------------------------------*
*&      Form  VBFS_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM vbfs_top_of_page.
  IF gd_vbfs_cb_program NE space
    AND gd_vbfs_cb_top_of_page NE space.
    PERFORM (gd_vbfs_cb_top_of_page)
      IN PROGRAM (gd_vbfs_cb_program)
      IF FOUND.
  ENDIF.
ENDFORM.                    "VBFS_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  HANDLE_MOREINFO_LANGU_L                                  *
*&---------------------------------------------------------------------*
*       Properly aligned of the columns for multibyte languages        *
*----------------------------------------------------------------------*
*      --> MSGID MSGNO INDEX                                           *
*      <-- TABLE MOREINFO                                              *
*----------------------------------------------------------------------*
FORM vbfs_handle_moreinfo_langu_l
   TABLES
    moreinfo STRUCTURE streeattr
   USING msgid
         msgno
         index
         text.

  DATA : ld_length_diff TYPE i.

  moreinfo-text1(1)   = sy-vline.
  moreinfo-kind1      = 'N'.
  moreinfo-tlength1   = 1.
  moreinfo-tlength    = 75.

  LOOP AT gt_longtext WHERE msgid = msgid
                      AND   msgno = msgno
                      AND   index = index.

    IF NOT gt_longtext-line(1) IS INITIAL.
      DO 3 TIMES.
        IF SY-INDEX = 2.
          moreinfo-text+1(74) = gt_longtext-line(74).
          moreinfo-tcolor = 3.
        ELSE.
          moreinfo-text+1(74) = sy-uline(74).
          moreinfo-tcolor  = 0.
        ENDIF.
        APPEND moreinfo.
      ENDDO.
    ELSE.
      moreinfo-text+1(74) = gt_longtext-line(74).
      moreinfo-tcolor  = 0.
      APPEND moreinfo.
    ENDIF.

  ENDLOOP.

  IF SY-SUBRC EQ 0.
    moreinfo-text+1(74) = sy-uline(74).
    APPEND moreinfo.
  ENDIF.
ENDFORM.                    "vbfs_handle_moreinfo_langu_l

*&---------------------------------------------------------------------*
*&      Form  HANDLE_MOREINFO_LANGU_T
*&---------------------------------------------------------------------*
*       Find the output length for multibyte languages
*----------------------------------------------------------------------*
*      -->LD_TEXT            TEXT                                      *
*      -->FS1                CONTENT                                   *
*      <--LD_TEXT            TEXT                                      *
*----------------------------------------------------------------------*
FORM vbfs_handle_moreinfo_langu_t using ld_text FS1.

  DATA : lentxt type i,
         foffs  TYPE sycolno,
         len type i,
         ld_text1(80) type c.

  len = 39.
  ld_text1(1) = sy-vline.

  TRY.
      CALL METHOD
        cl_abap_list_utilities=>replace_into_display_layout
        EXPORTING
          field          = ld_text+1(38)
          display_offset = 1
          display_length = 39
        CHANGING
          display_data   = ld_text1+1.

      lentxt = STRLEN( ld_text1 ).
      CALL METHOD cl_abap_list_utilities=>display_offset
        EXPORTING
          field          = ld_text1
          memory_offset  = lentxt
        IMPORTING
          display_offset = foffs.
    CATCH cx_parameter_invalid_range.
  ENDTRY.

  Foffs = len - foffs + lentxt.
  ld_text1+foffs(1) = sy-vline.
  foffs = foffs + 1.
  ld_text1+foffs(34) = fs1.
  foffs = foffs + 34.
  ld_text1+foffs(1) = sy-vline.
  ld_text = ld_text1.
ENDFORM.                               " HANDLE_MOREINFO_LANGU_T
