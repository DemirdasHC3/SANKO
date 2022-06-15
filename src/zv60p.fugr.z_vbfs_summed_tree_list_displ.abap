FUNCTION Z_VBFS_SUMMED_TREE_LIST_DISPL.
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
*"  TABLES
*"      I_VBFS STRUCTURE  VBFS
*"--------------------------------------------------------------------
  DATA : XVBFS_TABIX LIKE SY-TABIX.
  DATA : LD_XVBFS_TABIX LIKE SY-TABIX.
  DATA : LD_MSG_COUNTER LIKE SY-TABIX.
  GT_VBFS_SUMMED[] = I_VBFS[].
  GT_VBFS_SUMMED = I_VBFS.
  GT_VBFS[] = I_VBFS[].
  GT_VBFS = I_VBFS.
  SORT GT_VBFS_SUMMED BY MSGID MSGNO.
  GD_VBFS_S_CB_PROGRAM      = I_VBFS_S_CB_PROGRAM.
  GD_VBFS_S_CB_USER_COMMAND = I_VBFS_S_CB_USER_COMMAND.
  GD_VBFS_S_CB_GUI_STATUS   = I_VBFS_S_CB_GUI_STATUS.
  GD_VBFS_CB_PROGRAM      = I_VBFS_CB_PROGRAM.
  GD_VBFS_CB_USER_COMMAND = I_VBFS_CB_USER_COMMAND.
  GD_VBFS_CB_GUI_STATUS   = I_VBFS_CB_GUI_STATUS.
  GD_VBFS_CALLED_BY = I_VBFS_CALLED_BY.
  GD_VBFS_CB_TOP_OF_PAGE = I_VBFS_CB_TOP_OF_PAGE.
  GD_VBFS_S_CB_TOP_OF_PAGE = I_VBFS_S_CB_TOP_OF_PAGE.
  CLEAR TREELIST.
  REFRESH TREELIST.
  CLEAR GTs_LONGTEXT.
  REFRESH GTs_LONGTEXT.
  CLEAR GTs_SHORTTEXT.
  REFRESH GTs_SHORTTEXT.

  PERFORM VBFS_SUM_HEADER_NODE_PREPARE
            TABLES
              TREELIST.
  DATA : LD_MSGID LIKE VBFS-MSGID.
  DATA : LD_MSGNO LIKE VBFS-MSGNO.
  DATA : LD_STTABIX LIKE SY-TABIX.
  LOOP AT GT_VBFS_SUMMED.
    IF GT_VBFS_SUMMED-MSGID NE LD_MSGID OR
       GT_VBFS_SUMMED-MSGNO NE LD_MSGNO.
      LD_XVBFS_TABIX = SY-TABIX.
      IF NOT LD_MSGID IS INITIAL AND NOT LD_MSGNO IS INITIAL.
        XVBFS_TABIX = LD_XVBFS_TABIX - 1.
*       XVBFS_TABIX = LD_XVBFS_TABIX.
        READ TABLE GT_VBFS_SUMMED INTO I_VBFS INDEX XVBFS_TABIX.
        IF SY-SUBRC IS INITIAL.
          PERFORM VBFS_SUMMED_MESSAGE_COMPOSE
             TABLES
               GTs_LONGTEXT
               GTs_SHORTTEXT
             USING
               XVBFS_TABIX
               I_VBFS.
          ADD 1 TO LD_STTABIX.
          PERFORM VBFS_SUMMED_ST_NODE_PREPARE
                    USING
                      I_VBFS
                      XVBFS_TABIX
                      LD_STTABIX
                      LD_MSG_COUNTER.
          LOOP AT GTs_LONGTEXT WHERE MSGID = i_vbfs-MSGID
                               AND   MSGNO = i_vbfs-MSGNO
                               AND   INDEX = xvbfs_tabix.
            exit.
          endloop.
          if sy-subrc is initial.
          PERFORM VBFS_LT_NODE_PREPARE
                    USING
                      I_VBFS
                      XVBFS_TABIX.
            PERFORM VBFS_LT_SUB_NODE_PREPARE
                      USING
                        I_VBFS
                        XVBFS_TABIX.
          endif.
          PERFORM VBFS_TV_NODE_PREPARE
                    USING
                      I_VBFS
                      XVBFS_TABIX.
          PERFORM VBFS_SUM_TV_SUB_NODE_PREPARE
                    USING
                      I_VBFS
                      XVBFS_TABIX.
        ENDIF.
      ENDIF.
      LD_MSGID = GT_VBFS_SUMMED-MSGID.
      LD_MSGNO = GT_VBFS_SUMMED-MSGNO.
      LD_MSG_COUNTER = 1.
*     XVBFS_TABIX = LD_XVBFS_TABIX.
    ELSE.
      ADD 1 TO LD_MSG_COUNTER.
    ENDIF.
  ENDLOOP.
  IF NOT LD_MSGID IS INITIAL AND NOT LD_MSGNO IS INITIAL.
    READ TABLE GT_VBFS_SUMMED INTO I_VBFS INDEX LD_XVBFS_TABIX.
    IF SY-SUBRC IS INITIAL.
      PERFORM VBFS_SUMMED_MESSAGE_COMPOSE
         TABLES
           GTs_LONGTEXT
           GTs_SHORTTEXT
         USING
           LD_XVBFS_TABIX
           I_VBFS.
      ADD 1 TO LD_STTABIX.
      PERFORM VBFS_SUMMED_ST_NODE_PREPARE
                USING
                  I_VBFS
                  LD_XVBFS_TABIX
                  LD_STTABIX
                  LD_MSG_COUNTER.
      LoOP AT GTs_LONGTEXT WHERE MSGID = i_vbfs-MSGID
                           AND   MSGNO = i_vbfs-MSGNO
                           AND   INDEX = xvbfs_tabix.
        exit.
      endloop.
      if sy-subrc is initial.
      PERFORM VBFS_LT_NODE_PREPARE
                USING
                  I_VBFS
                  LD_XVBFS_TABIX.
        PERFORM VBFS_LT_SUB_NODE_PREPARE
                  USING
                    I_VBFS
                    LD_XVBFS_TABIX.
      endif.
      PERFORM VBFS_TV_NODE_PREPARE
                USING
                  I_VBFS
                  LD_XVBFS_TABIX.
      PERFORM VBFS_SUM_TV_SUB_NODE_PREPARE
                USING
                  I_VBFS
                  LD_XVBFS_TABIX.
    ENDIF.
  ENDIF.
  PERFORM VBFS_SUMMED_TREE_LIST_DISPLAY.
ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  VBFS_HEADER_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VBFS_SUM_HEADER_NODE_PREPARE
       TABLES
         TREELIST STRUCTURE SNODETEXT.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(20).
  CLEAR TREELIST.
  TREELIST-NAME = 'HEADER_NODE'.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-100.
  IF NOT GT_VBFS-SAMMG IS INITIAL.
    CONCATENATE TREELIST-TEXT TEXT-099 GT_VBFS-SAMMG TEXT-098
                                                     INTO TREELIST-TEXT
                SEPARATED BY SPACE.
  ENDIF.
  TREELIST-TLENGTH = STRLEN( TREELIST-TEXT ).
  TREELIST-TLEVEL = 1.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 60.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  APPEND TREELIST.
ENDFORM.                               " VBFS_HEADER_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_TREE_LIST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VBFS_SUMMED_TREE_LIST_DISPLAY.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
       TABLES
            NODETAB      = TREELIST
       EXCEPTIONS
            TREE_FAILURE = 1.

  SY-LSIND = 0.
  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
       EXPORTING
            CALLBACK_PROGRAM          = 'SAPLV60P'
            CALLBACK_GUI_STATUS       = 'VBFS_SUMMED_SET_STATUS'
            CALLBACK_USER_COMMAND     = 'VBFS_SUMMED_USER_COMMAND'
            CALLBACK_MOREINFO_DISPLAY = 'VBFS_SUMMED_HANDLE_MOREINFO'
            CALLBACK_TOP_OF_PAGE      = 'VBFS_SUMMED_TOP_OF_PAGE'.

ENDFORM.                               " VBFS_TREE_LIST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_HANDLE_MOREINFO
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
FORM VBFS_SUMMED_HANDLE_MOREINFO
       TABLES
         MOREINFO STRUCTURE STREEATTR
       USING
         P_NODE LIKE STREENODE.
  DATA : LD_MSGID LIKE VBFS-MSGID.
  DATA : LD_MSGNO LIKE VBFS-MSGNO.
  DATA : LD_INDEX LIKE SY-TABIX.
  DATA : LD_ART(1).
  DATA : LD_TEXT LIKE GTs_SHORTTEXT-LINE.
  DATA : VBFS_WA LIKE VBFS.
  DATA : LD_FIRST_LINE(1).
  DATA : LD_LONGTEXT_TABIX LIKE SY-TABIX.
  DATA : LD_MAX_LENGTH TYPE I VALUE 75.
  DATA : LD_LT_MAX_LENGTH TYPE I.
  DATA : LD_LT_STRLEN TYPE I.
  DATA : len TYPE I.
  FIELD-SYMBOLS <fs1> type any.
  LD_MSGID = P_NODE-NAME(20).
  TRANSLATE LD_MSGID USING '. '.
  LD_MSGNO = P_NODE-NAME+20(3).
  LD_INDEX = P_NODE-NAME+23(6).
  LD_ART   = P_NODE-NAME+29(1).
  CLEAR MOREINFO.
  REFRESH MOREINFO.

  CASE LD_ART.
    WHEN 'L'.
      CLEAR MOREINFO.
      REFRESH MOREINFO.
      MOREINFO-TCOLOR = 0.
      MOREINFO-TINTENSIV = 0.
      LOOP AT GTs_LONGTEXT WHERE MSGID = LD_MSGID
                          AND   MSGNO = LD_MSGNO
                          AND   INDEX = LD_INDEX.
        LD_LT_STRLEN = STRLEN( GTs_LONGTEXT-LINE ).
        IF LD_LT_STRLEN > LD_LT_MAX_LENGTH.
          LD_LT_MAX_LENGTH = LD_LT_STRLEN.
        ENDIF.
        IF GTs_LONGTEXT-LINE IS INITIAL.
          DELETE GTs_LONGTEXT INDEX SY-TABIX.
        ELSE.
          ADD 1 TO LD_LONGTEXT_TABIX.
        ENDIF.
      ENDLOOP.

      CLEAR LD_TEXT.
      LD_TEXT(1)    = SY-VLINE.
      IF LD_LT_MAX_LENGTH <= 73.
        LD_LT_MAX_LENGTH = 73 + 1.
      ELSE.
        LD_LT_MAX_LENGTH = LD_LT_MAX_LENGTH + 1.
      ENDIF.
      LD_TEXT+LD_LT_MAX_LENGTH(1) = SY-VLINE.
      LOOP AT GTs_LONGTEXT WHERE MSGID = LD_MSGID
                          AND   MSGNO = LD_MSGNO
                          AND   INDEX = LD_INDEX.
*         LD_LONGTEXT_TABIX = SY-TABIX.
        IF NOT GTs_LONGTEXT-LINE(1) IS INITIAL.
          IF LD_LT_MAX_LENGTH <= 74.
            MOREINFO-TEXT       = LD_TEXT.
            MOREINFO-TEXT+1(73) = SY-ULINE(73).
            MOREINFO-TEXT+74(1) = SY-VLINE.
          ELSE.
            MOREINFO-TEXT       = LD_TEXT.
            MOREINFO-TEXT+1(74) = SY-ULINE(74).
            DATA : LD_LENGTH_DIFF TYPE I.
            LD_LENGTH_DIFF = LD_LT_MAX_LENGTH - 74.
            MOREINFO-TEXT1 = SY-ULINE(LD_LENGTH_DIFF).
            MOREINFO-TEXT1+LD_LENGTH_DIFF(1) = SY-VLINE.
            MOREINFO-KIND1 = 'N'.
            MOREINFO-TLENGTH1 = LD_LENGTH_DIFF + 1.
            MOREINFO-TCOLOR1 = 0.
          ENDIF.
          MOREINFO-TCOLOR  = 0.
          MOREINFO-TLENGTH = 75.
          APPEND MOREINFO.
          MOREINFO-TEXT       = LD_TEXT.
          IF LD_LT_MAX_LENGTH <= 74.
            MOREINFO-TEXT+1(73) = GTs_LONGTEXT-LINE(73).
            MOREINFO-TEXT+74(1) = SY-VLINE.
          ELSE.
            MOREINFO-TEXT+1(74) = GTs_LONGTEXT-LINE(74).
            MOREINFO-TEXT1      = GTs_LONGTEXT-LINE+74(LD_LENGTH_DIFF).
            MOREINFO-TEXT1+LD_LENGTH_DIFF(1)     = SY-VLINE.
            MOREINFO-KIND1      = 'N'.
            MOREINFO-TLENGTH1   = LD_LENGTH_DIFF + 1.
            MOREINFO-TCOLOR1 = 3.
          ENDIF.
          MOREINFO-TCOLOR = 3.
          MOREINFO-TLENGTH = 75.
          APPEND MOREINFO.
          IF LD_LT_MAX_LENGTH <= 74.
            MOREINFO-TEXT       = LD_TEXT.
            MOREINFO-TEXT+1(73) = SY-ULINE(73).
            MOREINFO-TEXT+74(1) = SY-VLINE.
          ELSE.
            MOREINFO-TEXT       = LD_TEXT.
            MOREINFO-TEXT+1(74) = SY-ULINE(74).
            LD_LENGTH_DIFF = LD_LT_MAX_LENGTH - 74.
            MOREINFO-TEXT1 = SY-ULINE(LD_LENGTH_DIFF).
            MOREINFO-TEXT1+LD_LENGTH_DIFF(1) = SY-VLINE.
            MOREINFO-KIND1 = 'N'.
            MOREINFO-TLENGTH1 = LD_LENGTH_DIFF + 1 .
            MOREINFO-TCOLOR1 = 0.
          ENDIF.
          MOREINFO-TCOLOR = 0.
          MOREINFO-TLENGTH = 75.
          APPEND MOREINFO.
        ELSE.
          MOREINFO-TEXT       = LD_TEXT.
          IF LD_LT_MAX_LENGTH <= 74.
            MOREINFO-TEXT+1(73) = GTs_LONGTEXT-LINE(73).
            MOREINFO-TEXT+74(1) = SY-VLINE.
          ELSE.
            MOREINFO-TEXT+1(74) = GTs_LONGTEXT-LINE(74).
            MOREINFO-TEXT1      = GTs_LONGTEXT-LINE+74(LD_LENGTH_DIFF).
            MOREINFO-TEXT1+LD_LENGTH_DIFF(1)      = SY-VLINE.
            MOREINFO-KIND1      = 'N'.
            MOREINFO-TLENGTH1   = LD_LENGTH_DIFF + 1 .
            MOREINFO-TCOLOR1 = 0.
          ENDIF.
          MOREINFO-TCOLOR = 0.
          MOREINFO-TLENGTH = 75.
          APPEND MOREINFO.
        ENDIF.
      ENDLOOP.
      IF LD_LONGTEXT_TABIX NE 1.
        LD_TEXT(1)    = SY-VLINE.
        IF LD_LT_MAX_LENGTH <= 74.
          LD_TEXT+74(1) = SY-VLINE.
          LD_TEXT+1(73) = SY-ULINE(73).
          MOREINFO-TEXT = LD_TEXT.
        ELSE.
          LD_TEXT+74(1) = SY-VLINE.
          LD_TEXT+1(74) = SY-ULINE(74).
          MOREINFO-TEXT       = LD_TEXT.
          LD_LENGTH_DIFF = LD_LT_MAX_LENGTH - 74.
          MOREINFO-TEXT1 = SY-ULINE(LD_LENGTH_DIFF).
          MOREINFO-TEXT1+LD_LENGTH_DIFF(1) = SY-VLINE.
          MOREINFO-KIND1 = 'N'.
          MOREINFO-TLENGTH1 = LD_LENGTH_DIFF + 1 .
        ENDIF.
        MOREINFO-TLENGTH = 75.
        APPEND MOREINFO.
      ENDIF.
      READ TABLE GT_VBFS INDEX LD_INDEX INTO VBFS_WA.
      IF NOT SY-SUBRC IS INITIAL.
        MOREINFO-TEXT = TEXT-M02.
        MOREINFO-TLENGTH = 75.
        APPEND MOREINFO.
      ENDIF.
    WHEN 'T'.
      READ TABLE GT_VBFS INDEX LD_INDEX INTO VBFS_WA.
      IF SY-SUBRC IS INITIAL.
        LD_TEXT(1)    = SY-VLINE.
        LD_TEXT+74(1) = SY-VLINE.
        LD_TEXT+1(73) = SY-ULINE(73).
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-101.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MANDT.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TCOLOR = 2.
        MOREINFO-TINTENSIV = 0.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-102.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-SAMMG.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 1.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-103.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-VBELN.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 0.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-104.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-POSNR.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 1.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-105.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-ETENR.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 0.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-106.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-ZAEHL.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 1.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-107.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MSGID.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 0.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-108.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MSGNO.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 1.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-109.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MSGTY.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 0.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-110.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MSGV1.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 1.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-111.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MSGV2.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 0.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-112.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MSGV3.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 1.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-113.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MSGV4.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 0.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-114.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-SMART.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TINTENSIV = 1.
        MOREINFO-TCOLOR = 2.
        APPEND MOREINFO.
        LD_TEXT(1)    = SY-VLINE.
        LD_TEXT+74(1) = SY-VLINE.
        LD_TEXT+1(73) = SY-ULINE(73).
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        APPEND MOREINFO.
      ENDIF.
    WHEN 'K'.
      DATA F2 TYPE STRING.
      READ TABLE GT_VBFS_SUMMED INDEX LD_INDEX INTO VBFS_WA.
      IF SY-SUBRC IS INITIAL.
        LD_TEXT(1)    = SY-VLINE.
        LD_TEXT+74(1) = SY-VLINE.
        LD_TEXT+1(73) = SY-ULINE(73).
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        APPEND MOREINFO.
        LD_TEXT+1(38) = TEXT-101.
        LD_TEXT+39(1) = SY-VLINE.
        LD_TEXT+40(34) = VBFS_WA-MANDT.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        MOREINFO-TCOLOR = 2.
        MOREINFO-TINTENSIV = 0.
        APPEND MOREINFO.

        LD_TEXT+1(38) = TEXT-102.
        F2 = 'VBFS_WA-SAMMG'.
        ASSIGN (F2) to <fs1>.
        PERFORM vbfs_summed_moreinfo_langu_k
          TABLES moreinfo
          USING ld_text <fs1> '1'.

        LD_TEXT+1(38) = TEXT-105.
        F2 = 'VBFS_WA-ETENR'.
        ASSIGN (F2) to <fs1>.
        PERFORM vbfs_summed_moreinfo_langu_k
          TABLES moreinfo
          USING ld_text <fs1> '0'.

        LD_TEXT+1(38) = TEXT-106.
        F2 = 'VBFS_WA-ZAEHL'.
        ASSIGN (F2) to <fs1>.
        PERFORM vbfs_summed_moreinfo_langu_k
          TABLES moreinfo
          USING ld_text <fs1> '1'.

        LD_TEXT+1(38) = TEXT-107.
        F2 = 'VBFS_WA-MSGID'.
        ASSIGN (F2) to <fs1>.
        PERFORM vbfs_summed_moreinfo_langu_k
          TABLES moreinfo
          USING ld_text <fs1> '0'.

        LD_TEXT+1(38) = TEXT-108.
        F2 = 'VBFS_WA-MSGNO'.
        ASSIGN (F2) to <fs1>.
        PERFORM vbfs_summed_moreinfo_langu_k
          TABLES moreinfo
          USING ld_text <fs1> '1'.

        LD_TEXT+1(38) = TEXT-109.
        F2 = 'VBFS_WA-MSGTY'.
        ASSIGN (F2) to <fs1>.
        PERFORM vbfs_summed_moreinfo_langu_k
          TABLES moreinfo
          USING ld_text <fs1> '0'.

        LD_TEXT+1(38) = TEXT-114.
        F2 = 'VBFS_WA-SMART'.
        ASSIGN (F2) to <fs1>.
        PERFORM vbfs_summed_moreinfo_langu_k
          TABLES moreinfo
          USING ld_text <fs1> '1'.

        LD_TEXT(1)    = SY-VLINE.
        LD_TEXT+74(1) = SY-VLINE.
        LD_TEXT+1(73) = SY-ULINE(73).
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = 75.
        APPEND MOREINFO.

        CLEAR ld_text.
        LD_TEXT(1)    = SY-VLINE.
        LD_TEXT+11(1) = SY-VLINE.
        LD_TEXT+18(1) = SY-VLINE.
        LD_TEXT+29(1) = SY-VLINE.
        LD_TEXT+40(1) = SY-VLINE.
        LD_TEXT+51(1) = SY-VLINE.
        LD_TEXT+62(1) = SY-VLINE.
        LD_TEXT+1(10)   = TEXT-115.
        LD_TEXT+12(6)   = TEXT-116.
        LD_TEXT+19(10)  = TEXT-117.
        LD_TEXT+30(10)  = TEXT-118.
        LD_TEXT+41(10)  = TEXT-119.
        LD_TEXT+52(10)  = TEXT-120.
        len = 63.

          IF sy-langu CA '1JM3'.
          len = 70.
          PERFORM vbfs_summed_moreinfo_langu_k1
                 USING ld_text
                       len.
        ENDIF.

        MOREINFO-TEXT = SY-ULINE(len).
        MOREINFO-TLENGTH = len.
        APPEND MOREINFO.
        MOREINFO-TEXT = LD_TEXT.
        MOREINFO-TLENGTH = len.
        MOREINFO-TCOLOR = 2.
        MOREINFO-TINTENSIV = 1.
        APPEND MOREINFO.
        MOREINFO-TEXT = SY-ULINE(len).
        MOREINFO-TLENGTH = len.
        APPEND MOREINFO.
        DATA : VBFS_SUMMED_WA LIKE VBFS.
        DATA : LD_INTENSIV LIKE MOREINFO-TINTENSIV.
        LOOP AT GT_VBFS_SUMMED INTO VBFS_SUMMED_WA
                               WHERE MSGID = VBFS_WA-MSGID AND
                                     MSGNO = VBFS_WA-MSGNO.

          IF sy-langu CA '1JM3'.
            CLEAR LD_TEXT.
            LD_TEXT(1)    = SY-VLINE.
            LD_TEXT+16(1) = SY-VLINE.
            LD_TEXT+25(1) = SY-VLINE.
            LD_TEXT+36(1) = SY-VLINE.
            LD_TEXT+47(1) = SY-VLINE.
            LD_TEXT+58(1) = SY-VLINE.
            LD_TEXT+69(1) = SY-VLINE.
            LD_TEXT+1(10) = VBFS_SUMMED_WA-VBELN.
            LD_TEXT+17(6)  = VBFS_SUMMED_WA-POSNR.
            LD_TEXT+26(10) = VBFS_SUMMED_WA-MSGV1.
            LD_TEXT+37(10) = VBFS_SUMMED_WA-MSGV2.
            LD_TEXT+48(10) = VBFS_SUMMED_WA-MSGV3.
            LD_TEXT+59(10) = VBFS_SUMMED_WA-MSGV4.
          ELSE.
          LD_TEXT+1(10)   = VBFS_SUMMED_WA-VBELN.
          LD_TEXT+12(6)   = VBFS_SUMMED_WA-POSNR.
          LD_TEXT+19(10)  = VBFS_SUMMED_WA-MSGV1.
          LD_TEXT+30(10)  = VBFS_SUMMED_WA-MSGV2.
          LD_TEXT+41(10)  = VBFS_SUMMED_WA-MSGV3.
          LD_TEXT+52(10)  = VBFS_SUMMED_WA-MSGV4.
          ENDIF.

          MOREINFO-TEXT = LD_TEXT.
          MOREINFO-TLENGTH = len.
          MOREINFO-TCOLOR = 2.
          MOREINFO-TINTENSIV = LD_INTENSIV.
          IF LD_INTENSIV = 0.
            LD_INTENSIV = 1.
          ELSE.
            LD_INTENSIV = 0.
          ENDIF.
          APPEND MOREINFO.
        ENDLOOP.
        MOREINFO-TEXT = SY-ULINE(len).
        MOREINFO-TLENGTH = len.
        APPEND MOREINFO.
      ENDIF.
  ENDCASE.

ENDFORM.                               " HANDLE_MOREINFO
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_SET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM VBFS_SUMMED_SET_STATUS.
  IF NOT GD_VBFS_S_CB_GUI_STATUS IS INITIAL.
    PERFORM (GD_VBFS_S_CB_GUI_STATUS)
             IN PROGRAM (GD_VBFS_S_CB_PROGRAM) IF FOUND.
  ELSE.
    SET PF-STATUS 'VBFS_SUMMED_TREE'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VBFS_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->                                                             *
*      -->                                                             *
*----------------------------------------------------------------------*
FORM VBFS_SUMMED_USER_COMMAND TABLES   NODE STRUCTURE SEUCOMM
                              USING    COMMAND
                              CHANGING EXIT
                                       LIST_REFRESH.
  CASE COMMAND.
    WHEN 'EINZ'.
      IF SY-UCOMM = COMMAND AND NOT GD_VBFS_CALLED_BY IS INITIAL.
        EXIT = 'Y'.
      ELSE.
        CALL FUNCTION 'VBFS_TREE_LIST_DISPLAY'
             EXPORTING
                  I_VBFS_CALLED_BY         = GC_CHARK
                  I_VBFS_S_CB_PROGRAM      = GD_VBFS_S_CB_PROGRAM
                  I_VBFS_S_CB_USER_COMMAND = GD_VBFS_S_CB_USER_COMMAND
                  I_VBFS_S_CB_GUI_STATUS   = GD_VBFS_S_CB_GUI_STATUS
                  I_VBFS_CB_PROGRAM        = GD_VBFS_CB_PROGRAM
                  I_VBFS_CB_USER_COMMAND   = GD_VBFS_CB_USER_COMMAND
                  I_VBFS_CB_GUI_STATUS     = GD_VBFS_CB_GUI_STATUS
                  I_VBFS_CB_TOP_OF_PAGE    = GD_VBFS_CB_TOP_OF_PAGE
                  I_VBFS_S_CB_TOP_OF_PAGE  = GD_VBFS_S_CB_TOP_OF_PAGE
             TABLES
                  I_VBFS                   = GT_VBFS.
      ENDIF.
      IF ( SY-UCOMM = 'BACK' OR
           SY-UCOMM = 'EXIT' OR
        SY-UCOMM = 'CANC'    ) AND NOT GD_VBFS_CALLED_BY IS INITIAL AND
                                          COMMAND = 'EINZ'.
        EXIT = GC_CHARX.
      ENDIF.
    WHEN OTHERS.
      IF NOT GD_VBFS_S_CB_USER_COMMAND IS INITIAL AND
         NOT GD_VBFS_S_CB_PROGRAM      IS INITIAL.
        PERFORM (GD_VBFS_S_CB_USER_COMMAND)
                 IN PROGRAM (GD_VBFS_S_CB_PROGRAM)
                      TABLES NODE
                      USING COMMAND
                      CHANGING EXIT LIST_REFRESH
                      IF FOUND.
      ENDIF.
  ENDCASE.









ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_MESSAGE_COMPOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GTs_LONGTEXT  text
*      -->P_GTs_SHORTTEXT  text
*      -->P_SY_TABIX  text
*      -->P_GT_VBFS_SUMMED  text
*----------------------------------------------------------------------*
FORM VBFS_SUMMED_MESSAGE_COMPOSE
       TABLES
         GTs_LONGTEXT  STRUCTURE GTs_LONGTEXT
         GTs_SHORTTEXT STRUCTURE GTs_SHORTTEXT
       USING
         XVBFS_TABIX LIKE SY-TABIX
         I_VBFS LIKE VBFS.

  DATA : LT_LONGTEXT LIKE TLINE OCCURS 10 WITH HEADER LINE.
  DATA : LD_MSGNO LIKE SY-MSGNO.
  DATA : LD_SHORTTEXT LIKE SY-LISEL.
  LD_MSGNO = I_VBFS-MSGNO.
  refresh lt_longtext.
  CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
       EXPORTING
            MESSAGE_ID        = I_VBFS-MSGID
            MESSAGE_NUMBER    = LD_MSGNO
            MESSAGE_VAR1      = '<VAR1>'
            MESSAGE_VAR2      = '<VAR2>'
            MESSAGE_VAR3      = '<VAR3>'
            MESSAGE_VAR4      = '<VAR4>'
       IMPORTING
            MESSAGE_TEXT      = LD_SHORTTEXT
       TABLES
            LONGTEXT          = LT_LONGTEXT
       EXCEPTIONS
            MESSAGE_NOT_FOUND = 1
            OTHERS            = 2.

  GTs_SHORTTEXT = LD_SHORTTEXT.
  APPEND GTs_SHORTTEXT.
  READ TABLE LT_LONGTEXT INDEX 1.
*  IF NOT SY-SUBRC IS INITIAL.
*    LT_LONGTEXT-TDFORMAT = '/='.
*    LT_LONGTEXT-TDLINE+10(122)   = TEXT-M01.
*    APPEND LT_LONGTEXT.
*  ENDIF.

  DATA : LD_PREV_LINE_NOT_NEEDED(1) VALUE 'X'.
  DATA : LD_PREV_LINE_LINE(10) VALUE '__________'.
  DATA : LD_PREV_LINE_SAP(3) VALUE 'SAP'.
  LOOP AT LT_LONGTEXT.
    GTs_LONGTEXT-MSGID = I_VBFS-MSGID.
    GTs_LONGTEXT-MSGNO = I_VBFS-MSGNO.
    GTs_LONGTEXT-INDEX = XVBFS_TABIX.
    GTs_LONGTEXT-LINE  = LT_LONGTEXT-TDLINE+10(122).
    IF NOT ( GTs_LONGTEXT-LINE(10) IS INITIAL            OR
             GTs_LONGTEXT-LINE(10) EQ LD_PREV_LINE_LINE  OR
             GTs_LONGTEXT-LINE(3)  EQ LD_PREV_LINE_SAP      ).
      APPEND GTs_LONGTEXT.
      CLEAR LD_PREV_LINE_NOT_NEEDED.
    ELSE.
      IF LD_PREV_LINE_NOT_NEEDED IS INITIAL.
        APPEND GTs_LONGTEXT.
        LD_PREV_LINE_NOT_NEEDED = GC_CHARX.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " VBFS_SUMMED_MESSAGE_COMPOSE
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_ST_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_VBFS_SUMMED  text
*      -->P_XVBFS_TABIX  text
*----------------------------------------------------------------------*
FORM VBFS_SUMMED_ST_NODE_PREPARE
       USING
         XVBFS LIKE VBFS
         XVBFS_TABIX LIKE SY-TABIX
         STTABIX     LIKE SY-TABIX
         LD_MSG_COUNTER LIKE SY-TABIX.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(30).
  DATA : REP_MSGID LIKE VBFS-MSGID.
  DATA : LD_MSGID LIKE XVBFS-MSGID.
  DATA : LD_LENGTH TYPE I.
  DATA : LD_LENGTH2 TYPE I.
  DATA : LD_SHORTTEXT_LINE LIKE GTs_SHORTTEXT-LINE.
  DATA : LD_MAX_LENGTH TYPE I VALUE 75.
  DATA : LD_MSG_COUNTER_STRING(3).

**** begin tooltip for icon in tree
  DATA: wa_icont TYPE icont.

  CLEAR wa_icont.
**** end tooltip for icon in tree

  CLEAR TREELIST.
  LD_TABIX2CHAR = XVBFS_TABIX.
  SHIFT LD_TABIX2CHAR LEFT DELETING LEADING ' '.
  REP_MSGID = XVBFS-MSGID.
  TRANSLATE REP_MSGID USING ' .'.
  CONCATENATE REP_MSGID XVBFS-MSGNO LD_TABIX2CHAR INTO TREELIST-NAME.
  READ TABLE GTs_SHORTTEXT INDEX STTABIX.
  IF SY-SUBRC IS INITIAL.
    LD_LENGTH = STRLEN( GTs_SHORTTEXT-LINE ).
    LD_LENGTH2 = STRLEN( XVBFS-MSGID ).
    LD_LENGTH = LD_LENGTH + LD_LENGTH2 + 5.
    DATA : LD_LINE(255).
    DATA : Y LIKE XVBFS-MSGID.
    Y = XVBFS-MSGID(LD_LENGTH2).
    CONCATENATE Y
                XVBFS-MSGNO GTs_SHORTTEXT-LINE INTO LD_LINE
                SEPARATED BY SPACE.
    IF LD_LENGTH <= 75.
      TREELIST-TEXT2 = LD_LINE.
    ELSE.
      TREELIST-TEXT2 = LD_LINE(75).
      TREELIST-TEXT3 = LD_LINE+75(57).
      TREELIST-KIND3 = 'N'.
    ENDIF.
  ENDIF.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
**** begin tooltip for icon in tree
*    ICON_SUM
    SELECT * FROM icont INTO wa_icont
                     WHERE langu = sy-langu
                       and id    = '@3Z@'.
      CONCATENATE '@3Z@' wa_icont-quickinfo INTO treelist-text.
    ENDSELECT.
  TREELIST-KIND = 'L'.
* TREELIST-TEXT = 'ICON_SUM'.
* TREELIST-KIND = 'I'.
  TREELIST-TLENGTH = 4.
  TREELIST-TLEVEL = 2.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TLENGTH2 = STRLEN( TREELIST-TEXT2 ).
  TREELIST-TLENGTH3 = 75.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  LD_MSG_COUNTER_STRING = LD_MSG_COUNTER.
  SHIFT LD_MSG_COUNTER_STRING LEFT DELETING LEADING ' '.
  CONCATENATE '=' LD_MSG_COUNTER_STRING
                                 INTO TREELIST-TEXT1 SEPARATED BY SPACE.
  TREELIST-TLENGTH1 = 5.
  APPEND TREELIST.


ENDFORM.                               " VBFS_SUMMED_ST_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUM_TV_SUB_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBFS  text
*      -->P_XVBFS_TABIX  text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VBFS_TV_SUB_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VBFS_SUM_TV_SUB_NODE_PREPARE
       USING
         XVBFS LIKE VBFS
         XVBFS_TABIX LIKE SY-TABIX.

  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(30).
  DATA : REP_MSGID LIKE VBFS-MSGID.
  CLEAR TREELIST.
  LD_TABIX2CHAR = XVBFS_TABIX.
  SHIFT LD_TABIX2CHAR LEFT DELETING LEADING ' '.
  REP_MSGID = XVBFS-MSGID.
  TRANSLATE REP_MSGID USING ' .'.
  CONCATENATE REP_MSGID XVBFS-MSGNO LD_TABIX2CHAR INTO TREELIST-NAME.
  TREELIST-NAME+29(1) = 'K'.
*treelist-NAME = TEXT-A10.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-T02.
  TREELIST-TLENGTH = 20.
  TREELIST-TLEVEL = 4.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 30.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  TREELIST-MOREINFO = 'X'.
  APPEND TREELIST.

ENDFORM.                               " VBFS_SUM_TV_SUB_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM VBFS_SUMMED_TOP_OF_PAGE.
  IF GD_VBFS_S_CB_PROGRAM NE SPACE
    AND GD_VBFS_S_CB_TOP_OF_PAGE NE SPACE.
    PERFORM (GD_VBFS_S_CB_TOP_OF_PAGE)
      IN PROGRAM (GD_VBFS_S_CB_PROGRAM)
      IF FOUND.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_MOREINFO_LANGU_K
*&---------------------------------------------------------------------*
*       Find the output length for multibyte languages
*----------------------------------------------------------------------*
*      -->LD_TEXT            TEXT                                      *
*      -->LD_TEXT            TEXT                                      *
*      -->FS1                CONTENT                                   *
*      <--MOREINFO           TABLE                                     *
*      <--LD_TEXT            TEXT                                      *
*----------------------------------------------------------------------*
FORM vbfs_summed_moreinfo_langu_k
     TABLES moreinfo STRUCTURE streeattr
     USING ld_text FS1 intensiv.
  DATA : lentxt type i,
         foffs  TYPE sycolno,
         len type i,
         ld_text1(80) type c.

  len = 39.
  ld_text1(1) = sy-vline.
  IF sy-langu CA '1JM3'.
    TRY.
        CALL METHOD
          cl_abap_list_utilities=>replace_into_display_layout
          EXPORTING
            field          = ld_text+1(38)
            display_offset = 1
            display_length = len
          CHANGING
            display_data   = ld_text1+1.

        lentxt = STRLEN( ld_text1 ).
        call method cl_abap_list_utilities=>display_offset
          EXPORTING
            field          = ld_text1
            memory_offset  = lentxt
          IMPORTING
            display_offset = foffs.
      CATCH cx_parameter_invalid_range.
    ENDTRY.

    Foffs = len - foffs + lentxt.
    IF LD_TEXT+1(10) = TEXT-115.
      foffs = foffs + 34.
    ELSE.
      ld_text1+foffs(1) = sy-vline.
      foffs = foffs + 1.
      ld_text1+foffs(34) = fs1.
      foffs = foffs + 34.
    ENDIF.
    ld_text1+foffs(1) = sy-vline.
    ld_text = ld_text1.
  ELSE.
    ld_text+39(1) = SY-VLINE.
    ld_text+40(34) = fs1.
  ENDIF.

  moreinfo-text = ld_text.
  moreinfo-tlength = 75.
  moreinfo-tintensiv = intensiv.
  moreinfo-tcolor = 2.
  APPEND moreinfo.
ENDFORM.                   " HANDLE_SUMMED_MOREINFO_LANGU_K
*&---------------------------------------------------------------------*
*&      Form  VBFS_SUMMED_MOREINFO_LANGU_K1
*&---------------------------------------------------------------------*
*       Find the output length for multibyte languages
*----------------------------------------------------------------------*
*      -->LD_TEXT            TEXT                                      *
*      -->LEN                length                                    *
*      <--LD_TEXT            TEXT                                      *
*----------------------------------------------------------------------*
FORM vbfs_summed_moreinfo_langu_k1
          USING  ld_text
                 len.

  DATA : lentxt type i,
         foffs  TYPE sycolno,
         ld_text1(80) type c.

  ld_text1(1) = sy-vline.
  TRY.
      CALL METHOD
        cl_abap_list_utilities=>replace_into_display_layout
        EXPORTING
          field          = ld_text+1(68)
          display_offset = 1
          display_length = len
        CHANGING
          display_data   = ld_text1+1.

      lentxt = STRLEN( ld_text1 ).
      call method cl_abap_list_utilities=>display_offset
        EXPORTING
          field          = ld_text1
          memory_offset  = lentxt
        IMPORTING
          display_offset = foffs.
    CATCH cx_parameter_invalid_range.
  ENDTRY.
  ld_text = ld_text1.
ENDFORM.                   " HANDLE_SUMMED_MOREINFO_LANGU_K1
