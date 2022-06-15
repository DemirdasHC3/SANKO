FUNCTION Z_SPLIT_TREE_LIST_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_CALLER_VBTYP) DEFAULT 'F'
*"     VALUE(I_FIRST_STRUCTURE)
*"     VALUE(I_SECOND_STRUCTURE)
*"     VALUE(I_VBPA_EXCEPTION_FIELDS) OPTIONAL
*"     VALUE(I_HEADER_EXCEPTION_FIELDS) OPTIONAL
*"     VALUE(I_WITH_ITEM_INFO) DEFAULT 'X'
*"  TABLES
*"      I_VBPA STRUCTURE  VBPA OPTIONAL
*"      I_VBRP_F STRUCTURE  VBRP OPTIONAL
*"      I_VBRP_S STRUCTURE  VBRP OPTIONAL
*"--------------------------------------------------------------------
  DATA : LS_LIKP_F LIKE LIKP.
  DATA : LS_LIKP_S LIKE LIKP.

  DATA : LS_VBRK_F LIKE VBRK.
  DATA : LS_VBRK_S LIKE VBRK.

  DATA : LT_PAR_F LIKE VBPA OCCURS 5 WITH HEADER LINE.
  DATA : LT_PAR_S LIKE VBPA OCCURS 5 WITH HEADER LINE.

  DATA : SPLIT_REASON_FOUND TYPE C.

    GD_WITH_ITEM_INFO = I_WITH_ITEM_INFO.
    IF I_CALLER_VBTYP = IF_SD_DOC_CATEGORY=>SCHED_AGREE_EXT_SERV_AGENT.
      LS_VBRK_F = I_FIRST_STRUCTURE.
      LS_VBRK_S = I_SECOND_STRUCTURE.
      LD_KEY_F  = LS_VBRK_F-VBELN.
      LD_KEY_S  = LS_VBRK_S-VBELN.
      IF NOT GD_WITH_ITEM_INFO IS INITIAL.
        GT_VBRP_F[] = I_VBRP_F[].
        GT_VBRP_S[] = I_VBRP_S[].
      ENDIF.
    ENDIF.

    PERFORM LT_PARTNER_FILL
              TABLES
                I_VBPA
                LT_PAR_F
                LT_PAR_S
              USING
                LD_KEY_F
                LD_KEY_S.

    PERFORM SPLIT_EXAMINE
              TABLES
                LT_PAR_F
                LT_PAR_S
              USING
                I_CALLER_VBTYP
                LS_VBRK_F
                LS_VBRK_S
                I_VBPA_EXCEPTION_FIELDS
                I_HEADER_EXCEPTION_FIELDS.

    GT_VBPA[] = I_VBPA[].
    GT_VBPA   = I_VBPA  .
    CLEAR TREELIST.
    REFRESH TREELIST.
    PERFORM SPLIT_HEADER_NODE_PREPARE
              TABLES
                TREELIST.
    READ TABLE LT_ROLE_SPLIT INDEX 1.
    IF SY-SUBRC IS INITIAL.
      SPLIT_REASON_FOUND = 'X'.
      PERFORM SPLIT_ROLE_NODE_PREPARE.
    ENDIF.
    READ TABLE GT_PAR_FIELD_SPLIT INDEX 1.
    IF SY-SUBRC IS INITIAL.
      SPLIT_REASON_FOUND = 'X'.
      PERFORM SPLIT_PAR_FIELD_NODE_PREPARE.
    ENDIF.
    READ TABLE GT_HEADER_FIELD_SPLIT INDEX 1.
    IF SY-SUBRC IS INITIAL.
      SPLIT_REASON_FOUND = 'X'.
      PERFORM SPLIT_HEADER_F_NODE_PREPARE.
    ENDIF.
    IF SPLIT_REASON_FOUND IS INITIAL.
      PERFORM NO_SPLIT_NODE_PREPARE.
    ELSE.
      IF I_CALLER_VBTYP = IF_SD_DOC_CATEGORY=>SCHED_AGREE_EXT_SERV_AGENT.
        IF NOT GD_WITH_ITEM_INFO IS INITIAL.
          PERFORM ITEM_NODE_PREPARE.
        ENDIF.
      ENDIF.
    ENDIF.
    PERFORM SPLIT_TREE_LIST_DISPLAY.
ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  LT_PARTNER_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBPA  text                                               *
*      -->P_LT_PAR_F  text                                             *
*      -->P_LT_PAR_S  text                                             *
*      -->P_LD_KEY_F  text                                             *
*      -->P_LD_KEY_S  text                                             *
*----------------------------------------------------------------------*
FORM LT_PARTNER_FILL
       TABLES
         I_VBPA STRUCTURE VBPA
         LT_PAR_F STRUCTURE VBPA
         LT_PAR_S STRUCTURE VBPA
       USING
         LD_KEY_F LIKE VBRK-VBELN
         LD_KEY_S LIKE VBRK-VBELN.

  LOOP AT I_VBPA WHERE VBELN EQ LD_KEY_F AND
                       POSNR EQ 0.
    LT_PAR_F = I_VBPA.
    APPEND LT_PAR_F.
  ENDLOOP.
  LOOP AT I_VBPA WHERE VBELN EQ LD_KEY_S AND
                      POSNR EQ 0.
    LT_PAR_S = I_VBPA.
    APPEND LT_PAR_S.
  ENDLOOP.

ENDFORM.                               " LT_PARTNER_FILL

*&---------------------------------------------------------------------*
*&      Form  PARTNER_SPLIT_EXAMINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->                                                             *
*----------------------------------------------------------------------*
FORM SPLIT_EXAMINE
       TABLES
         LT_PAR_F STRUCTURE VBPA
         LT_PAR_S STRUCTURE VBPA
       USING
         LD_CALLER_VBTYP LIKE VBRK-VBTYP
         LS_VBRK_F LIKE VBRK
         LS_VBRK_S LIKE VBRK
         I_VBPA_EXCEPTION_FIELDS
         I_HEADER_EXCEPTION_FIELDS.

  FIELD-SYMBOLS: <VALUE1>, <VALUE2>.
  TABLES : TPART.
  DATA : PAR_F_TABIX LIKE SY-TABIX.
  DATA : PAR_S_TABIX LIKE SY-TABIX.

  REFRESH LT_ROLE_SPLIT.
  REFRESH GT_PAR_FIELD_SPLIT.
  REFRESH GT_HEADER_FIELD_SPLIT.

  CLEAR LT_ROLE_SPLIT.
  CLEAR GT_PAR_FIELD_SPLIT.
  CLEAR GT_HEADER_FIELD_SPLIT.

  LOOP AT LT_PAR_F.
    PAR_F_TABIX = SY-TABIX.
    READ TABLE LT_PAR_S WITH KEY POSNR = LT_PAR_F-POSNR
                                 PARVW = LT_PAR_F-PARVW.
    IF NOT SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LT_PAR_F TO LT_ROLE_SPLIT.
      APPEND LT_ROLE_SPLIT.
      DELETE LT_PAR_F INDEX PAR_F_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT LT_PAR_S.
    PAR_S_TABIX = SY-TABIX.
    READ TABLE LT_PAR_F WITH KEY POSNR = LT_PAR_S-POSNR
                                 PARVW = LT_PAR_S-PARVW.
    IF NOT SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LT_PAR_S TO LT_ROLE_SPLIT.
      APPEND LT_ROLE_SPLIT.
      DELETE LT_PAR_S INDEX PAR_S_TABIX.
    ENDIF.
  ENDLOOP.

  READ TABLE LT_ROLE_SPLIT INDEX 1.
  IF SY-SUBRC IS INITIAL.
    LOOP AT LT_ROLE_SPLIT.
      SELECT SINGLE * FROM TPART
                 WHERE SPRAS = SY-LANGU AND PARVW = LT_ROLE_SPLIT-PARVW.
      IF SY-SUBRC IS INITIAL.
        LT_ROLE_SPLIT-VTEXT = TPART-VTEXT.
        MODIFY LT_ROLE_SPLIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            LANGU               = SY-LANGU
            TABNAME             = 'VBPA'
       TABLES
            NAMETAB             = GT_PAR_NAMETAB
       EXCEPTIONS
            INTERNAL_ERROR      = 01
            NO_TEXTS_FOUND      = 02
            TABLE_HAS_NO_FIELDS = 03
            TABLE_NOT_ACTIV     = 04.

  LOOP AT LT_PAR_F.
    READ TABLE LT_PAR_S WITH KEY POSNR = LT_PAR_F-POSNR
                                 PARVW = LT_PAR_F-PARVW.

    SY-SUBRC = 0.
    WHILE SY-SUBRC = 0.
      ASSIGN COMPONENT SY-INDEX OF STRUCTURE LT_PAR_F TO <VALUE1>.
      CHECK : SY-SUBRC = 0.
      ASSIGN COMPONENT SY-INDEX OF STRUCTURE LT_PAR_S TO <VALUE2>.
      IF <VALUE1> NE <VALUE2>.
        READ TABLE GT_PAR_NAMETAB INDEX SY-INDEX.
        CHECK : SY-SUBRC = 0.
        IF I_VBPA_EXCEPTION_FIELDS NS GT_PAR_NAMETAB-FIELDNAME.
          GT_PAR_FIELD_SPLIT-VBELN_F  = LT_PAR_F-VBELN.
          GT_PAR_FIELD_SPLIT-VBELN_S  = LT_PAR_S-VBELN.
          GT_PAR_FIELD_SPLIT-PARVW    = LT_PAR_F-PARVW.
          GT_PAR_FIELD_SPLIT-FIELD    = GT_PAR_NAMETAB-FIELDTEXT.
          GT_PAR_FIELD_SPLIT-VALUE_F  = <VALUE1>.
          GT_PAR_FIELD_SPLIT-VALUE_S  = <VALUE2>.
          APPEND GT_PAR_FIELD_SPLIT.
        ENDIF.
      ENDIF.
    ENDWHILE.
  ENDLOOP.

  READ TABLE GT_PAR_FIELD_SPLIT INDEX 1.
  IF SY-SUBRC IS INITIAL.
    SORT GT_PAR_FIELD_SPLIT BY VBELN_F VBELN_S PARVW.
  ENDIF.

  IF LD_CALLER_VBTYP = IF_SD_DOC_CATEGORY=>SCHED_AGREE_EXT_SERV_AGENT.
    CALL FUNCTION 'NAMETAB_GET'
         EXPORTING
              LANGU               = SY-LANGU
              TABNAME             = 'VBRK'
         TABLES
              NAMETAB             = GT_HEADER_NAMETAB
         EXCEPTIONS
              INTERNAL_ERROR      = 01
              NO_TEXTS_FOUND      = 02
              TABLE_HAS_NO_FIELDS = 03
              TABLE_NOT_ACTIV     = 04.

    SY-SUBRC = 0.
    WHILE SY-SUBRC = 0.
      ASSIGN COMPONENT SY-INDEX OF STRUCTURE LS_VBRK_F TO <VALUE1>.
      CHECK : SY-SUBRC = 0.
      ASSIGN COMPONENT SY-INDEX OF STRUCTURE LS_VBRK_S TO <VALUE2>.
      IF <VALUE1> NE <VALUE2>.
        READ TABLE GT_HEADER_NAMETAB INDEX SY-INDEX.
        CHECK : SY-SUBRC = 0.
        IF I_HEADER_EXCEPTION_FIELDS NS GT_HEADER_NAMETAB-FIELDNAME.
          GT_HEADER_FIELD_SPLIT-VBELN_F  = LS_VBRK_F-VBELN.
          GT_HEADER_FIELD_SPLIT-VBELN_S  = LS_VBRK_S-VBELN.
          GT_HEADER_FIELD_SPLIT-FIELD    = GT_HEADER_NAMETAB-FIELDTEXT.
          GT_HEADER_FIELD_SPLIT-VALUE_F  = <VALUE1>.
          GT_HEADER_FIELD_SPLIT-VALUE_S  = <VALUE2>.
          APPEND GT_HEADER_FIELD_SPLIT.
        ENDIF.
      ENDIF.
    ENDWHILE.
  ENDIF.
ENDFORM.                    "SPLIT_EXAMINE
*&---------------------------------------------------------------------*
*&      Form  SPLIT_HEADER_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TREELIST  text                                             *
*----------------------------------------------------------------------*
FORM SPLIT_HEADER_NODE_PREPARE
       TABLES
         TREELIST STRUCTURE SNODETEXT.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(20).
  CLEAR TREELIST.
  TREELIST-NAME = 'HEADER_NODE'.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-S01.
  TREELIST-TLENGTH = STRLEN( TEXT-S01 ).
  TREELIST-TLEVEL = 1.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 30.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  APPEND TREELIST.
ENDFORM.                               " SPLIT_HEADER_NODE_PREPARE

*&---------------------------------------------------------------------*
*&      Form  SPLIT_ROLE_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBFS  text                                               *
*      -->P_XVBFS_TABIX  text                                          *
*----------------------------------------------------------------------*
FORM SPLIT_ROLE_NODE_PREPARE.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(20).
  CLEAR TREELIST.
  TREELIST-NAME = 'ROLE_NODE'.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-S02.
  TREELIST-TLENGTH = STRLEN( TEXT-S02 ).
  TREELIST-TLEVEL = 2.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 30.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  TREELIST-MOREINFO = 'X'.
  APPEND TREELIST.
ENDFORM.                               " SPLIT_ROLE_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  SPLIT_PAR_FIELD_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SPLIT_PAR_FIELD_NODE_PREPARE.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(20).
  CLEAR TREELIST.
  TREELIST-NAME = 'PAR_FIELD_NODE'.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-S03.
  TREELIST-TLENGTH = STRLEN( TEXT-S03 ).
  TREELIST-TLEVEL = 2.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 30.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  TREELIST-MOREINFO = 'X'.
  APPEND TREELIST.

ENDFORM.                               " SPLIT_PAR_FIELD_NODE_PREPARE

*&---------------------------------------------------------------------*
*&      Form  SPLIT_HEADER_F_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SPLIT_HEADER_F_NODE_PREPARE.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(20).
  CLEAR TREELIST.
  TREELIST-NAME = 'HEADER_F_NODE'.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-S04.
  TREELIST-TLENGTH = STRLEN( TEXT-S04 ).
  TREELIST-TLEVEL = 2.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 30.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  TREELIST-MOREINFO = 'X'.
  APPEND TREELIST.

ENDFORM.                               " SPLIT_HEADER_F_NODE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  NO_SPLIT_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NO_SPLIT_NODE_PREPARE.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(20).
  CLEAR TREELIST.
  TREELIST-NAME = 'PAR_FIELD_NODE'.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-S05.
  TREELIST-TLENGTH = STRLEN( TEXT-S05 ).
  TREELIST-TLEVEL = 2.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 30.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  APPEND TREELIST.

ENDFORM.                               " NO_SPLIT_NODE_PREPARE

*&---------------------------------------------------------------------*
*&      Form  SPLIT_TREE_LIST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SPLIT_TREE_LIST_DISPLAY.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
       TABLES
            NODETAB      = TREELIST
       EXCEPTIONS
            TREE_FAILURE = 1.

  SY-LSIND = 0.
  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
       EXPORTING
            CALLBACK_PROGRAM          = 'SAPLV60P'
            CALLBACK_USER_COMMAND     = 'SPLIT_USER_COMMAND'
            CALLBACK_GUI_STATUS       = 'SPLIT_SET_STATUS'
            CALLBACK_MOREINFO_DISPLAY = 'SPLIT_HANDLE_MOREINFO'.

ENDFORM.                    "SPLIT_TREE_LIST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SPLIT_SET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM SPLIT_SET_STATUS.

  DATA: BEGIN OF TAB OCCURS 10,
          FCODE LIKE RSMPE-FUNC,
        END OF TAB.

  REFRESH TAB.
  MOVE 'ITIN' TO TAB-FCODE.
  APPEND TAB.
  MOVE 'TREP' TO TAB-FCODE.
  APPEND TAB.
  MOVE 'TRCM' TO TAB-FCODE.
  APPEND TAB.
  SET PF-STATUS 'SPLIT_TREE' EXCLUDING TAB.

ENDFORM.                    "SPLIT_SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  SPLIT_HANDLE_MOREINFO
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
FORM SPLIT_HANDLE_MOREINFO
       TABLES
         MOREINFO STRUCTURE STREEATTR
       USING
         P_NODE LIKE STREENODE.

  DATA : LD_TEXT(80).
  DATA : LS_TPAUM TYPE TPAUM.
  DATA : LT_TPAUM TYPE TPAUM OCCURS 10.
  DATA : LEN TYPE I.

  SELECT * FROM TPAUM INTO TABLE LT_TPAUM
                               WHERE SPRAS = SY-LANGU ORDER BY PRIMARY KEY.
  IF P_NODE-NAME EQ 'ROLE_NODE'.
    READ TABLE LT_ROLE_SPLIT INDEX 1.
    IF SY-SUBRC IS INITIAL.
      CLEAR MOREINFO.
      REFRESH MOREINFO.
      MOREINFO-TLENGTH = 75.
      MOREINFO-TEXT = SY-ULINE(75).
      APPEND MOREINFO.
      CLEAR LD_TEXT.
      LD_TEXT(1) = SY-VLINE.
      LD_TEXT+32(1) = SY-VLINE.
      LD_TEXT+53(1) = SY-VLINE.
      LD_TEXT+74(1) = SY-VLINE.
      LD_TEXT+1(30) = TEXT-S07.
      LD_TEXT+33(10) = LD_KEY_F.
      LD_TEXT+54(10) = LD_KEY_S.
      MOREINFO-TEXT = LD_TEXT.
      MOREINFO-TCOLOR = 3.
      APPEND MOREINFO.
      MOREINFO-TEXT = SY-ULINE(75).
      MOREINFO-TLENGTH = 75.
      APPEND MOREINFO.
      MOREINFO-TCOLOR = 2.
      MOREINFO-TINTENSIV = 0.
      LOOP AT LT_ROLE_SPLIT.
        CLEAR LD_TEXT.
        LD_TEXT(1) = SY-VLINE.
        LD_TEXT+32(1) = SY-VLINE.
        LD_TEXT+53(1) = SY-VLINE.
        LD_TEXT+74(1) = SY-VLINE.
        LD_TEXT+1(20) = LT_ROLE_SPLIT-VTEXT(20).
        READ TABLE LT_TPAUM INTO LS_TPAUM
                    WITH KEY SPRAS = SY-LANGU
                             PARVW = LT_ROLE_SPLIT-PARVW
                             BINARY SEARCH.
        IF LT_ROLE_SPLIT-VBELN EQ LD_KEY_F.
          LD_TEXT+33(20) = LS_TPAUM-PABEZ.
        ELSE.
          LD_TEXT+54(20) = LS_TPAUM-PABEZ.
        ENDIF.
        MOREINFO-TEXT = LD_TEXT.
        IF MOREINFO-TINTENSIV = 0.
          MOREINFO-TINTENSIV = 1.
        ELSE.
          MOREINFO-TINTENSIV = 0.
        ENDIF.
        APPEND MOREINFO.
      ENDLOOP.
      MOREINFO-TEXT = SY-ULINE(75).
      MOREINFO-TLENGTH = 75.
      APPEND MOREINFO.
      EXIT.
    ENDIF.
  ENDIF.


  IF P_NODE-NAME EQ  'PAR_FIELD_NODE'.
    READ TABLE GT_PAR_FIELD_SPLIT INDEX 1.
    IF SY-SUBRC IS INITIAL.
      CLEAR MOREINFO.
      REFRESH MOREINFO.
      MOREINFO-TLENGTH = 75.
      MOREINFO-TEXT = SY-ULINE(75).
      APPEND MOREINFO.
      CLEAR LD_TEXT.
      LD_TEXT(1) = SY-VLINE.
      LD_TEXT+3(1) = SY-VLINE.
      LD_TEXT+32(1) = SY-VLINE.
      LD_TEXT+53(1) = SY-VLINE.
      LD_TEXT+74(1) = SY-VLINE.
      LD_TEXT+33(10) = LD_KEY_F.
      LD_TEXT+54(10) = LD_KEY_S.
      LD_TEXT+1(2) = 'PR'.
      LD_TEXT+4(20) = TEXT-S06.

      IF SY-LANGU CA '1JM'.
        LEN = 27.
        PERFORM SPLIT_HANDLE_MOREINFO_LANGU
                  USING LD_TEXT+4(76)
                        LD_KEY_F
                        LD_KEY_S
                        LEN.
      ENDIF.

      MOREINFO-TEXT = LD_TEXT.
      MOREINFO-TCOLOR = 3.
      APPEND MOREINFO.
      MOREINFO-TEXT = SY-ULINE(75).
      MOREINFO-TLENGTH = 75.
      APPEND MOREINFO.
      MOREINFO-TCOLOR = 2.
      MOREINFO-TINTENSIV = 0.
      LOOP AT GT_PAR_FIELD_SPLIT.
        CLEAR LD_TEXT.
        LD_TEXT(1) = SY-VLINE.
        LD_TEXT+3(1) = SY-VLINE.
        LD_TEXT+32(1) = SY-VLINE.
        LD_TEXT+53(1) = SY-VLINE.
        LD_TEXT+74(1) = SY-VLINE.
        READ TABLE LT_TPAUM INTO LS_TPAUM
                    WITH KEY SPRAS = SY-LANGU
                             PARVW = GT_PAR_FIELD_SPLIT-PARVW
                             BINARY SEARCH.
        LD_TEXT+1(2) = LS_TPAUM-PABEZ.
        LD_TEXT+4(20) = GT_PAR_FIELD_SPLIT-FIELD(20).

        IF SY-LANGU CA '1JM'.
          PERFORM SPLIT_HANDLE_MOREINFO_LANGU
                              USING LD_TEXT+4(76)
                                    GT_PAR_FIELD_SPLIT-VALUE_F
                                    GT_PAR_FIELD_SPLIT-VALUE_S
                                    LEN.
        ELSE.
          LD_TEXT+33(20) = GT_PAR_FIELD_SPLIT-VALUE_F.
          LD_TEXT+54(20) = GT_PAR_FIELD_SPLIT-VALUE_S.
        ENDIF.

        MOREINFO-TEXT = LD_TEXT.
        IF MOREINFO-TINTENSIV = 0.
          MOREINFO-TINTENSIV = 1.
        ELSE.
          MOREINFO-TINTENSIV = 0.
        ENDIF.
        APPEND MOREINFO.
      ENDLOOP.
      MOREINFO-TEXT = SY-ULINE(75).
      APPEND MOREINFO.
      EXIT.
    ENDIF.
  ENDIF.

  IF P_NODE-NAME EQ 'HEADER_F_NODE'.
    READ TABLE GT_HEADER_FIELD_SPLIT INDEX 1.
    IF SY-SUBRC IS INITIAL.
      CLEAR MOREINFO.
      REFRESH MOREINFO.
      MOREINFO-TLENGTH = 75.
      MOREINFO-TEXT = SY-ULINE(75).
      APPEND MOREINFO.
      CLEAR LD_TEXT.
      LD_TEXT(1) = SY-VLINE.
      LD_TEXT+32(1) = SY-VLINE.
      LD_TEXT+53(1) = SY-VLINE.
      LD_TEXT+74(1) = SY-VLINE.
      LD_TEXT+33(10) = LD_KEY_F.
      LD_TEXT+54(10) = LD_KEY_S.
      LD_TEXT+1(30) = TEXT-S06.

      IF SY-LANGU CA '1JM'.
        LEN = 30.
        PERFORM SPLIT_HANDLE_MOREINFO_LANGU
                  USING LD_TEXT+1(79)
                        LD_KEY_F
                        LD_KEY_S
                        LEN.
      ENDIF.

      MOREINFO-TEXT = LD_TEXT.
      MOREINFO-TCOLOR = 3.
      APPEND MOREINFO.
      MOREINFO-TEXT = SY-ULINE(75).
      MOREINFO-TLENGTH = 75.
      APPEND MOREINFO.
      MOREINFO-TCOLOR = 2.
      MOREINFO-TINTENSIV = 0.
      LOOP AT GT_HEADER_FIELD_SPLIT.
        CLEAR LD_TEXT.
        LD_TEXT(1) = SY-VLINE.
        LD_TEXT+32(1) = SY-VLINE.
        LD_TEXT+53(1) = SY-VLINE.
        LD_TEXT+74(1) = SY-VLINE.
        LD_TEXT+1(30) = GT_HEADER_FIELD_SPLIT-FIELD(30).

        IF SY-LANGU CA '1JM'.
          PERFORM SPLIT_HANDLE_MOREINFO_LANGU
                              USING LD_TEXT+1(79)
                                    GT_HEADER_FIELD_SPLIT-VALUE_F
                                    GT_HEADER_FIELD_SPLIT-VALUE_S
                                    LEN.
        ELSE.
          LD_TEXT+33(20) = GT_HEADER_FIELD_SPLIT-VALUE_F.
          LD_TEXT+54(20) = GT_HEADER_FIELD_SPLIT-VALUE_S.
        ENDIF.

        MOREINFO-TEXT = LD_TEXT.
        IF MOREINFO-TINTENSIV = 0.
          MOREINFO-TINTENSIV = 1.
        ELSE.
          MOREINFO-TINTENSIV = 0.
        ENDIF.
        APPEND MOREINFO.
      ENDLOOP.
      MOREINFO-TEXT = SY-ULINE(75).
      APPEND MOREINFO.
      EXIT.
    ENDIF.
  ENDIF.

  IF P_NODE-NAME(8) EQ 'ITEMINFO'.
    DATA : POSNR_STRING(6).
    DATA : LD_POSNR LIKE VBRP-POSNR.
    POSNR_STRING = P_NODE-NAME+8(6).
    LD_POSNR = POSNR_STRING.
    CLEAR MOREINFO.
    REFRESH MOREINFO.
    MOREINFO-TLENGTH = 75.
    MOREINFO-TEXT = SY-ULINE(75).
    APPEND MOREINFO.
    CLEAR LD_TEXT.
    LD_TEXT(1) = SY-VLINE.
    LD_TEXT+32(1) = SY-VLINE.
    LD_TEXT+53(1) = SY-VLINE.
    LD_TEXT+74(1) = SY-VLINE.
    LD_TEXT+33(10) = LD_KEY_F.
    LD_TEXT+54(10) = LD_KEY_S.
    LD_TEXT+1(30) = TEXT-S06.
    MOREINFO-TEXT = LD_TEXT.
    MOREINFO-TCOLOR = 3.
    APPEND MOREINFO.
    MOREINFO-TEXT = SY-ULINE(75).
    MOREINFO-TLENGTH = 75.
    APPEND MOREINFO.
    MOREINFO-TCOLOR = 2.
    MOREINFO-TINTENSIV = 0.
    LOOP AT GT_ITEM_INFO WHERE VBELN_F = LD_KEY_F AND
                               POSNR_F = LD_POSNR.
      CLEAR LD_TEXT.
      LD_TEXT(1) = SY-VLINE.
      LD_TEXT+32(1) = SY-VLINE.
      LD_TEXT+53(1) = SY-VLINE.
      LD_TEXT+74(1) = SY-VLINE.
      LD_TEXT+1(30) = GT_ITEM_INFO-FIELD(30).
      LD_TEXT+33(20) = GT_ITEM_INFO-VALUE_F.
      LD_TEXT+54(20) = GT_ITEM_INFO-VALUE_S.
      MOREINFO-TEXT = LD_TEXT.
      IF MOREINFO-TINTENSIV = 0.
        MOREINFO-TINTENSIV = 1.
      ELSE.
        MOREINFO-TINTENSIV = 0.
      ENDIF.
      APPEND MOREINFO.
    ENDLOOP.
    MOREINFO-TEXT = SY-ULINE(75).
    APPEND MOREINFO.
    EXIT.
  ENDIF.

ENDFORM.                               " HANDLE_MOREINFO
*&---------------------------------------------------------------------*
*&      Form  ITEM_NODE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ITEM_NODE_PREPARE.

  FIELD-SYMBOLS: <VALUE1>, <VALUE2>.
  DATA : LD_TABIX2CHAR(10).
  DATA : LD_NAME(20).
  DATA : LS_VBRP_F LIKE VBRP.
  DATA : LS_VBRP_S LIKE VBRP.
  CLEAR TREELIST.
  TREELIST-NAME = 'ITEM_NODE'.
  TREELIST-COLOR = 0.
  TREELIST-INTENSIV = '0'.
  TREELIST-TEXT = TEXT-S08.
  TREELIST-TLENGTH = STRLEN( TEXT-S08 ).
  TREELIST-TLEVEL = 2.
  TREELIST-TCOLOR = 0.
  TREELIST-TINTENSIV = '0'.
  TREELIST-TEXT1 = ''.
  TREELIST-TLENGTH1 = 30.
  TREELIST-TCOLOR1 = 0.
  TREELIST-TINTENSIV1 = '0'.
  APPEND TREELIST.

  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            LANGU               = SY-LANGU
            TABNAME             = 'VBRP'
       TABLES
            NAMETAB             = GT_ITEM_NAMETAB
       EXCEPTIONS
            INTERNAL_ERROR      = 01
            NO_TEXTS_FOUND      = 02
            TABLE_HAS_NO_FIELDS = 03
            TABLE_NOT_ACTIV     = 04.

  DATA : BEGIN OF LT_VBRP_COMP OCCURS 5,
           VBELN_F LIKE VBRP-VBELN,
           VBELN_S LIKE VBRP-VBELN,
           POSNR   LIKE VBRP-POSNR.
  DATA : END OF LT_VBRP_COMP.

  DATA : DIFFERENT_ITEMS.

  LOOP AT GT_VBRP_F.
    READ TABLE GT_VBRP_S WITH KEY VBELN = LD_KEY_S
                                  POSNR = GT_VBRP_F-POSNR.
    IF SY-SUBRC IS INITIAL.
      MOVE GT_VBRP_F-VBELN TO LT_VBRP_COMP-VBELN_F.
      MOVE GT_VBRP_S-VBELN TO LT_VBRP_COMP-VBELN_S.
      MOVE GT_VBRP_F-POSNR TO LT_VBRP_COMP-POSNR.
      APPEND LT_VBRP_COMP.
    ELSE.
      DIFFERENT_ITEMS = 'X'.
    ENDIF.
  ENDLOOP.

  IF NOT DIFFERENT_ITEMS IS INITIAL.
    CLEAR TREELIST.
    TREELIST-NAME = 'DIFF_ITEMS'.
    TREELIST-COLOR = 0.
    TREELIST-INTENSIV = '0'.
    TREELIST-TEXT = TEXT-S10.
    TREELIST-TLENGTH = STRLEN( TEXT-S10 ).
    TREELIST-TLEVEL = 3.
    TREELIST-TCOLOR = 0.
    TREELIST-TINTENSIV = '0'.
    TREELIST-TEXT1 = ''.
    TREELIST-TLENGTH1 = 30.
    TREELIST-TCOLOR1 = 0.
    TREELIST-TINTENSIV1 = '0'.
    APPEND TREELIST.
  ENDIF.



  LOOP AT LT_VBRP_COMP.
    READ TABLE GT_VBRP_F WITH KEY VBELN = LT_VBRP_COMP-VBELN_F
                                  POSNR = LT_VBRP_COMP-POSNR.
    READ TABLE GT_VBRP_S WITH KEY VBELN = LT_VBRP_COMP-VBELN_S
                                  POSNR = LT_VBRP_COMP-POSNR.
    IF SY-SUBRC IS INITIAL.
      LS_VBRP_F = GT_VBRP_F.
      LS_VBRP_S = GT_VBRP_S.
      SY-SUBRC = 0.
      WHILE SY-SUBRC = 0.
        ASSIGN COMPONENT SY-INDEX OF STRUCTURE LS_VBRP_F TO <VALUE1>.
        CHECK : SY-SUBRC = 0.
        ASSIGN COMPONENT SY-INDEX OF STRUCTURE LS_VBRP_S TO <VALUE2>.
        IF <VALUE1> NE <VALUE2>.
          READ TABLE GT_ITEM_NAMETAB INDEX SY-INDEX.
          CHECK : SY-SUBRC = 0.
          GT_ITEM_INFO-VBELN_F  = LS_VBRP_F-VBELN.
          GT_ITEM_INFO-POSNR_F  = LS_VBRP_F-POSNR.
          GT_ITEM_INFO-VBELN_S  = LS_VBRP_S-VBELN.
          GT_ITEM_INFO-POSNR_S  = LS_VBRP_S-POSNR.
          GT_ITEM_INFO-FIELD    = GT_ITEM_NAMETAB-FIELDTEXT.
          GT_ITEM_INFO-VALUE_F  = <VALUE1>.
          GT_ITEM_INFO-VALUE_S  = <VALUE2>.
          APPEND GT_ITEM_INFO.
        ENDIF.
      ENDWHILE.
    ENDIF.
  ENDLOOP.

  READ TABLE GT_ITEM_INFO INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATA : OLD_POSNR LIKE VBRP-POSNR.
    DATA : POSNR_STRING(6).
    LOOP AT GT_ITEM_INFO.
      IF GT_ITEM_INFO-POSNR_F NE OLD_POSNR.
        OLD_POSNR = GT_ITEM_INFO-POSNR_F.
        CLEAR TREELIST.
        CONCATENATE 'ITEMINFO' OLD_POSNR INTO TREELIST-NAME.
        TREELIST-COLOR = 0.
        TREELIST-INTENSIV = '0'.
        POSNR_STRING = OLD_POSNR.
        SHIFT POSNR_STRING LEFT  DELETING LEADING  '0'.
        CONCATENATE TEXT-S09 POSNR_STRING INTO TREELIST-TEXT
                    SEPARATED BY SPACE.
        TREELIST-TLENGTH = STRLEN( TREELIST-TEXT ).
        TREELIST-TLEVEL = 3.
        TREELIST-TCOLOR = 0.
        TREELIST-TINTENSIV = '0'.
        TREELIST-TEXT1 = ''.
        TREELIST-TLENGTH1 = 30.
        TREELIST-TCOLOR1 = 0.
        TREELIST-TINTENSIV1 = '0'.
        TREELIST-MOREINFO = 'X'.
        APPEND TREELIST.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " ITEM_NODE_PREPARE

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      SPLIT_HANDLE_MOREINFO_LANGU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM SPLIT_HANDLE_MOREINFO_LANGU
                            USING LD_TEXT FS1 FS2 LEN.
  DATA : lentxt type i,
         foffs  TYPE sycolno,
         ld_text1(80) type c.

  TRY.
      CALL METHOD
        cl_abap_list_utilities=>replace_into_display_layout
        EXPORTING
          field          = ld_text(20)
          display_offset = 1
          display_length = 25
        CHANGING
          display_data   = ld_text1.

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
  ld_text1+foffs(1) = sy-vline.
  foffs = foffs + 1.
  ld_text1+foffs(20) = fs1.
  foffs = foffs + 20.
  ld_text1+foffs(1) = sy-vline.
  foffs = foffs + 1.
  ld_text1+foffs(20) = fs2.
  foffs = foffs + 21.
  ld_text1+foffs(1) = sy-vline.
  ld_text = ld_text1.


ENDFORM. "SPLIT_HANDLE_MOREINFO_LANGU
