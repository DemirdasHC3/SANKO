FUNCTION Z_SD_COLOR_LEGEND_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_LEGEND
*"--------------------------------------------------------------------

  GT_LEGEND[] = IT_LEGEND[].
  CALL SCREEN 0900
    STARTING AT 20 10
    ENDING AT 75 18.


ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Module  COLOR_LEGEND  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COLOR_LEGEND OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING
    AND RETURN TO SCREEN 0.
  PERFORM COLOR_LEGEND.
  LEAVE SCREEN.
ENDMODULE.                 " COLOR_LEGEND  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  COLOR_LEGEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLOR_LEGEND.
  NEW-PAGE
    NO-HEADING
    NO-TITLE.
  SET BLANK LINES ON.
  SET TITLEBAR 'LEG'.
  SET PF-STATUS 'LEGEND'.
  FORMAT RESET.
  LOOP AT GT_LEGEND.
    WRITE:
      /, GT_LEGEND-STATF AS ICON,
      (50) GT_LEGEND-TEXT.
  ENDLOOP.
* WRITE:
*   /, ICON_CREATE AS ICON,
*   (50) TEXT-CL1.
* WRITE:
*   /, ICON_OKAY AS ICON,
*   (50) TEXT-CL2.
* WRITE:
*   /, ICON_CANCEL AS ICON,
*   (50) TEXT-CL3.
* WRITE:
*   /, ICON_LED_GREEN AS ICON,
*   (50) TEXT-CL4.
* WRITE:
*   /, ICON_LED_RED AS ICON,
*   (50) TEXT-CL5.
ENDFORM.                    " COLOR_LEGEND

*&---------------------------------------------------------------------*
*&      Form  ITEM_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM ITEM_COLOR
  USING NOEDIT
        NODETYPE.
  IF NOEDIT = CX_TRUE.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ELSEIF NOEDIT = 'A'.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED ON.
  ELSEIF NODETYPE CO ' BGkamfoe'.
    FORMAT COLOR COL_KEY INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.


ENDFORM.                    " ITEM_COLOR
