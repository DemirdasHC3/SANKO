*&---------------------------------------------------------------------*
*& Include          ZXUSRU01
*&---------------------------------------------------------------------*
if sy-uname EQ 'TEST_HE' .

  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      TITEL         = 'Welcome!'
      TXT1          = ''
      TXT2          ='------------------'
*     TXT3          = ' '
*     TXT4          = ' '
            .

  endif .
