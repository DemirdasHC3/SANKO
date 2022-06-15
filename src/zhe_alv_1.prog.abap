*&---------------------------------------------------------------------*
*& Report ZHE_ALV_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHE_ALV_1.


*START-OF-SELECTION .



*CALL SCREEN 100 .

DATA remainder TYPE i.
DO 20 TIMES.
  remainder = sy-index MOD 2.
  IF remainder <> 0.
    CONTINUE.
  ENDIF.
  cl_demo_output=>write_text( |{ sy-index }| ).
ENDDO.
cl_demo_output=>display( ).
