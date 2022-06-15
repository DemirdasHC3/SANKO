PROGRAM TEST.
DATA: OK_CODE LIKE SY-UCOMM,
      GT_SFLIGHT TYPE TABLE OF SFLIGHT,
      G_CONTAINER TYPE SCRFNAME VALUE 'BCALV_GRID_DEMO_0100_CONT1',
      GRID1  TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*

SELECT * FROM SFLIGHT INTO TABLE GT_SFLIGHT.

CALL SCREEN 100.


*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'MAIN100'.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
           EXPORTING CONTAINER_NAME = G_CONTAINER.
    CREATE OBJECT GRID1
           EXPORTING I_PARENT = G_CUSTOM_CONTAINER.
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'SFLIGHT'
         CHANGING  IT_OUTTAB        = GT_SFLIGHT.
  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE PAI INPUT.
*   to react on oi_custom_events:
    call method cl_gui_cfw=>dispatch.
  CASE OK_CODE.
    WHEN 'EXIT'.
      PERFORM EXIT_PROGRAM.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR OK_CODE.
ENDMODULE.
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM EXIT_PROGRAM.
*  CALL METHOD G_CUSTOM_CONTAINER->FREE.
*  CALL METHOD CL_GUI_CFW=>FLUSH.
  LEAVE PROGRAM.
ENDFORM.
