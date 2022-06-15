REPORT ztest.

PARAMETERS: p_file   TYPE localfile,
            p_colnum TYPE p,
            p_rownum TYPE p.





AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'PATH'
    IMPORTING
      file_name     = p_file.



START-OF-SELECTION.
  DATA(lo_excel_uploader) = NEW zcl_excel_uploader( iv_filename           = p_file
                                                    iv_last_column_number = 100
                                                    iv_number_of_rows     = 100 ).
*                                                  iv_first_data_row     = 4
*                                                  iv_names_row          = 1
*                                                  iv_dataelements_row   = 2 ).

  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE,
                 <ls_data> TYPE any.

  ASSIGN lo_excel_uploader->mt_data->* TO <lt_data>.

*DATA(lv_field_key) = 'ID'.
*READ TABLE <lt_data> ASSIGNING <ls_data> WITH KEY (lv_field_key) = '1'.
*
*LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
*  ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_id>).
*  ...
*ENDLOOP.

  lo_excel_uploader->display( ).
