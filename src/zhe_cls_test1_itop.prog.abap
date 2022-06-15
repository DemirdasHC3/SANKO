*&---------------------------------------------------------------------*
*& Include          ZHE_CLS_TEST1_ITOP
*&---------------------------------------------------------------------*



class dortislem DEFINITION .

PUBLIC SECTION .
METHODS :
  add importing value(num1) type i
                value(num2) type i
      exporting value(result) type i ,

  abs importing value(num1) type i
                value(num2) type i
      exporting value(result) type i,
   div importing value(num1) type i
                value(num2) type i
      exporting value(result) type i,
   mul  importing value(num1) type i
                value(num2) type i
      exporting value(result) type i .

ENDCLASS .


CLASS dortislem IMPLEMENTATION .
  method add.
  result = num1 + num2 .
  ENDMETHOD .

  method abs .
    result = num2 - num1 .
  ENDMETHOD .

  method div .
    result = num1 / num2 .
  ENDMETHOD .

  method mul .
    result = num1 * num2 .
  ENDMETHOD .


ENDCLASS .

data calc type ref to dortislem .
