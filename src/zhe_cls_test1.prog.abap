*&---------------------------------------------------------------------*
*& Report ZHE_CLS_TEST1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHE_CLS_TEST1.


data : lv_num1 type i value 6,
       lv_num2 type i value 20,
       lv_result type i .



* include zhe_cls_test1_itop .

PARAMETERS : p1 type i,
             p2 type i.

lv_num1 = p1.
lv_num2 = p2.

include zhe_cls_test1_itop .


START-OF-SELECTION .


create OBJECT calc .

call method calc->add( exporting
                        num1 = lv_num1
                        num2 = lv_num2
                       importing
                        result = lv_result ) .
write lv_result .
clear lv_result .
write lv_result .

call method calc->abs( exporting
                        num1 = lv_num1
                        num2 = lv_num2
                       importing
                        result = lv_result ) .
write lv_result .
