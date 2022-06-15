FUNCTION Z_SD_VMCFA_CUSTOMER_FUNCTION.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      C_VMCFA STRUCTURE  VMCFAO
*"--------------------------------------------------------------------
  CALL CUSTOMER-FUNCTION '013'
       TABLES
            C_VMCFA    = C_VMCFA
       EXCEPTIONS
            OTHERS     = 1.
ENDFUNCTION.






