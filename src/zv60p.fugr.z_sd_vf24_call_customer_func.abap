FUNCTION Z_SD_VF24_CALL_CUSTOMER_FUNC.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      C_VKDFIF STRUCTURE  VKDFIF
*"--------------------------------------------------------------------
  CALL CUSTOMER-FUNCTION '012'
       TABLES
            C_VKDFIF   = C_VKDFIF
       EXCEPTIONS
            OTHERS     = 1.
ENDFUNCTION.
