INTERFACE zif_hrpay_employee_staging
  PUBLIC .

  METHODS create_records
    IMPORTING !i_infotype  TYPE infotyp
              !i_msg       type zdt_empl_msg
              !i_data      type ref to data
    RETURNING
              VALUE(r_code) TYPE sysubrc.

  METHODS read_records
    IMPORTING !i_infotype   TYPE infotyp
              !i_persno     TYPE persno
              !i_actions    TYPE massn
              !i_stat       TYPE zdel_procstat
    RETURNING
              VALUE(r_data) TYPE ztt_empl_stage.

  METHODS update_records
    IMPORTING !i_infotype   TYPE infotyp
              !i_data       TYPE REF TO DATA
              !i_persno     TYPE persno
    RETURNING
              VALUE(r_code) TYPE sysubrc.

  METHODS delete_records
    IMPORTING !i_guid type sxmsmguid
              !i_infotype   TYPE infotyp
    RETURNING
              VALUE(r_code) TYPE sysubrc.

ENDINTERFACE.
