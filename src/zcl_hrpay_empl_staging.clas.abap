CLASS zcl_hrpay_empl_staging DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_hrpay_employee_staging.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_hrpay_empl_staging IMPLEMENTATION.
  METHOD zif_hrpay_employee_staging~create_records.
    r_code = 1. "start with an error

    FIELD-SYMBOLS <fs_tdata> TYPE STANDARD TABLE.

    ASSIGN i_data->* TO <fs_tdata>.
    IF sy-subrc EQ 0.

      CASE i_infotype.
        WHEN '0000'.
          INSERT zdt_infty0000 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0001'.
          INSERT zdt_infty0001 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0002'.
          INSERT zdt_infty0002 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0006'.
          INSERT zdt_infty0006 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0008'.
          INSERT zdt_infty0008 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0016'.
          INSERT zdt_infty0016 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0041'.
          INSERT zdt_infty0041 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0105'.
          INSERT zdt_infty0105 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
      ENDCASE.

      INSERT zdt_empl_msg FROM i_msg.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
      r_code = sy-subrc.

    ENDIF.
  ENDMETHOD.

  METHOD zif_hrpay_employee_staging~delete_records.
    r_code = 1.
    CASE i_infotype.
      WHEN '0000'.
        DELETE FROM zdt_infty0000 WHERE mssgguid = i_guid.
      WHEN '0001'.
        DELETE FROM zdt_infty0001 WHERE mssgguid = i_guid.
      WHEN '0002'.
        DELETE FROM zdt_infty0002 WHERE mssgguid = i_guid.
      WHEN '0006'.
        DELETE FROM zdt_infty0006 WHERE mssgguid = i_guid.
      WHEN '0008'.
        DELETE FROM zdt_infty0008 WHERE mssgguid = i_guid.
      WHEN '0016'.
        DELETE FROM zdt_infty0016 WHERE mssgguid = i_guid.
      WHEN '0041'.
        DELETE FROM zdt_infty0041 WHERE mssgguid = i_guid.
      WHEN '0105'.
        DELETE FROM zdt_infty0105 WHERE mssgguid = i_guid.
    ENDCASE.

    DELETE FROM zdt_empl_msg WHERE mssgguid = i_guid AND infotyp = i_infotype.
    COMMIT WORK AND WAIT.

    r_code = sy-subrc.
  ENDMETHOD.

  METHOD zif_hrpay_employee_staging~read_records.
    SELECT * FROM zvw_empl_stage
    INTO TABLE @r_data[]
    WHERE   infotyp = @i_infotype
      AND   p0000_massn = @i_actions.
  ENDMETHOD.

  METHOD zif_hrpay_employee_staging~update_records.
    r_code = 1. "start with an error

    FIELD-SYMBOLS <fs_tdata> TYPE STANDARD TABLE.

    ASSIGN i_data->* TO <fs_tdata>.
    IF sy-subrc EQ 0.

      CASE i_infotype.
        WHEN '0000'.
          UPDATE zdt_infty0000 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0001'.
          UPDATE zdt_infty0001 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0002'.
          UPDATE zdt_infty0002 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0006'.
          UPDATE zdt_infty0006 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0008'.
          UPDATE zdt_infty0008 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0016'.
          UPDATE zdt_infty0016 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0041'.
          UPDATE zdt_infty0041 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
        WHEN '0105'.
          UPDATE zdt_infty0105 FROM TABLE <fs_tdata>.
          r_code = sy-subrc.
          CHECK r_code EQ 0.
      ENDCASE.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
      r_code = sy-subrc.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
