CLASS zcl_employee_infotyp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_employee_infotyp.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ct_empl_stage TYPE STANDARD TABLE OF zvw_empl_stage,
          ct_ifassign   TYPE t777d_tab.
ENDCLASS.



CLASS zcl_employee_infotyp IMPLEMENTATION.

  METHOD zif_employee_infotyp~build_proposed_values.
    "this method will build the proposed values for calling FM
    "HR_MAINTAIN_MASTERDATA
    DATA: lt_dfies TYPE STANDARD TABLE OF dfies,
          ls_pprop TYPE pprop.

    FIELD-SYMBOLS: <fs_fval> TYPE pprop-fval.

    CLEAR: ls_pprop.
    REFRESH: lt_dfies[].

    READ TABLE ct_ifassign ASSIGNING FIELD-SYMBOL(<fs_assign>)
    WITH KEY infty = i_infotyp.
    IF sy-subrc EQ 0.
      "Get the structure information
      CALL FUNCTION 'DDIF_NAMETAB_GET'
        EXPORTING
          tabname   = <fs_assign>-ppnnn
        TABLES
          dfies_tab = lt_dfies
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc EQ 0.
        "loop at the staging table for the personnel number and infotyp
        LOOP AT ct_empl_stage ASSIGNING FIELD-SYMBOL(<fs_empl_stage>)
        WHERE pernr = i_persno
          AND infotyp = i_infotyp.
          "loop at field by field
          LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<fs_dfies>).
            "Build the zvw_empl_stage field
            CONCATENATE <fs_dfies>-tabname
                        <fs_dfies>-fieldname
                        INTO DATA(lv_stage_field)
                        SEPARATED BY '_'.

            ASSIGN COMPONENT lv_stage_field OF STRUCTURE <fs_empl_stage> TO <fs_fval>.
            "Check if assigned, as the field may not exist in the view
            IF <fs_fval> IS ASSIGNED.

              "build the fieldname
              CONCATENATE <fs_dfies>-tabname
                          <fs_dfies>-fieldname
                          INTO ls_pprop-fname
                          SEPARATED BY '-'.

              "Set the field value
              ls_pprop-fval = <fs_fval>.
              "set the infotype
              ls_pprop-infty = i_infotyp.

              "add the data to the returning table
              APPEND ls_pprop TO r_pprop[].
              CLEAR: ls_pprop.

            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_employee_infotyp~get_empl_stage.
    "getter for employee stage
    r_empl_stage_tab[] = ct_empl_stage[].
  ENDMETHOD.

  METHOD zif_employee_infotyp~get_infotyp_assignment.
    "get the infotype data
    SELECT * FROM t777d
    INTO TABLE r_ifassign[]
    WHERE infty = '0000'
    OR infty = '0001'
    OR infty = '0002'
    OR infty = '0006'
    OR infty = '0008'
    OR infty = '0016'
    OR infty = '0041'
    OR infty = '0105'.
    IF sy-subrc EQ 0.
      ct_ifassign[] = r_ifassign[].
    ENDIF.
  ENDMETHOD.

  METHOD zif_employee_infotyp~post_infotyp.
    DATA: ls_bapireturn  TYPE bapireturn,
          ls_bapireturn1 TYPE bapireturn1,
          ls_hrreturn    TYPE hrhrmm_msg.

    CLEAR: ls_bapireturn,
           ls_bapireturn1,
           ls_hrreturn.

    e_code = 1.
    "call the maintenance of infotyp
    CALL FUNCTION 'HR_MAINTAIN_MASTERDATA'
      EXPORTING
        pernr           = i_persno
        actio           = 'INS'
      IMPORTING
        return          = ls_bapireturn
        return1         = ls_bapireturn1
        hr_return       = ls_hrreturn
      TABLES
        proposed_values = i_pprop.

    "Set the return code
    IF ls_bapireturn-type NE 'A'
    AND ls_bapireturn-type NE 'E'
    AND ls_bapireturn1-type NE 'A'
    AND ls_bapireturn1-type NE 'E'
    AND ls_hrreturn-msgty NE 'A'
    AND ls_hrreturn-msgty NE 'E'.
      e_code = 0.
    ENDIF.
    e_return = ls_bapireturn.
  ENDMETHOD.

  METHOD zif_employee_infotyp~set_empl_stage.
    ct_empl_stage[] = i_emplstage_tab[].
  ENDMETHOD.

ENDCLASS.
