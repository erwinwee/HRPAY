INTERFACE zif_employee_infotyp
  PUBLIC .

  METHODS get_infotyp_assignment
    RETURNING VALUE(r_ifassign) TYPE t777d_tab.

  METHODS build_proposed_values
    IMPORTING !i_infotyp     TYPE infotyp
              !i_persno      TYPE persno
    RETURNING VALUE(r_pprop) TYPE hrpayfr_s_pprop.

  METHODS set_empl_stage
    IMPORTING !i_emplstage_tab TYPE ztt_empl_stage.

  METHODS get_empl_stage
    RETURNING VALUE(r_empl_stage_tab) TYPE ztt_empl_stage.

  METHODS post_infotyp
    IMPORTING !i_persno     TYPE persno
              !i_pprop      TYPE hrpayfr_s_pprop
    exporting
              !e_return type bapireturn
              !e_code TYPE sysubrc.
ENDINTERFACE.
