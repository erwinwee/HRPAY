*"* use this source file for your ABAP unit test classes
CLASS lcl_hrpay_empl_staging_cut DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION LONG.

  PRIVATE SECTION.
    CONSTANTS: lc_guid  TYPE sxmsmguid VALUE 'abc456def012ghi678jkl2345mnopx01',
               lc_pernr TYPE persno VALUE '00000003'.

    DATA: cut TYPE REF TO zcl_hrpay_empl_staging.

    METHODS setup.

    METHODS build_messaging
      IMPORTING i_infotyp     TYPE infotyp
      RETURNING
                VALUE(r_data) TYPE zdt_empl_msg.

    METHODS build_infty0000
      IMPORTING
        i_action      TYPE massn
      RETURNING
        VALUE(r_data) TYPE REF TO data.

    METHODS build_infty0001
      RETURNING
        VALUE(r_data) TYPE REF TO data.

    METHODS build_infty0002
      RETURNING
        VALUE(r_data) TYPE REF TO data.

    METHODS assert_read
      IMPORTING i_infotyp TYPE infotyp
                i_action  TYPE massn.

    METHODS assert_write
      IMPORTING i_action  TYPE massn
                i_infotyp TYPE infotyp.

    METHODS assert_delete
      IMPORTING i_infotyp TYPE infotyp.

    METHODS build_infty0006
      RETURNING VALUE(r_data) TYPE REF TO data.

    METHODS build_infty0008
      RETURNING VALUE(r_data) TYPE REF TO data.

    METHODS build_infty0016
      RETURNING VALUE(r_data) TYPE REF TO data.

    METHODS build_infty0041
      RETURNING VALUE(r_data) TYPE REF TO data.

    METHODS build_infty0105
      RETURNING VALUE(r_data) TYPE REF TO data.

    METHODS test_read_staging FOR TESTING RAISING cx_static_check.

    METHODS test_create_staging FOR TESTING RAISING cx_static_check.

    METHODS test_delete_staging FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS lcl_hrpay_empl_staging_cut IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_hrpay_empl_staging(  ).
  ENDMETHOD.

  METHOD test_create_staging.

    me->assert_write(  EXPORTING i_action = 'NH'
                                 i_infotyp = '0000' ).
  ENDMETHOD.

  METHOD test_read_staging.
    "Test reading of new hires
    me->assert_read( EXPORTING i_infotyp = '0000'
                               i_action = 'NW' ).

  ENDMETHOD.

  METHOD test_delete_staging.
    me->assert_delete( i_infotyp = '0000' ).
  ENDMETHOD.


  METHOD assert_read.
    DATA(lt_empl_stage) =  cut->zif_hrpay_employee_staging~read_records( EXPORTING i_infotype = i_infotyp
                                                        i_persno   = 0
                                                        i_actions  = i_action
                                                        i_stat     = '1' ).

    cl_abap_unit_assert=>assert_not_initial( act = lt_empl_stage ).

    IF lt_empl_stage[] IS NOT INITIAL.
      "Check each entry matches the correct action
      LOOP AT lt_empl_stage ASSIGNING FIELD-SYMBOL(<fs_empl_stage>).
        cl_abap_unit_assert=>assert_equals( EXPORTING act = <fs_empl_stage>-p0000_massn
                                                      exp = i_action ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD assert_write.

    DATA: lr_data      TYPE REF TO data,
          ls_messaging TYPE zdt_empl_msg.

    "build the messaging
    ls_messaging = me->build_messaging(  i_infotyp = i_infotyp ).

    CASE i_infotyp.
      WHEN '0000'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0000.
        "Build a new hire
        lr_data = me->build_infty0000( i_action = i_action ).
      WHEN '0001'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0001.
        "Build a new hire
        lr_data = me->build_infty0001( ).
      WHEN '0002'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0002.
        "Build a new hire
        lr_data = me->build_infty0002( ).
        "Build a new hire
      WHEN '0006'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0006.
        "Build a new hire
        lr_data = me->build_infty0006( ).
      WHEN '0008'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0008.
        "Build a new hire
        lr_data = me->build_infty0008( ).
      WHEN '0016'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0016.
        "Build a new hire
        lr_data = me->build_infty0016( ).
      WHEN '0041'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0041.
        "Build a new hire
        lr_data = me->build_infty0041( ).
      WHEN '0105'.
        CREATE DATA lr_data TYPE STANDARD TABLE OF zdt_infty0105.
        "Build a new hire
        lr_data = me->build_infty0105( ).
    ENDCASE.

    DATA(lv_rc) = cut->zif_hrpay_employee_staging~create_records(  EXPORTING i_infotype  = i_infotyp
                                                                             i_msg = ls_messaging
                                                                             i_data = lr_data ).

    cl_abap_unit_assert=>assert_subrc(  EXPORTING act = lv_rc
                                        exp = 0 ).

  ENDMETHOD.

  METHOD build_infty0000.
    "Build an into type 0000
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_empl_msg TYPE zdt_infty0000.

    ls_empl_msg-infotyp = '0000'.
    ls_empl_msg-client = sy-mandt.
    ls_empl_msg-mssgguid = lc_guid.
    ls_empl_msg-pernr = lc_pernr.
    ls_empl_msg-begda  = sy-datum.
    ls_empl_msg-endda  =  sy-datum.
    ls_empl_msg-massn  = i_action.
    ls_empl_msg-massg = ''.
    ls_empl_msg-stat2 = ''.
    ls_empl_msg-processed  = ''.

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0000.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_empl_msg INTO <fs_tdata> INDEX 1.
    ENDIF.

  ENDMETHOD.

  METHOD build_infty0001.
    "Build an into type 0001
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_infty TYPE zdt_infty0001.

    ls_infty-infotyp = '0001'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0001.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_infty INTO <fs_tdata> INDEX 1.
    ENDIF.

  ENDMETHOD.

  METHOD build_infty0002.
    "Build an into type 0002
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_infty TYPE zdt_infty0002.

    ls_infty-infotyp = '0002'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.
    ls_infty-nachn = 'Trump'.
    ls_infty-vorna = 'Melania'.
    ls_infty-inits = 'MT'.
    ls_infty-rufnm = 'Mel-T'.
    ls_infty-gbdat = '19700101'.
    ls_infty-sprsl = 'RU'.
    ls_infty-natio = 'RU'.
    ls_infty-gesch = '2'. "Female

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0002.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_infty INTO <fs_tdata> INDEX 1.
    ENDIF.

  ENDMETHOD.

  METHOD build_infty0006.
    "Build an into type 0006
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_infty TYPE zdt_infty0006.

    ls_infty-infotyp = '0006'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.
    ls_infty-anssa = 'Payroll'.
    ls_infty-stras = '80 Queen Street'.
    ls_infty-adr03 = 'Central'.
    ls_infty-ort01 = 'Auckland'.
    ls_infty-pstlz = '1021'.
    ls_infty-land1 = 'NZ'.
    ls_infty-natio = 'NZ'.

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0006.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_infty INTO <fs_tdata> INDEX 1.
    ENDIF.

  ENDMETHOD.

  METHOD build_infty0008.
    "Build an into type 0008
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_infty TYPE zdt_infty0008.

    ls_infty-infotyp = '0008'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0008.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_infty INTO <fs_tdata> INDEX 1.
    ENDIF.

  ENDMETHOD.

  METHOD build_messaging.
    "build the messaging

    r_data-infotyp = i_infotyp.
    r_data-client = sy-mandt.
    r_data-mssgguid = lc_guid.
    r_data-pernr = lc_pernr.

  ENDMETHOD.

  METHOD assert_delete.
    DATA(lv_rc) = cut->zif_hrpay_employee_staging~delete_records( EXPORTING i_infotype = i_infotyp
                                                                            i_guid = lc_guid ).

    cl_abap_unit_assert=>assert_subrc(  EXPORTING act = lv_rc
                                        exp = 0 ).
  ENDMETHOD.

  METHOD build_infty0016.
    "Build an into type 0016
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_infty TYPE zdt_infty0016.

    ls_infty-infotyp = '0016'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0016.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_infty INTO <fs_tdata> INDEX 1.
    ENDIF.
  ENDMETHOD.

  METHOD build_infty0041.
    "Build an into type 0041
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_infty TYPE zdt_infty0041.

    ls_infty-infotyp = '0041'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0041.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_infty INTO <fs_tdata> INDEX 1.
    ENDIF.
  ENDMETHOD.

  METHOD build_infty0105.
    "Build an into type 0105
    FIELD-SYMBOLS: <fs_tdata> TYPE STANDARD TABLE.

    DATA: ls_infty TYPE zdt_infty0105.

    ls_infty-infotyp = '0105'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.
    ls_infty-usrty = 'Mobile'.
    ls_infty-usrid = '02121212121'.

    CREATE DATA r_data TYPE STANDARD TABLE OF zdt_infty0105.
    IF sy-subrc EQ 0.
      ASSIGN r_data->* TO <fs_tdata>.
      INSERT ls_infty INTO <fs_tdata> INDEX 1.
      clear ls_infty.

    ls_infty-infotyp = '0105'.
    ls_infty-client = sy-mandt.
    ls_infty-mssgguid = lc_guid.
    ls_infty-pernr = lc_pernr.
    ls_infty-begda  = sy-datum.
    ls_infty-endda  =  sy-datum.
    ls_infty-processed  = ''.
    ls_infty-usrty = 'Email'.
    ls_infty-usrid = 'person@company-email.com'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
