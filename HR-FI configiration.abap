REPORT zhr_fi_configuration.

CLASS lcl_hr_fi_configuration DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      validate_employee_group IMPORTING iv_employee_group TYPE pa0001-pernr RETURNING VALUE(rv_validated) TYPE abap_bool,
      validate_payroll_area IMPORTING iv_payroll_area TYPE t549q-begda RETURNING VALUE(rv_validated) TYPE abap_bool,
      integrate_with_fi.
  PRIVATE SECTION.
    DATA:
      mv_employee_group TYPE pa0001-pernr,
      mv_payroll_area TYPE t549q-begda.
ENDCLASS.

CLASS lcl_hr_fi_configuration IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD validate_employee_group.
  ENDMETHOD.

  METHOD validate_payroll_area.
  ENDMETHOD.

  METHOD integrate_with_fi.
    DATA: lt_fi_data TYPE TABLE OF fi_data_structure,
          ls_fi_data TYPE fi_data_structure.

    ls_fi_data-field1 = 'Value1'.
    ls_fi_data-field2 = 'Value2'.
    APPEND ls_fi_data TO lt_fi_data.

    CALL FUNCTION 'BAPI_FI_CONFIGURATION_UPDATE'
      EXPORTING
        it_fi_data = lt_fi_data
      EXCEPTIONS
        others = 1.

    IF sy-subrc <> 0.
      WRITE 'Error occurred while updating FI data'.
    ELSE.
      WRITE 'FI data updated successfully'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

DATA: lo_hr_fi_config TYPE REF TO lcl_hr_fi_configuration,
      lv_employee_group TYPE pa0001-pernr,
      lv_payroll_area TYPE t549q-begda,
      lv_validated TYPE abap_bool.

PARAMETERS: p_employee_group TYPE pa0001-pernr,
            p_payroll_area TYPE t549q-begda.

START-OF-SELECTION.
  CREATE OBJECT lo_hr_fi_config.

  lv_employee_group = p_employee_group.
  lv_payroll_area = p_payroll_area.

  lv_validated = lo_hr_fi_config->validate_employee_group( iv_employee_group = lv_employee_group ).
  IF lv_validated = abap_true.
    lv_validated = lo_hr_fi_config->validate_payroll_area( iv_payroll_area = lv_payroll_area ).
    IF lv_validated = abap_true.
      lo_hr_fi_config->integrate_with_fi( ).
    ELSE.
      WRITE 'Invalid payroll area'.
    ENDIF.
  ELSE.
    WRITE 'Invalid employee group'.
  ENDIF.