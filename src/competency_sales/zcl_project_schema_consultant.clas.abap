class ZCL_PROJECT_SCHEMA_CONSULTANT definition
  public
  final
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_SCHEMA .
  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .

  methods CONSTRUCTOR
    importing
      !IV_KEY type STRING
      !IO_PARENT type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA
      !IV_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TY_CONSULTANT_SPREAD .
protected section.
private section.

  data KEY type CHAR20 .
  data G_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TY_CONSULTANT_SPREAD .
  data G_MANDAYS(5) type p decimals 3 .
ENDCLASS.



CLASS ZCL_PROJECT_SCHEMA_CONSULTANT IMPLEMENTATION.


  METHOD constructor.
    key = iv_key.
    zif_project_estimator_schema~key = iv_key.
    zif_project_estimator_schema~parent ?= io_parent.
    zif_project_estimator_schema~schema_name = 'CONSULTANT'.

    g_spread = iv_spread.

  ENDMETHOD.


  METHOD zif_project_estimator_schema~compute.

    DATA: lo_schema TYPE REF TO zcl_project_schema_level.

    lo_schema ?= zif_project_estimator_schema~parent.
*   FTE = % of consultant allocation to a Level * number of tickets for that level / productivity of the bann(consultant)
    G_MANDAYS = ( g_spread-spread / 100 ) * ( lo_schema->gv_spread_count / g_spread-productivity ).

  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_key_descriptor.
    rs_component-name = zif_project_estimator_schema~schema_name.
    rs_component-type ?= cl_abap_datadescr=>describe_by_data( key ).
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_parent.
    ro_parent ?= zif_project_estimator_schema~parent.
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_schema_descriptor.

    DATA: ls_component TYPE cl_abap_structdescr=>component.
    APPEND zif_project_estimator_schema~get_key_descriptor( ) TO ct_components.

    READ TABLE zif_project_estimator_schema~children INTO DATA(ls_child) INDEX 1.
    IF sy-subrc EQ 0.
      ls_child-child->get_schema_descriptor( CHANGING ct_components = ct_components ).
    ELSE.
      ls_component-name = 'MANDAYS'.
      ls_component-type ?= cl_abap_datadescr=>describe_by_data( g_mandays ).
      APPEND ls_component TO ct_components.
    ENDIF.
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_schema_output.
    ASSERT it_schema_output IS BOUND.

    FIELD-SYMBOLS: <lt_schema_output>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lfs_schema_out> TYPE any,
                   <lfs_value>      TYPE any.

    ASSIGN it_schema_output->* TO <lt_schema_output>.



    READ TABLE <lt_schema_output> ASSIGNING <lfs_schema_out> INDEX lines( <lt_schema_output> ).
    ASSIGN COMPONENT zif_project_estimator_schema~schema_name OF STRUCTURE <lfs_schema_out> TO <lfs_value>.

    <lfs_value> = key.

    ASSIGN COMPONENT 'MANDAYS' OF STRUCTURE <lfs_schema_out> TO <lfs_value>.

    <lfs_value> = g_mandays.


  ENDMETHOD.


  METHOD zif_project_estimator_schema~run_schema.
    zif_project_estimator_schema~compute( ).
  ENDMETHOD.


  METHOD zif_project_estimator_schema~set_child.
    DATA: ls_child TYPE zif_project_estimator_schema~ty_child.
    ls_child-child ?= io_child.
    APPEND ls_child TO zif_project_estimator_schema~children.
  ENDMETHOD.
ENDCLASS.
