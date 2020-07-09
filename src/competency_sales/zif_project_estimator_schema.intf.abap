interface ZIF_PROJECT_ESTIMATOR_SCHEMA
  public .


  types:
    BEGIN OF  ty_child,
      child TYPE REF TO zif_project_estimator_schema,
    END OF ty_child .
  types:
    tt_children TYPE STANDARD TABLE OF ty_child .

  data PARENT type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA .
  data:
    children TYPE STANDARD TABLE OF ty_child .
  data KEY type STRING .
  data SCHEMA_NAME type CHAR20 .

  methods GET_CHILDREN
    exporting
      value(RT_CHILDREN) type TT_CHILDREN .
  methods GET_PARENT
    returning
      value(RO_PARENT) type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA .
  methods SET_CHILD
    importing
      !IO_CHILD type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA .
  methods SET_PARENT
    importing
      !IO_PARENT type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA .
  methods INVOKE_SCHEMA .
  methods RUN_SCHEMA .
  methods COMPUTE .
  methods GET_KEY_DESCRIPTOR
    returning
      value(RS_COMPONENT) type CL_ABAP_STRUCTDESCR=>COMPONENT .
  methods GET_SCHEMA_DESCRIPTOR
    changing
      value(CT_COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  methods GET_SCHEMA_OUTPUT
    importing
      !IT_SCHEMA_OUTPUT type ref to DATA
    exporting
      !ET_SCHEMA_OUTPUT type STANDARD TABLE .
endinterface.
