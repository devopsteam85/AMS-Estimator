class ZCL_PROJECT_ESTIMATOR_DB definition
  public
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_DB .
  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .

  methods CONSTRUCTOR .
protected section.

  data G_UPDATE_FLAG type FLAG .

  methods UPDATE .
private section.
ENDCLASS.



CLASS ZCL_PROJECT_ESTIMATOR_DB IMPLEMENTATION.


  method CONSTRUCTOR.
  endmethod.


  METHOD update.
    DATA(ls_record) = zif_project_estimator_db~read_key_data( ).
    MODIFY zprojest FROM ls_record.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ELSE.
*     Handle exception
    ENDIF.
  ENDMETHOD.


  METHOD zif_project_estimator_db~get_key.
    rv_key = zif_project_estimator_attr~gs_project_key.
  ENDMETHOD.


  METHOD zif_project_estimator_db~load_project.
    IF zif_project_estimator_attr~g_project IS INITIAL.
      DATA: ls_textid      TYPE scx_t100key.
      ls_textid-msgid = '00'. ls_textid-msgno = '000'.
      ls_textid-attr1 = 'Please use ZCL_PROJECT_ESTIMATOR_FACTORY to create instance'.

      RAISE EXCEPTION TYPE zcx_project_estimator
        EXPORTING
          textid = ls_textid
          MSGTY  = 'E'.
    ENDIF.
    SELECT * FROM zprojest INTO TABLE zif_project_estimator_attr~gt_project_ins
   WHERE project = zif_project_estimator_attr~g_project.
  ENDMETHOD.


  METHOD zif_project_estimator_db~read_key_data.
    DATA:  ls_textid     TYPE scx_t100key.
    TRY.
        READ TABLE zif_project_estimator_attr~gt_project_ins INTO rv_key_record
             WITH KEY project = zif_project_estimator_attr~gs_project_key-project
                         type = zif_project_estimator_attr~gs_project_key-type
                        value = zif_project_estimator_attr~gs_project_key-value.

        IF sy-subrc NE 0.
*         Raise exception
          ls_textid-msgid = '00'. ls_textid-msgno = '000'.
          ls_textid-attr1 = 'Record not found for the key'.

          RAISE exception TYPE zcx_project_estimator
            EXPORTING
              textid = ls_textid
              msgty  = 'E'.

        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_project_estimator_db~set_key.
    zif_project_estimator_attr~gs_project_key = iv_key.
  ENDMETHOD.


  METHOD zif_project_estimator_db~update_db.
    IF g_update_flag = abap_true.
      update( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_project_estimator_db~update_key_data.
    FIELD-SYMBOLS <lfs_record> TYPE zprojest.
    READ TABLE zif_project_estimator_attr~gt_project_ins ASSIGNING <lfs_record>
    WITH KEY project = iv_key_record-project
              type   = iv_key_record-type
              value  = iv_key_record-value.
    IF <lfs_record> IS ASSIGNED.
      <lfs_record>-data = iv_key_record-data.
    ELSE.
      APPEND INITIAL LINE TO zif_project_estimator_attr~gt_project_ins ASSIGNING <lfs_record>.
      <lfs_record> = iv_key_record.
    ENDIF.
    g_update_flag = abap_true.
  ENDMETHOD.
ENDCLASS.
