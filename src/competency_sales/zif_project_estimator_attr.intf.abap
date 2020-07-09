interface ZIF_PROJECT_ESTIMATOR_ATTR
  public .


  types:
    BEGIN OF ty_consultant_spread,
           module       TYPE char20,
           level        TYPE char20,
           consultant   TYPE char20,
           productivity TYPE char20,
           spread       TYPE numc3,
         END OF ty_consultant_spread .
  types:
    BEGIN OF ty_level_spread,
      priority TYPE char20,
      level    TYPE char20,
      spread   TYPE numc3,
    END OF ty_level_spread .
  types:
    BEGIN OF ty_modules ,
      module TYPE char20,
    END OF ty_modules .
  types:
    BEGIN OF ty_priority,
      priority TYPE char20,
    END OF ty_priority .
  types:
    BEGIN OF ty_schema,
      module TYPE char20,
      schema TYPE REF TO zif_project_estimator_schema,
    END OF ty_schema .
  types:
    BEGIN OF ty_dump,
      reported_date TYPE char20,
      reported_time TYPE char20,
      time_zone     TYPE char20,
      module        TYPE char20,
      priority      TYPE char20,
      complexity    TYPE char20,
      start_date    TYPE char20,
      start_time    TYPE char20,
      end_date      TYPE char20,
      end_time      TYPE char20,
    END OF ty_dump .
  types:
    tt_projest TYPE STANDARD TABLE OF zprojest .
  types:
    tt_schema   TYPE STANDARD TABLE OF ty_schema .
  types:
    tt_module   TYPE STANDARD TABLE OF ty_modules .
  types:
    tt_priority TYPE STANDARD TABLE OF ty_priority .
  types:
    tt_dump     TYPE STANDARD TABLE OF ty_dump .
  types:
    tt_level_spread TYPE STANDARD TABLE OF ty_level_spread .
  types:
    tt_consultant_spread TYPE STANDARD TABLE OF ty_consultant_spread .
  types:
    BEGIN OF ty_meta,
      name          TYPE char30,
      absolute_name TYPE  abap_abstypename,
      decimals      TYPE  i,
      false         TYPE  abap_bool,
      kind          TYPE  abap_typecategory,
      length        TYPE  i,
      type_kind     TYPE  abap_typekind,
    END OF ty_meta .

  data G_PROJECT type CHAR4 .
  constants C_PROJECT_AMS type CHAR4 value 'AMS' ##NO_TEXT.
  constants C_PROJECT_AD type CHAR4 value 'AD' ##NO_TEXT.
  constants C_TEMPLATE_UPLOAD type CHAR10 value 'UPLOAD' ##NO_TEXT.
  constants C_TEMPLATE_RLS type CHAR10 value 'RLS' ##NO_TEXT.
  data GT_PROJECT_INS type TT_PROJEST .
  data GS_PROJECT_KEY type ZPROJEST .
  data GT_OUTDATA type ref to DATA .
endinterface.
