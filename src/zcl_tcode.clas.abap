CLASS zcl_tcode DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES : BEGIN OF ty_cobj,
              field TYPE tstca-field,
              value TYPE tstca-value,
              olen  TYPE dfies-outputlen,
            END OF ty_cobj,
            tt_cobj TYPE TABLE OF ty_cobj,
            "! report transaction
            BEGIN OF ty_report,
              program_name    TYPE tstc-pgmna,
              screen_number   TYPE tstc-dypno,
              program_variant TYPE rsstcd-repo_vari,
              auth_object     TYPE tstca-objct,
              t_cobj          TYPE TABLE OF ty_cobj WITH DEFAULT KEY,
            END OF ty_report,
            "! dialog transaction
            BEGIN OF ty_dialog,
              program_name              TYPE tstc-pgmna,
              screen_number             TYPE tstc-dypno,
              allow_std_transac_variant TYPE flag,
              auth_object               TYPE tstca-objct,
              t_cobj                    TYPE TABLE OF ty_cobj WITH DEFAULT KEY,
            END OF ty_dialog,
            "! parameter transaction
            BEGIN OF ty_parameter,
              called_tcode     TYPE tstc-tcode,
              skip_init_screen TYPE flag,
              inherit_gui_attr TYPE flag,
              program_name     TYPE tstc-pgmna,
              screen_number    TYPE tstc-dypno,
              t_param          TYPE s_param,
            END OF ty_parameter,
            "! variant transaction
            BEGIN OF ty_variant,
              called_tcode     TYPE tstc-tcode,
              transac_variant  TYPE rsstcd-variant,
              cross_client     TYPE flag,
              inherit_gui_attr TYPE flag,
            END OF ty_variant,
            "! object transaction
            BEGIN OF ty_object,
              transaction_model TYPE flag,
              local_class       TYPE flag,
              global_class_name TYPE seoclsname,
              local_class_name  TYPE seoclsname,
              method_name       TYPE seocpdname,
              program_name      TYPE tstc-pgmna,
              update_mode       TYPE char01, "only for transaction model
              auth_object       TYPE tstca-objct,
              t_cobj            TYPE TABLE OF ty_cobj WITH DEFAULT KEY,
            END OF ty_object,
            ty_transaction_type TYPE char01.

    CONSTANTS: BEGIN OF c_type,
                 report    TYPE ty_transaction_type VALUE 'R',
                 dialog    TYPE ty_transaction_type VALUE 'D',
                 object    TYPE ty_transaction_type VALUE 'O',
                 menu_area TYPE ty_transaction_type VALUE 'M',
                 variant   TYPE ty_transaction_type VALUE 'V',
                 parameter TYPE ty_transaction_type VALUE 'P',
               END OF c_type.

    DATA : tcode           TYPE tstc-tcode READ-ONLY,
           type            TYPE ty_transaction_type READ-ONLY,
           s_report        TYPE ty_report READ-ONLY,
           s_dialog        TYPE ty_dialog READ-ONLY,
           s_object        TYPE ty_object READ-ONLY,
           s_parameter     TYPE ty_parameter READ-ONLY,
           s_variant       TYPE ty_variant READ-ONLY,
           locked_via_sm01 TYPE flag READ-ONLY,
           professional    TYPE flag READ-ONLY,
           easy_web        TYPE flag READ-ONLY,
           ew_service      TYPE tstcc-s_service READ-ONLY, "easy web
           ew_pervasive    TYPE tstcc-s_pervas READ-ONLY, "easy web
           gui_html        TYPE tstcc-s_webgui READ-ONLY,
           gui_win32       TYPE tstcc-s_win32 READ-ONLY,
           gui_java        TYPE tstcc-s_platin READ-ONLY.

    CLASS-METHODS load
      IMPORTING
        tcode         TYPE tcode
      RETURNING
        VALUE(result) TYPE REF TO zcl_tcode.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS _load
      IMPORTING
        tcode TYPE tcode.

ENDCLASS.



CLASS zcl_tcode IMPLEMENTATION.

  METHOD load.

    result = NEW zcl_tcode( ).
    result->_load( tcode ).

  ENDMETHOD.

  METHOD _load.

    FIELD-SYMBOLS:
      <ls_tstc>                     TYPE tstc,
      <ls_tstcc>                    TYPE tstcc,
      <lt_cobj>                     TYPE tt_cobj,
      <ls_rsstcd>                   TYPE rsstcd,
      <lt_param>                    TYPE s_param,
      <l_easy_web>                  TYPE flag,
      <l_professional>              TYPE flag,
      <l_transaction_variant_flag>  TYPE flag,
      <l_transaction_type>          TYPE tstc-cinfo,
      <l_authorization_object_flag> TYPE syhex01,
      <l_locked_via_sm01>           TYPE syhex01,
      <l_gui_inherit>               TYPE flag.
    DATA l_auth_object TYPE tstca-objct.

    PERFORM select_tstc_tables_new IN PROGRAM saplseuk
          USING tcode sy-langu sy-langu.

    ASSIGN ('(SAPLSEUK)TSTC') TO <ls_tstc>.
    ASSIGN ('(SAPLSEUK)TSTCC') TO <ls_tstcc>.
    ASSIGN ('(SAPLSEUK)COBJ[]') TO <lt_cobj>.
    ASSIGN ('(SAPLSEUK)RSSTCD') TO <ls_rsstcd>.
    ASSIGN ('(SAPLSEUK)PARAM[]') TO <lt_param>.
    ASSIGN ('(SAPLSEUK)G_IAC_EWT') TO <l_easy_web>.
    ASSIGN ('(SAPLSEUK)G_PROFI_TRAN') TO <l_professional>.
    ASSIGN ('(SAPLSEUK)PARAM_VARI') TO <l_transaction_variant_flag>.
    ASSIGN ('(SAPLSEUK)TC_TYP') TO <l_transaction_type>.
    ASSIGN ('(SAPLSEUK)TC_CHK') TO <l_authorization_object_flag>.
    ASSIGN ('(SAPLSEUK)TC_ENQ') TO <l_locked_via_sm01>.
    ASSIGN ('(SAPLSEUK)G_GUI_INHE') TO <l_gui_inherit>.

    me->tcode = tcode.

    IF NOT <l_authorization_object_flag> IS INITIAL.
      SELECT SINGLE objct FROM tstca
            INTO l_auth_object
            WHERE tcode = tcode.
    ENDIF.

    CASE <l_transaction_type>.
      WHEN '80'.
        " Report transaction
        type = c_type-report.
        s_report-program_name = <ls_tstc>-pgmna.
        s_report-screen_number = <ls_tstc>-dypno.
        s_report-program_variant = <ls_rsstcd>-repo_vari.
        s_report-auth_object = l_auth_object.
        s_report-t_cobj = <lt_cobj>.
      WHEN '00'.
        type = c_type-dialog.
        s_dialog-program_name  = <ls_tstc>-pgmna.
        s_dialog-screen_number = <ls_tstc>-dypno.
        s_dialog-allow_std_transac_variant = <ls_rsstcd>-trans_var.
        s_dialog-auth_object   = l_auth_object.
        s_dialog-t_cobj        = <lt_cobj>.
      WHEN '01'.
        type = c_type-menu_area. "menu area (obsolete transaction type)
      WHEN '08'.
        type = c_type-object.
        IF <ls_rsstcd>-call_tcode = 'OS_APPLICATION'.
          s_object-transaction_model = 'X'.
          s_object-global_class_name = <ls_rsstcd>-classname.
* Update mode is stored in TSTCP-PARM like %UPDATE_MODE=?%
          IF <ls_rsstcd>-s_upddir = 'X'.
            s_object-update_mode = 'S'.
          ELSEIF <ls_rsstcd>-s_updtask = 'X'.
            s_object-update_mode = 'A'.
          ELSEIF <ls_rsstcd>-s_updlok = 'X'.
            s_object-update_mode = 'L'.
          ENDIF.
        ELSE.
          IF NOT <ls_tstc>-pgmna IS INITIAL.
            s_object-local_class       = 'X'.
            s_object-program_name      = <ls_tstc>-pgmna.
            s_object-local_class_name  = <ls_rsstcd>-classname.
          ELSE.
            s_object-global_class_name = <ls_rsstcd>-classname.
          ENDIF.
        ENDIF.
        s_object-method_name   = <ls_rsstcd>-method.
        s_object-auth_object   = l_auth_object.
        s_object-t_cobj        = <lt_cobj>.
      WHEN '02'.
        IF <l_transaction_variant_flag> = 'X'.
          " variant transaction
          type = c_type-variant.
          s_variant-called_tcode      = <ls_rsstcd>-call_tcode.
          s_variant-transac_variant   = <ls_rsstcd>-variant.
          s_variant-cross_client      = <ls_rsstcd>-s_ind_vari.
          s_variant-inherit_gui_attr  = <l_gui_inherit>.
        ELSE.
          " parameter transaction
          type = c_type-parameter.
          s_parameter-called_tcode      = <ls_rsstcd>-call_tcode.
          s_parameter-skip_init_screen  = <ls_rsstcd>-st_skip_1.
          s_parameter-inherit_gui_attr  = <l_gui_inherit>.
          s_parameter-program_name      = <ls_tstc>-pgmna.
          s_parameter-screen_number     = <ls_tstc>-dypno.
          s_parameter-t_param           = <lt_param>.
        ENDIF.
    ENDCASE.

    IF NOT <l_locked_via_sm01> IS INITIAL.
      locked_via_sm01 = 'X'.
    ENDIF.

    professional = <l_professional>.
    easy_web = <l_easy_web>.
    ew_service   = <ls_tstcc>-s_service.
    ew_pervasive = <ls_tstcc>-s_pervas.
    gui_html     = <ls_tstcc>-s_webgui.
    gui_win32    = <ls_tstcc>-s_win32.
    gui_java     = <ls_tstcc>-s_platin.

  ENDMETHOD.

ENDCLASS.
