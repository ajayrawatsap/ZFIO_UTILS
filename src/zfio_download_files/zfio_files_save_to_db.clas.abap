CLASS zfio_files_save_to_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS create_files IMPORTING files TYPE zfio_tt_files RAISING zcm_fio_checks.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: files_create TYPE zfio_tt_files.
    METHODS: validate_inputs IMPORTING files TYPE zfio_tt_files RAISING zcm_fio_checks,
      save_prepare IMPORTING files TYPE zfio_tt_files RAISING zcm_fio_checks,
      save.

ENDCLASS.



CLASS zfio_files_save_to_db IMPLEMENTATION.
  METHOD create_files.
    DATA lv_uuid TYPE sysuuid_x16.

    validate_inputs( files ).

    save_prepare( files ).

    save(  ).
  ENDMETHOD.


  METHOD validate_inputs.

    IF files IS INITIAL.
      RAISE EXCEPTION NEW zcm_fio_checks( textid = zcm_fio_checks=>files_table_initial ).

    ENDIF.

    LOOP AT files INTO DATA(file).

      IF file-file_name IS INITIAL.

        RAISE EXCEPTION NEW zcm_fio_checks( textid     = zcm_fio_checks=>field_value_initial
                                            field_name = 'File Name' ).

      ENDIF.

      IF file-file_mimetype IS INITIAL.

        RAISE EXCEPTION NEW zcm_fio_checks( textid     = zcm_fio_checks=>field_value_initial
                                            field_name = 'Mime Type' ).

      ENDIF.

      IF file-file_content IS INITIAL.

        RAISE EXCEPTION NEW zcm_fio_checks( textid     = zcm_fio_checks=>field_value_initial
                                            field_name = 'File Content' ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD save_prepare.

    me->files_create = files.
    LOOP AT me->files_create ASSIGNING FIELD-SYMBOL(<file>).
      " Generate a new UUID
      TRY.
          DATA(uuid) = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error INTO DATA(uuid_error).
          RAISE EXCEPTION NEW zcm_fio_checks( textid   = zcm_fio_checks=>error_in_guid_creation
                                              previous = uuid_error ).
      ENDTRY.
      <file>-file_uuid        = uuid.
      <file>-client           = sy-mandt.
      <file>-local_created_by = sy-uname.
      <file>-local_created_at = xco_cp=>sy->moment( )->as( xco_cp_time=>format->abap )->value.

    ENDLOOP.

  ENDMETHOD.

  METHOD save.
    INSERT zfio_files FROM TABLE @me->files_create.
  ENDMETHOD.

ENDCLASS.
