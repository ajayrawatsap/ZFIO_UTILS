CLASS ltc_zfio_files_save_to_db DEFINITION DEFERRED.
CLASS zfio_files_save_to_db DEFINITION LOCAL FRIENDS ltc_zfio_files_save_to_db.

CLASS ltc_zfio_files_save_to_db DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      cut   TYPE REF TO zfio_files_save_to_db,
      files TYPE zfio_tt_files.

    METHODS:
      setup,
      test_empty_files_table      FOR TESTING,
      test_empty_file_name        FOR TESTING,
      test_empty_mimetype         FOR TESTING,
      test_empty_file_content     FOR TESTING,
      test_valid_input            FOR TESTING,


      test_save_prepare_one_entry FOR TESTING
        RAISING
          zcm_fio_checks,
      test_save_prepare_multiple  FOR TESTING
        RAISING
          zcm_fio_checks,

      test_save for testing,


      test_create_success FOR TESTING
        RAISING
          zcm_fio_checks,
      test_create_empty_table FOR TESTING,
      test_create_missing_name FOR TESTING,
      test_create_missing_mimetype FOR TESTING,
      test_files_missing_content FOR TESTING.


ENDCLASS.

CLASS ltc_zfio_files_save_to_db IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT cut.


    CLEAR files.

  ENDMETHOD.

  METHOD test_empty_files_table.
    TRY.

        cut->validate_inputs( files ).

        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Check if the correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>files_table_initial
          msg = 'Wrong exception raised for empty files table' ).
    ENDTRY.

  ENDMETHOD.

  .

  METHOD test_empty_file_name.

    " Test case: File with empty name should raise exception

    " Prepare test data
    DATA: ls_file LIKE LINE OF files.

    " File with empty name but valid mimetype and content
    ls_file-file_name = ''.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = 'AB01CD'.
    APPEND ls_file TO files.

    TRY.
        cut->validate_inputs( files ).

        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Check if the correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>field_value_initial
          msg = 'Wrong exception raised for empty file name' ).

        " Check if the field name is correct
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->field_name
          exp = 'File Name'
          msg = 'Wrong field name in exception for empty file name' ).
    ENDTRY.


  ENDMETHOD.

  METHOD test_empty_mimetype.

    " Test case: File with empty mimetype should raise exception

    " Prepare test data
    DATA: ls_file LIKE LINE OF files.

    " File with valid name but empty mimetype
    ls_file-file_name = 'test.pdf'.
    ls_file-file_mimetype = ''.
    ls_file-file_content = 'AB01CD'.
    APPEND ls_file TO files.

    TRY.
        cut->validate_inputs( files ).

        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Check if the correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>field_value_initial
          msg = 'Wrong exception raised for empty mimetype' ).

        " Check if the field name is correct
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->field_name
          exp = 'Mime Type'
          msg = 'Wrong field name in exception for empty mimetype' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_empty_file_content.

    " Prepare test data
    DATA: ls_file LIKE LINE OF files.

    " File with valid name and mimetype but empty content
    ls_file-file_name = 'test.pdf'.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = ''.
    APPEND ls_file TO files.

    TRY.
        cut->validate_inputs( files ).

        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Check if the correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>field_value_initial
          msg = 'Wrong exception raised for empty content' ).

        " Check if the field name is correct
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->field_name
          exp = 'File Content'
          msg = 'Wrong field name in exception for empty content' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_valid_input.

    DATA: ls_file LIKE LINE OF files.

    ls_file-file_name = 'test.pdf'.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = 'AB01CD'.
    APPEND ls_file TO files.

    TRY.
        " Should not raise an exception
        cut->validate_inputs( files ).

        " If we got here, the test passed
        cl_abap_unit_assert=>assert_true(
          act = abap_true
          msg = 'Validation passed for valid input' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        cl_abap_unit_assert=>fail( 'Exception raised for valid input' ).
    ENDTRY.
  ENDMETHOD.



  METHOD test_save_prepare_multiple.

    " Prepare test data with multiple files
    DATA: ls_file LIKE LINE OF files.

    " First file
    ls_file-file_name = 'test1.pdf'.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = 'ABCD'.
    APPEND ls_file TO files.

    " Second file
    ls_file-file_name = 'test2.jpg'.
    ls_file-file_mimetype = 'image/jpeg'.
    ls_file-file_content = 'ABF67161'.
    APPEND ls_file TO files.


    cut->save_prepare( files ).

    " Assertions
    cl_abap_unit_assert=>assert_equals(
      act = lines( cut->files_create )
      exp = 2
      msg = 'Two entries should be in files_create' ).

  ENDMETHOD.

  METHOD test_save_prepare_one_entry.

    " Test with one file entry

    " Prepare test data with one file
    DATA: ls_file LIKE LINE OF files.
    ls_file-file_name = 'test.pdf'.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = 'ABCF9877'.
    APPEND ls_file TO files.


    cut->save_prepare( files ).


    " Assertions
    cl_abap_unit_assert=>assert_equals(
      act = lines( cut->files_create )
      exp = 1
      msg = 'One entry should be in files_create' ).

    " Check if first entry has required fields populated
    READ TABLE cut->files_create INDEX 1 INTO DATA(ls_result).

    " Check that UUID was generated (not initial)
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-file_uuid
      msg = 'UUID should be generated' ).

    " Check client is set to current client
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-client
      exp = sy-mandt
      msg = 'Client should be set to current client' ).

    " Check username is set correctly
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-local_created_by
      exp = sy-uname
      msg = 'Username should be set to current user' ).

    " Check timestamp is set (not initial)
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-local_created_at
      msg = 'Timestamp should be generated' ).

    " Check original fields remain unchanged
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-file_name
      exp = ls_file-file_name
      msg = 'Original file name should be preserved' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-file_mimetype
      exp = ls_file-file_mimetype
      msg = 'Original mimetype should be preserved' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-file_content
      exp = ls_file-file_content
      msg = 'Original content should be preserved' ).
  ENDMETHOD.





  METHOD test_save.
    " Create test double for database operations
    " Using ABAP Test Double Framework
    DATA: lo_sql_double TYPE REF TO if_osql_test_environment.


    " Create the test environment for our table
    lo_sql_double = cl_osql_test_environment=>create(
                      i_dependency_list = VALUE #( ( 'ZFIO_FILES' ) ) ).

    " Prepare test data
    DATA: ls_file LIKE LINE OF cut->files_create.

    ls_file-file_uuid = '0123456789ABCDEF'.
    ls_file-file_name = 'test.pdf'.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = '2333ABCD'.
    ls_file-client = sy-mandt.
    ls_file-local_created_by = 'TEST_USER'.
    ls_file-local_created_at = '20250326105047'.

    APPEND ls_file TO cut->files_create.

    " Call method under test
    cut->save( ).


    SELECT * FROM zfio_files INTO TABLE @DATA(lt_result).

    " Verify data was inserted correctly
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1
      msg = 'One record should be inserted' ).

    IF lines( lt_result ) > 0.
      cl_abap_unit_assert=>assert_equals(
        act = lt_result[ 1 ]-file_uuid
        exp = ls_file-file_uuid
        msg = 'UUID should match' ).

      cl_abap_unit_assert=>assert_equals(
        act = lt_result[ 1 ]-file_name
        exp = ls_file-file_name
        msg = 'File name should match' ).
    ENDIF.

    " Clean up the test environment
    lo_sql_double->destroy( ).
  ENDMETHOD.




  METHOD test_create_empty_table.
   " Prepare empty table
    CLEAR files.

    TRY.
        " Call method under test
        cut->create_files( files ).

        " Should not reach here - method should raise exception
        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Verify correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>files_table_initial
          msg = 'Should raise exception for empty files table' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_create_missing_mimetype.

  " Test with missing file name - should raise exception

    " Prepare test data with missing file name
    DATA: ls_file TYPE zfio_files.

    ls_file-file_name = ''.  " Empty file name
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = 'ACBF4500'.
    APPEND ls_file TO files.

    TRY.
        " Call method under test
        cut->create_files( files ).

        " Should not reach here - method should raise exception
        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Verify correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>field_value_initial
          msg = 'Should raise exception for empty file name' ).

        " Verify field name in exception
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->field_name
          exp = 'File Name'
          msg = 'Exception should indicate File Name field' ).
    ENDTRY.
  ENDMETHOD.


  METHOD test_create_missing_name.
  " Test with missing file name - should raise exception

    " Prepare test data with missing file name
    DATA: ls_file like LINE OF files.

    ls_file-file_name = ''.  " Empty file name
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = 'ABC560'.
    APPEND ls_file TO files.

    TRY.
        " Call method under test
        cut->create_files( files ).

        " Should not reach here - method should raise exception
        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Verify correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>field_value_initial
          msg = 'Should raise exception for empty file name' ).

        " Verify field name in exception
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->field_name
          exp = 'File Name'
          msg = 'Exception should indicate File Name field' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_create_success.

  " Create test double for UUID generation and database operations
    DATA: lo_sql_double TYPE REF TO if_osql_test_environment.



     lo_sql_double = cl_osql_test_environment=>create(
                      i_dependency_list = VALUE #( ( 'ZFIO_FILES' ) ) ).

    " Mock UUID generation to return a predictable value
    " Note: This requires advanced test doubles for static methods
    " which may not be available in all environments
    DATA: lo_uuid TYPE REF TO cl_system_uuid.

    " Prepare test data
    DATA: ls_file TYPE zfio_files.

    ls_file-file_name = 'test.pdf'.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = 'ABCD887000'.
    APPEND ls_file TO files.

    " Call the method under test
     cut->create_files( files ).

    " Verify data was inserted correctly
    SELECT * FROM zfio_files
      WHERE file_name = 'test.pdf'
      INTO TABLE @DATA(lt_result).

    " Expect to find 1 record
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1
      msg = 'One record should be inserted' ).

    IF lines( lt_result ) > 0.
      cl_abap_unit_assert=>assert_equals(
        act = lt_result[ 1 ]-file_name
        exp = ls_file-file_name
        msg = 'File name should match' ).

      cl_abap_unit_assert=>assert_equals(
        act = lt_result[ 1 ]-file_mimetype
        exp = ls_file-file_mimetype
        msg = 'Mime type should match' ).

        cl_abap_unit_assert=>assert_equals(
        act = lt_result[ 1 ]-file_content
        exp = ls_file-file_content
        msg = 'Mime type should match' ).



    ENDIF.

    " Clean up the test environment
    lo_sql_double->destroy( ).

  ENDMETHOD.

  METHOD test_files_missing_content.

  " Test with missing content - should raise exception

    " Prepare test data with missing content
    DATA: ls_file TYPE zfio_files.

    ls_file-file_name = 'test.pdf'.
    ls_file-file_mimetype = 'application/pdf'.
    ls_file-file_content = ''.  " Empty content
    APPEND ls_file TO files.

    TRY.
        " Call method under test
        cut->create_files( files ).

        " Should not reach here - method should raise exception
        cl_abap_unit_assert=>fail( 'Exception was expected but not raised' ).
      CATCH zcm_fio_checks INTO DATA(lo_exception).
        " Verify correct exception is raised
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->if_t100_message~t100key
          exp = zcm_fio_checks=>field_value_initial
          msg = 'Should raise exception for empty content' ).

        " Verify field name in exception
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->field_name
          exp = 'File Content'
          msg = 'Exception should indicate File Content field' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
