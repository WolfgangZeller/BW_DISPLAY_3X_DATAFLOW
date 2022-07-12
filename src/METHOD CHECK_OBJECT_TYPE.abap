* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_BW_TOOLS=>CHECK_OBJECT_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJECT_NAME                  TYPE        STRING
* | [<-()] R_OBJECT_TYPE                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_object_type.

    SELECT 'ADSO' AS type
    FROM rsoadso
    WHERE objvers = 'A'
    AND adsonm = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

    SELECT 'ODSO' AS type
    FROM rsdodso
    WHERE objvers = 'A'
    AND odsobject = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

    SELECT 'IOBJ' AS type
    FROM rsdiobj
    WHERE objvers = 'A'
    AND iobjnm = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

    SELECT 'CUBE' AS type
    FROM rsdcube
    WHERE objvers = 'A'
    AND infocube = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

  ENDMETHOD.
